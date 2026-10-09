(* @archlint.module core
   @archlint.domain branch-reconcile *)

open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let materialization_failure_is_unsafe reason =
  List.mem
    [
      "materialization_revision_changed";
      "materialization_branch_changed";
      "contradictory_materialization_receipts";
    ]
    reason ~equal:String.equal

type sequencer =
  | Rebase of {
      target : string;
      original : string;
      step : string;
      head_ref : string;
      merge_heads : string list; [@yojson.default []]
    }
  | Merge of { target : string }
  | Cherry_pick of { target : string }
  | None_active
[@@deriving eq, compare, sexp_of, yojson]

type t = {
  valid : bool;
  branch : string option;
  head : string;
  staged : string list;
  unstaged : string list;
  untracked : string list;
  conflicts : string list;
  sequencer : sequencer;
}
[@@deriving eq, compare, sexp_of, yojson]

let split_nul s =
  let entries = String.split s ~on:'\000' in
  match List.rev entries with "" :: rest -> List.rev rest | _ -> entries

let of_porcelain ~branch ~head ~sequencer status =
  let rec entries acc = function
    | [] -> acc
    | record :: rest when String.length record < 4 ->
        entries { acc with valid = false } rest
    | record :: rest ->
        let x = String.get record 0 and y = String.get record 1 in
        let path = String.drop_prefix record 3 in
        let renamed =
          Char.equal x 'R' || Char.equal x 'C' || Char.equal y 'R'
          || Char.equal y 'C'
        in
        let valid_char = function
          | ' ' | 'M' | 'T' | 'A' | 'D' | 'R' | 'C' | 'U' | '?' | '!' -> true
          | _ -> false
        in
        let valid =
          acc.valid && valid_char x && valid_char y
          && Char.equal (String.get record 2) ' '
          && (not (Char.equal x ' ' && Char.equal y ' '))
          && ((not renamed) || not (List.is_empty rest))
        in
        let acc = { acc with valid } in
        let rest =
          if renamed then match rest with [] -> [] | _ :: tail -> tail
          else rest
        in
        let conflicted =
          Char.equal x 'U' || Char.equal y 'U'
          || String.equal (String.prefix record 2) "AA"
          || String.equal (String.prefix record 2) "DD"
        in
        let acc =
          if conflicted then { acc with conflicts = path :: acc.conflicts }
          else if Char.equal x '?' && Char.equal y '?' then
            { acc with untracked = path :: acc.untracked }
          else if Char.equal x '!' && Char.equal y '!' then acc
          else
            {
              acc with
              staged =
                (if Char.equal x ' ' then acc.staged else path :: acc.staged);
              unstaged =
                (if Char.equal y ' ' then acc.unstaged else path :: acc.unstaged);
            }
        in
        entries acc rest
  in
  let t =
    entries
      {
        valid = String.is_empty status || String.is_suffix status ~suffix:"\000";
        branch;
        head;
        staged = [];
        unstaged = [];
        untracked = [];
        conflicts = [];
        sequencer;
      }
      (split_nul status)
  in
  {
    t with
    staged = List.rev t.staged;
    unstaged = List.rev t.unstaged;
    untracked = List.rev t.untracked;
    conflicts = List.rev t.conflicts;
  }

let clean t =
  t.valid && List.is_empty t.staged && List.is_empty t.unstaged
  && List.is_empty t.untracked && List.is_empty t.conflicts

let repair_ready t =
  t.valid && List.is_empty t.conflicts && List.is_empty t.unstaged
  &&
  match t.sequencer with
  | Rebase _ | Merge _ | Cherry_pick _ -> true
  | None_active -> false

let sequencer_key = function
  | Rebase { target; original; step; head_ref = _; merge_heads } ->
      String.concat ~sep:":"
        ([ "rebase"; target; original; step ]
        @ List.map merge_heads ~f:(fun head -> "merge:" ^ head))
  | Merge { target } -> "merge:" ^ target
  | Cherry_pick { target } -> "cherry-pick:" ^ target
  | None_active -> "idle"

let progress_key t = sequencer_key t.sequencer

let completed_integration ~branch ~source ~target ~head ~reflog =
  let complete_lines =
    match List.rev (String.split reflog ~on:'\n') with
    | "" :: rest -> List.rev rest
    | _ :: rest -> List.rev rest
    | [] -> []
  in
  List.exists complete_lines ~f:(fun line ->
      match String.lsplit2 line ~on:'\t' with
      | None -> false
      | Some (metadata, message) -> (
          let ids = String.split metadata ~on:' ' in
          match ids with
          | before :: after :: _ ->
              String.equal before source && String.equal after head
              && (String.equal message
                    ("rebase (finish): refs/heads/" ^ branch ^ " onto " ^ target)
                 || String.is_prefix message
                      ~prefix:("merge " ^ target ^ ": Merge made by ")
                 || String.equal message ("merge " ^ target ^ ": Fast-forward")
                 )
          | [] | [ _ ] -> false))

type continuation = Blocked | Continue | Skip_empty_replay | Complete_merge
[@@deriving eq, compare, sexp_of]

let continuation t =
  if not (repair_ready t) then Blocked
  else
    match t.sequencer with
    | Rebase { merge_heads = []; _ } when List.is_empty t.staged ->
        Skip_empty_replay
    | Rebase { merge_heads = _ :: _; _ } when List.is_empty t.staged ->
        Complete_merge
    | Rebase _ | Merge _ | Cherry_pick _ | None_active -> Continue
