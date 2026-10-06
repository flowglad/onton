(* @archlint.module core
   @archlint.domain rewrite-lineage *)

open Base

type t = { branch : string; local_sha : string; remote_sha : string }

let authorizes t ~branch ~local_sha ~remote_sha =
  String.equal t.branch branch
  && String.equal t.local_sha local_sha
  && String.equal t.remote_sha remote_sha

let valid_sha s =
  (String.length s = 40 || String.length s = 64)
  && String.for_all s ~f:(function
    | '0' .. '9' | 'a' .. 'f' -> true
    | _ -> false)

let of_reflog ~branch ~local_sha ~remote_sha ~reflog ~ancestor_oracle =
  let parse line =
    match String.lsplit2 line ~on:'\t' with
    | Some (metadata, message) -> (
        match String.split metadata ~on:' ' with
        | before :: after :: _ when valid_sha before && valid_sha after ->
            Some (before, after, message)
        | _ -> None)
    | None -> None
  in
  let prefix = "rebase (finish): refs/heads/" ^ branch ^ " onto " in
  let rec walk ~expected ~rewritten entries =
    if rewritten && ancestor_oracle remote_sha ~descendant:expected then
      Some { branch; local_sha; remote_sha }
    else
      match entries with
      | Some (before, after, message) :: rest when String.equal expected after
        ->
          let rebase_target = String.chop_prefix message ~prefix in
          let completed_rebase =
            Option.value_map rebase_target ~default:false ~f:(fun target ->
                valid_sha target && ancestor_oracle target ~descendant:after)
          in
          if completed_rebase || ancestor_oracle before ~descendant:after then
            walk ~expected:before
              ~rewritten:(rewritten || completed_rebase)
              rest
          else None
      | _ -> None
  in
  if not (valid_sha local_sha && valid_sha remote_sha) then None
  else
    walk ~expected:local_sha ~rewritten:false
      (List.rev (List.map (String.split_lines reflog) ~f:parse))
