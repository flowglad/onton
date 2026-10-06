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
  let rec walk ~expected entries =
    match entries with
    | Some (before, after, message) :: rest when String.equal expected after
      -> (
        match String.chop_prefix message ~prefix with
        | Some target ->
            if
              valid_sha target
              && ancestor_oracle remote_sha ~descendant:target
              && ancestor_oracle target ~descendant:after
              && ancestor_oracle remote_sha ~descendant:before
            then Some { branch; local_sha; remote_sha }
            else None
        | None ->
            if ancestor_oracle before ~descendant:after then
              walk ~expected:before rest
            else None)
    | _ -> None
  in
  if not (valid_sha local_sha && valid_sha remote_sha) then None
  else
    (* Git terminates each record with a newline. A concurrent append can leave
       a partial tail in the captured bytes; it is not a published record. *)
    let complete_records =
      Option.value_map (String.rsplit2 reflog ~on:'\n') ~default:""
        ~f:(fun (complete, _) -> complete)
    in
    walk ~expected:local_sha
      (List.rev (List.map (String.split_lines complete_records) ~f:parse))
