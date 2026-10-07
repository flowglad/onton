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
  && (not (String.for_all s ~f:(Char.equal '0')))
  && String.for_all s ~f:(function
    | '0' .. '9' | 'a' .. 'f' -> true
    | _ -> false)

let changes_preserved ~remote_changed_paths ~local_changed_paths
    ~target_deleted_paths ~local_deleted_paths =
  let decode paths =
    if String.is_empty paths then Some (Set.empty (module String))
    else
      Option.bind (String.chop_suffix paths ~suffix:"\000") ~f:(fun paths ->
          let paths = String.split paths ~on:'\000' in
          if List.for_all paths ~f:(fun path -> not (String.is_empty path)) then
            Some (Set.of_list (module String) paths)
          else None)
  in
  match
    ( decode remote_changed_paths,
      decode local_changed_paths,
      decode target_deleted_paths,
      decode local_deleted_paths )
  with
  | Some remote, Some local, Some target_deleted, Some local_deleted ->
      let retained_deletions = Set.inter target_deleted local_deleted in
      Set.is_empty (Set.diff (Set.inter remote local) retained_deletions)
  | _ -> false

let of_reflog ~branch ~local_sha ~remote_sha ~reflog ~ancestor_oracle
    ~content_oracle =
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
              && ancestor_oracle remote_sha ~descendant:before
              && ancestor_oracle target ~descendant:after
              && (ancestor_oracle remote_sha ~descendant:target
                 || content_oracle ~remote_sha ~target ~result_sha:after)
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
