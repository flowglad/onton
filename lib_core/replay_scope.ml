(* @archlint.module core
   @archlint.domain branch-reconcile *)

open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t = {
  boundary : string;
  source : string;
  commits : string list;
  omitted_merges : string list;
}
[@@deriving eq, compare, sexp_of]

type error =
  | Missing_boundary
  | Invalid_history
  | Nonlinear_history
  | Disconnected_history
[@@deriving eq, compare, sexp_of]

let valid_oid value =
  (String.length value = 40 || String.length value = 64)
  && (not (String.for_all value ~f:(Char.equal '0')))
  && String.for_all value ~f:(function
    | '0' .. '9' | 'a' .. 'f' -> true
    | _ -> false)

let capture_history ~omit_merges ~boundary ~source history =
  match boundary with
  | None -> Error Missing_boundary
  | Some boundary ->
      if
        (not (valid_oid boundary && valid_oid source))
        || not (String.is_empty history || String.is_suffix history ~suffix:"\n")
      then Error Invalid_history
      else
        let lines = String.split_lines history in
        let rec walk expected seen commits omitted_merges = function
          | [] ->
              if String.equal expected boundary then
                Ok { boundary; source; commits; omitted_merges }
              else Error Disconnected_history
          | line :: rest -> (
              match String.split line ~on:'\000' with
              | [ revision; parents ] -> (
                  let parents = String.split parents ~on:' ' in
                  if
                    (not (valid_oid revision))
                    || not (List.for_all parents ~f:valid_oid)
                  then Error Invalid_history
                  else if
                    (not (String.equal revision expected))
                    || String.equal revision boundary
                    || Set.mem seen revision
                  then Error Disconnected_history
                  else
                    match parents with
                    | [ parent ] ->
                        walk parent (Set.add seen revision)
                          (revision :: commits) omitted_merges rest
                    | parent :: _ :: _ when omit_merges ->
                        walk parent (Set.add seen revision) commits
                          (revision :: omitted_merges)
                          rest
                    | [] | _ :: _ :: _ -> Error Nonlinear_history)
              | _ -> Error Invalid_history)
        in
        walk source (Set.empty (module String)) [] [] lines

let capture = capture_history ~omit_merges:false
let capture_first_parent = capture_history ~omit_merges:true

let error_reason = function
  | Missing_boundary -> "replay_scope_missing_boundary"
  | Invalid_history -> "replay_scope_invalid_history"
  | Nonlinear_history -> "replay_scope_foreign_ancestry"
  | Disconnected_history -> "replay_scope_disconnected_history"

let todo_allowed scope text =
  let rec walk previous = function
    | [] -> true
    | line :: rest -> (
        let line = String.strip line in
        if
          String.is_empty line
          || String.is_prefix line ~prefix:"#"
          || String.equal line "noop"
        then walk previous rest
        else
          match
            String.split_on_chars line ~on:[ ' '; '\t' ]
            |> List.filter ~f:(fun word -> not (String.is_empty word))
          with
          | ( "pick" | "p" | "reword" | "r" | "edit" | "e" | "drop" | "d"
            | "squash" | "s" | "fixup" | "f" )
            :: prefix :: _
            when String.length prefix >= 4 -> (
              let matching =
                List.filter_mapi scope.commits ~f:(fun index revision ->
                    if String.is_prefix revision ~prefix then Some index
                    else None)
              in
              match matching with
              | [ index ] when index > previous -> walk index rest
              | [] | [ _ ] | _ :: _ :: _ -> false)
          | _ -> false)
  in
  walk (-1) (String.split_lines text)

type request =
  | Unproven
  | Identity of string
  | Replay of { source : string; boundary : string; target : string }
  | Merge of { source : string; target : string }
[@@deriving eq, compare, sexp_of, yojson]

let request_of_yojson json = try request_of_yojson json with _ -> Unproven

let valid_request = function
  | Unproven -> false
  | Identity source -> valid_oid source
  | Replay { source; boundary; target } ->
      valid_oid source && valid_oid boundary && valid_oid target
  | Merge { source; target } -> valid_oid source && valid_oid target

let extend_local request extension =
  if
    (not (valid_request request))
    || not (List.is_empty extension.omitted_merges)
  then None
  else
    match request with
    | Identity source when String.equal source extension.boundary ->
        Some (Identity extension.source)
    | Replay scope when String.equal scope.source extension.boundary ->
        Some (Replay { scope with source = extension.source })
    | Merge scope when String.equal scope.source extension.boundary ->
        Some (Merge { scope with source = extension.source })
    | Unproven | Identity _ | Replay _ | Merge _ -> None

let revisions = function
  | Unproven -> []
  | Identity source -> [ source ]
  | Replay { source; boundary; target } -> [ source; boundary; target ]
  | Merge { source; target } -> [ source; target ]

let rebase_continuation request ~scope ~original ~target ~current ~todo =
  match request with
  | Replay expected ->
      String.equal original expected.source
      && String.equal target expected.target
      && String.equal scope.source expected.source
      && String.equal scope.boundary expected.boundary
      && Option.exists current ~f:(fun revision ->
          List.mem scope.commits revision ~equal:String.equal)
      && todo_allowed scope todo
  | Unproven | Identity _ | Merge _ -> false

let merge_continuation request ~head ~target =
  match request with
  | Merge expected ->
      String.equal head expected.source && String.equal target expected.target
  | Unproven | Identity _ | Replay _ -> false

type tree_plan = {
  request : request;
  expected_tree : string;
  conflicts : string list;
}
[@@deriving eq, compare, sexp_of]

let nul_records text =
  if String.is_empty text then Some []
  else
    Option.bind (String.chop_suffix text ~suffix:"\000") ~f:(fun body ->
        let records = String.split body ~on:'\000' in
        if List.exists records ~f:String.is_empty then None else Some records)

let prediction request ~status output =
  match nul_records output with
  | Some (expected_tree :: conflicts)
    when valid_request request && valid_oid expected_tree
         && ((status = 0 && List.is_empty conflicts)
            || (status = 1 && not (List.is_empty conflicts))) ->
      Ok
        {
          request;
          expected_tree;
          conflicts = List.dedup_and_sort conflicts ~compare:String.compare;
        }
  | Some _ | None -> Error "replay_scope_invalid_tree_prediction"

let prepare scope ~target ~status output =
  prediction
    (Replay { source = scope.source; boundary = scope.boundary; target })
    ~status output

let prepare_composed scope ~target ~initial_tree ~predictions =
  let request =
    Replay { source = scope.source; boundary = scope.boundary; target }
  in
  let rec collect remaining tree conflicts = function
    | [] when List.is_empty remaining ->
        if valid_request request && valid_oid tree then
          Ok
            {
              request;
              expected_tree = tree;
              conflicts = List.dedup_and_sort conflicts ~compare:String.compare;
            }
        else Error "replay_scope_invalid_tree_prediction"
    | (origin, status, output) :: rest -> (
        match remaining with
        | expected :: tail when String.equal origin expected -> (
            match prediction request ~status output with
            | Error _ as error -> error
            | Ok plan ->
                collect tail plan.expected_tree
                  (plan.conflicts @ conflicts)
                  rest)
        | [] | _ :: _ -> Error "replay_scope_prediction_origin_changed")
    | [] -> Error "replay_scope_incomplete_prediction"
  in
  if not (valid_oid initial_tree) then
    Error "replay_scope_invalid_tree_prediction"
  else collect scope.commits initial_tree [] predictions

let prepare_merge ~source ~target ~status output =
  prediction (Merge { source; target }) ~status output

let prepare_identity ~source ~tree =
  prediction (Identity source) ~status:0 (tree ^ "\000")

type verified = { request : request; candidate : string; tree : string }
[@@deriving eq, compare, sexp_of]

let merge_history ~source ~target ~candidate history =
  if String.is_empty history then
    String.equal candidate source || String.equal candidate target
  else if not (String.is_suffix history ~suffix:"\n") then false
  else
    let rec walk expected seen = function
      | [] -> false
      | line :: rest -> (
          match String.split line ~on:'\000' with
          | [ revision; parents ]
            when valid_oid revision
                 && String.equal revision expected
                 && not (Set.mem seen revision) -> (
              match String.split parents ~on:' ' with
              | [ first; second ] ->
                  List.is_empty rest
                  && ((String.equal first source && String.equal second target)
                     || (String.equal first target && String.equal second source)
                     )
              | [ parent ] when valid_oid parent ->
                  walk parent (Set.add seen revision) rest
              | _ -> false)
          | _ -> false)
    in
    walk candidate (Set.empty (module String)) (String.split_lines history)

let verify (plan : tree_plan) ~candidate ~tree ~history ~changed_paths =
  let topology =
    match plan.request with
    | Unproven -> false
    | Identity source ->
        String.equal candidate source && String.is_empty history
    | Replay { target; _ } ->
        Result.is_ok (capture ~boundary:(Some target) ~source:candidate history)
    | Merge { source; target } ->
        merge_history ~source ~target ~candidate history
  in
  if not (valid_oid candidate && valid_oid tree) then
    Error "replay_scope_invalid_candidate_tree"
  else if not topology then Error "replay_scope_foreign_ancestry"
  else
    match nul_records changed_paths with
    | None -> Error "replay_scope_invalid_tree_diff"
    | Some paths ->
        if Bool.(String.equal tree plan.expected_tree <> List.is_empty paths)
        then Error "replay_scope_inconsistent_tree_diff"
        else if
          not
            (Set.is_subset
               (Set.of_list (module String) paths)
               ~of_:(Set.of_list (module String) plan.conflicts))
        then Error "replay_scope_unrelated_changes"
        else Ok { request = plan.request; candidate; tree }

let matches_request (verified : verified) ~request ~candidate =
  valid_request request
  && equal_request verified.request request
  && String.equal verified.candidate candidate

let matches (verified : verified) ~source ~boundary ~target ~candidate =
  matches_request verified
    ~request:(Replay { source; boundary; target })
    ~candidate
