open Base

type scope = Repo | Workflow | Read_org | Other of string
[@@deriving show, eq, sexp_of, compare]

let normalize_token s =
  let s = String.strip s in
  let s = String.lowercase s in
  (* GitHub sometimes returns scopes in a quoted comma list; strip them. *)
  let s =
    String.strip ~drop:(fun c -> Char.equal c '"' || Char.equal c '\'') s
  in
  String.strip s

let scope_of_token token =
  match normalize_token token with
  | "" -> None
  | "repo" -> Some Repo
  | "workflow" -> Some Workflow
  | "read:org" -> Some Read_org
  | other -> Some (Other other)

let parse_header header =
  if String.is_empty (String.strip header) then []
  else String.split header ~on:',' |> List.filter_map ~f:scope_of_token

let required_for_files ~paths =
  let touches_workflows =
    List.exists paths ~f:(fun p ->
        let p = String.strip p in
        String.is_prefix p ~prefix:".github/workflows/"
        || String.is_prefix p ~prefix:"./.github/workflows/")
  in
  if touches_workflows then [ Workflow ] else []

let missing ~required ~present =
  List.fold required ~init:[] ~f:(fun acc r ->
      if List.exists present ~f:(equal_scope r) then acc
      else if List.exists acc ~f:(equal_scope r) then acc
      else acc @ [ r ])

let show_scope_short = function
  | Repo -> "repo"
  | Workflow -> "workflow"
  | Read_org -> "read:org"
  | Other s -> s
