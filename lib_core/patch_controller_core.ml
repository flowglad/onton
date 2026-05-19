open Base

type start_mode = Naive | Greedy [@@deriving show, eq, sexp_of]

let start_mode_to_string = function Naive -> "naive" | Greedy -> "greedy"

let start_mode_of_string s =
  match String.lowercase (String.strip s) with
  | "naive" -> Ok Naive
  | "greedy" -> Ok Greedy
  | other ->
      Error
        (Printf.sprintf
           "invalid start mode %S; expected \"naive\" or \"greedy\"" other)

let start_deps_satisfied graph patch_id ~has_merged ~has_pr ~start_mode =
  match start_mode with
  | Greedy -> Graph.deps_satisfied graph patch_id ~has_merged ~has_pr
  | Naive ->
      Graph.deps graph patch_id |> List.for_all ~f:(fun dep -> has_merged dep)
