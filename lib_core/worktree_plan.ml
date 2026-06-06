(* @archlint.module core
   @archlint.domain worktree-plan *)

open Base

type op =
  | Ensure_worktree
  | Fetch_origin
  | Capture_anchor of { ref_name : string; slot : int }
  | Rebase_onto of Types.Branch.t
  | Record_anchor_on_success of { slot : int; base : Types.Branch.t }
[@@deriving show, eq, sexp_of, compare]

type t = op list [@@deriving show, eq, sexp_of, compare]

type anchor_event = Anchor_recorded of Anchor.t | Anchor_capture_failed
[@@deriving show, eq, sexp_of, compare]

let origin_of branch =
  Types.Branch.of_string
    (Printf.sprintf "origin/%s" (Types.Branch.to_string branch))

let ref_string_of branch =
  Printf.sprintf "origin/%s" (Types.Branch.to_string branch)

let for_rebase ~new_base =
  [
    Ensure_worktree;
    Fetch_origin;
    Capture_anchor { ref_name = ref_string_of new_base; slot = 0 };
    Rebase_onto (origin_of new_base);
    Record_anchor_on_success { slot = 0; base = new_base };
  ]

let for_merge_conflict ~base =
  [
    Ensure_worktree;
    Fetch_origin;
    Capture_anchor { ref_name = ref_string_of base; slot = 0 };
    Rebase_onto (origin_of base);
    Record_anchor_on_success { slot = 0; base };
  ]

let for_start ~base =
  [
    Ensure_worktree;
    Fetch_origin;
    Capture_anchor { ref_name = ref_string_of base; slot = 0 };
    Record_anchor_on_success { slot = 0; base };
  ]

let ensures_worktree_before_fs (plan : t) =
  let rec loop ensured = function
    | [] -> true
    | Ensure_worktree :: rest -> loop true rest
    | ( Fetch_origin | Capture_anchor _ | Rebase_onto _
      | Record_anchor_on_success _ )
      :: rest ->
        ensured && loop ensured rest
  in
  loop false plan
