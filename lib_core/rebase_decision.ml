open Base

(* ── Structured plan ─────────────────────────────────────────────────── *)

type reason =
  | Anchor_matches_head
  | History_fallback of string
  | No_anchor
  | Anchor_unreachable_from_head
  | Head_unobservable
[@@deriving show, eq, sexp_of, compare]

type plan =
  | Onto of { target : string; upstream : string; reason : reason }
  | Plain of { target : string; reason : reason }
[@@deriving show, eq, sexp_of, compare]

type input = {
  anchor : Anchor.t option;
  recorded_history : Anchor.t list;
  base_branch : Types.Branch.t;
  head_sha : string option;
}

(* Canonical, newest-first SHA candidate list. The newest anchor is preferred
   and duplicates are removed so [List.hd candidates] is unambiguously "the
   primary anchor" for the [Anchor_matches_head] vs [History_fallback]
   reason distinction. *)
let candidates_of (input : input) : string list =
  let from_history = List.map input.recorded_history ~f:Anchor.sha in
  let with_primary =
    match input.anchor with
    | None -> from_history
    | Some a ->
        let asha = Anchor.sha a in
        if List.mem from_history asha ~equal:String.equal then
          asha
          :: List.filter from_history ~f:(fun s -> not (String.equal s asha))
        else asha :: from_history
  in
  List.remove_consecutive_duplicates with_primary ~equal:String.equal

let plan input ~ancestor_oracle =
  let target = Types.Branch.to_string input.base_branch in
  let candidates = candidates_of input in
  match input.head_sha with
  | None -> Plain { target; reason = Head_unobservable }
  | Some head -> (
      let reachable =
        List.find candidates ~f:(fun sha ->
            ancestor_oracle sha ~descendant:head)
      in
      match reachable with
      | None -> (
          match candidates with
          | [] -> Plain { target; reason = No_anchor }
          | _ -> Plain { target; reason = Anchor_unreachable_from_head })
      | Some sha ->
          let primary =
            match List.hd candidates with
            | Some h -> String.equal h sha
            | None -> false
          in
          if primary then
            Onto { target; upstream = sha; reason = Anchor_matches_head }
          else Onto { target; upstream = sha; reason = History_fallback sha })

let anchor_after_result ~prev ~(result : Worktree_parser.rebase_result)
    ~resolved_remote_sha ~base_branch =
  match result with
  | Worktree_parser.Conflict _ | Worktree_parser.Error _ -> prev
  | Worktree_parser.Ok | Worktree_parser.Noop -> (
      match resolved_remote_sha with
      | None -> prev
      | Some sha -> (
          match Anchor.make ~base:base_branch ~sha ~observed_at_remote:true with
          | Some a -> Some a
          | None -> prev))
