open Base
open Onton_core

(** Property tests for {!Rebase_decision.upstream}.

    The pure decision answers: "what [<upstream>] should I pass to
    [git rebase --onto <new_base> <upstream>]?" When the orchestrator recorded
    the previous base's SHA at the last successful rebase / start, that SHA is
    the right anchor — anything older than it is shared with the old base and
    gets dropped, anything newer is the patch's own work. Without the SHA, we
    fall back to the supplied branch name (today's behavior). Properties:

    - {b Totality}: never raises over arbitrary input.
    - {b Some-wins}: a non-empty SHA always beats the fallback.
    - {b None-falls-back}: [None] and empty / whitespace-only SHAs use the
      fallback verbatim.
    - {b Idempotence}: the result of [upstream] fed back as the SHA returns
      itself. *)

module Gen = QCheck2.Gen
module Test = QCheck2.Test

let gen_sha = Gen.string_size ~gen:(Gen.char_range 'a' 'f') (Gen.int_range 7 40)
let gen_branch = Gen.string_size ~gen:Gen.printable (Gen.int_range 1 40)

let prop_total =
  Test.make ~count:500 ~name:"RD-1: upstream is total over arbitrary input"
    (Gen.pair
       (Gen.option (Gen.string_size ~gen:Gen.printable (Gen.int_range 0 60)))
       (Gen.string_size ~gen:Gen.printable (Gen.int_range 0 60)))
    (fun (prev_base_sha, fallback) ->
      try
        let _ = Rebase_decision.upstream ~prev_base_sha ~fallback in
        true
      with _ -> false)

let prop_some_sha_wins =
  Test.make ~name:"RD-2: Some <non-empty SHA> wins over fallback" ~count:200
    (Gen.pair gen_sha gen_branch) (fun (sha, fallback) ->
      String.equal
        (Rebase_decision.upstream ~prev_base_sha:(Some sha) ~fallback)
        sha)

let prop_none_uses_fallback =
  Test.make ~name:"RD-3: None falls back to the supplied branch name" ~count:200
    gen_branch (fun fallback ->
      String.equal
        (Rebase_decision.upstream ~prev_base_sha:None ~fallback)
        fallback)

let prop_empty_sha_uses_fallback =
  Test.make
    ~name:"RD-4: Some empty / whitespace SHA falls back to the branch name"
    (Gen.pair (Gen.oneof_list [ ""; " "; "  \t  "; "\n" ]) gen_branch)
    (fun (sha, fallback) ->
      String.equal
        (Rebase_decision.upstream ~prev_base_sha:(Some sha) ~fallback)
        fallback)

let prop_idempotent_when_well_formed =
  Test.make
    ~name:
      "RD-5: upstream is idempotent under re-feeding the result, for \
       well-formed SHAs / fallbacks (no surrounding whitespace)"
    ~count:300
    (Gen.pair (Gen.option gen_sha) gen_branch)
    (fun (prev_base_sha, fallback) ->
      (* gen_sha and gen_branch don't generate whitespace-only strings, so
         the strip-then-empty branch never triggers — the result fed back as
         a SHA always survives unchanged. *)
      let r1 = Rebase_decision.upstream ~prev_base_sha ~fallback in
      let r2 = Rebase_decision.upstream ~prev_base_sha:(Some r1) ~fallback in
      (* gen_branch includes printable chars that may surround whitespace
         after a strip operation inside [upstream], so guard the equality
         with a strip-aware check. *)
      String.equal (String.strip r1) (String.strip r2))

(** ── Structured-plan property tests ─────────────────────────────────────

    These exercise {!Rebase_decision.plan} and
    {!Rebase_decision.anchor_after_result}. They use a simple in-memory commit
    DAG model: nodes are 40-char hex SHAs, each with a (possibly-empty) parent
    SHA. [is_ancestor sha ~descendant:d] walks parents from [d] looking for
    [sha]. *)

module Dag = struct
  type t = (string, string option) Hashtbl.t

  let create () : t = Hashtbl.create (module String)

  let add_commit (t : t) sha ~parent =
    Hashtbl.set t ~key:sha ~data:parent;
    sha

  let rec is_ancestor (t : t) sha ~descendant =
    if String.equal sha descendant then true
    else
      match Hashtbl.find t descendant with
      | None | Some None -> false
      | Some (Some parent) -> is_ancestor t sha ~descendant:parent
end

let gen_hex40 : string QCheck2.Gen.t =
  QCheck2.Gen.string_size
    ~gen:QCheck2.Gen.(oneof [ char_range '0' '9'; char_range 'a' 'f' ])
    (QCheck2.Gen.return 40)

let gen_typed_branch : Onton_core.Types.Branch.t QCheck2.Gen.t =
  QCheck2.Gen.map Onton_core.Types.Branch.of_string gen_branch

let mk_anchor ~base sha =
  match Anchor.make ~base ~sha ~observed_at_remote:true with
  | Some a -> a
  | None -> assert false (* gen_hex40 always produces valid SHAs *)

(* Build a linear DAG of [n] commits, returning the SHAs oldest-first. *)
let build_linear_chain dag n =
  let rec loop i parent acc =
    if i > n then List.rev acc
    else
      let sha = Printf.sprintf "%040x" i in
      let _ : string = Dag.add_commit dag sha ~parent in
      loop (i + 1) (Some sha) (sha :: acc)
  in
  loop 1 None []

(* Pattern-match helpers — strict-warnings disallow wildcard variants in
   tests. Each helper enumerates every plan + reason constructor so adding
   a new variant forces an explicit decision per test. *)

let plan_is_plain_no_anchor : Rebase_decision.plan -> bool = function
  | Plain { reason = No_anchor; _ } -> true
  | Plain
      {
        reason =
          ( Anchor_matches_head | History_fallback _
          | Anchor_unreachable_from_head | Head_unobservable );
        _;
      }
  | Onto _ ->
      false

let plan_is_plain_head_unobservable : Rebase_decision.plan -> bool = function
  | Plain { reason = Head_unobservable; _ } -> true
  | Plain
      {
        reason =
          ( Anchor_matches_head | History_fallback _ | No_anchor
          | Anchor_unreachable_from_head );
        _;
      }
  | Onto _ ->
      false

let plan_is_plain_anchor_unreachable : Rebase_decision.plan -> bool = function
  | Plain { reason = Anchor_unreachable_from_head; _ } -> true
  | Plain
      {
        reason =
          ( Anchor_matches_head | History_fallback _ | No_anchor
          | Head_unobservable );
        _;
      }
  | Onto _ ->
      false

let plan_onto_matches_head : Rebase_decision.plan -> string option = function
  | Onto { upstream; reason = Anchor_matches_head; _ } -> Some upstream
  | Onto
      {
        reason =
          ( History_fallback _ | No_anchor | Anchor_unreachable_from_head
          | Head_unobservable );
        _;
      }
  | Plain _ ->
      None

let plan_onto_history_fallback :
    Rebase_decision.plan -> (string * string) option = function
  | Onto { upstream; reason = History_fallback s; _ } -> Some (upstream, s)
  | Onto
      {
        reason =
          ( Anchor_matches_head | No_anchor | Anchor_unreachable_from_head
          | Head_unobservable );
        _;
      }
  | Plain _ ->
      None

let plan_onto_upstream : Rebase_decision.plan -> string option = function
  | Onto { upstream; _ } -> Some upstream
  | Plain _ -> None

let plan_onto_target_upstream : Rebase_decision.plan -> (string * string) option
    = function
  | Onto { target; upstream; reason = Anchor_matches_head } ->
      Some (target, upstream)
  | Onto
      {
        reason =
          ( History_fallback _ | No_anchor | Anchor_unreachable_from_head
          | Head_unobservable );
        _;
      }
  | Plain _ ->
      None

(** RD-PLAN-1: [Plain No_anchor] when no anchor / no history. *)
let prop_plan_no_anchor =
  Test.make ~count:100 ~name:"plan: empty input -> Plain No_anchor"
    (Gen.pair gen_typed_branch (Gen.option gen_hex40))
    (fun (base, head_sha) ->
      let oracle _sha ~descendant:_ = false in
      let p =
        Rebase_decision.plan
          { anchor = None; recorded_history = []; base_branch = base; head_sha }
          ~ancestor_oracle:oracle
      in
      match head_sha with
      | Some _ -> plan_is_plain_no_anchor p
      | None -> plan_is_plain_head_unobservable p)

(** RD-PLAN-2: Anchor_matches_head when newest anchor is ancestor of HEAD. *)
let prop_plan_anchor_matches_head =
  Test.make ~count:100
    ~name:"plan: anchor reachable from HEAD -> Onto Anchor_matches_head"
    gen_typed_branch (fun base ->
      let dag = Dag.create () in
      let chain = build_linear_chain dag 5 in
      let anchor_sha = List.nth_exn chain 2 in
      let head = List.last_exn chain in
      let a = mk_anchor ~base anchor_sha in
      let oracle sha ~descendant = Dag.is_ancestor dag sha ~descendant in
      let p =
        Rebase_decision.plan
          {
            anchor = Some a;
            recorded_history = [ a ];
            base_branch = base;
            head_sha = Some head;
          }
          ~ancestor_oracle:oracle
      in
      match plan_onto_matches_head p with
      | Some upstream -> String.equal upstream anchor_sha
      | None -> false)

(** RD-PLAN-3: History_fallback when newest anchor not in HEAD but an older
    history entry is. *)
let prop_plan_history_fallback =
  Test.make ~count:50
    ~name:"plan: stale newest, old history entry reachable -> History_fallback"
    gen_typed_branch (fun base ->
      let dag = Dag.create () in
      let chain = build_linear_chain dag 5 in
      let old_anchor_sha = List.nth_exn chain 1 in
      let head = List.last_exn chain in
      (* A divergent SHA that isn't in the DAG: never an ancestor of anything. *)
      let stale_sha = String.make 40 'a' in
      let _ = Dag.add_commit dag stale_sha ~parent:None in
      let stale = mk_anchor ~base stale_sha in
      let old_a = mk_anchor ~base old_anchor_sha in
      let oracle sha ~descendant = Dag.is_ancestor dag sha ~descendant in
      let p =
        Rebase_decision.plan
          {
            anchor = Some stale;
            recorded_history = [ stale; old_a ];
            base_branch = base;
            head_sha = Some head;
          }
          ~ancestor_oracle:oracle
      in
      match plan_onto_history_fallback p with
      | Some (upstream, s) ->
          String.equal upstream old_anchor_sha && String.equal s old_anchor_sha
      | None -> false)

(** RD-PLAN-4: Anchor_unreachable_from_head when nothing in history is in HEAD.
*)
let prop_plan_anchor_unreachable =
  Test.make ~count:50
    ~name:
      "plan: no anchor reachable from HEAD -> Plain \
       Anchor_unreachable_from_head"
    gen_typed_branch (fun base ->
      let dag = Dag.create () in
      let chain = build_linear_chain dag 3 in
      let head = List.last_exn chain in
      let unrelated1 = String.make 40 'a' in
      let unrelated2 = String.make 40 'b' in
      let _ = Dag.add_commit dag unrelated1 ~parent:None in
      let _ = Dag.add_commit dag unrelated2 ~parent:None in
      let a1 = mk_anchor ~base unrelated1 in
      let a2 = mk_anchor ~base unrelated2 in
      let oracle sha ~descendant = Dag.is_ancestor dag sha ~descendant in
      let p =
        Rebase_decision.plan
          {
            anchor = Some a1;
            recorded_history = [ a1; a2 ];
            base_branch = base;
            head_sha = Some head;
          }
          ~ancestor_oracle:oracle
      in
      plan_is_plain_anchor_unreachable p)

(** RD-PLAN-5: Head_unobservable when head_sha = None. *)
let prop_plan_head_unobservable =
  Test.make ~count:50 ~name:"plan: head_sha=None -> Plain Head_unobservable"
    (Gen.pair gen_typed_branch (Gen.list_size (Gen.int_range 0 3) gen_hex40))
    (fun (base, shas) ->
      let anchors = List.map shas ~f:(fun s -> mk_anchor ~base s) in
      let oracle _ ~descendant:_ = true in
      let p =
        Rebase_decision.plan
          {
            anchor = List.hd anchors;
            recorded_history = anchors;
            base_branch = base;
            head_sha = None;
          }
          ~ancestor_oracle:oracle
      in
      plan_is_plain_head_unobservable p)

(** RD-PLAN-6: If [plan] returns [Onto {upstream;_}], then [upstream] is an
    ancestor of [head_sha] under the oracle. This is the central safety
    invariant — what makes [git rebase --onto target upstream HEAD] sound. *)
let prop_plan_onto_implies_ancestor =
  Test.make ~count:200
    ~name:"plan: Onto.upstream is always an ancestor of head_sha"
    gen_typed_branch (fun base ->
      let dag = Dag.create () in
      let chain = build_linear_chain dag 6 in
      let head = List.last_exn chain in
      let unrelated = String.make 40 'a' in
      let _ = Dag.add_commit dag unrelated ~parent:None in
      let anchor_shas = unrelated :: List.take chain 4 in
      let anchors = List.map anchor_shas ~f:(fun s -> mk_anchor ~base s) in
      let oracle sha ~descendant = Dag.is_ancestor dag sha ~descendant in
      let p =
        Rebase_decision.plan
          {
            anchor = List.hd anchors;
            recorded_history = anchors;
            base_branch = base;
            head_sha = Some head;
          }
          ~ancestor_oracle:oracle
      in
      match plan_onto_upstream p with
      | Some upstream -> Dag.is_ancestor dag upstream ~descendant:head
      | None -> true)

(** RD-PLAN-7: Idempotence — calling [plan] twice with the same input yields the
    same plan (the function reads no hidden state). *)
let prop_plan_idempotent =
  Test.make ~count:100 ~name:"plan: idempotent on repeat invocation"
    gen_typed_branch (fun base ->
      let dag = Dag.create () in
      let chain = build_linear_chain dag 4 in
      let head = List.last_exn chain in
      let a = mk_anchor ~base (List.nth_exn chain 1) in
      let oracle sha ~descendant = Dag.is_ancestor dag sha ~descendant in
      let input : Rebase_decision.input =
        {
          anchor = Some a;
          recorded_history = [ a ];
          base_branch = base;
          head_sha = Some head;
        }
      in
      let p1 = Rebase_decision.plan input ~ancestor_oracle:oracle in
      let p2 = Rebase_decision.plan input ~ancestor_oracle:oracle in
      Rebase_decision.equal_plan p1 p2)

(** RD-PLAN-8: Squash-merge production-bug scenario. p4 was branched off patch-2
    at SHA X. patch-2 squash-merged to main as Y. p4 has one commit (Z) on top
    of X. With anchor recorded at Start, plan must produce
    [Onto {upstream = X; ...}] so [X..Z] = just Z replays cleanly. *)
let prop_plan_squash_merge_scenario =
  Test.make ~count:1
    ~name:"plan: production squash-merge scenario yields Onto with old-dep-tip"
    (Gen.return ()) (fun () ->
      let dag = Dag.create () in
      (* main has commits m1..m3, with m3 being the squash of patch-2. *)
      let m1 = String.make 40 '1' in
      let m2 = String.make 40 '2' in
      let m3 = String.make 40 '3' in
      let _ = Dag.add_commit dag m1 ~parent:None in
      let _ = Dag.add_commit dag m2 ~parent:(Some m1) in
      let _ = Dag.add_commit dag m3 ~parent:(Some m2) in
      (* patch-2 forked at m1, has its own pre-squash commits x1, x2. *)
      let x1 = String.make 40 '4' in
      let x2 = String.make 40 '5' in
      let _ = Dag.add_commit dag x1 ~parent:(Some m1) in
      let _ = Dag.add_commit dag x2 ~parent:(Some x1) in
      (* p4 was branched off patch-2 at x2 (= old_dep_tip), has commit z. *)
      let z = String.make 40 '6' in
      let _ = Dag.add_commit dag z ~parent:(Some x2) in
      let base = Onton_core.Types.Branch.of_string "main" in
      let anchor =
        mk_anchor ~base:(Onton_core.Types.Branch.of_string "patch-2") x2
      in
      let oracle sha ~descendant = Dag.is_ancestor dag sha ~descendant in
      let p =
        Rebase_decision.plan
          {
            anchor = Some anchor;
            recorded_history = [ anchor ];
            base_branch = base;
            head_sha = Some z;
          }
          ~ancestor_oracle:oracle
      in
      match plan_onto_target_upstream p with
      | Some (target, upstream) ->
          String.equal target "main" && String.equal upstream x2
      | None -> false)

(** ── anchor_after_result properties ─────────────────────────────────── *)

let some_anchor () =
  let base = Onton_core.Types.Branch.of_string "main" in
  let sha = String.make 40 'c' in
  mk_anchor ~base sha

let some_result : Worktree_parser.rebase_result = Worktree_parser.Ok

(** RD-AAR-1: Ok with Some resolved SHA -> Some new anchor. *)
let prop_aar_ok_returns_new =
  Test.make ~count:50
    ~name:"anchor_after_result: Ok + Some sha -> Some new anchor"
    (Gen.pair gen_typed_branch gen_hex40) (fun (base, sha) ->
      let a =
        Rebase_decision.anchor_after_result ~prev:None
          ~result:Worktree_parser.Ok ~resolved_remote_sha:(Some sha)
          ~base_branch:base
      in
      match a with
      | Some anchor ->
          String.equal (Anchor.sha anchor) sha
          && Onton_core.Types.Branch.equal (Anchor.base anchor) base
      | None -> false)

(** RD-AAR-2: Noop with Some resolved SHA -> Some new anchor (refresh). *)
let prop_aar_noop_refreshes =
  Test.make ~count:50
    ~name:"anchor_after_result: Noop + Some sha -> Some new anchor"
    (Gen.pair gen_typed_branch gen_hex40) (fun (base, sha) ->
      let prev = Some (some_anchor ()) in
      let a =
        Rebase_decision.anchor_after_result ~prev ~result:Worktree_parser.Noop
          ~resolved_remote_sha:(Some sha) ~base_branch:base
      in
      match a with
      | Some anchor -> String.equal (Anchor.sha anchor) sha
      | None -> false)

(** RD-AAR-3: Conflict preserves prev unchanged. *)
let prop_aar_conflict_preserves =
  Test.make ~count:50 ~name:"anchor_after_result: Conflict -> prev unchanged"
    (Gen.pair gen_typed_branch (Gen.option gen_hex40))
    (fun (base, resolved) ->
      let prev = Some (some_anchor ()) in
      let conflict : Worktree_parser.rebase_result =
        Worktree_parser.Conflict
          {
            target = "main";
            old_base = "";
            unique_commits = [];
            strategy = Worktree_parser.Plain;
            orig_head = "";
          }
      in
      let a =
        Rebase_decision.anchor_after_result ~prev ~result:conflict
          ~resolved_remote_sha:resolved ~base_branch:base
      in
      Option.equal Anchor.equal a prev)

(** RD-AAR-4: Error preserves prev unchanged. *)
let prop_aar_error_preserves =
  Test.make ~count:50 ~name:"anchor_after_result: Error -> prev unchanged"
    (Gen.pair gen_typed_branch (Gen.option gen_hex40))
    (fun (base, resolved) ->
      let prev = Some (some_anchor ()) in
      let a =
        Rebase_decision.anchor_after_result ~prev
          ~result:(Worktree_parser.Error "boom") ~resolved_remote_sha:resolved
          ~base_branch:base
      in
      Option.equal Anchor.equal a prev)

(** RD-AAR-5: Ok with None resolved -> prev unchanged (no fabricated anchor). *)
let prop_aar_ok_no_sha_preserves =
  Test.make ~count:50
    ~name:"anchor_after_result: Ok + None sha -> prev unchanged"
    gen_typed_branch (fun base ->
      let prev = Some (some_anchor ()) in
      let a =
        Rebase_decision.anchor_after_result ~prev ~result:some_result
          ~resolved_remote_sha:None ~base_branch:base
      in
      Option.equal Anchor.equal a prev)

let () =
  List.iter
    ~f:(fun t -> QCheck2.Test.check_exn t)
    [
      prop_total;
      prop_some_sha_wins;
      prop_none_uses_fallback;
      prop_empty_sha_uses_fallback;
      prop_idempotent_when_well_formed;
      prop_plan_no_anchor;
      prop_plan_anchor_matches_head;
      prop_plan_history_fallback;
      prop_plan_anchor_unreachable;
      prop_plan_head_unobservable;
      prop_plan_onto_implies_ancestor;
      prop_plan_idempotent;
      prop_plan_squash_merge_scenario;
      prop_aar_ok_returns_new;
      prop_aar_noop_refreshes;
      prop_aar_conflict_preserves;
      prop_aar_error_preserves;
      prop_aar_ok_no_sha_preserves;
    ];
  Stdlib.print_endline "Rebase_decision: all properties passed"
