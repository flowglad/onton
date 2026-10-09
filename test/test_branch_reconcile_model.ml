(* @archlint.module test
   @archlint.domain branch-reconcile *)

(* The oracle models a repository, not the owner's phases. It interprets commands
   against independent durable Git facts; no transition is selected by reading
   an operation phase. Generated faults are followed by a fair, healthy suffix. *)
open Onton_core
module B = Branch_reconcile
module Gen = QCheck2.Gen

let commit c =
  match B.Commit.make (String.make 40 c) with
  | Some value -> value
  | None -> assert false

let source = commit 'a'
let base = commit 'b'
let integrated = commit 'c'
let destination = B.Remote_id.of_destination "model-origin"

type repository = {
  head : B.Commit.t;
  remote : B.Commit.t option;
  pinned : bool;
  integrated : bool;
  mutations : int;
}

type fault =
  | Advance
  | Lose_ack
  | Restart
  | Duplicate
  | Request
  | Compete of int
  | Purpose of int
  | Outage
  | Wake

let show_fault = function
  | Advance -> "advance"
  | Lose_ack -> "lose-ack"
  | Restart -> "restart"
  | Duplicate -> "duplicate"
  | Request -> "request"
  | Compete n -> "compete-" ^ string_of_int n
  | Purpose n -> "purpose-" ^ string_of_int n
  | Outage -> "outage"
  | Wake -> "wake"

let observation (repo : repository) : B.observation =
  {
    destination;
    head = repo.head;
    source = repo.head;
    target = base;
    remote = repo.remote;
    boundary = Recorded source;
    topology = (if repo.remote = Some repo.head then Equal else Diverged);
    clean = true;
    sequencer = None;
    conflicts = 0;
    target_included = repo.integrated;
    base_contains_source = false;
    completed_integration = repo.integrated;
  }

let execute ~captured ~verification repo (command : B.command) =
  let observe () =
    let observed = observation repo in
    if verification then { observed with B.target = repo.head } else observed
  in
  match command.kind with
  | B.Observe -> (repo, B.Observed (observe ()))
  | B.Inspect ->
      ( { repo with pinned = repo.pinned || captured },
        if captured then B.Inspected (observe ()) else B.Observed (observe ())
      )
  | B.Pin { source = captured; target } ->
      assert (
        captured = repo.head
        && target = if verification then repo.head else base);
      ({ repo with pinned = true }, B.Pinned)
  | B.Integrate { source = captured; target; policy = _; boundary = _ } ->
      assert (repo.pinned && captured = repo.head && target = base);
      assert (not repo.integrated);
      ( {
          repo with
          head = integrated;
          integrated = true;
          mutations = repo.mutations + 1;
        },
        B.Integrated integrated )
  | B.Publish { candidate; expected } ->
      assert (repo.pinned && repo.integrated && candidate = repo.head);
      assert (expected = repo.remote);
      ( { repo with remote = Some candidate; mutations = repo.mutations + 1 },
        B.Published )
  | B.Confirm candidate ->
      assert (candidate = integrated);
      ( repo,
        B.Remote
          {
            sha = repo.remote;
            topology =
              (if repo.remote = Some candidate then Equal else Diverged);
          } )
  | B.Commit_merge _ | B.Plan_remote_replay _ | B.Checkout_remote _
  | B.Verify_recovery | B.Continue _ ->
      failwith "healthy linear repository requires no repair or remote replay"

let roundtrip state =
  match B.decode (B.yojson_of_t state) with
  | Ok restored ->
      assert (B.equal state restored);
      restored
  | Error reason -> failwith reason

let scenario ?(materialize_first = true) ?(observe_settled = fun _ -> ()) policy
    faults =
  let intent = B.{ base = "main"; policy; purpose = Reconcile_base } in
  let materialization = B.Materialized (B.New_branch source) in
  let request = B.Request intent in
  let events =
    if materialize_first then [ materialization; request ]
    else [ request; materialization ]
  in
  let state =
    List.fold_left
      (fun state event -> fst (B.step (roundtrip state) event))
      B.empty events
  in
  let initial_repo =
    {
      head = source;
      remote = Some source;
      pinned = false;
      integrated = false;
      mutations = 0;
    }
  in
  let desired = ref intent in
  let now = ref 0. in
  let history = ref [] in
  let published_operations = ref [] in
  let step state event = fst (B.step state event) in
  let act (state, repo) fault =
    now := !now +. 1000.;
    let state, repo =
      match fault with
      | Request ->
          let next, effects = B.step state (B.Request !desired) in
          assert (B.equal state next && effects = []);
          (next, repo)
      | Compete n | Purpose n ->
          let identity = "request-" ^ string_of_int n in
          let purpose =
            match fault with
            | Purpose _ -> (
                match n mod 8 with
                | 0 -> B.Reconcile_base
                | 1 ->
                    B.Reconcile_scoped
                      {
                        request = identity;
                        project = "model";
                        ancestors = [ Types.Patch_id.of_string "dependency" ];
                      }
                | 2 -> B.Publish_session identity
                | 4 -> B.Publish_revision integrated
                | 5 -> B.Verify_publication
                | 7 -> B.Provision_checkout identity
                | 6 ->
                    B.Integrate_revision
                      { contributor = identity; revision = base }
                | _ -> B.Reconcile_request identity)
            | Compete _ | Advance | Lose_ack | Restart | Duplicate | Request
            | Outage | Wake ->
                B.Reconcile_request identity
          in
          let requested =
            {
              intent with
              purpose;
              policy =
                (match purpose with
                | B.Verify_publication | B.Integrate_revision _ ->
                    B.Preserve_ancestry
                | B.Provision_checkout _ | B.Reconcile_base
                | B.Reconcile_request _ | B.Reconcile_scoped _
                | B.Publish_revision _ | B.Publish_session _ ->
                    intent.policy);
            }
          in
          let next, effects = B.step state (B.Request requested) in
          if B.is_unsettled state then (
            assert (effects = []);
            assert (B.operation next = B.operation state);
            assert (B.pending next = B.pending state));
          desired := requested;
          (next, repo)
      | Duplicate ->
          let state =
            List.fold_left
              (fun state (token, event) ->
                let current =
                  match B.pending state with
                  | Some command -> B.equal_token token command.token
                  | None -> false
                in
                let next, effects = B.step state event in
                if not current then assert (B.equal state next && effects = []);
                next)
              state !history
          in
          (state, repo)
      | Restart -> (step (roundtrip state) B.Recover, repo)
      | Wake -> (step state (B.Tick !now), repo)
      | Advance | Lose_ack | Outage -> (
          match B.pending state with
          | None ->
              ( step state
                  (Option.value
                     (B.wake_event ~at:!now state)
                     ~default:(B.Tick !now)),
                repo )
          | Some command ->
              let repo, result =
                if fault = Outage then
                  ( repo,
                    B.Retryable { reason = "model outage"; retry_after = None }
                  )
                else
                  let () =
                    match command.B.kind with
                    | B.Publish _ ->
                        assert (
                          not
                            (List.mem command.token.operation
                               !published_operations));
                        published_operations :=
                          command.token.operation :: !published_operations
                    | B.Observe | B.Pin _ | B.Integrate _ | B.Inspect
                    | B.Confirm _ | B.Commit_merge _ | B.Plan_remote_replay _
                    | B.Checkout_remote _ | B.Verify_recovery | B.Continue _ ->
                        ()
                  in
                  let captured =
                    match B.operation state with
                    | Some op -> Option.is_some op.source
                    | None -> false
                  in
                  let verification =
                    match B.operation state with
                    | Some op -> op.intent.purpose = B.Verify_publication
                    | None -> false
                  in
                  let provisioning =
                    match B.operation state with
                    | Some op -> B.is_provisioning op.intent.purpose
                    | None -> false
                  in
                  let next_repo, result =
                    if provisioning then
                      match command.kind with
                      | B.Observe | B.Inspect -> (repo, B.Checkout_ready)
                      | B.Pin _ | B.Integrate _ | B.Publish _ | B.Confirm _
                      | B.Commit_merge _ | B.Plan_remote_replay _
                      | B.Checkout_remote _ | B.Verify_recovery | B.Continue _
                        ->
                          failwith
                            "provisioning attempted a reconciliation mutation"
                    else execute ~captured ~verification repo command
                  in
                  (match B.operation state with
                  | Some op
                    when provisioning
                         || op.intent.purpose = B.Verify_publication ->
                      assert (
                        next_repo.head = repo.head
                        && next_repo.remote = repo.remote
                        && next_repo.mutations = repo.mutations)
                  | Some _ | None -> ());
                  (next_repo, result)
              in
              let event =
                B.Result { token = command.token; at = !now; result }
              in
              if fault = Lose_ack then (
                history := (command.token, event) :: !history;
                (step (roundtrip state) B.Recover, repo))
              else
                let state = step state event in
                history := (command.token, event) :: !history;
                (state, repo))
    in
    assert (
      B.publication_status state <> `Published || repo.remote = Some integrated);
    (match B.operation state with
    | Some op ->
        assert (
          match op.repair with
          | None | Some { mode = B.Diagnosis _; _ } -> true
          | Some { mode = B.Content_repair | B.History_recovery _; _ } -> false)
    | None -> ());
    (state, repo)
  in
  let state, repo = List.fold_left act (state, initial_repo) faults in
  let rec settle state repo remaining =
    if
      B.phase state = Some B.Settled
      &&
      match B.operation state with
      | Some op -> B.equal_intent op.intent !desired
      | None -> false
    then (state, repo)
    else if remaining = 0 then failwith "healthy suffix failed to converge"
    else
      let state, repo = act (state, repo) Advance in
      settle state repo (remaining - 1)
  in
  let state, repo = settle state repo 30 in
  assert (repo.head = integrated && repo.remote = Some integrated);
  let publication_intents =
    List.fold_left
      (fun count -> function
        | Purpose n when n mod 8 = 2 || n mod 8 = 4 -> count + 1
        | Purpose _ | Compete _ | Advance | Lose_ack | Restart | Duplicate
        | Request | Outage | Wake ->
            count)
      0 faults
  in
  assert (repo.mutations >= 2 && repo.mutations <= 2 + publication_intents);
  assert (repo.mutations = 1 + List.length !published_operations);
  (match B.operation state with
  | Some op -> assert (B.equal_intent op.intent !desired)
  | None -> failwith "latest request lost its settled operation");
  let settled, effects = B.step state (B.Request !desired) in
  assert (B.equal state settled && effects = []);
  let settled, effects = B.step (roundtrip state) (B.Tick !now) in
  assert (B.equal state settled && effects = []);
  observe_settled repo;
  true

module Conflicting_sequencer = struct
  (* Repository facts advance independently of owner phases. Each continuation
     consumes one staged resolution and exposes the next conflicting commit. *)
  let scenario policy steps actions =
    let repo =
      ref
        {
          head = source;
          remote = Some source;
          pinned = false;
          integrated = false;
          mutations = 0;
        }
    in
    let cursor = ref 0 in
    let active = ref false in
    let staged = ref false in
    let starts = ref 0 in
    let continuations = ref 0 in
    let offered = ref None in
    let history = ref [] in
    let now = ref 0. in
    let sequencer () = "sequence:" ^ string_of_int !cursor in
    let observe () =
      let o = observation !repo in
      {
        o with
        B.sequencer = (if !active then Some (sequencer ()) else None);
        conflicts = (if !active && not !staged then 1 else 0);
        clean = not !active;
      }
    in
    let step state event =
      let next, effects = B.step state event in
      List.iter
        (function
          | B.Repair token -> offered := Some token
          | B.Execute _ | B.Start_repair _ | B.Completed _ -> ())
        effects;
      next
    in
    let state = step B.empty (B.Materialized (B.New_branch source)) in
    let state =
      step state
        (B.Request B.{ base = "main"; policy; purpose = Reconcile_base })
    in
    let execute ~captured command =
      match command.B.kind with
      | B.Observe -> B.Observed (observe ())
      | B.Inspect ->
          if not captured then B.Observed (observe ())
          else (
            repo := { !repo with pinned = true };
            B.Inspected_active { observation = observe (); ready = !staged })
      | B.Integrate { source = captured; target; _ } ->
          assert (!repo.pinned && captured = source && target = base);
          assert (!starts = 0);
          incr starts;
          active := true;
          B.Conflict
            { head = !repo.head; sequencer = sequencer (); conflicts = 1 }
      | B.Continue { head; target; sequencer = captured } ->
          assert (!active && !staged && head = !repo.head && target = base);
          assert (captured = sequencer ());
          incr continuations;
          repo := { !repo with mutations = !repo.mutations + 1 };
          incr cursor;
          staged := false;
          if !cursor = steps then (
            active := false;
            repo := { !repo with head = integrated; integrated = true };
            B.Integrated integrated)
          else (
            repo := { !repo with head = commit (Char.chr (48 + !cursor)) };
            B.Conflict
              { head = !repo.head; sequencer = sequencer (); conflicts = 1 })
      | B.Pin _ | B.Publish _ | B.Confirm _ ->
          let next, result =
            execute ~captured:true ~verification:false !repo command
          in
          repo := next;
          result
      | B.Commit_merge _ | B.Plan_remote_replay _ | B.Checkout_remote _
      | B.Verify_recovery ->
          failwith "staged sequencer unexpectedly entered history recovery"
    in
    let act state action =
      now := !now +. 1000.;
      let next =
        match action with
        | 0 -> step (roundtrip state) B.Recover
        | 3 ->
            List.fold_left
              (fun state (token, event) ->
                let current =
                  match B.pending state with
                  | Some command -> B.equal_token token command.token
                  | None -> false
                in
                if current then state
                else
                  let next, effects = B.step state event in
                  assert (B.equal next state && effects = []);
                  next)
              state !history
        | _ -> (
            match B.pending state with
            | Some command ->
                let result =
                  if action = 1 then
                    B.Retryable { reason = "model outage"; retry_after = None }
                  else
                    let captured =
                      match B.operation state with
                      | Some op -> Option.is_some op.source
                      | None -> false
                    in
                    execute ~captured command
                in
                let event =
                  B.Result { token = command.token; at = !now; result }
                in
                history := (command.token, event) :: !history;
                if action = 2 then step (roundtrip state) B.Recover
                else step state event
            | None -> (
                match !offered with
                | None -> step state (B.Tick !now)
                | Some token -> (
                    offered := None;
                    let claimed, effects =
                      B.step state (B.Repair_started token)
                    in
                    let started =
                      List.exists
                        (function
                          | B.Start_repair current ->
                              B.equal_token token current
                          | B.Execute _ | B.Repair _ | B.Completed _ -> false)
                        effects
                    in
                    if not started then claimed
                    else
                      let turn =
                        Option.get (B.repair_turn claimed ~branch:"model" token)
                      in
                      match turn.mode with
                      | B.Diagnosis _ ->
                          (* Diagnostic assistance cannot stage or rewrite model work. *)
                          step claimed (B.Repair_completed { token; at = !now })
                      | B.Content_repair | B.History_recovery _ ->
                          assert (!active && not !staged);
                          if action = 4 then
                            step claimed
                              (B.Repair_interrupted
                                 { token; at = !now; reason = "backend outage" })
                          else (
                            staged := true;
                            step (roundtrip claimed)
                              (B.Repair_completed { token; at = !now })))))
      in
      (match B.operation next with
      | Some { repair = Some repair; _ } -> (
          match repair.mode with
          | B.Content_repair -> assert (repair.attempts_without_progress = 0)
          | B.Diagnosis _ -> assert (repair.attempts_without_progress <= 2)
          | B.History_recovery _ -> failwith "unexpected model history recovery"
          )
      | Some { repair = None; _ } | None -> ());
      assert (!continuations <= steps && !starts <= 1);
      assert (!repo.remote <> Some integrated || (!cursor = steps && not !active));
      next
    in
    let state = List.fold_left act state actions in
    let rec finish state fuel =
      if B.phase state = Some B.Settled then state
      else if fuel = 0 then failwith "staged sequencer failed to converge"
      else
        let state =
          match B.phase state with
          | Some (B.Intervention _) ->
              let op = Option.get (B.operation state) in
              let repair = Option.get op.repair in
              assert (
                repair.attempts_without_progress >= 2
                && Option.is_some repair.last_turn);
              step state B.Resume
          | None
          | Some
              ( Preparing | Integrating | Repairing _ | Publishing | Confirming
              | Waiting _ | Recovering | Settled ) ->
              state
        in
        finish (act state 5) (fuel - 1)
    in
    let settled = finish state (30 + (steps * 10)) in
    assert (!starts = 1 && !continuations = steps);
    assert (!repo.head = integrated && !repo.remote = Some integrated);
    assert (!repo.mutations = steps + 1);
    let before = !repo in
    let again = List.fold_left act settled [ 3; 5; 5 ] in
    assert (B.equal settled again && !repo = before);
    true

  let test policy =
    QCheck2.Test.make
      ~name:
        ("model: interrupted multi-step conflicting sequencer ("
        ^ (if policy = B.Rewrite then "rewrite" else "merge")
        ^ ")")
      ~count:1000
      ~print:(fun (steps, actions) ->
        string_of_int steps ^ ": "
        ^ String.concat "," (List.map string_of_int actions))
      Gen.(pair (int_range 1 6) (list_size (int_range 0 100) (int_range 0 5)))
      (fun (steps, actions) ->
        try scenario policy steps actions
        with exn -> QCheck2.Test.fail_report (Printexc.to_string exn))
end

module Remote_writers = struct
  type node = {
    sha : B.Commit.t;
    parents : B.Commit.t list;
    content : int list;
  }

  type repo = {
    nodes : node list;
    head : B.Commit.t;
    remote : B.Commit.t;
    next : int;
    obligations : int list;
    integrations : (B.Commit.t * B.Commit.t * B.Commit.t) list;
    mutations : int;
  }

  type action =
    | Step
    | External_write
    | External_rewrite
    | Drop_remote
    | Retarget of bool
    | Bad_repair
    | Force_recovery
    | Resume
    | Restart
    | Lose_ack
    | Duplicate
    | Outage

  let sha n =
    match B.Commit.make (Printf.sprintf "%040x" n) with
    | Some sha -> sha
    | None -> assert false

  let root = sha 1
  let base = sha 2
  let source = sha 3
  let alternate = sha 4
  let target_for name = if name = "topic" then alternate else base
  let find repo sha = List.find (fun n -> n.sha = sha) repo.nodes
  let content repo sha = (find repo sha).content
  let union a b = List.sort_uniq Int.compare (a @ b)
  let contains a b = List.for_all (fun x -> List.mem x a) b

  let rec ancestor repo old tip =
    old = tip || List.exists (ancestor repo old) (find repo tip).parents

  let preserved repo policy candidate original =
    match policy with
    | B.Rewrite -> contains (content repo candidate) (content repo original)
    | B.Preserve_ancestry -> ancestor repo original candidate

  let topology repo candidate remote =
    if candidate = remote then B.Equal
    else if ancestor repo remote candidate then B.Includes
    else if ancestor repo candidate remote then B.Behind
    else B.Diverged

  let create repo ~parents ~content =
    let revision = sha repo.next in
    ( {
        repo with
        nodes = { sha = revision; parents; content } :: repo.nodes;
        next = repo.next + 1;
      },
      revision )

  let observation (repo : repo) (op : B.operation) : B.observation =
    let target = Option.value op.target ~default:(target_for op.intent.base) in
    {
      destination;
      head = repo.head;
      source = repo.head;
      target;
      remote = Some repo.remote;
      boundary = B.Recorded root;
      topology = topology repo repo.head repo.remote;
      clean = true;
      sequencer = None;
      conflicts = 0;
      target_included = ancestor repo target repo.head;
      base_contains_source = ancestor repo repo.head target;
      completed_integration =
        List.exists
          (fun (_, t, h) -> t = target && h = repo.head)
          repo.integrations;
    }

  let execute repo (op : B.operation) (command : B.command) =
    match command.kind with
    | B.Observe -> (repo, B.Observed (observation repo op))
    | B.Inspect ->
        ( repo,
          if Option.is_none op.source then B.Observed (observation repo op)
          else B.Inspected (observation repo op) )
    | B.Pin { source; target } ->
        ignore (find repo source);
        ignore (find repo target);
        (repo, B.Pinned)
    | B.Integrate { source; target; policy; boundary = _ } ->
        assert (source = repo.head);
        let parents =
          match policy with
          | B.Rewrite -> [ target ]
          | B.Preserve_ancestry -> [ source; target ]
        in
        let repo, head =
          create repo ~parents
            ~content:(union (content repo source) (content repo target))
        in
        ( {
            repo with
            head;
            mutations = repo.mutations + 1;
            integrations = (source, target, head) :: repo.integrations;
            obligations = union repo.obligations (content repo target);
          },
          B.Integrated head )
    | B.Plan_remote_replay request ->
        assert (repo.head = request.preserved);
        assert (ancestor repo root request.incoming);
        (repo, B.Remote_replay_selected (B.Recorded root))
    | B.Checkout_remote replay ->
        assert (repo.head = replay.preserved || repo.head = replay.incoming);
        ( { repo with head = replay.incoming; mutations = repo.mutations + 1 },
          B.Remote_checked_out )
    | B.Publish { candidate; expected } ->
        assert (candidate = repo.head);
        let remote_preserved =
          Option.fold ~none:true
            ~some:(preserved repo (B.execution_policy op) candidate)
            expected
        in
        if not remote_preserved then
          (repo, B.Recovery_required "remote_work_not_incorporated")
        else if expected <> Some repo.remote then
          (repo, B.Retryable { reason = "lease_violation"; retry_after = None })
        else (
          (* A lease alone is insufficient: every remote contribution must be
             in the candidate's independently modelled tree. *)
          assert (preserved repo (B.execution_policy op) candidate repo.remote);
          assert (contains (content repo candidate) repo.obligations);
          ( { repo with remote = candidate; mutations = repo.mutations + 1 },
            B.Published ))
    | B.Confirm candidate ->
        ( repo,
          B.Remote
            {
              sha = Some repo.remote;
              topology = topology repo candidate repo.remote;
            } )
    | B.Verify_recovery ->
        let retained =
          op.recovery_revisions @ Option.to_list op.source
          @ Option.to_list op.candidate
        in
        let preserved = preserved repo (B.execution_policy op) repo.head in
        ( repo,
          B.Recovery_verified
            {
              observation = observation repo op;
              source_preserved = List.for_all preserved retained;
              remote_preserved =
                preserved repo.remote
                && List.for_all preserved (Option.to_list op.expected);
            } )
    | B.Commit_merge _ | B.Continue _ ->
        failwith "disjoint model contributions cannot create content conflicts"

  let show = function
    | Step -> "step"
    | External_write -> "external-write"
    | External_rewrite -> "external-rewrite"
    | Drop_remote -> "drop-remote"
    | Retarget topic -> if topic then "retarget-topic" else "retarget-main"
    | Bad_repair -> "bad-repair"
    | Force_recovery -> "force-recovery"
    | Resume -> "resume"
    | Restart -> "restart"
    | Lose_ack -> "lose-ack"
    | Duplicate -> "duplicate"
    | Outage -> "outage"

  let scenario ?(expect_stopped = false) ?(expect_drop = false) policy actions =
    let repo =
      {
        nodes =
          [
            { sha = root; parents = []; content = [] };
            { sha = base; parents = [ root ]; content = [ 1 ] };
            { sha = source; parents = [ root ]; content = [ 2 ] };
            { sha = alternate; parents = [ root ]; content = [ 3 ] };
          ];
        head = source;
        remote = source;
        next = 5;
        obligations = [ 1; 2 ];
        integrations = [];
        mutations = 0;
      }
    in
    let intent = B.{ base = "main"; policy; purpose = Reconcile_base } in
    let desired = ref intent in
    let state = fst (B.step B.empty (B.Materialized (B.New_branch root))) in
    let state = fst (B.step state (B.Request intent)) in
    let history = ref [] and now = ref 0. and offered = ref None in
    let bad_turns = ref 0 and drops = ref 0 in
    let step state event =
      let next, effects = B.step state event in
      List.iter
        (function
          | B.Repair token -> offered := Some token
          | B.Execute _ | B.Start_repair _ | B.Completed _ -> ())
        effects;
      next
    in
    let act (state, repo) action =
      now := !now +. 1000.;
      match action with
      | Retarget topic ->
          let requested =
            { intent with base = (if topic then "topic" else "main") }
          in
          let next = step state (B.Request requested) in
          if B.is_unsettled state then (
            assert (B.operation next = B.operation state);
            assert (B.pending next = B.pending state));
          desired := requested;
          (next, repo)
      | Drop_remote ->
          (* Only erase remote work already incorporated locally. Unobserved
             commits overwritten by another writer are not recoverable input. *)
          if not (contains (content repo repo.head) repo.obligations) then
            (state, repo)
          else (
            incr drops;
            let repo, remote = create repo ~parents:[ root ] ~content:[] in
            (step state B.Reconfirm_publication, { repo with remote }))
      | External_write | External_rewrite ->
          let contribution = repo.next in
          let repo, remote =
            create repo
              ~parents:
                [ (if action = External_rewrite then root else repo.remote) ]
              ~content:(union (content repo repo.remote) [ contribution ])
          in
          let repo =
            { repo with remote; obligations = contribution :: repo.obligations }
          in
          (step state B.Reconfirm_publication, repo)
      | Resume -> (step state B.Resume, repo)
      | Restart ->
          offered := None;
          (step (roundtrip state) B.Recover, repo)
      | Duplicate ->
          let state =
            List.fold_left
              (fun state (token, event) ->
                let current =
                  match B.pending state with
                  | Some pending -> B.equal_token token pending.token
                  | None -> false
                in
                let next, effects = B.step state event in
                if not current then assert (B.equal state next && effects = []);
                step state event)
              state !history
          in
          (state, repo)
      | Step | Lose_ack | Outage | Bad_repair | Force_recovery -> (
          match (B.operation state, B.pending state) with
          | Some op, Some command ->
              let repo, result =
                if action = Outage then
                  ( repo,
                    B.Retryable { reason = "transport"; retry_after = None } )
                else if action = Force_recovery then
                  match command.kind with
                  | B.Integrate _ ->
                      ( repo,
                        B.Recovery_required
                          "model deterministic recovery exhausted" )
                  | B.Observe | B.Inspect | B.Pin _ | B.Publish _ | B.Confirm _
                  | B.Plan_remote_replay _ | B.Checkout_remote _
                  | B.Verify_recovery | B.Commit_merge _ | B.Continue _ ->
                      execute repo op command
                else execute repo op command
              in
              let event =
                B.Result { token = command.token; at = !now; result }
              in
              history := (command.token, event) :: !history;
              ( (if action = Lose_ack then step (roundtrip state) B.Recover
                 else step state event),
                repo )
          | Some op, None when Option.is_some !offered ->
              let token =
                match !offered with Some token -> token | None -> assert false
              in
              offered := None;
              let state, effects = B.step state (B.Repair_started token) in
              if
                not
                  (List.exists
                     (function
                       | B.Start_repair started -> B.equal_token token started
                       | B.Execute _ | B.Repair _ | B.Completed _ -> false)
                     effects)
              then (state, repo)
              else if
                match
                  (Option.get (B.repair_turn state ~branch:"model" token)).mode
                with
                | B.Diagnosis _ -> true
                | B.Content_repair | B.History_recovery _ -> false
              then (step state (B.Repair_completed { token; at = !now }), repo)
              else
                (* A successful fallback agent retains all checkpointed work;
                   its report alone does not authorize publication. *)
                let parents =
                  List.sort_uniq B.Commit.compare
                    ((repo.head :: repo.remote :: base :: op.recovery_revisions)
                    @ Option.to_list op.source @ Option.to_list op.target
                    @ Option.to_list op.candidate
                    @ Option.to_list op.expected)
                in
                let all =
                  List.fold_left
                    (fun acc revision -> union acc (content repo revision))
                    [] parents
                in
                let parents, all =
                  if action = Bad_repair then (
                    incr bad_turns;
                    ([ root ], []))
                  else (parents, all)
                in
                let repo, head = create repo ~parents ~content:all in
                let repo = { repo with head; mutations = repo.mutations + 1 } in
                (step state (B.Repair_completed { token; at = !now }), repo)
          | None, _ | _, None -> (step state (B.Tick !now), repo))
    in
    let state, repo = List.fold_left act (state, repo) actions in
    if expect_drop then assert (!drops > 0);
    if expect_stopped then (
      assert (!bad_turns = 2);
      assert (
        B.phase state = Some (B.Intervention "recovery_preservation_unproven"));
      assert (repo.remote = source);
      assert (B.pending state = None);
      let stopped, unchanged =
        List.fold_left act (state, repo) [ Step; Restart; Duplicate; Step ]
      in
      assert (B.phase stopped = B.phase state);
      assert (unchanged = repo && !bad_turns = 2));
    let rec settle state repo remaining =
      if B.publication_status state = `Published then (state, repo)
      else if remaining = 0 then
        failwith
          ("remote-writer model did not converge: "
          ^ Sexplib0.Sexp.to_string_hum (B.sexp_of_t state))
      else
        (* The healthy suffix explicitly authorizes retry after an exhausted
           agent budget; runtime must not retry that intervention itself. *)
        let action =
          match B.phase state with
          | Some (B.Intervention _) ->
              let op = Option.get (B.operation state) in
              let repair = Option.get op.repair in
              assert (
                repair.attempts_without_progress >= 2
                || op.publication_recovery_attempts >= 2);
              assert (Option.is_some repair.last_turn);
              Resume
          | None
          | Some
              ( B.Preparing | B.Integrating | B.Repairing _ | B.Publishing
              | B.Confirming | B.Waiting _ | B.Recovering | B.Settled ) ->
              Step
        in
        let state, repo = act (state, repo) action in
        settle state repo (remaining - 1)
    in
    (* A delayed result can acknowledge a publication that a later external
       writer has already replaced. Fair recovery includes a fresh remote
       observation after external writes stop, not only cached completion. *)
    let state = step state B.Reconfirm_publication in
    let state, repo = settle state repo 100 in
    assert (contains (content repo repo.remote) repo.obligations);
    let final_target = target_for !desired.base in
    assert (contains (content repo repo.remote) (content repo final_target));
    (match B.operation state with
    | Some op -> assert (B.equal_intent op.intent !desired)
    | None -> failwith "retargeted request did not settle");
    (match policy with
    | B.Rewrite -> ()
    | B.Preserve_ancestry ->
        if not (ancestor repo source repo.remote) then
          failwith
            ("ancestry missing after settlement: remote="
            ^ B.Commit.to_string repo.remote
            ^ " head="
            ^ B.Commit.to_string repo.head
            ^ " state="
            ^ Sexplib0.Sexp.to_string_hum (B.sexp_of_t state));
        assert (ancestor repo base repo.remote);
        assert (ancestor repo final_target repo.remote));
    let stable, effects = B.step (roundtrip state) (B.Request !desired) in
    assert (B.equal stable state && effects = []);
    let settled, unchanged =
      List.fold_left act (state, repo) [ Step; Restart; Step; Duplicate; Step ]
    in
    assert (B.publication_status settled = `Published);
    assert (unchanged.mutations = repo.mutations);
    true

  let tests =
    let recovery_race =
      [
        External_write;
        Step;
        Step;
        Step;
        Step;
        Step;
        Step;
        External_write;
        Step;
        Step;
        External_write;
      ]
    in
    let delayed_confirmation =
      List.init 23 (fun _ -> Step)
      @ [
          External_rewrite;
          Step;
          Step;
          Step;
          Restart;
          Restart;
          Restart;
          Lose_ack;
          External_rewrite;
          Duplicate;
        ]
    in
    let delayed_confirmation_tests =
      List.map
        (fun policy ->
          QCheck2.Test.make ~count:1
            ~name:
              (match policy with
              | B.Rewrite ->
                  "model: fresh confirmation repairs rewrite after delayed \
                   completion"
              | B.Preserve_ancestry ->
                  "model: fresh confirmation repairs ancestry after delayed \
                   completion")
            (Gen.return delayed_confirmation)
            (fun actions ->
              try scenario policy actions
              with exn -> QCheck2.Test.fail_report (Printexc.to_string exn)))
        [ B.Rewrite; B.Preserve_ancestry ]
    in
    let regressions =
      List.map
        (fun policy ->
          QCheck2.Test.make ~count:1
            ~name:
              (match policy with
              | B.Rewrite ->
                  "model: rewrite recovery progress survives advancing remote"
              | B.Preserve_ancestry ->
                  "model: merge recovery progress survives advancing remote")
            (Gen.return recovery_race)
            (fun actions -> try scenario policy actions with _ -> false))
        [ B.Rewrite; B.Preserve_ancestry ]
    in
    let actions =
      Gen.list_size (Gen.int_range 0 70)
        (Gen.oneof_list
           [
             Step;
             Step;
             External_write;
             External_rewrite;
             Restart;
             Lose_ack;
             Duplicate;
             Outage;
           ])
    in
    let retargets =
      Gen.list_size (Gen.int_range 0 90)
        (Gen.oneof_list
           [
             Step;
             Step;
             External_write;
             External_rewrite;
             Restart;
             Lose_ack;
             Duplicate;
             Outage;
             Retarget false;
             Retarget true;
           ])
    in
    let retarget_tests =
      List.map
        (fun policy ->
          QCheck2.Test.make ~count:500
            ~name:
              (match policy with
              | B.Rewrite ->
                  "model: retargeted rewrite preserves captured and remote work"
              | B.Preserve_ancestry ->
                  "model: retargeted merge preserves captured and remote work")
            ~print:(fun xs -> String.concat ", " (List.map show xs))
            retargets
            (fun actions ->
              try scenario policy actions
              with exn -> QCheck2.Test.fail_report (Printexc.to_string exn)))
        [ B.Rewrite; B.Preserve_ancestry ]
    in
    let hostile =
      Gen.list_size (Gen.int_range 0 100)
        (Gen.oneof_list
           [
             Step;
             Step;
             Bad_repair;
             Force_recovery;
             External_write;
             External_rewrite;
             Restart;
             Lose_ack;
             Duplicate;
             Outage;
             Resume;
             Retarget false;
             Retarget true;
           ])
    in
    let hostile_tests =
      List.concat_map
        (fun policy ->
          let label =
            match policy with
            | B.Rewrite -> "rewrite"
            | B.Preserve_ancestry -> "merge"
          in
          [
            QCheck2.Test.make ~count:1
              ~name:
                ("model: two invalid " ^ label
               ^ " repairs stop before publication")
              (Gen.return
                 [
                   Step;
                   Step;
                   Force_recovery;
                   Step;
                   Bad_repair;
                   Step;
                   Bad_repair;
                   Step;
                 ])
              (fun actions ->
                try scenario ~expect_stopped:true policy actions
                with exn -> QCheck2.Test.fail_report (Printexc.to_string exn));
            QCheck2.Test.make ~count:500
              ~name:
                ("model: hostile " ^ label
               ^ " recovery preserves work and resumes explicitly")
              ~print:(fun xs -> String.concat ", " (List.map show xs))
              hostile
              (fun actions ->
                try scenario policy actions
                with exn -> QCheck2.Test.fail_report (Printexc.to_string exn));
          ])
        [ B.Rewrite; B.Preserve_ancestry ]
    in
    let destructive =
      Gen.list_size (Gen.int_range 0 90)
        (Gen.oneof_list
           [
             Step;
             Step;
             Drop_remote;
             External_write;
             External_rewrite;
             Restart;
             Lose_ack;
             Duplicate;
             Outage;
             Bad_repair;
             Force_recovery;
             Resume;
             Retarget false;
             Retarget true;
           ])
    in
    let destructive_tests =
      List.map
        (fun policy ->
          QCheck2.Test.make ~count:500
            ~name:
              (match policy with
              | B.Rewrite ->
                  "model: destructive remote rewrites retain captured content"
              | B.Preserve_ancestry ->
                  "model: destructive remote rewrites retain captured ancestry")
            ~print:(fun xs -> String.concat ", " (List.map show xs))
            destructive
            (fun actions ->
              try
                scenario ~expect_drop:true policy
                  ([ Step; Step; Step; Drop_remote ] @ actions)
              with exn -> QCheck2.Test.fail_report (Printexc.to_string exn)))
        [ B.Rewrite; B.Preserve_ancestry ]
    in
    regressions @ delayed_confirmation_tests @ retarget_tests @ hostile_tests
    @ destructive_tests
    @ List.map
        (fun policy ->
          QCheck2.Test.make ~count:500
            ~name:
              (match policy with
              | B.Rewrite -> "model: remote writers survive rewrite publication"
              | B.Preserve_ancestry ->
                  "model: remote writers survive merge publication")
            ~print:(fun xs -> String.concat ", " (List.map show xs))
            actions
            (fun actions ->
              match
                try Ok (scenario policy actions)
                with exn -> Error (Printexc.to_string exn)
              with
              | Ok passed -> passed
              | Error reason -> QCheck2.Test.fail_report reason))
        [ B.Rewrite; B.Preserve_ancestry ]
end

(* Multiple independent owners share object storage, but retain separate local
   and remote refs. Parent targets come from those refs, never owner phases. *)
module Stack = struct
  module R = Remote_writers

  let scenario policy (depth, actions) =
    let initial : R.repo =
      {
        nodes = [ { sha = R.root; parents = []; content = [] } ];
        head = R.root;
        remote = R.root;
        next = 2;
        obligations = [];
        integrations = [];
        mutations = 0;
      }
    in
    let graph = ref initial and main = ref R.root in
    let repos =
      Array.init depth (fun i ->
          let previous = !graph in
          let next, head =
            R.create previous ~parents:[ previous.head ]
              ~content:(R.content previous previous.head @ [ i + 1 ])
          in
          let next =
            { next with R.head; remote = head; obligations = [ i + 1 ] }
          in
          graph := next;
          next)
    in
    let starts = Array.map (fun (repo : R.repo) -> repo.head) repos in
    let states = Array.make depth B.empty in
    let histories = Array.make depth [] in
    let now = ref 0. and generation = ref 0 in
    let step i event = states.(i) <- fst (B.step states.(i) event) in
    let request i =
      step i
        (B.Request
           {
             base = (if i = 0 then "main" else "patch-" ^ string_of_int i);
             policy;
             purpose = B.Reconcile_request (string_of_int !generation);
           })
    in
    Array.iteri
      (fun i _ ->
        let boundary = if i = 0 then R.root else starts.(i - 1) in
        step i (B.Materialized (B.New_branch boundary));
        request i)
      states;
    let act i action =
      now := !now +. 1000.;
      let repo =
        { (repos.(i)) with R.nodes = !graph.nodes; next = !graph.next }
      in
      if action = 0 then step i B.Recover
      else if action = 1 then (
        states.(i) <- roundtrip states.(i);
        step i B.Recover)
      else if action = 2 then (
        incr generation;
        request i)
      else if action = 3 then
        List.iter
          (fun (token, event) ->
            if
              not
                (Option.fold ~none:false
                   ~some:(fun (pending : B.command) ->
                     B.equal_token pending.token token)
                   (B.pending states.(i)))
            then
              let next, effects = B.step states.(i) event in
              assert (B.equal next states.(i) && effects = []))
          histories.(i)
      else
        match (B.operation states.(i), B.pending states.(i)) with
        | Some op, Some command ->
            let target =
              Option.value op.target
                ~default:(if i = 0 then !main else repos.(i - 1).remote)
            in
            let observation () =
              let observed = R.observation repo op in
              {
                observed with
                B.target;
                boundary = B.Recorded (if i = 0 then R.root else starts.(i - 1));
                target_included = R.ancestor repo target repo.head;
                base_contains_source = R.ancestor repo repo.head target;
                completed_integration =
                  List.exists
                    (fun (_, t, h) -> t = target && h = repo.head)
                    repo.integrations;
              }
            in
            let repo, result =
              if action = 4 then
                (repo, B.Retryable { reason = "transport"; retry_after = None })
              else
                match command.kind with
                | B.Observe -> (repo, B.Observed (observation ()))
                | B.Inspect ->
                    ( repo,
                      if Option.is_none op.source then
                        B.Observed (observation ())
                      else B.Inspected (observation ()) )
                | B.Pin _ | B.Integrate _ | B.Publish _ | B.Confirm _
                | B.Plan_remote_replay _ | B.Checkout_remote _
                | B.Verify_recovery | B.Commit_merge _ | B.Continue _ ->
                    R.execute repo op command
            in
            repos.(i) <- repo;
            graph := repo;
            let event = B.Result { token = command.token; at = !now; result } in
            histories.(i) <- (command.token, event) :: histories.(i);
            if action = 5 then (
              states.(i) <- roundtrip states.(i);
              step i B.Recover)
            else step i event
        | None, _ | Some _, None ->
            step i
              (Option.value
                 (B.wake_event ~at:!now states.(i))
                 ~default:(B.Tick !now))
    in
    List.iter
      (fun (owner, action) ->
        if action = 7 then (
          let repo, head =
            R.create !graph ~parents:[ !main ]
              ~content:(R.content !graph !main @ [ !graph.next + 100 ])
          in
          graph := repo;
          main := head;
          incr generation;
          Array.iteri (fun i _ -> request i) states)
        else act (owner mod depth) action)
      actions;
    (* Once external writes stop, schedule a fresh intent in dependency order.
       This represents fair observation of each parent's final publication. *)
    Array.iteri
      (fun i _ ->
        incr generation;
        request i;
        let rec settle remaining =
          if remaining = 0 then
            failwith
              ("stack failed to converge: "
              ^ Sexplib0.Sexp.to_string_hum (B.sexp_of_t states.(i)));
          if B.is_unsettled states.(i) then (
            act i 6;
            settle (remaining - 1))
        in
        settle 100;
        assert (B.publication_status states.(i) = `Published);
        let repo = { (repos.(i)) with R.nodes = !graph.nodes } in
        let target = if i = 0 then !main else repos.(i - 1).remote in
        let expected =
          R.union (R.content repo !main) (List.init (i + 1) (( + ) 1))
        in
        assert (R.content repo repo.remote = expected);
        assert (R.ancestor repo target repo.remote);
        (match policy with
        | B.Rewrite -> ()
        | B.Preserve_ancestry -> assert (R.ancestor repo starts.(i) repo.remote));
        let mutations = repo.mutations in
        List.iter (act i) [ 1; 3; 6; 6 ];
        assert (repos.(i).mutations = mutations))
      states;
    true

  let test policy =
    QCheck2.Test.make ~count:500
      ~name:
        ("model: interleaved stack convergence "
        ^
        match policy with
        | B.Rewrite -> "rewrite"
        | B.Preserve_ancestry -> "merge")
      ~print:(fun (depth, actions) ->
        Printf.sprintf "depth=%d [%s]" depth
          (String.concat ";"
             (List.map (fun (i, a) -> Printf.sprintf "%d:%d" i a) actions)))
      Gen.(
        pair (int_range 3 6)
          (list_size (int_range 0 100) (pair (int_range 0 5) (int_range 0 7))))
      (fun input ->
        try scenario policy input
        with exn -> QCheck2.Test.fail_report (Printexc.to_string exn))
end

let () =
  let faults =
    Gen.list_size (Gen.int_range 0 100)
      (Gen.oneof_list
         [ Advance; Lose_ack; Restart; Duplicate; Request; Outage; Wake ])
  in
  let test policy name =
    QCheck2.Test.make ~name ~count:1000
      ~print:(fun xs -> String.concat ", " (List.map show_fault xs))
      faults
      (fun faults -> try scenario policy faults with _ -> false)
  in
  let competing_faults =
    Gen.list_size (Gen.int_range 0 100)
      (Gen.oneof_weighted
         [
           ( 6,
             Gen.oneof_list
               [ Advance; Lose_ack; Restart; Duplicate; Request; Outage; Wake ]
           );
           (2, Gen.map (fun n -> Compete n) (Gen.int_range 0 4));
         ])
  in
  let competing policy name =
    QCheck2.Test.make ~name ~count:1000
      ~print:(fun xs -> String.concat ", " (List.map show_fault xs))
      competing_faults
      (fun faults ->
        try scenario policy faults
        with exn -> QCheck2.Test.fail_report (Printexc.to_string exn))
  in
  let mixed_purposes policy =
    QCheck2.Test.make
      ~name:
        ("model: all reconciliation purposes serialize ("
        ^ (if policy = B.Rewrite then "rewrite" else "merge")
        ^ ")")
      ~count:1000
      ~print:(fun xs -> String.concat ", " (List.map show_fault xs))
      (Gen.list_size (Gen.int_range 0 100)
         (Gen.oneof_weighted
            [
              ( 6,
                Gen.oneof_list
                  [
                    Advance; Lose_ack; Restart; Duplicate; Request; Outage; Wake;
                  ] );
              (3, Gen.map (fun n -> Purpose n) (Gen.int_range 0 15));
            ]))
      (fun faults ->
        try scenario policy faults
        with exn -> QCheck2.Test.fail_report (Printexc.to_string exn))
  in
  let compatible_order policy =
    QCheck2.Test.make ~count:1000
      ~name:
        ("model: compatible initial observations preserve outcomes ("
        ^ (if policy = B.Rewrite then "rewrite" else "merge")
        ^ ")")
      ~print:(fun xs -> String.concat ", " (List.map show_fault xs))
      competing_faults
      (fun faults ->
        try
          let first = ref None and second = ref None in
          let run materialize_first output =
            scenario ~materialize_first
              ~observe_settled:(fun repo -> output := Some repo)
              policy faults
          in
          run true first && run false second && Option.is_some !first
          && !first = !second
        with exn -> QCheck2.Test.fail_report (Printexc.to_string exn))
  in
  QCheck_base_runner.run_tests_main
    ([
       QCheck2.Test.make
         ~name:
           "model: provisioning before and after publication survives lost \
            acknowledgements" ~count:1 Gen.unit (fun () ->
           try
             List.for_all
               (fun policy ->
                 List.for_all (scenario policy)
                   [
                     [ Purpose 7; Restart; Lose_ack; Duplicate ];
                     List.init 12 (fun _ -> Advance)
                     @ [ Purpose 7; Lose_ack; Restart; Duplicate; Outage; Wake ];
                     List.init 12 (fun _ -> Advance)
                     @ [ Purpose 7; Purpose 2; Lose_ack; Restart; Duplicate ];
                   ])
               [ B.Rewrite; B.Preserve_ancestry ]
           with exn -> QCheck2.Test.fail_report (Printexc.to_string exn));
       compatible_order B.Rewrite;
       compatible_order B.Preserve_ancestry;
       Conflicting_sequencer.test B.Rewrite;
       Conflicting_sequencer.test B.Preserve_ancestry;
       mixed_purposes B.Rewrite;
       mixed_purposes B.Preserve_ancestry;
       competing B.Rewrite
         "model: competing rewrite requests retain one mutation owner";
       competing B.Preserve_ancestry
         "model: competing merge requests retain one mutation owner";
     ]
    @ [ Stack.test B.Rewrite; Stack.test B.Preserve_ancestry ]
    @ Remote_writers.tests
    @ [
        test B.Rewrite "model: rewrite publication survives generated faults";
        test B.Preserve_ancestry
          "model: merge publication survives generated faults";
      ])
