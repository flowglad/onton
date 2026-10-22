(* @archlint.module test
   @archlint.domain branch-reconcile *)

open Onton_core
module B = Branch_reconcile
module G = QCheck2.Gen

[@@@warning "-4-42"]

let sha c = Option.get (B.Commit.make (String.make 40 c))
let patch = sha 'a'
let base = sha 'b'
let candidate = sha 'c'
let intent = B.{ base = "main"; policy = Rewrite; purpose = Reconcile_base }

let roundtrip state =
  match B.decode (B.yojson_of_t state) with
  | Ok restored -> B.equal state restored
  | Error _ -> false

let request () = B.step B.empty (B.Request intent)
let lose state = B.step state (B.Workspace_lost { request = "attempt-1" })

let reply state result =
  match B.pending state with
  | None -> failwith "missing command"
  | Some command ->
      B.step state (B.Result { token = command.B.token; at = 1.; result })

let observed state ~patch_revision ~base_revision =
  fst
    (reply state
       (B.Workspace_observed
          B.
            {
              push_destination = Remote_id.of_destination "push-origin";
              patch_revision;
              base_revision;
            }))

let ordinary_observation : B.observation =
  B.
    {
      destination = Remote_id.of_destination "push-origin";
      head = patch;
      source = patch;
      target = base;
      remote = Some patch;
      boundary = Plain;
      topology = Equal;
      clean = true;
      sequencer = None;
      conflicts = 0;
      target_included = false;
      base_contains_source = false;
      completed_integration = false;
    }

let publishing () =
  let state, _ = request () in
  let state = fst (reply state (B.Observed ordinary_observation)) in
  let state = fst (reply state B.Pinned) in
  fst (reply state (B.Integrated candidate))

let total_matrix =
  QCheck2.Test.make ~name:"workspace decision matrix" ~count:1000
    G.(tup5 bool bool bool bool bool)
    (fun (probe_ok, branch_present, base_confirmed, published, renewed) ->
      let observation =
        Some
          B.
            {
              push_destination = Remote_id.of_destination "push-origin";
              patch_revision = (if branch_present then Some patch else None);
              base_revision = (if base_confirmed then Some base else None);
            }
      in
      let result =
        B.decide_workspace ~probe_ok ~observation
          ~never_published:(not published)
          ~forge:(if published then B.Forge_not_merged else B.Forge_unknown)
          ~renewed ~renewal_id:"receipt"
      in
      if not probe_ok then result = B.Workspace_wait
      else if branch_present then
        result
        =
        if base_confirmed then B.Workspace_patch (patch, base)
        else B.Workspace_wait
      else if (not published) || renewed then
        result
        = if base_confirmed then B.Workspace_base base else B.Workspace_wait
      else result = B.Workspace_attention "receipt")

let loss_fences_old_reply =
  QCheck2.Test.make ~name:"loss is idempotent and fences stale tokens"
    ~count:100 G.string_small (fun suffix ->
      let state, _ = request () in
      let old = B.pending state in
      let request = "attempt-" ^ suffix in
      let lost, effects = B.step state (B.Workspace_lost { request }) in
      let repeated, duplicate_effects =
        B.step lost (B.Workspace_lost { request })
      in
      let stale =
        match old with
        | None -> lost
        | Some command ->
            fst
              (B.step lost
                 (B.Result
                    { token = command.B.token; at = 2.; result = B.Published }))
      in
      B.equal lost repeated && duplicate_effects = [] && B.equal lost stale
      && roundtrip lost
      &&
      match (effects, B.operation lost) with
      | [ B.Execute command ], Some operation ->
          command.B.kind = B.Observe && operation.B.lost_workspace
          && operation.B.source = None
          && operation.B.candidate = None
          && operation.B.pending = Some command
      | _ -> false)

let uncertainty_and_renewal =
  QCheck2.Test.make ~name:"uncertain push needs renewed operation" ~count:100
    G.bool (fun merged ->
      let before = publishing () in
      let lost, _ = lose before in
      let absent =
        observed lost ~patch_revision:None ~base_revision:(Some base)
      in
      let retry, _ = B.step absent B.Recover in
      let forge = if merged then B.Forge_merged else B.Forge_not_merged in
      let settled, _ =
        B.step absent
          (B.Workspace_forge_observed { request = "attempt-1"; forge })
      in
      let renewed =
        match B.workspace_outcome settled with
        | Some (B.Workspace_attention renewal_id) ->
            fst
              (B.step settled
                 (B.Renew_workspace_intent { request = "attempt-2"; renewal_id }))
        | _ -> settled
      in
      let renewed =
        match B.pending renewed with
        | Some _ ->
            observed renewed ~patch_revision:None ~base_revision:(Some base)
        | None -> renewed
      in
      let renewed, _ =
        B.step renewed
          (B.Workspace_forge_observed { request = "attempt-2"; forge })
      in
      B.equal absent retry && roundtrip lost && roundtrip settled
      && roundtrip renewed
      &&
      match B.workspace_outcome renewed with
      | Some B.Workspace_merged -> merged
      | Some (B.Workspace_base actual) ->
          (not merged) && B.Commit.equal actual base
      | Some (B.Workspace_wait | B.Workspace_patch _ | B.Workspace_attention _)
      | None ->
          false)

let stale_loss_and_renewal =
  QCheck2.Test.make ~name:"old loss and renewal identities stay fenced"
    ~count:100 G.bool (fun reappeared ->
      let before = publishing () in
      let lost, _ = lose before in
      let absent =
        observed lost ~patch_revision:None ~base_revision:(Some base)
      in
      let attention, _ =
        B.step absent
          (B.Workspace_forge_observed
             { request = "attempt-1"; forge = B.Forge_not_merged })
      in
      let wrong, _ =
        B.step attention
          (B.Renew_workspace_intent
             { request = "attempt-2"; renewal_id = "wrong" })
      in
      let receipt =
        match B.workspace_outcome attention with
        | Some (B.Workspace_attention receipt) -> receipt
        | Some
            ( B.Workspace_wait | B.Workspace_patch _ | B.Workspace_base _
            | B.Workspace_merged )
        | None ->
            ""
      in
      let renewed, _ =
        B.step attention
          (B.Renew_workspace_intent
             { request = "attempt-2"; renewal_id = receipt })
      in
      let reused, _ =
        B.step renewed
          (B.Renew_workspace_intent
             { request = "attempt-3"; renewal_id = receipt })
      in
      let observed =
        observed renewed
          ~patch_revision:(if reappeared then Some candidate else None)
          ~base_revision:(Some base)
      in
      let observed, _ =
        B.step observed
          (B.Workspace_forge_observed
             { request = "attempt-2"; forge = B.Forge_not_merged })
      in
      let resumed, _ = B.step observed B.Workspace_resumed in
      let duplicate, effects =
        B.step resumed (B.Workspace_lost { request = "attempt-1" })
      in
      B.equal wrong attention && B.equal reused renewed
      && B.equal duplicate resumed && effects = [] && roundtrip resumed
      &&
      match B.workspace_outcome observed with
      | Some (B.Workspace_patch (head, _)) ->
          reappeared && B.Commit.equal head candidate
      | Some (B.Workspace_base _) -> not reappeared
      | Some (B.Workspace_wait | B.Workspace_attention _ | B.Workspace_merged)
      | None ->
          false)

let () =
  QCheck_base_runner.run_tests_main
    [
      total_matrix;
      loss_fences_old_reply;
      uncertainty_and_renewal;
      stale_loss_and_renewal;
    ]
