(* @archlint.module test
   @archlint.domain test-support *)

open Base
open Onton_core
module B = Branch_reconcile

(* Stable valid Git-shaped identities for arbitrary property-test labels. *)
let sha label = "00000000" ^ Stdlib.Digest.to_hex (Stdlib.Digest.string label)

(* Drive the owner protocol: a fixture cannot manufacture a receipt or turn an
   arbitrary scalar into publication authority. *)
let completion ?(at = 100.) ~state t result =
  match B.pending (state t) with
  | None ->
      QCheck2.Test.fail_reportf "publication fixture expected an owned command"
  | Some command -> B.Result { token = command.token; at; result }

let reply ?at ~step ~state t result = step t (completion ?at ~state t result)

let publishing ~candidate ~step ~state initial =
  let revision =
    match B.Commit.make candidate with
    | Some revision -> revision
    | None ->
        QCheck2.Test.fail_reportf
          "publication fixture requires a valid candidate"
  in
  let reply = reply ~step ~state in
  let t =
    step initial
      (B.Request
         {
           base = "main";
           policy = Rewrite;
           purpose = Publish_revision revision;
         })
    |> fun t ->
    reply t
      (B.Observed
         {
           destination = B.Remote_id.of_destination "fixture-origin";
           head = revision;
           source = revision;
           target = revision;
           remote = None;
           boundary = Recorded revision;
           topology = Includes;
           clean = true;
           sequencer = None;
           conflicts = 0;
           target_included = true;
           base_contains_source = false;
           completed_integration = false;
         })
    |> fun t -> reply t B.Pinned
  in
  assert (
    Option.exists
      (B.pending (state t))
      ~f:(fun command ->
        B.equal_command_kind command.kind
          (B.Publish { candidate = revision; expected = None })));
  t

let confirmed ~candidate ~step ~state initial =
  let t = publishing ~candidate ~step ~state initial in
  let revision =
    match B.publication_observation_target (state t) with
    | Some identity -> identity.revision
    | None -> QCheck2.Test.fail_reportf "publication fixture missing candidate"
  in
  let t = reply ~step ~state t B.Published in
  let t =
    reply ~step ~state t (B.Remote { sha = Some revision; topology = Equal })
  in
  assert (Option.equal B.equal_phase (B.phase (state t)) (Some B.Settled));
  assert (Option.is_some (B.publication_observation_pending (state t)));
  t

let agent ~candidate t =
  confirmed ~candidate
    ~step:(fun t event -> fst (Patch_agent.reconcile_branch t event))
    ~state:(fun t -> t.Patch_agent.branch_reconcile)
    t

(* A no-op integration still records the observed base through the owner. *)
let reconciled_agent ~base t =
  let step t event = fst (Patch_agent.reconcile_branch t event) in
  let state t = t.Patch_agent.branch_reconcile in
  let sequence =
    match B.operation (state t) with None -> 1 | Some op -> op.B.id + 1
  in
  let revision =
    match B.Commit.make (sha ("base:" ^ Types.Branch.to_string base)) with
    | Some revision -> revision
    | None -> QCheck2.Test.fail_reportf "invalid fixture revision"
  in
  let reply = reply ~step ~state in
  let t =
    step t
      (B.Request
         {
           base = Types.Branch.to_string base;
           policy = Preserve_ancestry;
           purpose = Reconcile_request ("base-fixture:" ^ Int.to_string sequence);
         })
  in
  let t =
    reply t
      (B.Observed
         {
           destination = B.Remote_id.of_destination "fixture-origin";
           head = revision;
           source = revision;
           target = revision;
           remote = Some revision;
           boundary = Plain;
           topology = Equal;
           clean = true;
           sequencer = None;
           conflicts = 0;
           target_included = true;
           base_contains_source = false;
           completed_integration = false;
         })
  in
  let t = reply t B.Pinned in
  let t = reply t (B.Remote { sha = Some revision; topology = Equal }) in
  assert (Option.equal B.equal_phase (B.phase (state t)) (Some B.Settled));
  let head = Some (B.Commit.to_string revision) in
  Patch_agent.observe_publication_head (Patch_agent.set_head_oid t head) head
