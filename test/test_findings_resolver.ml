(* @archlint.module test
   @archlint.domain findings-registry *)

open Onton
open Onton_core

let entry ~finding_id : Findings_registry.entry =
  {
    backend_name = "fake";
    owner = "owner";
    repo = "repo";
    pr_number = 42;
    finding_id;
  }

let finding ~id : Review_service.finding =
  {
    id;
    github_comment_id = None;
    posting_sha = "deadbeef";
    path = "lib/example.ml";
    start_line = 1;
    end_line = 1;
    severity = Review_service.Note;
    body = "finding";
    created_at = "2026-08-07T00:00:00Z";
    outcome =
      {
        kind = Review_service.Outstanding;
        detected_at = None;
        actor = None;
        reason = None;
        last_reply = None;
      };
  }

let active_registration :
    (Findings_registry.t * string * Findings_registry.entry) option ref =
  ref None

module Fake_review_client = struct
  type error = Review_service_client.error

  let show_error = Review_service_client.show_error
  let name = "fake"

  let list_findings ~owner:_ ~repo:_ ~pr_number:_ () =
    failwith "list_findings is not used by this test"

  let mark_resolved ~owner:_ ~repo:_ ~pr_number:_ ~finding_id ~kind:_ ?actor:_
      ?reason:_ () =
    (match !active_registration with
    | Some (registry, key, replacement) ->
        Findings_registry.register registry ~key replacement
    | None -> failwith "missing replacement registry entry");
    let outcome : Review_service.outcome =
      {
        kind = Review_service.Addressed;
        detected_at = None;
        actor = None;
        reason = None;
        last_reply = None;
      }
    in
    Ok ({ id = finding_id; outcome } : Review_service.resolve_response)
end

let expect_some_finding_id expected = function
  | Some (actual : Findings_registry.entry) ->
      if not (String.equal actual.finding_id expected) then
        failwith
          (Printf.sprintf "expected finding id %s, got %s" expected
             actual.finding_id)
  | None -> failwith (Printf.sprintf "expected registry entry %s" expected)

let test_take_removes_once () =
  let registry = Findings_registry.create () in
  let key = "key" in
  Findings_registry.register registry ~key (entry ~finding_id:"original");
  Findings_registry.take registry ~key |> expect_some_finding_id "original";
  match Findings_registry.take registry ~key with
  | None -> ()
  | Some _ -> failwith "take returned an entry twice"

let test_resolver_preserves_concurrent_refresh () =
  let registry = Findings_registry.create () in
  let key =
    Findings_registry.make_key ~backend_name:"fake" ~owner:"owner" ~repo:"repo"
      ~pr_number:42 ~finding_id:"raw-id"
  in
  Findings_registry.register registry ~key (entry ~finding_id:"raw-id");
  active_registration := Some (registry, key, entry ~finding_id:"refreshed-id");
  Fun.protect
    ~finally:(fun () -> active_registration := None)
    (fun () ->
      Findings_resolver.resolve_after_session
        ~review_clients:
          [
            (module Fake_review_client : Review_service_client.S
              with type error = Review_service_client.error);
          ]
        ~log:ignore ~findings_registry:registry
        ~artifact_dir:
          (Filename.concat
             (Filename.get_temp_dir_name ())
             (Printf.sprintf "onton-missing-wontfix-%d" (Unix.getpid ())))
        ~delivered:[ finding ~id:key ]
        ());
  Findings_registry.take registry ~key |> expect_some_finding_id "refreshed-id"

let () =
  Eio_main.run @@ fun _env ->
  test_take_removes_once ();
  test_resolver_preserves_concurrent_refresh ()
