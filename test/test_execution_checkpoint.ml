(* @archlint.module test
   @archlint.domain worker-checkpoint *)

open Onton
open Onton_core
open Onton_test_support.Branch_reconcile_git_fixture

let check label condition = if not condition then failwith label

let () =
  Eio_main.run @@ fun _ ->
  let runtime = make_runtime () in
  let before = Runtime.read runtime (fun snap -> snap.Runtime.orchestrator) in
  let entered, signal_entered = Eio.Promise.create () in
  let ack, signal_ack = Eio.Promise.create () in
  let finished = ref false in
  Eio.Fiber.both
    (fun () ->
      let result =
        Runtime.update_persisting runtime
          ~persist:(fun _ ->
            Eio.Promise.resolve signal_entered ();
            Eio.Promise.await ack;
            Ok ())
          (fun snap ->
            let orchestrator =
              Orchestrator.set_worktree_path snap.Runtime.orchestrator
                (Types.Patch_id.of_string "1")
                "/acknowledged"
            in
            ({ snap with Runtime.orchestrator }, ()))
      in
      check "accepted checkpoint" (Result.is_ok result);
      finished := true)
    (fun () ->
      Eio.Promise.await entered;
      check "candidate not published before ACK" (not !finished);
      (* The ACK path uses its own mailbox. It must not call Runtime.read here:
         update_persisting intentionally holds the runtime mutex. *)
      Eio.Promise.resolve signal_ack ());
  let after = Runtime.read runtime (fun snap -> snap.Runtime.orchestrator) in
  check "ACK publishes candidate" (not (before == after));
  let rejected =
    Runtime.update_persisting runtime
      ~persist:(fun _ -> Error "rejected")
      (fun snap -> ({ snap with Runtime.orchestrator = before }, ()))
  in
  check "rejected checkpoint" (Result.is_error rejected);
  check "rejection retains prior memory"
    (Runtime.read runtime (fun snap -> snap.Runtime.orchestrator == after));
  print_endline "execution checkpoint barrier: acknowledged, rejected, delayed"
