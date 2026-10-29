(* @archlint.module test
   @archlint.domain coordinator *)

open Onton
open Onton_core

let check ok message = if not ok then failwith message

let () =
  let helper = Filename.temp_file "onton-command-forge" ".sh" in
  Fun.protect
    ~finally:(fun () -> Sys.remove helper)
    (fun () ->
      let output = open_out_bin helper in
      output_string output
        "#!/bin/sh\n\
         cat >/dev/null\n\
         printf '%s\\n' \\\n\
        \         '{\"ok\":false,\"error\":\"queue \
         required\",\"duplicate_change\":true,\"permanent\":true,\"merge_queue_required\":true}'\n";
      close_out output;
      Unix.chmod helper 0o700;
      let module F = (val Coordinator_command_forge.make ~helper ~owner:"owner")
      in
      match F.pr_state (Types.Pr_number.of_int 1) with
      | Ok _ -> failwith "helper failure returned as success"
      | Error error ->
          check
            (F.show_error error = "queue required")
            "helper error message discarded";
          check
            (F.is_duplicate_change_error error)
            "duplicate-change classification discarded";
          check
            (F.is_permanent_error error)
            "permanent classification discarded";
          check
            (F.is_merge_queue_required_error error)
            "merge-queue-required classification discarded");
  print_endline
    "coordinator command forge: structured error classifications preserved"
