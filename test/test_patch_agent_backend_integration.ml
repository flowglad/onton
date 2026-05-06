open Base

let pending_test =
  QCheck2.Test.make
    ~name:"Patch_agent_backend_integration > one-turn smoke against real binary"
    ~count:1 QCheck2.Gen.unit (fun () ->
      (* PENDING: implemented in patch 6 *)
      Stdlib.prerr_endline
        "SKIP: pending integration smoke for patch-agent backend (implemented \
         in patch 6)";
      true)

let () =
  let exit_code = QCheck_base_runner.run_tests ~verbose:true [ pending_test ] in
  if exit_code <> 0 then Stdlib.exit exit_code
