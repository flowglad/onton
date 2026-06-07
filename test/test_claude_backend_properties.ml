(* @archlint.module test
   @archlint.domain claude-backend *)

open Onton

let create_exposes_backend_name =
  QCheck2.Test.make ~name:"claude backend creation preserves name" ~count:1
    QCheck2.Gen.unit (fun () ->
      Eio_main.run @@ fun env ->
      let backend =
        Claude_backend.create ~name:"claude" ~model:None
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~clock:(Eio.Stdenv.clock env) ~timeout:1.0 ~setsid_exec:None
      in
      String.equal backend.Llm_backend.name "claude")

let () = QCheck2.Test.check_exn create_exposes_backend_name
