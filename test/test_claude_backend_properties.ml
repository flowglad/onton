(* @archlint.module test
   @archlint.domain claude-backend *)

open Onton

(* [create] must preserve the [name] it is given on the resulting backend
   record, regardless of the generated name string. *)
let create_preserves_generated_name =
  QCheck2.Test.make ~name:"claude backend creation preserves generated name"
    ~count:50 QCheck2.Gen.string_small (fun name ->
      Eio_main.run @@ fun env ->
      let backend =
        Claude_backend.create ~name ~model:None
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~clock:(Eio.Stdenv.clock env) ~timeout:1.0 ~setsid_exec:None
      in
      String.equal backend.Llm_backend.name name)

let () = QCheck2.Test.check_exn create_preserves_generated_name
