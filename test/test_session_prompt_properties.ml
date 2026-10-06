(* @archlint.module test
   @archlint.domain session-driver *)

open Onton_core

let () =
  let open QCheck2 in
  let input = Gen.(pair string string) in
  let tests =
    [
      Test.make ~name:"fresh sessions receive exact context and turn" ~count:500
        input (fun (context, turn) ->
          let prompt = Session_prompt.create ~context ~turn in
          String.equal
            (Session_prompt.render ~resume_session:None prompt)
            (context ^ turn));
      Test.make
        ~name:"existing IDs receive exact turn regardless of context or ID"
        ~count:500
        Gen.(triple string string string)
        (fun (context, turn, id) ->
          String.equal
            (Session_prompt.render ~resume_session:(Some id)
               (Session_prompt.create ~context ~turn))
            turn);
      Test.make
        ~name:
          "full context occurs once per session across resume and fresh \
           fallback interleavings"
        ~count:500
        Gen.(list bool)
        (fun resets ->
          let context = "<complete patch context>" in
          let turn = "<next turn>" in
          let prompt = Session_prompt.create ~context ~turn in
          let rec run session deliveries next_id = function
            | [] -> true
            | reset :: rest -> (
                let session = if reset then None else session in
                let sent =
                  Session_prompt.render ~resume_session:session prompt
                in
                match session with
                | None ->
                    String.equal sent (context ^ turn)
                    && run (Some (string_of_int next_id)) 1 (next_id + 1) rest
                | Some _ ->
                    String.equal sent turn && deliveries = 1
                    && run session deliveries next_id rest)
          in
          run None 0 0 resets);
      Test.make
        ~name:
          "rendering is deterministic for arbitrary fresh and existing sessions"
        ~count:500
        Gen.(triple string string (option string))
        (fun (context, turn, resume_session) ->
          let prompt = Session_prompt.create ~context ~turn in
          String.equal
            (Session_prompt.render ~resume_session prompt)
            (Session_prompt.render ~resume_session prompt));
    ]
  in
  QCheck_base_runner.run_tests_main tests
