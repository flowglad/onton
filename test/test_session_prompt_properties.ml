(* @archlint.module test
   @archlint.domain session-driver *)

open Onton_core

let contains text needle =
  let text_length = String.length text
  and needle_length = String.length needle in
  let rec scan index =
    index + needle_length <= text_length
    && (String.sub text index needle_length = needle || scan (index + 1))
  in
  scan 0

let () =
  let open QCheck2 in
  let input = Gen.(pair string string) in
  let backend_names =
    Gen.oneof
      [
        Gen.string;
        Gen.oneof_list
          [
            "pi"; "Pi"; "PI"; "pI"; ""; "claude"; "codex"; "opencode"; "gemini";
          ];
      ]
  in
  let tests =
    [
      Test.make ~name:"backend response transport is total and case invariant"
        ~count:500 backend_names (fun backend_name ->
          Session_prompt.response_transport ~backend_name
          = Session_prompt.response_transport
              ~backend_name:(String.uppercase_ascii backend_name));
      Test.make ~name:"only Pi selects tool responses" ~count:500 backend_names
        (fun backend_name ->
          Session_prompt.response_transport ~backend_name
          = Session_prompt.Pi_tools
          = String.equal (String.lowercase_ascii backend_name) "pi");
      Test.make ~name:"opt-out offers preserve eligibility in both transports"
        ~count:500 Gen.string (fun artifact_path ->
          List.for_all
            (fun response_transport ->
              let text =
                Session_prompt.render_opt_out ~response_transport ~artifact_path
              in
              (match response_transport with
                | Session_prompt.File_artifacts ->
                    contains text
                      (Printf.sprintf "write your reason to `%s`" artifact_path)
                | Session_prompt.Pi_tools -> true)
              && contains text "Before making your first commit"
              && contains text "unavailable after any patch commit or PR exists"
              && contains text "end your turn without committing")
            [ Session_prompt.File_artifacts; Session_prompt.Pi_tools ]);
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
      Test.make ~name:"recovery context preserves all supplied text" ~count:500
        Gen.(tup5 string string string string string)
        (fun (requirements, operation, guidance, patch, base) ->
          let context =
            Session_prompt.create_recovery ~requirements ~operation
              ~guidance:[ guidance ]
              ~patch_revision:(Session_prompt.Revision patch)
              ~base_revision:base
          in
          let rendered = Session_prompt.render_recovery context in
          List.for_all (contains rendered)
            [
              Printf.sprintf "Patch requirements (%d bytes):\n%s\n"
                (String.length requirements)
                requirements;
              Printf.sprintf "Outstanding operation (%d bytes):\n%s\n"
                (String.length operation) operation;
              Printf.sprintf "Guidance 1 (%d bytes):\n%s\n"
                (String.length guidance) guidance;
              Printf.sprintf "Observed patch revision (%d bytes):\n%s\n"
                (String.length patch) patch;
              Printf.sprintf "Observed base revision (%d bytes):\n%s\n"
                (String.length base) base;
              "previous workspace and conversation are unavailable";
              "Inspect the current checkout";
              "Validate the resulting work";
            ]);
    ]
  in
  QCheck_base_runner.run_tests_main tests
