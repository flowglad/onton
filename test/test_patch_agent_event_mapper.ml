(* @archlint.module test
   @archlint.domain patch-agent-event-mapper *)

open Base
open Onton
open Onton_core

let gen_char =
  QCheck2.Gen.map
    (fun c -> if Char.( = ) c '\n' || Char.( = ) c '\r' then 'x' else c)
    QCheck2.Gen.printable

let gen_string =
  QCheck2.Gen.string_size ~gen:gen_char (QCheck2.Gen.int_range 0 128)

let gen_int = QCheck2.Gen.int_range 0 1_000_000

let gen_usage =
  let open QCheck2.Gen in
  let* input_tokens = gen_int in
  let* output_tokens = gen_int in
  let* cache_read_tokens = gen_int in
  let* cache_write_tokens = gen_int in
  return
    Patch_agent_rpc.
      { input_tokens; output_tokens; cache_read_tokens; cache_write_tokens }

let gen_event =
  let open QCheck2.Gen in
  oneof_weighted
    [
      ( 1,
        let* session_id = gen_string in
        let* model_id = gen_string in
        let* provider = gen_string in
        return (Patch_agent_rpc.Session_init { session_id; model_id; provider })
      );
      ( 1,
        let* turn_index = gen_int in
        return (Patch_agent_rpc.Turn_started { turn_index }) );
      ( 2,
        let* delta = gen_string in
        return (Patch_agent_rpc.Text_delta { delta }) );
      ( 2,
        let* name = gen_string in
        let* input = gen_string in
        let* call_id = gen_string in
        return (Patch_agent_rpc.Tool_call { name; input; call_id }) );
      ( 2,
        let* stop_reason = gen_string in
        let* final_text = gen_string in
        let* usage = option gen_usage in
        return (Patch_agent_rpc.Done { stop_reason; final_text; usage }) );
      ( 1,
        let* code = gen_string in
        let* message = gen_string in
        return (Patch_agent_rpc.Error { code; message }) );
    ]

let prop_total =
  QCheck2.Test.make ~name:"Patch_agent_event_mapper > mapping is total"
    ~count:1000 gen_event (fun event ->
      try
        ignore (Patch_agent_event_mapper.map_event event);
        true
      with _ -> false)

let prop_session_id_preserved =
  QCheck2.Test.make ~name:"Patch_agent_event_mapper > session_id preserved"
    ~count:500 gen_string (fun session_id ->
      Types.Stream_event.equal
        (Patch_agent_event_mapper.map_event
           (Patch_agent_rpc.Session_init
              { session_id; model_id = "model"; provider = "provider" }))
        (Types.Stream_event.Session_init
           {
             session_id;
             api_key_source = None;
             model = None;
             claude_code_version = None;
             permission_mode = None;
           }))

let prop_tool_call_preserved =
  QCheck2.Test.make
    ~name:"Patch_agent_event_mapper > tool_call name and input preserved"
    ~count:500
    QCheck2.(Gen.pair gen_string gen_string)
    (fun (name, input) ->
      Types.Stream_event.equal
        (Patch_agent_event_mapper.map_event
           (Patch_agent_rpc.Tool_call { name; input; call_id = "call-id" }))
        (Types.Stream_event.Tool_use { name; input; status = None }))

let prop_final_text_preserved =
  QCheck2.Test.make ~name:"Patch_agent_event_mapper > final_text preserved"
    ~count:500 gen_string (fun final_text ->
      Types.Stream_event.equal
        (Patch_agent_event_mapper.map_event
           (Patch_agent_rpc.Done
              { stop_reason = "stop"; final_text; usage = None }))
        (Types.Stream_event.Final_result
           { text = final_text; stop_reason = Types.Stop_reason.End_turn }))

let prop_stop_reason_mapping_contract =
  QCheck2.Test.make ~name:"Patch_agent_event_mapper > stop_reason contract"
    QCheck2.Gen.unit (fun () ->
      let check raw expected =
        Types.Stream_event.equal
          (Patch_agent_event_mapper.map_event
             (Patch_agent_rpc.Done
                { stop_reason = raw; final_text = "t"; usage = None }))
          (Types.Stream_event.Final_result
             { text = "t"; stop_reason = expected })
      in
      check "stop" Types.Stop_reason.End_turn
      && check "tool_use" Types.Stop_reason.Tool_use
      && check "max_tokens" Types.Stop_reason.Max_tokens
      && check "error" Types.Stop_reason.End_turn
      && check "some_future_value" Types.Stop_reason.End_turn)

let prop_whitespace_code_uses_message =
  QCheck2.Test.make
    ~name:"Patch_agent_event_mapper > whitespace-only code uses message"
    QCheck2.Gen.unit (fun () ->
      Types.Stream_event.equal
        (Patch_agent_event_mapper.map_event
           (Patch_agent_rpc.Error { code = "   "; message = "oops" }))
        (Types.Stream_event.Error "oops"))

let prop_deterministic =
  QCheck2.Test.make ~name:"Patch_agent_event_mapper > deterministic" ~count:500
    gen_event (fun event ->
      Types.Stream_event.equal
        (Patch_agent_event_mapper.map_event event)
        (Patch_agent_event_mapper.map_event event))

let prop_public_surface_is_linked =
  QCheck2.Test.make ~name:"Patch_agent_event_mapper > public surface is linked"
    QCheck2.Gen.unit (fun () ->
      ignore Patch_agent_event_mapper.parse_stop_reason;
      true)

let () =
  let suite =
    [
      prop_total;
      prop_session_id_preserved;
      prop_tool_call_preserved;
      prop_final_text_preserved;
      prop_stop_reason_mapping_contract;
      prop_whitespace_code_uses_message;
      prop_deterministic;
      prop_public_surface_is_linked;
    ]
  in
  let exit_code = QCheck_base_runner.run_tests ~verbose:true suite in
  if exit_code <> 0 then Stdlib.exit exit_code
