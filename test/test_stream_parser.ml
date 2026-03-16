open Base
open Onton.Types

(* ========== Generators ========== *)

let all_stop_reasons =
  Stop_reason.
    [
      End_turn;
      Tool_use;
      Max_tokens;
      Stop_sequence;
      Pause_turn;
      Refusal;
      Model_context_window_exceeded;
    ]

let gen_stop_reason = QCheck2.Gen.oneof_list all_stop_reasons

let stop_reason_to_string = function
  | Stop_reason.End_turn -> "end_turn"
  | Stop_reason.Tool_use -> "tool_use"
  | Stop_reason.Max_tokens -> "max_tokens"
  | Stop_reason.Stop_sequence -> "stop_sequence"
  | Stop_reason.Pause_turn -> "pause_turn"
  | Stop_reason.Refusal -> "refusal"
  | Stop_reason.Model_context_window_exceeded -> "model_context_window_exceeded"

(** Generate safe text that won't break JSON encoding — printable ASCII, no
    backslash or double-quote. *)
let gen_safe_text =
  QCheck2.Gen.(
    string_size
      ~gen:
        (map
           (fun c -> if Char.equal c '\\' || Char.equal c '"' then 'x' else c)
           printable)
      (int_range 0 80))

let gen_tool_name =
  QCheck2.Gen.(string_size ~gen:(char_range 'a' 'z') (int_range 1 20))

(* ========== Round-trip generators: produce (json_line, expected_event) ========== *)

let gen_text_delta_pair =
  QCheck2.Gen.(
    map
      (fun text ->
        let json =
          Printf.sprintf
            {|{"type":"content_block_delta","delta":{"type":"text_delta","text":"%s"}}|}
            text
        in
        (json, Stream_event.Text_delta text))
      gen_safe_text)

let gen_tool_use_pair =
  QCheck2.Gen.(
    map
      (fun name ->
        let json =
          Printf.sprintf
            {|{"type":"content_block_start","content_block":{"type":"tool_use","name":"%s"}}|}
            name
        in
        (json, Stream_event.Tool_use { name; input = "" }))
      gen_tool_name)

let gen_result_pair =
  QCheck2.Gen.(
    map2
      (fun text stop_reason ->
        let sr_str = stop_reason_to_string stop_reason in
        let json =
          Printf.sprintf {|{"type":"result","result":"%s","stop_reason":"%s"}|}
            text sr_str
        in
        (json, Stream_event.Final_result { text; stop_reason }))
      gen_safe_text gen_stop_reason)

let gen_message_delta_pair =
  QCheck2.Gen.(
    map
      (fun stop_reason ->
        let sr_str = stop_reason_to_string stop_reason in
        let json =
          Printf.sprintf
            {|{"type":"message_delta","delta":{"stop_reason":"%s"}}|} sr_str
        in
        (json, Stream_event.Final_result { text = ""; stop_reason }))
      gen_stop_reason)

let gen_error_pair =
  QCheck2.Gen.(
    map
      (fun msg ->
        let json =
          Printf.sprintf {|{"type":"error","error":{"message":"%s"}}|} msg
        in
        (json, Stream_event.Error msg))
      gen_safe_text)

(* ========== Properties ========== *)

let () =
  let open QCheck2 in
  (* Property: text_delta round-trips *)
  let prop_text_delta =
    Test.make ~name:"stream: text_delta round-trips" gen_text_delta_pair
      (fun (json, expected) ->
        match Onton.Claude_runner.parse_stream_event json with
        | Some evt -> Stream_event.equal evt expected
        | None -> false)
  in

  (* Property: tool_use round-trips *)
  let prop_tool_use =
    Test.make ~name:"stream: tool_use round-trips" gen_tool_use_pair
      (fun (json, expected) ->
        match Onton.Claude_runner.parse_stream_event json with
        | Some evt -> Stream_event.equal evt expected
        | None -> false)
  in

  (* Property: result round-trips *)
  let prop_result =
    Test.make ~name:"stream: result round-trips" gen_result_pair
      (fun (json, expected) ->
        match Onton.Claude_runner.parse_stream_event json with
        | Some evt -> Stream_event.equal evt expected
        | None -> false)
  in

  (* Property: message_delta round-trips *)
  let prop_message_delta =
    Test.make ~name:"stream: message_delta round-trips" gen_message_delta_pair
      (fun (json, expected) ->
        match Onton.Claude_runner.parse_stream_event json with
        | Some evt -> Stream_event.equal evt expected
        | None -> false)
  in

  (* Property: error round-trips *)
  let prop_error =
    Test.make ~name:"stream: error round-trips" gen_error_pair
      (fun (json, expected) ->
        match Onton.Claude_runner.parse_stream_event json with
        | Some evt -> Stream_event.equal evt expected
        | None -> false)
  in

  (* Property: unknown type returns None *)
  let prop_unknown_type =
    Test.make ~name:"stream: unknown type returns None"
      Gen.(string_size ~gen:(char_range 'a' 'z') (int_range 3 20))
      (fun typ ->
        let json = Printf.sprintf {|{"type":"%s"}|} typ in
        match Onton.Claude_runner.parse_stream_event json with
        | None -> true
        | Some _ ->
            (* If the random type happens to match a known type, that's fine *)
            List.mem
              [
                "content_block_delta";
                "content_block_start";
                "message_stop";
                "message_delta";
                "result";
                "error";
              ]
              typ ~equal:String.equal)
  in

  (* Property: parse_stream_event is total — never raises *)
  let prop_total =
    Test.make ~name:"stream: parse never raises"
      Gen.(string_size ~gen:char (int_range 0 200))
      (fun s ->
        let (_ : Stream_event.t option) =
          Onton.Claude_runner.parse_stream_event s
        in
        true)
  in

  List.iter
    ~f:(fun t -> QCheck2.Test.check_exn t)
    [
      prop_text_delta;
      prop_tool_use;
      prop_result;
      prop_message_delta;
      prop_error;
      prop_unknown_type;
      prop_total;
    ];
  Stdlib.print_endline "stream parser: all properties passed"
