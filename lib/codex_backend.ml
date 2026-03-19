open Base

let build_args ~cwd_path ~prompt ~continue =
  if continue then
    [ "codex"; "exec"; "resume"; "--last"; "--json"; "-C"; cwd_path ]
  else
    [
      "codex";
      "exec";
      prompt;
      "--json";
      "--dangerously-bypass-approvals-and-sandbox";
      "-C";
      cwd_path;
    ]

let parse_event (line : string) : Types.Stream_event.t list =
  match Yojson.Safe.from_string line with
  | json -> (
      let open Yojson.Safe.Util in
      let typ = member "type" json |> to_string_option in
      match typ with
      | Some "item.completed" -> (
          let item = member "item" json in
          let item_type = member "type" item |> to_string_option in
          match item_type with
          | Some "agent_message" ->
              let content =
                match member "content" item with
                | `List l ->
                    List.filter_map l ~f:(fun block ->
                        match member "type" block |> to_string_option with
                        | Some "output_text" ->
                            member "text" block |> to_string_option
                        | _ -> None)
                | _ -> []
              in
              let text = String.concat ~sep:"" content in
              if String.is_empty text then []
              else [ Types.Stream_event.Text_delta text ]
          | Some "command_execution" -> []
          | _ -> [])
      | Some "item.started" -> (
          let item = member "item" json in
          let item_type = member "type" item |> to_string_option in
          match item_type with
          | Some "command_execution" -> (
              match member "command" item |> to_string_option with
              | Some cmd when not (String.is_empty cmd) ->
                  [ Types.Stream_event.Tool_use { name = "Bash"; input = cmd } ]
              | _ -> [])
          | _ -> [])
      | Some "turn.completed" ->
          [
            Types.Stream_event.Final_result
              { text = ""; stop_reason = Types.Stop_reason.End_turn };
          ]
      | Some "error" ->
          let msg =
            member "message" json |> to_string_option
            |> Option.value ~default:"unknown codex error"
          in
          [ Types.Stream_event.Error msg ]
      | _ -> [])
  | exception Yojson.Json_error _ -> []
  | exception Yojson.Safe.Util.Type_error _ -> []

let run_streaming ~process_mgr ~cwd ~patch_id ~prompt ~continue ~on_event =
  ignore (patch_id : Types.Patch_id.t);
  let cwd_path = snd cwd in
  let args = build_args ~cwd_path ~prompt ~continue in
  let process_line line =
    let trimmed = String.strip line in
    if String.is_empty trimmed then [] else parse_event trimmed
  in
  Llm_backend.spawn_and_stream ~process_mgr ~cwd ~args ~process_line ~on_event

let create ~process_mgr : Llm_backend.t =
  {
    name = "Codex";
    run_streaming =
      (fun ~cwd ~patch_id ~prompt ~continue ~on_event ->
        run_streaming ~process_mgr ~cwd ~patch_id ~prompt ~continue ~on_event);
  }

let%test "build_args without continue" =
  let args =
    build_args ~cwd_path:"/tmp/work" ~prompt:"do stuff" ~continue:false
  in
  List.equal String.equal args
    [
      "codex";
      "exec";
      "do stuff";
      "--json";
      "--dangerously-bypass-approvals-and-sandbox";
      "-C";
      "/tmp/work";
    ]

let%test "build_args with continue" =
  let args =
    build_args ~cwd_path:"/tmp/work" ~prompt:"do stuff" ~continue:true
  in
  List.equal String.equal args
    [ "codex"; "exec"; "resume"; "--last"; "--json"; "-C"; "/tmp/work" ]

let%test "parse_event agent_message" =
  let line =
    {|{"type":"item.completed","item":{"type":"agent_message","content":[{"type":"output_text","text":"hello"}]}}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [ Types.Stream_event.Text_delta "hello" ]

let%test "parse_event command_execution started" =
  let line =
    {|{"type":"item.started","item":{"type":"command_execution","command":"ls -la"}}|}
  in
  List.equal Types.Stream_event.equal (parse_event line)
    [ Types.Stream_event.Tool_use { name = "Bash"; input = "ls -la" } ]

let%test "parse_event command_execution completed is ignored" =
  let line =
    {|{"type":"item.completed","item":{"type":"command_execution","command":"ls -la"}}|}
  in
  List.is_empty (parse_event line)

let%test "parse_event turn.completed" =
  let line = {|{"type":"turn.completed"}|} in
  List.equal Types.Stream_event.equal (parse_event line)
    [
      Types.Stream_event.Final_result
        { text = ""; stop_reason = Types.Stop_reason.End_turn };
    ]

let%test "parse_event error" =
  let line = {|{"type":"error","message":"rate limited"}|} in
  List.equal Types.Stream_event.equal (parse_event line)
    [ Types.Stream_event.Error "rate limited" ]

let%test "parse_event invalid json" =
  List.is_empty (parse_event "not json at all")

let%test "parse_event unknown type" =
  List.is_empty (parse_event {|{"type":"ping"}|})
