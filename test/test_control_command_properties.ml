(* @archlint.module test
   @archlint.domain control-command *)

open Onton_core

let arbitrary_json =
  QCheck2.Gen.(
    oneof
      [ map (fun s -> `String s) string; map (fun n -> `Int n) int; pure `Null ])

let totality =
  QCheck2.Test.make ~name:"control command decoding is total" ~count:1000
    arbitrary_json (fun json ->
      ignore (Control_command.decode json);
      true)

let roundtrip =
  QCheck2.Test.make ~name:"set automerge preserves id, patch, and value"
    ~count:500
    QCheck2.Gen.(triple string string bool)
    (fun (id, patch_id, enabled) ->
      if id = "" || patch_id = "" then true
      else
        let json =
          `Assoc
            [
              ("version", `Int 1);
              ("id", `String id);
              ("type", `String "set_automerge");
              ( "payload",
                `Assoc
                  [ ("patch_id", `String patch_id); ("enabled", `Bool enabled) ]
              );
            ]
        in
        match Control_command.decode json with
        | Ok (Control_command.Set_automerge command) ->
            command.id = id
            && Types.Patch_id.to_string command.patch_id = patch_id
            && command.enabled = enabled
        | Ok (Control_command.Bump _ | Control_command.Send_human_message _) ->
            false
        | Error _ -> false)

let envelope ~id ~kind payload =
  `Assoc
    [
      ("version", `Int 1);
      ("id", `String id);
      ("type", `String kind);
      ("payload", `Assoc payload);
    ]

let bump_roundtrip =
  QCheck2.Test.make ~name:"bump preserves id and patch" ~count:500
    QCheck2.Gen.(pair string string)
    (fun (id, patch_id) ->
      if id = "" || patch_id = "" then true
      else
        match
          Control_command.decode
            (envelope ~id ~kind:"bump" [ ("patch_id", `String patch_id) ])
        with
        | Ok (Control_command.Bump command) ->
            command.id = id
            && Types.Patch_id.to_string command.patch_id = patch_id
        | Ok
            ( Control_command.Set_automerge _
            | Control_command.Send_human_message _ )
        | Error _ ->
            false)

let human_message_roundtrip =
  QCheck2.Test.make ~name:"human message preserves id, patch, and message"
    ~count:500
    QCheck2.Gen.(triple string string string)
    (fun (id, patch_id, message) ->
      if id = "" || patch_id = "" || String.trim message = "" then true
      else
        match
          Control_command.decode
            (envelope ~id ~kind:"send_human_message"
               [ ("patch_id", `String patch_id); ("message", `String message) ])
        with
        | Ok (Control_command.Send_human_message command) ->
            command.id = id
            && Types.Patch_id.to_string command.patch_id = patch_id
            && command.message = message
        | Ok (Control_command.Set_automerge _ | Control_command.Bump _)
        | Error _ ->
            false)

let rejects_invalid_new_commands =
  QCheck2.Test.make ~name:"bump and human message require valid fields" ~count:1
    QCheck2.Gen.unit (fun () ->
      let invalid =
        [
          envelope ~id:"" ~kind:"bump" [ ("patch_id", `String "patch") ];
          envelope ~id:"id" ~kind:"bump" [ ("patch_id", `String "") ];
          envelope ~id:"id" ~kind:"bump" [];
          envelope ~id:"" ~kind:"send_human_message"
            [ ("patch_id", `String "patch"); ("message", `String "text") ];
          envelope ~id:"id" ~kind:"send_human_message"
            [ ("patch_id", `String ""); ("message", `String "text") ];
          envelope ~id:"id" ~kind:"send_human_message"
            [ ("patch_id", `String "patch"); ("message", `String " \t ") ];
          envelope ~id:"id" ~kind:"send_human_message"
            [ ("patch_id", `String "patch") ];
        ]
      in
      List.for_all
        (fun json -> Result.is_error (Control_command.decode json))
        invalid)

let retained_ids_window =
  QCheck2.Test.make ~name:"control id retention keeps the newest 1024 IDs"
    ~count:1 QCheck2.Gen.unit (fun () ->
      let limit = Control_command.max_retained_ids in
      let rec apply next ids =
        if next = limit + 20 then ids
        else
          apply (next + 1) (Control_command.record_id (string_of_int next) ids)
      in
      let ids = apply 0 [] in
      List.length ids = limit
      && (match ids with
        | head :: _ -> head = string_of_int (limit + 19)
        | [] -> false)
      && List.nth_opt ids (limit - 1) = Some "20"
      && not (List.mem "19" ids))

let rejects_incomplete =
  QCheck2.Test.make ~name:"commands require every field" ~count:1
    QCheck2.Gen.unit (fun () ->
      Result.is_error
        (Control_command.decode
           (`Assoc
              [
                ("version", `Int 1);
                ("id", `String "1");
                ("type", `String "set_automerge");
                ("payload", `Assoc [ ("patch_id", `String "1") ]);
              ])))

let rejects_unsupported_versions =
  QCheck2.Test.make ~name:"commands require supported envelope version" ~count:1
    QCheck2.Gen.unit (fun () ->
      let fields =
        [
          ("id", `String "1");
          ("type", `String "set_automerge");
          ( "payload",
            `Assoc [ ("patch_id", `String "1"); ("enabled", `Bool true) ] );
        ]
      in
      List.for_all
        (fun version ->
          let fields =
            match version with
            | None -> fields
            | Some value -> ("version", value) :: fields
          in
          Control_command.decode (`Assoc fields)
          = Error "unsupported command version")
        [ None; Some (`Int 0); Some (`Int 2); Some (`String "1"); Some `Null ])

let () =
  QCheck2.Test.check_exn totality;
  QCheck2.Test.check_exn roundtrip;
  QCheck2.Test.check_exn bump_roundtrip;
  QCheck2.Test.check_exn human_message_roundtrip;
  QCheck2.Test.check_exn rejects_invalid_new_commands;
  QCheck2.Test.check_exn retained_ids_window;
  QCheck2.Test.check_exn rejects_incomplete;
  QCheck2.Test.check_exn rejects_unsupported_versions
