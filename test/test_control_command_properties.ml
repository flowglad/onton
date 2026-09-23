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
        | Error _ -> false)

let rejects_incomplete =
  QCheck2.Test.make ~name:"commands require every field" ~count:1
    QCheck2.Gen.unit (fun () ->
      Result.is_error
        (Control_command.decode
           (`Assoc
              [
                ("id", `String "1");
                ("type", `String "set_automerge");
                ("payload", `Assoc [ ("patch_id", `String "1") ]);
              ])))

let () =
  QCheck2.Test.check_exn totality;
  QCheck2.Test.check_exn roundtrip;
  QCheck2.Test.check_exn rejects_incomplete
