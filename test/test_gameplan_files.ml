(* @archlint.module test
   @archlint.domain gameplan-parser *)

open Onton_core

let () =
  if Array.length Sys.argv <> 2 then failwith "Expected YAML fixture";
  let document =
    In_channel.with_open_bin Sys.argv.(1) In_channel.input_all
    |> Gameplan_document.of_string
  in
  let json =
    match document with Ok json -> json | Error msg -> failwith msg
  in
  let path = Filename.temp_file "onton-gameplan-compatibility-" ".json" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      Out_channel.with_open_bin path (fun channel ->
          Yojson.Safe.to_channel channel json);
      match
        ( Gameplan_parser.parse_file Sys.argv.(1),
          Gameplan_parser.parse_file path )
      with
      | Ok yaml, Ok json ->
          if
            not
              (Types.Gameplan.equal yaml.Gameplan_parser.gameplan
                 json.Gameplan_parser.gameplan)
          then failwith "YAML and JSON fixtures produce different plans";
          if
            not
              (Base.Map.equal
                 (Base.List.equal Types.Patch_id.equal)
                 yaml.Gameplan_parser.dependency_graph
                 json.Gameplan_parser.dependency_graph)
          then
            failwith
              "YAML and JSON fixtures produce different dependency graphs"
      | Error msg, _ | Ok _, Error msg -> failwith msg)

let () =
  let rejected =
    [
      {|{"projectName":"p","projectName":"other","patches":[]}|};
      {|{"projectName":"p","patches":[],"unknown":{"a":1,"a":2}}|};
      {|{"projectName":"p","patches":[],"unknown":1e999}|};
    ]
  in
  List.iter
    (fun suffix ->
      List.iter
        (fun content ->
          let path = Filename.temp_file "onton-invalid-gameplan-" suffix in
          Fun.protect
            ~finally:(fun () -> Sys.remove path)
            (fun () ->
              Out_channel.with_open_bin path (fun channel ->
                  Out_channel.output_string channel content);
              match Gameplan_parser.parse_file path with
              | Error _ -> ()
              | Ok _ -> failwith ("Malformed gameplan accepted: " ^ suffix)))
        rejected)
    [ ".json"; ".yaml" ];
  print_endline "JSON and YAML file validation boundaries: OK"
