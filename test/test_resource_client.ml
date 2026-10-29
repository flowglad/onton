(* @archlint.module test
   @archlint.domain resource-client *)

open Onton_core

let check ok msg = if not ok then failwith msg

let valid =
  {|{"version":"resource-v1","op":"observe","run_id":"r","request_id":"q","coordinator_id":"c","owner_epoch":"0","revision":"0","resource_id":"p"}|}

let req =
  match Resource_protocol.make_request valid with
  | Ok x -> x
  | Error e -> failwith e

let () =
  let manifest =
    Onton.Resource_client.worker_manifest ~executable:"onton-worker"
      ~args:[ "--repo-root"; "repo"; "--gameplan"; "inputs/gameplan" ]
      ~inputs:[ ("gameplan", "patches: []") ]
  in
  check (Result.is_ok manifest) "bounded v3 coding manifest";
  check
    (Result.is_error
       (Onton.Resource_client.worker_manifest ~executable:"onton-worker"
          ~args:[]
          ~inputs:[ ("../escape", "x") ]))
    "manifest paths cannot escape inputs";
  let received = ref "" in
  let client =
    Onton.Resource_client.create ~helper:"fixture"
      ~transport:(fun ~argv ~input ->
        received := input;
        check (argv = [ "fixture"; "resources"; "observe" ]) "public argv";
        Ok (0, {|{"status":"ok","reason":"none","resource_id":"p"}|}, ""))
  in
  let result = Onton.Resource_client.request client req in
  check (Result.is_ok result && !received = valid) "exact request bytes";
  let changed =
    Resource_protocol.make_request
      {|{"version":"resource-v1","op":"observe","run_id":"r","request_id":"q","coordinator_id":"c","owner_epoch":"0","revision":"1","resource_id":"p"}|}
  in
  (match changed with
  | Ok changed ->
      check
        (Result.is_error (Onton.Resource_client.request client changed))
        "changed guard with same ID must not reach helper"
  | Error e -> failwith e);
  let failed =
    Onton.Resource_client.create ~helper:"fixture"
      ~transport:(fun ~argv:_ ~input:_ ->
        Ok (2, {|{"status":"ok","reason":"none"}|}, "diagnostic"))
  in
  (match Onton.Resource_client.request failed req with
  | Error (Onton.Resource_client.Helper_exit (code, r)) ->
      check
        (code = 2 && r.Resource_protocol.status = Resource_protocol.Ok)
        "nonzero exit retains JSON outcome separately"
  | Error
      ( Onton.Resource_client.Transport _
      | Onton.Resource_client.Invalid_response _ )
  | Ok _ ->
      failwith "nonzero exit must not confirm JSON success");
  let bad =
    Onton.Resource_client.create ~helper:"fixture"
      ~transport:(fun ~argv:_ ~input:_ -> Ok (0, "{", ""))
  in
  check
    (Result.is_error (Onton.Resource_client.request bad req))
    "malformed response";
  let broken =
    Onton.Resource_client.create ~helper:"fixture"
      ~transport:(fun ~argv:_ ~input:_ -> Ok (7, "{", "diagnostic"))
  in
  (match Onton.Resource_client.request broken req with
  | Error (Onton.Resource_client.Invalid_response (exit, _)) ->
      check (exit = 7) "malformed response retains independent process exit"
  | Error
      (Onton.Resource_client.Transport _ | Onton.Resource_client.Helper_exit _)
  | Ok _ ->
      failwith "invalid JSON must not erase exit status");
  let script = Filename.temp_file "onton-resource-helper" ".sh" in
  Fun.protect
    ~finally:(fun () -> Sys.remove script)
    (fun () ->
      let out = open_out script in
      output_string out
        "#!/bin/sh\n\
         cat >/dev/null\n\
         printf '{\"status\":\"pending\",\"reason\":\"launch_uncertain\"}'\n";
      close_out out;
      Unix.chmod script 0o700;
      let fixture =
        Onton.Resource_client.create ~helper:script
          ~transport:Onton.Resource_client.subprocess
      in
      match Onton.Resource_client.request fixture req with
      | Ok r ->
          check
            (r.Resource_protocol.status = Resource_protocol.Pending)
            "EOF then pending response"
      | Error _ -> failwith "helper failed");
  let stream_script = Filename.temp_file "onton-resource-stream" ".sh" in
  Fun.protect
    ~finally:(fun () -> Sys.remove stream_script)
    (fun () ->
      let out = open_out stream_script in
      output_string out "#!/bin/sh\nexec cat\n";
      close_out out;
      Unix.chmod stream_script 0o700;
      let client =
        Onton.Resource_client.create ~helper:stream_script
          ~transport:Onton.Resource_client.subprocess
      in
      let connect () =
        Onton.Resource_client.connect client ~run_id:"r" ~coordinator_id:"c"
          ~owner_epoch:0L ~revision:0L ~request_id:"stream-1" ~resource_id:"p"
      in
      check
        (Result.is_error
           (Onton.Resource_client.connect client ~run_id:"\255"
              ~coordinator_id:"c" ~owner_epoch:0L ~revision:0L
              ~request_id:"invalid" ~resource_id:"p"))
        "invalid UTF-8 selector rejected before attachment registration";
      match connect () with
      | Error e -> failwith e
      | Ok stream ->
          check (Result.is_error (connect ())) "second attachment rejected";
          ignore
            (Unix.write_substring
               (Onton.Resource_client.input stream)
               "raw\000bytes" 0 9);
          let buf = Bytes.create 9 in
          let n = Unix.read (Onton.Resource_client.output stream) buf 0 9 in
          check
            (n = 9 && Bytes.to_string buf = "raw\000bytes")
            "raw stream bytes";
          check
            (Result.is_ok (Onton.Resource_client.disconnect stream))
            "supervisor process tree reaped");
  print_endline "resource client public helper passed"
