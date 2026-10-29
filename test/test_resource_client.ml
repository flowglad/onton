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
      | Ok stream -> (
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
          match Onton.Resource_client.disconnect stream with
          | Ok () -> ()
          | Error reason ->
              failwith ("supervisor process tree reaped: " ^ reason)));
  let teardown_script = Filename.temp_file "onton-resource-descendant" ".sh" in
  let child_file = Filename.temp_file "onton-resource-child" ".pid" in
  Fun.protect
    ~finally:(fun () ->
      (try Sys.remove teardown_script with _ -> ());
      try Sys.remove child_file with _ -> ())
    (fun () ->
      Sys.remove child_file;
      let out = open_out_bin teardown_script in
      output_string out
        ("#!/bin/sh\n(sleep 60) &\nchild=$!\nprintf '%s\\n' \"$child\" > "
       ^ Filename.quote child_file ^ "\nwait \"$child\"\n");
      close_out out;
      Unix.chmod teardown_script 0o700;
      let client =
        Onton.Resource_client.create ~helper:teardown_script
          ~transport:Onton.Resource_client.subprocess
      in
      match
        Onton.Resource_client.connect client ~run_id:"r" ~coordinator_id:"c"
          ~owner_epoch:0L ~revision:0L ~request_id:"tree" ~resource_id:"tree"
      with
      | Error reason -> failwith reason
      | Ok attachment ->
          let rec read_child attempts =
            if attempts <= 0 then failwith "descendant did not start";
            try
              let input = open_in child_file in
              Fun.protect
                ~finally:(fun () -> close_in_noerr input)
                (fun () -> int_of_string (input_line input))
            with Sys_error _ | End_of_file | Failure _ ->
              Unix.sleepf 0.01;
              read_child (attempts - 1)
          in
          let child = read_child 200 in
          check
            (Result.is_ok (Onton.Resource_client.disconnect attachment))
            "supervisor teardown acknowledgment";
          let rec gone attempts =
            try
              Unix.kill child 0;
              if attempts <= 0 then false
              else (
                Unix.sleepf 0.01;
                gone (attempts - 1))
            with Unix.Unix_error (Unix.ESRCH, _, _) -> true
          in
          check (gone 200) "descendant survived confirmed teardown");
  let crash_script = Filename.temp_file "onton-resource-crash" ".sh" in
  let crash_file = Filename.temp_file "onton-resource-crash-child" ".pid" in
  let reaped_file =
    Filename.temp_file "onton-resource-crash-reaped" ".marker"
  in
  let child_pid = ref None in
  let wait_reaped attempts =
    let rec loop attempts =
      if Sys.file_exists reaped_file then true
      else if attempts <= 0 then false
      else (
        Unix.sleepf 0.01;
        loop (attempts - 1))
    in
    loop attempts
  in
  let stop_and_wait pid =
    (try Unix.kill pid Sys.sigkill
     with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
    if not (wait_reaped 500) then
      failwith "crash fixture child was not reaped by its surviving parent"
  in
  Fun.protect
    ~finally:(fun () ->
      Option.iter stop_and_wait !child_pid;
      (try Sys.remove crash_script with _ -> ());
      (try Sys.remove crash_file with _ -> ());
      try Sys.remove reaped_file with _ -> ())
    (fun () ->
      Sys.remove crash_file;
      Sys.remove reaped_file;
      let out = open_out_bin crash_script in
      output_string out
        ("#!/bin/sh\nsleep 60 &\nchild=$!\nprintf '%s\\n' \"$child\" > "
       ^ Filename.quote crash_file
       ^ "\nkill -KILL \"$PPID\"\nwait \"$child\"\nprintf reaped > "
       ^ Filename.quote reaped_file ^ "\n");
      close_out out;
      Unix.chmod crash_script 0o700;
      let client =
        Onton.Resource_client.create ~helper:crash_script
          ~transport:Onton.Resource_client.subprocess
      in
      match
        Onton.Resource_client.connect client ~run_id:"r" ~coordinator_id:"c"
          ~owner_epoch:0L ~revision:0L ~request_id:"crash" ~resource_id:"crash"
      with
      | Error reason -> failwith reason
      | Ok attachment ->
          let rec read_pid attempts =
            if attempts <= 0 then failwith "crash fixture did not start";
            try
              let input = open_in crash_file in
              let pid =
                Fun.protect
                  ~finally:(fun () -> close_in_noerr input)
                  (fun () -> int_of_string (input_line input))
              in
              child_pid := Some pid;
              pid
            with Sys_error _ | End_of_file | Failure _ ->
              Unix.sleepf 0.01;
              read_pid (attempts - 1)
          in
          let child = read_pid 200 in
          check
            (Result.is_error (Onton.Resource_client.disconnect attachment))
            "abnormal supervisor exit must not confirm teardown";
          check
            (Result.is_error (Onton.Resource_client.disconnect attachment))
            "unconfirmed teardown must remain fenced on replay";
          stop_and_wait child;
          print_endline "  crash fixture child reaped by surviving shell");
  print_endline "resource client public helper passed"
