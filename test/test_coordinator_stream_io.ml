(* @archlint.module test
   @archlint.domain coordinator-stream-io *)

open Onton

let check message yes = if not yes then failwith message

let with_connection mono_clock script f =
  let helper = Filename.temp_file "onton-coordinator-io-" ".sh" in
  let output = open_out_bin helper in
  output_string output ("#!/bin/sh\n" ^ script ^ "\n");
  close_out output;
  Unix.chmod helper 0o755;
  let client =
    Resource_client.create ~helper ~transport:(fun ~argv:_ ~input:_ ->
        Error "unexpected RPC")
  in
  Fun.protect
    ~finally:(fun () -> Sys.remove helper)
    (fun () ->
      match
        Resource_client.connect client ~run_id:"r" ~coordinator_id:"c"
          ~owner_epoch:1L ~revision:1L ~request_id:"connect"
          ~resource_id:"worker"
      with
      | Error why -> failwith why
      | Ok attachment ->
          Fun.protect
            ~finally:(fun () -> ignore (Resource_client.disconnect attachment))
            (fun () -> f (Coordinator_stream_io.create ~mono_clock attachment)))

let () =
  Eio_main.run @@ fun env ->
  let mono_clock = Eio.Stdenv.mono_clock env in
  with_connection mono_clock "printf 'first\\nsecond\\n'" (fun io ->
      check "first complete frame"
        (Coordinator_stream_io.read io ~timeout:3. = Ok "first\n");
      check "buffered frame survives peer EOF"
        (Coordinator_stream_io.read io ~timeout:3. = Ok "second\n");
      check "EOF is not a frame or retirement"
        (Result.is_error (Coordinator_stream_io.read io ~timeout:3.)));
  with_connection mono_clock "printf 'partial'; sleep 30" (fun io ->
      let started = Unix.gettimeofday () in
      check "partial frame cannot enter protocol"
        (Result.is_error (Coordinator_stream_io.read io ~timeout:0.05));
      check "partial frame read is bounded"
        (Unix.gettimeofday () -. started < 3.));
  with_connection mono_clock "sleep 30" (fun io ->
      let started = Unix.gettimeofday () in
      check "backpressured send is uncertain"
        (Result.is_error
           (Coordinator_stream_io.send io ~timeout:0.05
              [ String.make (1024 * 1024) 'a' ^ "\n" ]));
      check "unterminated frame rejected before write"
        (Result.is_error (Coordinator_stream_io.send io ~timeout:1. [ "{}" ]));
      check "multiple frames in one item rejected"
        (Result.is_error
           (Coordinator_stream_io.send io ~timeout:1. [ "{}\n{}\n" ]));
      check "backpressure cannot indefinitely block cancellation"
        (Unix.gettimeofday () -. started < 3.);
      List.iter
        (fun timeout ->
          check "invalid read timeout"
            (Result.is_error (Coordinator_stream_io.read io ~timeout));
          check "invalid send timeout"
            (Result.is_error
               (Coordinator_stream_io.send io ~timeout [ "frame\n" ])))
        [ 0.; -1.; infinity; nan ]);
  print_endline
    "coordinator stream I/O: framing, EOF, partial-frame deadline and \
     uncertain/backpressured send"
