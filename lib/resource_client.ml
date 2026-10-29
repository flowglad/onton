(* @archlint.module exempt
   @archlint.exempt-reason effect-boundary *)

open Base

type transport =
  argv:string list -> input:string -> (int * string * string, string) Result.t

type t = {
  helper : string;
  transport : transport;
  mutable attachments : string list;
  mutable submitted : (string * string * string) list;
}

let create ~helper ~transport =
  { helper; transport; attachments = []; submitted = [] }

let close fd = try Unix.close fd with _ -> ()

let rec wait pid =
  try ignore (Unix.waitpid [] pid) with
  | Unix.Unix_error (Unix.ECHILD, _, _) -> ()
  | Unix.Unix_error (Unix.EINTR, _, _) -> wait pid

let supervise ~argv ~input ~limit =
  let supervisor = Process_tree.supervisor_path () in
  if String.is_empty supervisor || not (Stdlib.Sys.file_exists supervisor) then
    Error "supervisor unavailable"
  else
    let owned = ref [] and child = ref None in
    let pipe () =
      let a, b = Unix.pipe ~cloexec:true () in
      owned := a :: b :: !owned;
      (a, b)
    in
    let release fd =
      close fd;
      owned := List.filter !owned ~f:(fun x -> not (Poly.equal x fd))
    in
    Stdlib.Fun.protect
      ~finally:(fun () ->
        Option.iter !child ~f:(fun pid ->
            (try Unix.kill pid Stdlib.Sys.sigterm with _ -> ());
            wait pid);
        List.iter !owned ~f:close)
      (fun () ->
        let in_r, in_w = pipe ()
        and out_r, out_w = pipe ()
        and err_r, err_w = pipe () in
        let args =
          Process_tree.supervised_command ~supervisor ~streaming:false argv
          |> Array.of_list
        in
        let pid =
          Unix.create_process_env supervisor args (Unix.environment ()) in_r
            out_w err_w
        in
        child := Some pid;
        List.iter [ in_r; out_w; err_w ] ~f:release;
        Unix.set_nonblock in_w;
        Unix.set_nonblock out_r;
        Unix.set_nonblock err_r;
        let stdout = Buffer.create 256 and stderr = Buffer.create 256 in
        let written = ref 0 and reads = ref [ out_r; err_r ] in
        if String.is_empty input then release in_w;
        let deadline = Unix.gettimeofday () +. 60. in
        let buf = Bytes.create 8192 in
        let rec loop () =
          if List.is_empty !reads && !written = String.length input then ()
          else if Float.(Unix.gettimeofday () >= deadline) then
            failwith "resource helper timeout"
          else
            let rd, wr, _ =
              try
                Unix.select !reads
                  (if !written < String.length input then [ in_w ] else [])
                  []
                  (Float.min 0.5 (deadline -. Unix.gettimeofday ()))
              with Unix.Unix_error (Unix.EINTR, _, _) -> ([], [], [])
            in
            if List.mem wr in_w ~equal:Poly.equal then (
              (try
                 written :=
                   !written
                   + Unix.write_substring in_w input !written
                       (Int.min 8192 (String.length input - !written))
               with Unix.Unix_error ((Unix.EAGAIN | Unix.EINTR), _, _) -> ());
              if !written = String.length input then release in_w);
            List.iter rd ~f:(fun fd ->
                let n =
                  try Unix.read fd buf 0 (Bytes.length buf)
                  with Unix.Unix_error ((Unix.EAGAIN | Unix.EINTR), _, _) ->
                    -1
                in
                if n = 0 then (
                  release fd;
                  reads :=
                    List.filter !reads ~f:(fun x -> not (Poly.equal x fd)))
                else if n > 0 then (
                  let target = if Poly.equal fd out_r then stdout else stderr in
                  if Buffer.length target + n > limit then
                    failwith "resource helper output too large";
                  Buffer.add_subbytes target buf ~pos:0 ~len:n));
            loop ()
        in
        try
          loop ();
          let rec await () =
            let done_pid, status =
              try Unix.waitpid [ Unix.WNOHANG ] pid
              with Unix.Unix_error (Unix.EINTR, _, _) -> (0, Unix.WEXITED 0)
            in
            if done_pid <> 0 then status
            else if Float.(Unix.gettimeofday () >= deadline) then
              failwith "resource helper exit timeout"
            else (
              Unix.sleepf 0.01;
              await ())
          in
          let status = await () in
          child := None;
          let code =
            match status with
            | Unix.WEXITED n -> n
            | Unix.WSIGNALED n | Unix.WSTOPPED n -> 128 + n
          in
          Ok (code, Buffer.contents stdout, Buffer.contents stderr)
        with exn -> Error (Exn.to_string exn))

let subprocess ~argv ~input =
  supervise ~argv ~input ~limit:(Resource_protocol.max_envelope + 4096)

type error =
  | Transport of string
  | Invalid_response of int * string
  | Helper_exit of int * Resource_protocol.response

let worker_manifest ~executable ~args ~inputs =
  let manifest =
    `Assoc
      [
        ("executable", `String executable);
        ( "args",
          `List
            (List.map ("--protocol-version" :: "3" :: args) ~f:(fun x ->
                 `String x)) );
        ("preparation", `String "coding");
        ( "inputs",
          `List
            (List.map inputs ~f:(fun (path, data) ->
                 `Assoc
                   [
                     ("path", `String ("inputs/" ^ path));
                     ("data", `String (Base64.encode_string data));
                   ])) );
      ]
  in
  let request =
    `Assoc
      [
        ("version", `String "resource-v1");
        ("op", `String "launch");
        ("run_id", `String "validate");
        ("request_id", `String "validate");
        ("coordinator_id", `String "validate");
        ("owner_epoch", `String "0");
        ("revision", `String "0");
        ("chain_id", `String "validate");
        ("patch_id", `String "validate");
        ("resource_id", `String "validate");
        ("launch_digest", `String "validate");
        ("manifest", manifest);
      ]
  in
  match Resource_protocol.make_request (Yojson.Safe.to_string request) with
  | Error e -> Error e
  | Ok _ -> Ok manifest

let request t req =
  match Resource_protocol.encode_request req with
  | Error e -> Error (Transport e)
  | Ok input -> (
      let previous =
        List.find t.submitted ~f:(fun (run, id, _) ->
            String.equal run req.run_id && String.equal id req.request_id)
      in
      match previous with
      | Some (_, _, bytes) when not (String.equal bytes input) ->
          Error (Transport "changed request identity")
      | _ -> (
          if Option.is_none previous then
            t.submitted <- (req.run_id, req.request_id, input) :: t.submitted;
          match
            t.transport
              ~argv:
                [
                  t.helper; "resources"; Resource_protocol.op_name req.operation;
                ]
              ~input
          with
          | Error e -> Error (Transport e)
          | Ok (exit, stdout, _stderr) -> (
              match Resource_protocol.decode_response stdout with
              | Error e -> Error (Invalid_response (exit, e))
              | Ok r when exit <> 0 -> Error (Helper_exit (exit, r))
              | Ok r -> Ok r)))

type attachment = {
  stdin : Unix.file_descr;
  stdout : Unix.file_descr;
  pid : int;
  mutable closed : bool;
  mutable reaped : bool;
}

let input a = a.stdin
let output a = a.stdout

let utf8_valid s =
  let rec loop i =
    if i = String.length s then true
    else
      let decoded = Stdlib.String.get_utf_8_uchar s i in
      Stdlib.Uchar.utf_decode_is_valid decoded
      && loop (i + Stdlib.Uchar.utf_decode_length decoded)
  in
  loop 0

let disconnect ?(timeout = 5.) a =
  if not a.closed then (
    a.closed <- true;
    close a.stdin;
    close a.stdout);
  if a.reaped then Ok ()
  else if (not (Float.is_finite timeout)) || Float.(timeout <= 0.) then
    Error "invalid supervisor teardown timeout"
  else (
    (try Unix.kill a.pid Stdlib.Sys.sigterm with _ -> ());
    let rec await attempts =
      match Unix.waitpid [ Unix.WNOHANG ] a.pid with
      | pid, _ when pid = a.pid ->
          a.reaped <- true;
          Ok ()
      | 0, _ when attempts <= 0 ->
          Error "worker supervisor teardown timed out; attachment retained"
      | 0, _ ->
          Unix.sleepf 0.01;
          await (attempts - 1)
      | _ -> Error "unexpected supervisor wait result"
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> await attempts
      | exception Unix.Unix_error (Unix.ECHILD, _, _) ->
          a.reaped <- true;
          Ok ()
    in
    let attempts = Stdlib.int_of_float (Stdlib.ceil (timeout /. 0.01)) in
    await attempts)

let connect t ~run_id ~coordinator_id ~owner_epoch ~revision ~request_id
    ~resource_id =
  let selectors = [ run_id; coordinator_id; request_id; resource_id ] in
  if
    (not
       (List.for_all selectors ~f:(fun s ->
            let n = String.length s in
            n > 0 && n <= 256 && utf8_valid s
            && not
                 (String.exists s ~f:(fun c ->
                      Char.equal c '\000' || Char.equal c '\n'
                      || Char.equal c '\r')))))
    || Int64.(owner_epoch < 0L || revision < 0L)
  then Error "invalid connect selector"
  else if List.mem t.attachments resource_id ~equal:String.equal then
    Error "attachment already consumed"
  else
    let supervisor = Process_tree.supervisor_path () in
    if String.is_empty supervisor || not (Stdlib.Sys.file_exists supervisor)
    then Error "supervisor unavailable"
    else
      let in_r, in_w = Unix.pipe ~cloexec:true () in
      let out_r, out_w = Unix.pipe ~cloexec:true () in
      let err = Unix.openfile "/dev/null" [ Unix.O_WRONLY; Unix.O_CLOEXEC ] 0 in
      try
        let argv =
          Process_tree.supervised_command ~supervisor ~streaming:true
            [
              t.helper;
              "resources";
              "connect";
              run_id;
              coordinator_id;
              Int64.to_string owner_epoch;
              Int64.to_string revision;
              request_id;
              resource_id;
            ]
          |> Array.of_list
        in
        let pid =
          Unix.create_process_env supervisor argv (Unix.environment ()) in_r
            out_w err
        in
        List.iter [ in_r; out_w; err ] ~f:close;
        t.attachments <- resource_id :: t.attachments;
        Ok { stdin = in_w; stdout = out_r; pid; closed = false; reaped = false }
      with exn ->
        List.iter [ in_r; in_w; out_r; out_w; err ] ~f:close;
        Error (Exn.to_string exn)
