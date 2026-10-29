(* @archlint.module shell
   @archlint.domain coordinator-state *)
open Base

[@@@warning "-4"]

external peer_uid : Unix.file_descr -> int = "caml_onton_control_peer_uid"

let authorized_peer flow =
  match Eio_unix.Resource.fd_opt flow with
  | None -> false
  | Some fd ->
      Eio_unix.Fd.use fd
        ~if_closed:(fun () -> false)
        (fun raw -> Int.equal (peer_uid raw) (Unix.geteuid ()))

let handle host bytes =
  match Coordinator_state.decode_control bytes with
  | Error _ -> "rejected"
  | Ok control -> (
      match
        Coordinator_host.submit host (Coordinator_state.Control control)
      with
      | Ok (Coordinator_host.Applied [ Coordinator_state.Already_applied ]) ->
          "already_applied"
      | Ok (Coordinator_host.Applied [ Coordinator_state.Accepted ]) ->
          "accepted"
      | Ok (Coordinator_host.Rejected _) -> "rejected"
      | Ok _ | Error _ -> "not_accepted")

let response status =
  Yojson.Safe.to_string (`Assoc [ ("status", `String status) ]) ^ "\n"

let run ~net ~path ~submit () =
  if Stdlib.Filename.is_relative path then
    invalid_arg "control socket path must be absolute";
  let parent = Stdlib.Filename.dirname path in
  (try Unix.mkdir parent 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let stat = Unix.lstat parent in
  if
    (not (Poly.equal stat.Unix.st_kind Unix.S_DIR))
    || stat.Unix.st_uid <> Unix.geteuid ()
    || stat.Unix.st_perm land 0o077 <> 0
  then invalid_arg "control socket parent must be owner-only";
  let bound = ref false in
  Stdlib.Fun.protect
    ~finally:(fun () ->
      if !bound then
        try Unix.unlink path with Unix.Unix_error (Unix.ENOENT, _, _) -> ())
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      let socket =
        Eio.Net.listen ~sw ~backlog:16 ~reuse_addr:true net (`Unix path)
      in
      bound := true;
      Unix.chmod path 0o600;
      Eio.Net.run_server ~max_connections:16
        ~on_error:(fun ex ->
          Stdlib.Printf.eprintf "onton coordinator control: %s\n%!"
            (Exn.to_string ex))
        socket
        (fun flow _ ->
          let status =
            if not (authorized_peer flow) then "unauthorized"
            else
              try
                let reader = Eio.Buf_read.of_flow ~max_size:262144 flow in
                let line = Eio.Buf_read.line reader in
                submit line
              with End_of_file | Eio.Buf_read.Buffer_limit_exceeded ->
                "rejected"
          in
          Eio.Flow.copy_string (response status) flow))
