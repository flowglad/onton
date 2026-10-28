(* @archlint.module shell
   @archlint.domain coordinator-stream-io *)

type t = {
  input : Unix.file_descr;
  output : Unix.file_descr;
  mutable pending : string;
}

let create attachment =
  let input = Resource_client.input attachment
  and output = Resource_client.output attachment in
  Unix.set_nonblock input;
  Unix.set_nonblock output;
  { input; output; pending = "" }

let ready t =
  try
    String.contains t.pending '\n'
    ||
    let readable, _, _ = Unix.select [ t.output ] [] [] 0. in
    readable <> []
  with _ -> false

let max_frame = 1024 * 1024
let valid_timeout timeout = Float.is_finite timeout && timeout > 0.

let waiting deadline reads writes =
  let remaining = deadline -. Unix.gettimeofday () in
  if remaining <= 0. then false
  else
    let readable, writable, _ = Unix.select reads writes [] remaining in
    readable <> [] || writable <> []

let read t ~timeout =
  if not (valid_timeout timeout) then Error "invalid stream timeout"
  else
    let deadline = Unix.gettimeofday () +. timeout in
    let bytes = Bytes.create 4096 in
    let rec loop () =
      match String.index_opt t.pending '\n' with
      | Some index ->
          let length = index + 1 in
          if length > max_frame then Error "oversized worker frame"
          else
            let frame = String.sub t.pending 0 length in
            t.pending <-
              String.sub t.pending length (String.length t.pending - length);
            Ok frame
      | None when String.length t.pending >= max_frame ->
          Error "oversized worker frame"
      | None -> (
          if not (waiting deadline [ t.output ] []) then
            Error "worker frame timed out"
          else
            match Unix.read t.output bytes 0 (Bytes.length bytes) with
            | 0 -> Error "worker stream lost (not retirement)"
            | count ->
                t.pending <- t.pending ^ Bytes.sub_string bytes 0 count;
                loop ()
            | exception
                Unix.Unix_error
                  ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _) ->
                loop ())
    in
    try loop () with _ -> Error "worker frame read failed"

let send t ~timeout frames =
  if not (valid_timeout timeout) then Error "invalid stream timeout"
  else
    let deadline = Unix.gettimeofday () +. timeout in
    let rec write frame offset =
      if offset = String.length frame then Ok ()
      else if not (waiting deadline [] [ t.input ]) then
        Error "worker send uncertain (timeout)"
      else
        match
          Unix.single_write_substring t.input frame offset
            (String.length frame - offset)
        with
        | 0 -> Error "worker send uncertain (closed)"
        | count -> write frame (offset + count)
        | exception
            Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _)
          ->
            write frame offset
    in
    let rec loop = function
      | [] -> Ok ()
      | frame :: rest -> (
          match write frame 0 with Ok () -> loop rest | Error _ as e -> e)
    in
    try loop frames with _ -> Error "worker send uncertain (I/O failure)"
