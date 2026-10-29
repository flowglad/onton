(* @archlint.module exempt
   @archlint.exempt-reason effect-boundary *)

type t = {
  input : Unix.file_descr;
  output : Unix.file_descr;
  mutable pending : string;
  monotonic_now : unit -> float;
}

let create ~mono_clock attachment =
  let input = Resource_client.input attachment
  and output = Resource_client.output attachment in
  Unix.set_nonblock input;
  Unix.set_nonblock output;
  {
    input;
    output;
    pending = "";
    monotonic_now =
      (fun () ->
        Int64.to_float (Mtime.to_uint64_ns (Eio.Time.Mono.now mono_clock))
        /. 1e9);
  }

let ready t =
  try
    String.contains t.pending '\n'
    ||
    let readable, _, _ = Unix.select [ t.output ] [] [] 0. in
    readable <> []
  with _ -> false

let max_frame = 1024 * 1024
let valid_timeout timeout = Float.is_finite timeout && timeout > 0.

let rec waiting t deadline reads writes =
  let remaining = deadline -. t.monotonic_now () in
  if remaining <= 0. then false
  else
    try
      let readable, writable, _ = Unix.select reads writes [] remaining in
      readable <> [] || writable <> []
    with Unix.Unix_error (Unix.EINTR, _, _) -> waiting t deadline reads writes

let read t ~timeout =
  if not (valid_timeout timeout) then Error "invalid stream timeout"
  else
    let deadline = t.monotonic_now () +. timeout in
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
          if not (waiting t deadline [ t.output ] []) then
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
  else if
    Stdlib.List.exists
      (fun frame ->
        String.length frame = 0
        || (not (Char.equal frame.[String.length frame - 1] '\n'))
        ||
        match String.index_opt frame '\n' with
        | Some index -> index <> String.length frame - 1
        | None -> true)
      frames
  then Error "each send item must be one newline-terminated frame"
  else
    let deadline = t.monotonic_now () +. timeout in
    let rec write frame offset =
      if offset = String.length frame then Ok ()
      else if not (waiting t deadline [] [ t.input ]) then
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
    try loop frames with
    | Unix.Unix_error (err, _, _) ->
        Error ("worker send uncertain: " ^ Unix.error_message err)
    | _ -> Error "worker send uncertain (I/O failure)"
