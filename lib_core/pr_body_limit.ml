open Base

let max_bytes = 60_000

let omission_note =
  "\n\n[PR description truncated; see the gameplan for full details.]"

let is_continuation_byte byte = byte land 0xc0 = 0x80

let valid_utf8_sequence_length body pos =
  let length = String.length body in
  let byte offset = Char.to_int (String.get body (pos + offset)) in
  let continuation offset = is_continuation_byte (byte offset) in
  let first = byte 0 in
  if first <= 0x7f then Some 1
  else if first >= 0xc2 && first <= 0xdf && pos + 1 < length && continuation 1
  then Some 2
  else if
    pos + 2 < length
    && continuation 2
    && ((first = 0xe0 && byte 1 >= 0xa0 && byte 1 <= 0xbf)
       || ((first >= 0xe1 && first <= 0xec) && continuation 1)
       || (first = 0xed && byte 1 >= 0x80 && byte 1 <= 0x9f)
       || ((first >= 0xee && first <= 0xef) && continuation 1))
  then Some 3
  else if
    pos + 3 < length
    && continuation 2 && continuation 3
    && ((first = 0xf0 && byte 1 >= 0x90 && byte 1 <= 0xbf)
       || ((first >= 0xf1 && first <= 0xf3) && continuation 1)
       || (first = 0xf4 && byte 1 >= 0x80 && byte 1 <= 0x8f))
  then Some 4
  else None

let utf8_boundary body cutoff =
  let rec walk pos =
    if pos >= cutoff then cutoff
    else
      match valid_utf8_sequence_length body pos with
      | Some sequence_length when pos + sequence_length > cutoff -> pos
      | Some sequence_length -> walk (pos + sequence_length)
      | None -> walk (pos + 1)
  in
  walk 0

let fence_start_at body pos cutoff =
  let rec skip_spaces current remaining =
    if
      remaining > 0 && current < cutoff
      && Char.equal (String.get body current) ' '
    then skip_spaces (current + 1) (remaining - 1)
    else current
  in
  let marker = skip_spaces pos 3 in
  marker + 3 <= cutoff
  && String.equal (String.sub body ~pos:marker ~len:3) "```"

let unmatched_fence_start body cutoff =
  let rec scan line_start open_fence =
    if line_start >= cutoff then open_fence
    else
      let open_fence =
        if fence_start_at body line_start cutoff then
          match open_fence with Some _ -> None | None -> Some line_start
        else open_fence
      in
      match String.index_from body line_start '\n' with
      | Some newline when newline < cutoff -> scan (newline + 1) open_fence
      | Some _ | None -> open_fence
  in
  scan 0 None

let limit body =
  if String.length body <= max_bytes then body
  else
    let end_pos = max_bytes - String.length omission_note in
    let byte_boundary = utf8_boundary body end_pos in
    let markdown_boundary =
      Option.value
        (unmatched_fence_start body byte_boundary)
        ~default:byte_boundary
    in
    String.sub body ~pos:0 ~len:markdown_boundary ^ omission_note

let summarize_files files =
  let visible = 30 in
  let count = List.length files in
  if count <= visible then files
  else
    let omitted = count - visible in
    let noun = if omitted = 1 then "file" else "files" in
    List.take files visible
    @ [
        Printf.sprintf "… and %d more %s; see the gameplan for the full list."
          omitted noun;
      ]
