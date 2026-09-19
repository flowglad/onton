open Base

let max_bytes = 60_000

let omission_note =
  "\n\n[PR description truncated; see the gameplan for full details.]"

let limit body =
  if String.length body <= max_bytes then body
  else
    let end_pos = max_bytes - String.length omission_note in
    let rec utf8_boundary pos =
      if pos = 0 then 0
      else if Char.to_int (String.get body pos) land 0xc0 = 0x80 then
        utf8_boundary (pos - 1)
      else pos
    in
    String.sub body ~pos:0 ~len:(utf8_boundary end_pos) ^ omission_note

let summarize_files files =
  let visible = 30 in
  let count = List.length files in
  if count <= visible then files
  else
    List.take files visible
    @ [
        Printf.sprintf
          "… and %d more files; see the gameplan for the full list."
          (count - visible);
      ]
