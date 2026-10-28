(* @archlint.module pure
   @archlint.domain coordinator-state *)
open Base

(* A bounded-window, lossless byte codec. Tokens are literal runs or backward
   copies; overlapping copies deliberately support long repeated runs. No
   journal entry, checkpoint or protocol event is discarded. *)
let encode bytes =
  let size = String.length bytes in
  let output = Stdlib.Buffer.create (Int.min size 65536) in
  let byte n = Stdlib.Buffer.add_char output (Char.of_int_exn (n land 255)) in
  let short n =
    byte n;
    byte (n lsr 8)
  in
  for i = 0 to 7 do
    byte
      (Stdlib.Int64.to_int
         (Stdlib.Int64.logand 255L
            (Stdlib.Int64.shift_right_logical (Int64.of_int size) (8 * i))))
  done;
  let positions = Array.create ~len:65536 (-1) in
  let code i = Char.to_int bytes.[i] in
  let hash i =
    ((((((code i * 251) + code (i + 1)) * 251) + code (i + 2)) * 251)
    + code (i + 3))
    land 65535
  in
  let literal start stop =
    let cursor = ref start in
    while !cursor < stop do
      let count = Int.min 65535 (stop - !cursor) in
      byte 0;
      short count;
      Stdlib.Buffer.add_substring output bytes !cursor count;
      cursor := !cursor + count
    done
  in
  let cursor = ref 0 and start = ref 0 in
  while !cursor + 4 <= size do
    let key = hash !cursor in
    let previous = positions.(key) in
    positions.(key) <- !cursor;
    let count = ref 0 in
    if previous >= 0 && !cursor - previous <= 65535 then
      while
        !count < 65535
        && !cursor + !count < size
        && Char.equal bytes.[previous + !count] bytes.[!cursor + !count]
      do
        Int.incr count
      done;
    if !count >= 4 then (
      literal !start !cursor;
      byte 1;
      short (!cursor - previous);
      short !count;
      cursor := !cursor + !count;
      start := !cursor)
    else Int.incr cursor
  done;
  literal !start size;
  Stdlib.Buffer.contents output

exception Invalid_archive

let decode ~max_bytes bytes =
  try
    let size = String.length bytes in
    if size < 8 || max_bytes < 0 then raise Invalid_archive;
    let declared = ref 0L in
    for i = 0 to 7 do
      declared :=
        Stdlib.Int64.logor !declared
          (Stdlib.Int64.shift_left
             (Int64.of_int (Char.to_int bytes.[i]))
             (8 * i))
    done;
    if Int64.(!declared < 0L || !declared > of_int max_bytes) then
      raise Invalid_archive;
    let length = Stdlib.Int64.to_int !declared in
    let output = Stdlib.Bytes.create length in
    let cursor = ref 8 and written = ref 0 in
    let byte () =
      if !cursor >= size then raise Invalid_archive;
      let value = Char.to_int bytes.[!cursor] in
      Int.incr cursor;
      value
    in
    let short () =
      let lo = byte () in
      lo lor (byte () lsl 8)
    in
    while !cursor < size do
      match byte () with
      | 0 ->
          let count = short () in
          if count = 0 || count > size - !cursor || count > length - !written
          then raise Invalid_archive;
          Stdlib.Bytes.blit_string bytes !cursor output !written count;
          cursor := !cursor + count;
          written := !written + count
      | 1 ->
          let distance = short () in
          let count = short () in
          if
            distance = 0 || distance > !written || count < 4
            || count > length - !written
          then raise Invalid_archive;
          for i = 0 to count - 1 do
            Stdlib.Bytes.set output (!written + i)
              (Stdlib.Bytes.get output (!written + i - distance))
          done;
          written := !written + count
      | _ -> raise Invalid_archive
    done;
    if !written <> length then raise Invalid_archive;
    Ok (Stdlib.Bytes.to_string output)
  with Invalid_archive | Invalid_argument _ ->
    Error "invalid or oversized archive"
