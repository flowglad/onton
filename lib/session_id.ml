(* @archlint.module core
   @archlint.domain session-id *)

open Base

let rng_ready =
  lazy
    (Mirage_crypto_rng_unix.use_default ();
     ())

let hex = "0123456789abcdef"

let append_hex_byte buf byte =
  Buffer.add_char buf hex.[byte lsr 4];
  Buffer.add_char buf hex.[byte land 0x0f]

let mint () =
  Lazy.force rng_ready;
  let bytes = Mirage_crypto_rng.generate 16 in
  let byte i = Char.to_int bytes.[i] in
  let b6 = byte 6 land 0x0f lor 0x40 in
  let b8 = byte 8 land 0x3f lor 0x80 in
  let normalized =
    [|
      byte 0;
      byte 1;
      byte 2;
      byte 3;
      byte 4;
      byte 5;
      b6;
      byte 7;
      b8;
      byte 9;
      byte 10;
      byte 11;
      byte 12;
      byte 13;
      byte 14;
      byte 15;
    |]
  in
  let buf = Buffer.create 36 in
  Array.iteri normalized ~f:(fun i byte ->
      (match i with 4 | 6 | 8 | 10 -> Buffer.add_char buf '-' | _ -> ());
      append_hex_byte buf byte);
  Buffer.contents buf

let%test "mint produces UUID-v4-shaped strings" =
  let id = mint () in
  String.length id = 36
  && Char.equal id.[8] '-'
  && Char.equal id.[13] '-'
  && Char.equal id.[18] '-'
  && Char.equal id.[23] '-'
  && Char.equal id.[14] '4'
  && match id.[19] with '8' | '9' | 'a' | 'b' -> true | _ -> false

let%test "distinct mints differ" = not (String.equal (mint ()) (mint ()))
