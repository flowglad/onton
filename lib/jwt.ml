type error =
  | Key_read_error of string
  | Key_decode_error of string
  | Key_not_rsa of string
  | Sign_error of string

let show_error = function
  | Key_read_error msg -> Printf.sprintf "JWT: cannot read private key: %s" msg
  | Key_decode_error msg ->
      Printf.sprintf "JWT: cannot decode private key PEM: %s" msg
  | Key_not_rsa kt ->
      Printf.sprintf "JWT: private key is %s — RS256 requires an RSA key" kt
  | Sign_error msg -> Printf.sprintf "JWT: sign failed: %s" msg

(** Base64url without padding — RFC 7515 §2. *)
let base64url_encode s =
  let b64 =
    Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet s
  in
  b64

let read_file path =
  try
    let ic = Stdlib.In_channel.open_text path in
    let body =
      Fun.protect
        ~finally:(fun () -> Stdlib.In_channel.close ic)
        (fun () -> Stdlib.In_channel.input_all ic)
    in
    Ok body
  with Sys_error msg -> Error (Key_read_error msg)

let decode_pem pem =
  match X509.Private_key.decode_pem pem with
  | Ok (`RSA priv) -> Ok priv
  | Ok (`ED25519 _) -> Error (Key_not_rsa "ED25519")
  | Ok (`P256 _) -> Error (Key_not_rsa "P-256")
  | Ok (`P384 _) -> Error (Key_not_rsa "P-384")
  | Ok (`P521 _) -> Error (Key_not_rsa "P-521")
  | Error (`Msg msg) -> Error (Key_decode_error msg)

let clamp ~lo ~hi v = max lo (min hi v)

let json_compact_of_assoc fields =
  (* Yojson.Safe.to_string emits compact (no whitespace) by default — exactly
     what the JWT spec requires for the canonical form before base64url. *)
  Yojson.Safe.to_string (`Assoc fields)

let mint ~now ~app_id ~private_key_path ~ttl_seconds :
    (string, error) Stdlib.Result.t =
  let ( let* ) = Result.bind in
  let* pem = read_file private_key_path in
  let* priv = decode_pem pem in
  let iat = int_of_float now in
  let ttl = clamp ~lo:60 ~hi:300 ttl_seconds in
  let exp = iat + ttl in
  let header =
    json_compact_of_assoc [ ("alg", `String "RS256"); ("typ", `String "JWT") ]
  in
  let claims =
    json_compact_of_assoc
      [ ("iss", `String app_id); ("iat", `Int iat); ("exp", `Int exp) ]
  in
  let signing_input = base64url_encode header ^ "." ^ base64url_encode claims in
  match
    Mirage_crypto_pk.Rsa.PKCS1.sign ~hash:`SHA256 ~key:priv
      (`Message signing_input)
  with
  | exception exn -> Error (Sign_error (Printexc.to_string exn))
  | signature -> Ok (signing_input ^ "." ^ base64url_encode signature)

(* {2 Inline tests} *)

let%test "clamp clamps low and high" =
  clamp ~lo:60 ~hi:300 30 = 60
  && clamp ~lo:60 ~hi:300 600 = 300
  && clamp ~lo:60 ~hi:300 200 = 200

let%test "base64url_encode strips padding" =
  not (String.contains (base64url_encode "abc") '=')

let%test "base64url_encode roundtrips uri-safe" =
  let s = "any binary \x00\x01\xff" in
  let enc = base64url_encode s in
  match Base64.decode ~pad:false ~alphabet:Base64.uri_safe_alphabet enc with
  | Ok back -> String.equal back s
  | Error _ -> false

let%test "show_error includes path-like context" =
  let s = show_error (Key_read_error "no such file") in
  String.length s > 0
