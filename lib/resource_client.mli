(* Image-local public helper adapter. Transport retains exit independently of JSON. *)
type transport =
  argv:string list -> input:string -> (int * string * string, string) Result.t

type t

type error =
  | Transport of string
  | Invalid_response of string
  | Helper_exit of int * Resource_protocol.response

val create : helper:string -> transport:transport -> t
val subprocess : transport

(* Construct a coding worker manifest from trusted executable/arguments. The
    helper owns workspace preparation and places immutable files under inputs/;
    it creates the fresh checkout at repo/. No caller environment is embedded. *)
val worker_manifest :
  executable:string ->
  args:string list ->
  inputs:(string * string) list ->
  (Yojson.Safe.t, string) result

val request :
  t -> Resource_protocol.request -> (Resource_protocol.response, error) result

(* Raw single-use attachment. [connect] is never retried. The caller owns the
    returned file descriptors and must close them; an EOF is not retirement. *)
type attachment

val connect :
  t ->
  run_id:string ->
  coordinator_id:string ->
  owner_epoch:int64 ->
  revision:int64 ->
  request_id:string ->
  resource_id:string ->
  (attachment, string) result

val input : attachment -> Unix.file_descr
val output : attachment -> Unix.file_descr
val disconnect : attachment -> unit
