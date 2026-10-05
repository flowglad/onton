(* @archlint.module interface
   @archlint.domain graph *)

type t [@@deriving show, eq, sexp_of, compare]
type persisted = t option [@@deriving show, eq, sexp_of, compare]

val normalize_directory : string -> (string, string) result
(** A leading slash denotes the repository root, never the host root. *)

val create :
  directory:string ->
  project_name:string ->
  yaml:bool ->
  content:string ->
  (t, string) result

val path : t -> string
val content : t -> string
val yojson_of_t : t -> Yojson.Safe.t

val of_yojson : Yojson.Safe.t -> (t, string) result
(** Total, validated decoding of persisted publication metadata. *)

val parse_optional : Yojson.Safe.t option -> (persisted, string) result
val yojson_of_persisted : persisted -> Yojson.Safe.t

val persisted_of_yojson : Yojson.Safe.t -> persisted
(** Total PPX adapter. Malformed values decode to None; persistence boundaries
    validate with parse_optional before invoking generated record decoders. *)
