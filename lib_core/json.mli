(* @archlint.module interface
   @archlint.domain json *)

(** Total accessors over [Yojson.Safe.t] — the sanctioned alternative to
    [Yojson.Safe.Util], whose [member]/[to_string]/[to_*] are partial (they
    raise on a type mismatch or on [`Null], invisibly to the type checker). Here
    a missing key, an explicit [`Null], and a type mismatch all collapse to
    [None], so nullability becomes an [option] the caller must handle rather
    than a latent runtime exception. This is the only module permitted to
    reference [Yojson.Safe.Util] (see [scripts/check-no-raw-yojson.sh]). *)

type t = Yojson.Safe.t

val field : string -> t -> t option
(** [field key j] is the value at [key] when [j] is an object and the value is
    present and non-[`Null]; otherwise [None]. Never raises. *)

val string : t -> string option
(** [Some s] iff [j] is [`String s]; else [None]. *)

val int : t -> int option
(** [Some i] iff [j] is [`Int i]; else [None]. *)

val bool : t -> bool option
(** [Some b] iff [j] is [`Bool b]; else [None]. *)

val list : t -> t list option
(** [Some xs] iff [j] is [`List xs]; else [None]. *)

val assoc : t -> (string * t) list option
(** [Some kvs] iff [j] is [`Assoc kvs]; else [None]. *)

val string_field : string -> t -> string option
(** [field] composed with [string]. *)

val int_field : string -> t -> int option
(** [field] composed with [int]. *)

val bool_field : string -> t -> bool option
(** [field] composed with [bool]. *)

val try_of_yojson : (t -> 'a) -> t -> ('a, string) result
(** Run a raising [ppx_yojson_conv] deserializer, catching
    [Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error] and
    [Yojson.Safe.Util.Type_error] and returning them as [Error string]. *)
