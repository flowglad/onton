(** GitHub PR description bounds. The byte cap leaves room below GitHub's
    65,536-character field limit, including for multibyte text. *)

val max_bytes : int

val limit : string -> string
(** Preserve the beginning of a description and append a notice if needed.
    Truncation does not split a valid UTF-8 code point. *)

val summarize_files : string list -> string list
(** Show the first 30 paths, followed by the omitted count. *)
