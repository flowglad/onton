(** One-shot gate that signals the LLM has produced resumable conversation
    content. See [content_gate.ml] for the why. *)

type t

val create : unit -> t

val should_persist : t -> Types.Stream_event.t -> bool
(** Returns [true] exactly once — on the first [Text_delta] or [Tool_use] event
    — and [false] for every other event (including all events after the first
    persist). *)

val has_persisted : t -> bool
(** Whether [should_persist] has already returned [true] for this gate. *)
