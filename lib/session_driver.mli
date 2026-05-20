(** Drive one backend session for one patch.

    [run] is the layer above [Llm_backend.run_streaming]: it owns the session
    lifecycle (resume vs fresh, fallback chain), worktree provisioning,
    transcript buffer accumulation, PR-number sniffing from streamed text,
    activity-log streaming, post-session push, and result classification.

    The function is large because it ties together all of those concerns —
    splitting it would scatter the supervisor's view of one session across
    several modules. The backend below is already abstracted; this is the single
    place where "run a session for this patch" lives. *)

(** Construction-time environment for session driving. Values here are fixed for
    the lifetime of the module instance and never vary per call. *)
module type ENV = sig
  include Run_env.S
  val owner : string
  val repo : string
  val transcripts : (Types.Patch_id.t, string) Stdlib.Hashtbl.t
end

module Make (_ : Worktree.S) (_ : ENV) : sig
  val run :
    kind:Types.Operation_kind.t option ->
    patch_id:Types.Patch_id.t ->
    prompt:string ->
    agent:Patch_agent.t ->
    on_pr_detected:(Types.Pr_number.t -> unit) ->
    backend:Llm_backend.t ->
    complexity:int option ->
    [ `Ok | `Failed | `Retry_push ] * (string * string) list
  (** Returns the supervisor disposition and the list of [(tool_name, status)]
      pairs for any tool calls that did not reach a [completed] state (used by
      the Pr_body classifier to disambiguate "agent chose not to write" from
      "Write was blocked"). Callers may also produce a [`Stale] variant from
      pre-flight checks before invoking this function — the polymorphic-variant
      union widens at the call site. *)

  type long_lived_session
  (** Mutable per-patch long-lived backend session state. The backend's
      existential handle type remains tied to the backend that created it. *)

  val create_long_lived_session :
    backend:Llm_backend_long_lived.t ->
    provider:string ->
    model:string ->
    effort:string ->
    gameplan_prompt:string ->
    patch_prompt:string ->
    long_lived_session

  val update_long_lived_session_prompts :
    long_lived_session -> gameplan_prompt:string -> patch_prompt:string -> unit

  val long_lived_session_failed : long_lived_session -> bool
  val shutdown_long_lived_session : long_lived_session -> unit

  val run_long_lived :
    sw:Eio.Switch.t ->
    kind:Types.Operation_kind.t option ->
    patch_id:Types.Patch_id.t ->
    prompt:string ->
    agent:Patch_agent.t ->
    on_pr_detected:(Types.Pr_number.t -> unit) ->
    session:long_lived_session ->
    complexity:int option ->
    [ `Ok | `Failed | `Retry_push ] * (string * string) list
  (** Long-lived backend counterpart to {!run}. It shares the same supervisor
      bookkeeping and delivers the rendered turn over [backend.prompt] instead
      of spawning a fresh backend process. *)

  val session_mode : Patch_agent.t -> [ `Resume of string | `Fresh | `Give_up ]
  (** Inspect the agent's session-fallback state to decide whether the next
      invocation should resume an existing session, start fresh, or give up. *)

  val extract_pr_number_from_text :
    ?at_end_of_stream:bool ->
    owner:string ->
    repo:string ->
    string ->
    Types.Pr_number.t option
  (** Scan [text] for a [github.com/<owner>/<repo>/pull/N] URL and return the
      first [N] found. Used to sniff PR creation from the agent's stdout stream.

      A digit run terminating at the end of [text] is treated as
      potentially-incomplete by default, so this function will return [None] for
      [".../pull/12"] when the next stream chunk could turn it into
      [.../pull/1234]. Pass [~at_end_of_stream:true] when calling on a complete
      buffer (e.g. on [Final_result]) to treat end-of-buffer as a valid
      terminator. *)
end
