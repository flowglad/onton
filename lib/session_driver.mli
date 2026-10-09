(* @archlint.module interface
   @archlint.domain session-driver *)

(** Drive one backend session for one patch.

    [run] is the layer above [Llm_backend.run_streaming]: it owns the session
    lifecycle (resume vs fresh, fallback chain), worktree provisioning,
    transcript buffer accumulation, PR-number sniffing from streamed text,
    activity-log streaming, durable publication, and result classification.

    The function is large because it ties together all of those concerns —
    splitting it would scatter the supervisor's view of one session across
    several modules. The backend below is already abstracted; this is the single
    place where "run a session for this patch" lives. *)

type disposition = [ `Ok | `Failed | `Retry_push | `No_commits ]
type prompt

type run_result = {
  disposition : disposition;
  tool_failures : (string * string) list;
  turn_accepted : bool;
      (** Whether the backend emitted positive evidence that it accepted the
          turn. Callers use this to restore Human guidance when an otherwise
          successful process exits without processing the prompt. *)
}

(** Construction-time environment for session driving. Values here are fixed for
    the lifetime of the module instance and never vary per call. *)
module type ENV = sig
  include Run_env.S

  val owner : string
  val repo : string
  val transcripts : (Types.Patch_id.t, string) Stdlib.Hashtbl.t
  val transcript_updates : (Types.Patch_id.t, string) Stdlib.Hashtbl.t
  val event_log : Event_log.t
end

module Make (_ : Worktree.S) (_ : ENV) : sig
  type nonrec run_result = run_result

  val create_prompt :
    context:(worktree_path:string -> string) -> turn:string -> prompt
  (** The context renderer receives the ensured worktree path and is called once
      for a fresh session, including fallback, and never when resuming, giving
      up, or failing to provision a worktree. *)

  val publish_completion :
    write_owner:Runtime.patch_write ->
    patch_id:Types.Patch_id.t ->
    agent:Patch_agent.t ->
    path:string ->
    Session_result.completion ->
    [ `Published | `No_work | `Pending ]
  (** Reconcile a checkpointed local completion. Scripted gameplan publication
      and backend sessions use the same captured-revision publication protocol.
      Pending publication never changes the recorded implementation outcome. *)

  val run_repair :
    patch_id:Types.Patch_id.t ->
    agent:Patch_agent.t ->
    backend:Llm_backend.t ->
    complexity:int option ->
    cwd:Eio.Fs.dir_ty Eio.Path.t ->
    context:string ->
    guidance:string list ->
    turn:Branch_reconcile.repair_turn ->
    read_head:(unit -> string option) ->
    Branch_reconcile.event
  (** Continue the patch conversation for every repair mode and append all
      streamed content to its transcript. The caller holds patch ownership. This
      does not invoke implementation completion or publication. *)

  val run_owned :
    write_owner:Runtime.patch_write ->
    kind:Types.Operation_kind.t option ->
    delivery_mode:Patch_decision.delivery_mode ->
    patch_id:Types.Patch_id.t ->
    prompt:prompt ->
    agent:Patch_agent.t ->
    on_pr_detected:(Types.Pr_number.t -> unit) ->
    backend:Llm_backend.t ->
    complexity:int option ->
    run_result
  (** Reuse the session action's scoped patch ownership. *)

  val run :
    kind:Types.Operation_kind.t option ->
    delivery_mode:Patch_decision.delivery_mode ->
    patch_id:Types.Patch_id.t ->
    prompt:prompt ->
    agent:Patch_agent.t ->
    on_pr_detected:(Types.Pr_number.t -> unit) ->
    backend:Llm_backend.t ->
    complexity:int option ->
    run_result
  (** Returns the supervisor disposition, backend-acceptance evidence, and the
      list of [(tool_name, status)] pairs for tool calls that did not reach a
      [completed] state. [delivery_mode] records whether Start or Respond
      initiated the turn and must remain stable across in-session PR discovery.
      Callers may also produce a [`Stale] variant from pre-flight checks before
      invoking this function. *)

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
