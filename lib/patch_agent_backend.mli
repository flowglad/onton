open Base

(** Patch-agent long-lived backend parity contract.

    Event parity today:
    - [session_init] maps 1:1 to [Stream_event.Session_init].
    - [turn_started] maps 1:1 to [Stream_event.Turn_started].
    - [text_delta] maps 1:1 to [Stream_event.Text_delta].
    - [tool_call] maps to [Stream_event.Tool_use] with [status = None] because
      patch-agent does not expose per-tool lifecycle states in M4.
    - [done] maps to [Stream_event.Final_result], with patch-agent stop-reason
      strings normalized by [Patch_agent_event_mapper].
    - [error] maps to [Stream_event.Error].

    Feature parity:
    - Supported: existing [--backend] / model-routing selection, layered prompt
      rendering, transcript capture, PR sniffing from streamed text, session
      result classification, post-session push, and supervisor state updates.
    - Deferred: resume-after-restart / auto-respawn, MCP-server plumbing, and
      long-lived-process reuse across multiple orchestrator turns. M4 surfaces
      crashes as normal session failures and starts from [$PATH] only.

    Checklist for future long-lived-specific changes:
    - Update this contract when adding or changing an RPC event mapping.
    - Keep prompt-layer file paths at [<worktree>/.patch-agent/].
    - Revisit crash semantics before adding auto-respawn.
    - Add or update property/integration tests for any new lifecycle behavior.

    This backend receives the same layered prompt text that the existing
    ephemeral backends receive from [Patch_controller]. The gameplan and patch
    layers are written once to [<worktree>/.patch-agent/], and each rendered
    turn layer is delivered over the persistent patch-agent stdio RPC. Event
    framing is delegated to [Patch_agent_rpc], and event interpretation is
    delegated to [Patch_agent_event_mapper]. This module owns only process,
    pipe, file, timeout, and lifecycle management.

    The [start] config's [worktree] must be a native absolute path, because it
    is passed both as the subprocess working directory and as patch-agent's
    [--worktree] argument. *)

val create :
  process_mgr:Eio_unix.Process.mgr_ty Eio.Resource.t ->
  clock:_ Eio.Time.clock ->
  binary_path:string ->
  setsid_exec:string option ->
  Llm_backend_long_lived.t
