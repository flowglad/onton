open Base

(** Patch-agent long-lived backend.

    Parity contract: this backend receives the same layered prompt text that the
    existing ephemeral backends receive from [Patch_controller]. The gameplan
    and patch layers are written once to [<worktree>/.patch-agent/], and each
    rendered turn layer is delivered over the persistent patch-agent stdio RPC.
    Event framing is delegated to [Patch_agent_rpc], and event interpretation is
    delegated to [Patch_agent_event_mapper]. This module owns only process,
    pipe, file, timeout, and lifecycle management. *)

val create :
  process_mgr:[> [ `Generic ] Eio.Process.mgr_ty ] Eio.Resource.t ->
  clock:[> float Eio.Time.clock_ty ] Eio.Resource.t ->
  binary_path:string ->
  setsid_exec:string option ->
  Llm_backend_long_lived.t
