open Base

module Tui_env : sig
  module type S = sig
    val runtime : Runtime.t
    val clock : float Eio.Time.clock_ty Eio.Time.clock
    val project_name : string
    val github_owner : string
    val github_repo : string
    val backend_name : string
    val transcripts : (Types.Patch_id.t, string) Stdlib.Hashtbl.t
    val resolve_routing : complexity:int option -> Backend_routing.decision
    val find_pr_number : patch_id:Types.Patch_id.t -> Types.Pr_number.t option

    val register_pr :
      patch_id:Types.Patch_id.t -> pr_number:Types.Pr_number.t -> unit

    val unregister_pr : patch_id:Types.Patch_id.t -> unit
    val pr_state : Types.Pr_number.t -> (Pr_state.t, string) Result.t
  end
end

module Make (_ : Tui_env.S) : sig
  exception Quit_tui

  val tui_fiber :
    state:Tui_state.t ->
    stdout:[> Eio.Flow.sink_ty ] Eio.Resource.t ->
    unit ->
    unit

  val input_fiber :
    state:Tui_state.t ->
    process_mgr:[> `Process_mgr | `Platform of [> `Generic ] ] Eio.Resource.t ->
    unit ->
    unit
end
