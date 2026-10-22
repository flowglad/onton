(* @archlint.module interface
   @archlint.domain orchestrator *)

(** Attempt-scoped durable checkpoint boundary. Implementations must freeze the
    submitted candidate before waiting for an acknowledgement. *)
module type S = sig
  val persist : Runtime.snapshot -> (unit, string) result

  val record_session_id :
    patch_id:Types.Patch_id.t -> session_id:string -> (unit, string) result
end

module Local (_ : sig
  val project_name : string
end) : S
