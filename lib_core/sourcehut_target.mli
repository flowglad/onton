(** Pure SourceHut repository target logic. *)

val validate_target : owner:string -> repo:string -> (unit, string) Result.t
val infer_owner_repo_from_url : string -> (string * string) option
val clone_url : owner:string -> repo:string -> string

val change_id : Types.Branch.t -> Types.Pr_number.t
(** Stable, positive branch identifier used where the forge-neutral runtime
    still calls its change handle a [Pr_number.t]. It is local metadata only and
    is never sent to SourceHut. *)
