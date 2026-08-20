(* @archlint.module interface
   @archlint.domain sourcehut-builds *)

(** Total decoding and decisions for builds.sr.ht jobs. *)

type job = {
  id : int;
  status : string;
  note : string;
  tags : string list;
  created : string option;
  owner : string;
  manifest : string;
  visibility : string;
  logs : string list;
}
[@@deriving show, eq]

val jobs_of_response : string -> (job list * string option, string) Result.t
val job_of_response : string -> (job, string) Result.t
val submit_id_of_response : string -> (int, string) Result.t

val checks_for_commit :
  owner:string ->
  repo:string ->
  branch:Types.Branch.t ->
  sha:string ->
  job list ->
  Types.Ci_check.t list

val conclusion_of_status : string -> string
val log_source : job -> Ci_log_digest.source
