(* @archlint.module interface
   @archlint.domain repo-git *)

module type S = sig
  val infer_owner_repo : unit -> (string * string) option
  val fetch_managed_repo : unit -> (unit, string) Result.t
  val infer_default_branch : unit -> Types.Branch.t

  val validate_branch_resolves :
    main_branch:Types.Branch.t -> (unit, string) Result.t
end

type client = (module S)

val make : repo_root:string -> client
