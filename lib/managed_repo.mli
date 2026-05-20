val ensure_managed_repo :
  project_name:string ->
  token:string ->
  owner:string ->
  repo:string ->
  (string, string) result
(** Ensure the onton-managed checkout for [project_name] exists and is current.
    Returns the checkout root on success. *)

val infer_github_token : unit -> string
(** Resolve a GitHub token from [GITHUB_TOKEN] or [gh auth token]. *)
