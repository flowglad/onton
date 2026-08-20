(** Reviewless SourceHut forge backed by git.sr.ht branches and builds.sr.ht. *)

type error =
  | Http_error of { status : int; body : string }
  | Api_error of string
  | Timeout of float
  | Transport_error of string
  | Git_error of string
  | Unsupported of string

val show_error : error -> string

val make :
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  token:string ->
  owner:string ->
  repo:string ->
  repo_root:string ->
  main_branch:Types.Branch.t ->
  changes:(Types.Branch.t * Types.Branch.t) list ->
  (module Forge.S with type error = error)
