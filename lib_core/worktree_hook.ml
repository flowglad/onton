(* @archlint.module core
   @archlint.domain branch-reconcile *)

open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type request = { id : string; path : string; branch : string; script : string }
[@@deriving eq, compare, sexp_of, yojson]

type run = { request : request; attempt : int }
[@@deriving eq, compare, sexp_of]

type outcome = Succeeded | Failed of string
[@@deriving eq, compare, sexp_of, yojson]

type status = Planned | Running | Finished of outcome
[@@deriving eq, compare, sexp_of, yojson]

type record = { request : request; attempts : int; status : status }
[@@deriving eq, compare, sexp_of, yojson]

type t = record option [@@deriving eq, compare, sexp_of, yojson]

type event =
  | Plan of request
  | Started of run
  | Completed of { id : string; attempt : int; outcome : outcome }
[@@deriving eq, compare, sexp_of]

type decision = Ready | Run of run | Stop of string [@@deriving eq]

let empty = None

let request_valid (r : request) =
  List.for_all [ r.id; r.path; r.branch; r.script ] ~f:(fun value ->
      not (String.is_empty value || String.contains value '\000'))

let valid (t : t) =
  match t with
  | None -> true
  | Some r -> (
      request_valid r.request && r.attempts >= 0
      &&
      match r.status with
      | Planned -> true
      | Running | Finished _ -> r.attempts > 0)

let decode json =
  match Json.try_of_yojson t_of_yojson json with
  | Ok t when valid t -> Ok t
  | Ok _ -> Error "invalid worktree hook checkpoint"
  | Error message -> Error message

let next t ~path ~branch =
  match t with
  | None | Some { status = Finished _; _ } -> Ready
  | Some r
    when not
           (String.equal r.request.path path
           && String.equal r.request.branch branch) ->
      Stop "worktree_create_hook_context_changed"
  | Some { status = Running; _ } -> Stop "worktree_create_hook_outcome_unknown"
  | Some { status = Planned; attempts; request } ->
      if attempts = Int.max_value then
        Stop "worktree_create_hook_attempt_exhausted"
      else Run { request; attempt = attempts + 1 }

let step t = function
  | Plan request -> (
      if not (request_valid request) then t
      else
        match t with
        | None -> Some { request; attempts = 0; status = Planned }
        | Some { status = Finished _; request = previous; _ }
          when not (String.equal previous.id request.id) ->
            Some { request; attempts = 0; status = Planned }
        | Some { status = Planned | Running | Finished _; _ } -> t)
  | Started run -> (
      match t with
      | Some ({ status = Planned; _ } as r)
        when equal_request r.request run.request
             && r.attempts < Int.max_value
             && run.attempt = r.attempts + 1 ->
          Some { r with status = Running; attempts = run.attempt }
      | Some { status = Planned | Running | Finished _; _ } | None -> t)
  | Completed { id; attempt; outcome } -> (
      match t with
      | Some ({ status = Running; _ } as r)
        when String.equal r.request.id id && r.attempts = attempt ->
          Some { r with status = Finished outcome }
      | Some { status = Planned | Running | Finished _; _ } | None -> t)

let resume = function
  | Some ({ status = Running; _ } as r) -> Some { r with status = Planned }
  | (None | Some { status = Planned | Finished _; _ }) as t -> t

let is_pending = function
  | Some { status = Planned | Running; _ } -> true
  | None | Some { status = Finished _; _ } -> false
