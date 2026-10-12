(* @archlint.module core
   @archlint.domain orchestrator *)

open Base

type t = Pending of Set.M(String).t | Closed

let create ids = Pending (Set.of_list (module String) ids)

let complete state id =
  match state with
  | Closed -> (state, false)
  | Pending remaining -> (
      match id with
      | None when Set.is_empty remaining -> (Closed, true)
      | Some id when Set.mem remaining id ->
          let remaining = Set.remove remaining id in
          if Set.is_empty remaining then (Closed, true)
          else (Pending remaining, false)
      | None | Some _ -> (state, false))

let cancel _ = Closed
