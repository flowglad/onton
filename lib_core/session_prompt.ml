(* @archlint.module core
   @archlint.domain session-driver *)

type t = { context : string; turn : string }

let create ~context ~turn = { context; turn }

let render ~resume_session t =
  match resume_session with None -> t.context ^ t.turn | Some _ -> t.turn
