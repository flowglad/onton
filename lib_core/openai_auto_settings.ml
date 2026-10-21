(* @archlint.module core
   @archlint.domain backend-routing *)

(** Shared built-in complexity ladder for the Codex and Pi backends. *)

let settings ~complexity =
  match complexity with
  | Some 1 -> ("gpt-6-luna", "medium")
  | Some 2 -> ("gpt-6-luna", "xhigh")
  | Some _ | None -> ("gpt-6-sol", "medium")

let auto_model ~complexity = Some (fst (settings ~complexity))
let auto_effort ~complexity = Some (snd (settings ~complexity))
