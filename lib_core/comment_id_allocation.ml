(* @archlint.module core
   @archlint.domain types *)

let seed_floor ~current ~observed = List.fold_left min (min current 0) observed
let next current = if current = min_int then None else Some (min current 0 - 1)
