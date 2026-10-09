(* @archlint.module core
   @archlint.domain types *)

let seed_floor ~current ~observed = List.fold_left min (min current 0) observed
