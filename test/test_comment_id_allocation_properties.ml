(* @archlint.module test
   @archlint.domain types *)

open Onton_core
module A = Comment_id_allocation
module G = QCheck2.Gen

let tests =
  [
    QCheck2.Test.make ~count:500
      ~name:"comment ID restoration is total and bounded"
      G.(pair int (list_size (int_range 0 100) int))
      (fun (current, observed) ->
        let floor = A.seed_floor ~current ~observed in
        floor <= current && floor <= 0
        && List.for_all (fun id -> floor <= id) observed
        && List.mem floor (0 :: current :: observed));
    QCheck2.Test.make ~count:500
      ~name:"comment ID restoration is idempotent and order independent"
      G.(
        triple int
          (list_size (int_range 0 50) int)
          (list_size (int_range 0 50) int))
      (fun (current, left, right) ->
        let floor = A.seed_floor ~current ~observed:(left @ right) in
        floor
        = A.seed_floor
            ~current:(A.seed_floor ~current ~observed:left)
            ~observed:right
        && floor
           = A.seed_floor
               ~current:(A.seed_floor ~current ~observed:right)
               ~observed:left
        && floor
           = A.seed_floor ~current:floor ~observed:(List.rev (left @ right)));
    QCheck2.Test.make ~count:200
      ~name:"comment ID restoration handles empty and extreme boundaries" G.int
      (fun current ->
        A.seed_floor ~current ~observed:[] = min current 0
        && A.seed_floor ~current ~observed:[ max_int; 0 ] = min current 0
        && A.seed_floor ~current ~observed:[ min_int; max_int ] = min_int);
  ]

let () = QCheck_base_runner.run_tests_main tests
