(* @archlint.module test
   @archlint.domain anchor-history *)

open Onton_core

let anchor_of n =
  let sha = String.make 39 '0' ^ string_of_int (n mod 10) in
  Anchor.make
    ~base:(Types.Branch.of_string "main")
    ~sha ~observed_at_remote:true
  |> Option.get

let anchor_history_is_bounded =
  QCheck2.Test.make ~name:"anchor history keeps bounded newest entries"
    ~count:200
    QCheck2.Gen.(list_size (int_range 0 30) (int_range 0 9))
    (fun ids ->
      let history =
        List.fold_left
          (fun history n -> Anchor_history.push history (anchor_of n))
          Anchor_history.empty ids
      in
      Anchor_history.length history <= 8
      && List.length (Anchor_history.to_list history)
         = Anchor_history.length history)

let anchor_history_json_round_trips =
  QCheck2.Test.make ~name:"anchor history JSON round-trips" ~count:200
    QCheck2.Gen.(list_size (int_range 0 30) (int_range 0 9))
    (fun ids ->
      let history =
        List.fold_left
          (fun history n -> Anchor_history.push history (anchor_of n))
          Anchor_history.empty ids
      in
      let json = Anchor_history.yojson_of_t history in
      Anchor_history.equal (Anchor_history.t_of_yojson json) history
      && Option.equal Anchor_history.equal
           (Anchor_history.of_yojson_opt json)
           (Some history))

let () =
  QCheck2.Test.check_exn anchor_history_is_bounded;
  QCheck2.Test.check_exn anchor_history_json_round_trips
