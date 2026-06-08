(* @archlint.module test
   @archlint.domain session-id *)

let session_ids_are_uuid_v4_shaped =
  QCheck2.Test.make ~name:"mint returns uuid-v4 shaped strings" ~count:20
    QCheck2.Gen.unit (fun () ->
      let id = Onton.Session_id.mint () in
      String.length id = 36
      && id.[14] = '4'
      && match id.[19] with '8' | '9' | 'a' | 'b' -> true | _ -> false)

(* Mint [n] ids for a generated [n] and assert they are all distinct and
   uuid-v4 shaped — [mint] must never collide within a batch. *)
let minted_ids_are_distinct =
  QCheck2.Test.make ~name:"mint returns distinct ids within a batch" ~count:50
    QCheck2.Gen.(int_range 1 50)
    (fun n ->
      let ids = List.init n (fun _ -> Onton.Session_id.mint ()) in
      let module S = Set.Make (String) in
      let unique = List.fold_left (fun s id -> S.add id s) S.empty ids in
      S.cardinal unique = n
      && List.for_all (fun id -> String.length id = 36) ids)

let () =
  QCheck2.Test.check_exn session_ids_are_uuid_v4_shaped;
  QCheck2.Test.check_exn minted_ids_are_distinct
