(* @archlint.module test
   @archlint.domain session-id *)

let session_ids_are_uuid_v4_shaped =
  QCheck2.Test.make ~name:"mint returns uuid-v4 shaped strings" ~count:20
    QCheck2.Gen.unit (fun () ->
      let id = Onton.Session_id.mint () in
      String.length id = 36
      && id.[14] = '4'
      && match id.[19] with '8' | '9' | 'a' | 'b' -> true | _ -> false)

let () = QCheck2.Test.check_exn session_ids_are_uuid_v4_shaped
