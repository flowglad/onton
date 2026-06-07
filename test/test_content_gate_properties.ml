(* @archlint.module test
   @archlint.domain content-gate *)

open Onton_core

let final_result =
  Types.Stream_event.Final_result
    { text = ""; stop_reason = Types.Stop_reason.End_turn }

let content_gate_persists_at_most_once =
  QCheck2.Test.make ~name:"content gate persists at most once" ~count:200
    QCheck2.Gen.(list_size (int_range 0 20) bool)
    (fun flags ->
      let gate = ref (Content_gate.create ()) in
      let persists = ref 0 in
      List.iter
        (fun is_final ->
          let event =
            if is_final then final_result else Types.Stream_event.Turn_started
          in
          let next, persist = Content_gate.should_persist !gate event in
          gate := next;
          if persist then incr persists)
        flags;
      !persists <= 1
      && Bool.equal (Content_gate.has_persisted !gate) (!persists = 1))

let () = QCheck2.Test.check_exn content_gate_persists_at_most_once
