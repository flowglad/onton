open Base
open Types
module Ok = Operation_kind

let priority (k : Ok.t) : int =
  match k with
  | Ok.Rebase -> 0
  | Ok.Human -> 1
  | Ok.Merge_conflict -> 2
  | Ok.Ci -> 3
  | Ok.Review_comments -> 4

let is_feedback (k : Ok.t) : bool =
  match k with
  | Ok.Human | Ok.Merge_conflict | Ok.Ci | Ok.Review_comments -> true
  | Ok.Rebase -> false

type t = Ok.t list [@@deriving show, eq, sexp_of]
(** The queue is a set of operation kinds, stored as a sorted list (by priority
    ascending, so head is highest-priority). *)

let empty : t = []
let is_empty (q : t) : bool = List.is_empty q

let enqueue (q : t) (k : Ok.t) : t =
  if List.mem q k ~equal:Ok.equal then q
  else
    List.sort (k :: q) ~compare:(fun a b ->
        Int.compare (priority a) (priority b))

let peek_highest (q : t) : Ok.t option = List.hd q

let dequeue_highest (q : t) : (Ok.t * t) option =
  match q with [] -> None | x :: rest -> Some (x, rest)

let mem (q : t) (k : Ok.t) : bool = List.mem q k ~equal:Ok.equal
let to_list (q : t) : Ok.t list = q

let highest_priority (q : t) (k : Ok.t) : bool =
  mem q k
  &&
  match peek_highest q with
  | None -> false
  | Some top -> priority k <= priority top

let%test_module "Priority" =
  (module struct
    let%test "priority ordering" =
      priority Ok.Rebase < priority Ok.Human
      && priority Ok.Human < priority Ok.Merge_conflict
      && priority Ok.Merge_conflict < priority Ok.Ci
      && priority Ok.Ci < priority Ok.Review_comments

    let%test "is_feedback" =
      is_feedback Ok.Human
      && is_feedback Ok.Merge_conflict
      && is_feedback Ok.Ci
      && is_feedback Ok.Review_comments
      && not (is_feedback Ok.Rebase)

    let%test "empty queue" =
      is_empty empty && Option.is_none (peek_highest empty)

    let%test "enqueue and peek" =
      let q = enqueue (enqueue empty Ok.Ci) Ok.Rebase in
      Option.equal Ok.equal (peek_highest q) (Some Ok.Rebase)

    let%test "dequeue ordering" =
      let q =
        enqueue (enqueue (enqueue empty Ok.Review_comments) Ok.Rebase) Ok.Human
      in
      let first = dequeue_highest q in
      let second = Option.bind first ~f:(fun (_, r) -> dequeue_highest r) in
      let third = Option.bind second ~f:(fun (_, r) -> dequeue_highest r) in
      Option.value_map first ~default:false ~f:(fun (k, _) ->
          Ok.equal k Ok.Rebase)
      && Option.value_map second ~default:false ~f:(fun (k, _) ->
          Ok.equal k Ok.Human)
      && Option.value_map third ~default:false ~f:(fun (k, rest) ->
          Ok.equal k Ok.Review_comments && is_empty rest)

    let%test "enqueue is idempotent" =
      let q = enqueue empty Ok.Ci in
      let q2 = enqueue q Ok.Ci in
      List.equal Ok.equal q q2

    let%test "highest_priority spec" =
      let q =
        enqueue (enqueue (enqueue empty Ok.Review_comments) Ok.Ci) Ok.Human
      in
      highest_priority q Ok.Human
      && (not (highest_priority q Ok.Ci))
      && (not (highest_priority q Ok.Review_comments))
      && not (highest_priority q Ok.Rebase)

    let%test "highest_priority with single element" =
      let q = enqueue empty Ok.Review_comments in
      highest_priority q Ok.Review_comments

    let%test "mem" =
      let q = enqueue (enqueue empty Ok.Ci) Ok.Rebase in
      mem q Ok.Ci && mem q Ok.Rebase && not (mem q Ok.Human)

    let%test "to_list is sorted by priority" =
      let q =
        enqueue (enqueue (enqueue empty Ok.Review_comments) Ok.Rebase) Ok.Ci
      in
      let l = to_list q in
      List.equal Ok.equal l [ Ok.Rebase; Ok.Ci; Ok.Review_comments ]

    (* --- additional deterministic tests --- *)

    let%test "priority values are exact" =
      priority Ok.Rebase = 0
      && priority Ok.Human = 1
      && priority Ok.Merge_conflict = 2
      && priority Ok.Ci = 3
      && priority Ok.Review_comments = 4

    let%test "is_empty after enqueue is false" =
      not (is_empty (enqueue empty Ok.Rebase))

    let%test "dequeue_highest empty returns None" =
      Option.is_none (dequeue_highest empty)

    let%test "dequeue_highest single element returns element and empty rest" =
      match dequeue_highest (enqueue empty Ok.Merge_conflict) with
      | None -> false
      | Some (k, rest) -> Ok.equal k Ok.Merge_conflict && is_empty rest

    let%test "peek_highest is non-destructive" =
      let q = enqueue (enqueue empty Ok.Ci) Ok.Human in
      let first = peek_highest q in
      let second = peek_highest q in
      Option.equal Ok.equal first second

    let%test "enqueue all variants produces fully sorted list" =
      let q =
        enqueue
          (enqueue
             (enqueue
                (enqueue (enqueue empty Ok.Review_comments) Ok.Merge_conflict)
                Ok.Ci)
             Ok.Human)
          Ok.Rebase
      in
      List.equal Ok.equal (to_list q)
        [ Ok.Rebase; Ok.Human; Ok.Merge_conflict; Ok.Ci; Ok.Review_comments ]

    let%test "exhaust queue via dequeue leaves empty" =
      let q =
        enqueue (enqueue (enqueue empty Ok.Rebase) Ok.Human) Ok.Merge_conflict
      in
      let step q =
        Option.bind (dequeue_highest q) ~f:(fun (_, rest) ->
            if is_empty rest then None else Some rest)
      in
      let q1 = Option.value_exn (dequeue_highest q) |> snd in
      let q2 = Option.value_exn (dequeue_highest q1) |> snd in
      let q3 = Option.value_exn (dequeue_highest q2) |> snd in
      is_empty q3 && Option.is_none (step q3)

    let%test "peek_highest equals fst of dequeue_highest" =
      let q =
        enqueue (enqueue (enqueue empty Ok.Review_comments) Ok.Rebase) Ok.Human
      in
      Option.equal Ok.equal (peek_highest q)
        (Option.map (dequeue_highest q) ~f:fst)

    let%test "highest_priority false when element not in queue" =
      let q = enqueue (enqueue empty Ok.Ci) Ok.Human in
      not (highest_priority q Ok.Rebase)
      && not (highest_priority q Ok.Merge_conflict)
      && not (highest_priority q Ok.Review_comments)

    let%test "highest_priority false on empty queue" =
      not (highest_priority empty Ok.Rebase)

    let%test "enqueue order-independent" =
      let q1 = enqueue (enqueue empty Ok.Human) Ok.Ci in
      let q2 = enqueue (enqueue empty Ok.Ci) Ok.Human in
      List.equal Ok.equal q1 q2

    let%test "dequeue then peek shows new head" =
      let q =
        enqueue (enqueue (enqueue empty Ok.Review_comments) Ok.Rebase) Ok.Ci
      in
      match dequeue_highest q with
      | None -> false
      | Some (top, rest) ->
        Ok.equal top Ok.Rebase
        && Option.equal Ok.equal (peek_highest rest) (Some Ok.Ci)

    let%test "mem empty returns false for all kinds" =
      not (mem empty Ok.Rebase)
      && not (mem empty Ok.Human)
      && not (mem empty Ok.Merge_conflict)
      && not (mem empty Ok.Ci)
      && not (mem empty Ok.Review_comments)

    let%test "enqueue all five then mem all returns true" =
      let q =
        enqueue
          (enqueue
             (enqueue
                (enqueue (enqueue empty Ok.Rebase) Ok.Human)
                Ok.Merge_conflict)
             Ok.Ci)
          Ok.Review_comments
      in
      mem q Ok.Rebase
      && mem q Ok.Human
      && mem q Ok.Merge_conflict
      && mem q Ok.Ci
      && mem q Ok.Review_comments

    (* --- QCheck property-based tests --- *)

    let all_kinds =
      [ Ok.Rebase; Ok.Human; Ok.Merge_conflict; Ok.Ci; Ok.Review_comments ]

    let kind_gen = QCheck2.Gen.oneofl all_kinds

    let queue_gen =
      QCheck2.Gen.map
        (fun kinds -> List.fold kinds ~init:empty ~f:enqueue)
        (QCheck2.Gen.list_of_size (QCheck2.Gen.int_range 0 5) kind_gen)

    let%test "qcheck: enqueue then mem" =
      let test =
        QCheck2.Test.make ~name:"enqueue then mem" kind_gen (fun k ->
            mem (enqueue empty k) k)
      in
      QCheck2.Test.check_exn test;
      true

    let%test "qcheck: double enqueue idempotent" =
      let test =
        QCheck2.Test.make ~name:"double enqueue idempotent"
          QCheck2.Gen.(pair queue_gen kind_gen)
          (fun (q, k) ->
            let q1 = enqueue q k in
            let q2 = enqueue q1 k in
            List.equal Ok.equal q1 q2)
      in
      QCheck2.Test.check_exn test;
      true

    let%test "qcheck: to_list is sorted ascending by priority" =
      let test =
        QCheck2.Test.make ~name:"to_list sorted" queue_gen (fun q ->
            let l = to_list q in
            let rec is_sorted = function
              | [] | [ _ ] -> true
              | a :: b :: rest ->
                priority a <= priority b && is_sorted (b :: rest)
            in
            is_sorted l)
      in
      QCheck2.Test.check_exn test;
      true

    let%test "qcheck: peek equals fst of dequeue" =
      let test =
        QCheck2.Test.make ~name:"peek = fst dequeue" queue_gen (fun q ->
            Option.equal Ok.equal (peek_highest q)
              (Option.map (dequeue_highest q) ~f:fst))
      in
      QCheck2.Test.check_exn test;
      true

    let%test "qcheck: dequeue rest has one fewer element" =
      let test =
        QCheck2.Test.make ~name:"dequeue shrinks by one" queue_gen (fun q ->
            match dequeue_highest q with
            | None -> is_empty q
            | Some (_, rest) ->
              List.length (to_list rest) = List.length (to_list q) - 1)
      in
      QCheck2.Test.check_exn test;
      true

    let%test "qcheck: highest_priority only true for minimum priority item" =
      let test =
        QCheck2.Test.make ~name:"highest_priority correct" queue_gen (fun q ->
            match peek_highest q with
            | None -> true
            | Some top ->
              List.for_all all_kinds ~f:(fun k ->
                  let expected =
                    mem q k && priority k <= priority top
                  in
                  Bool.equal (highest_priority q k) expected))
      in
      QCheck2.Test.check_exn test;
      true

    let%test "qcheck: is_empty iff to_list is empty" =
      let test =
        QCheck2.Test.make ~name:"is_empty iff to_list empty" queue_gen
          (fun q -> Bool.equal (is_empty q) (List.is_empty (to_list q)))
      in
      QCheck2.Test.check_exn test;
      true
  end)