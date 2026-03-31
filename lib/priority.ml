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
  | Ok.Implementation_notes -> 5

let is_feedback (k : Ok.t) : bool =
  match k with
  | Ok.Human | Ok.Merge_conflict | Ok.Ci | Ok.Review_comments
  | Ok.Implementation_notes ->
      true
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
  end)
