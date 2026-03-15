open Base

module Patch_id = struct
  module T = struct
    type t = int [@@deriving show, eq, ord, sexp_of, compare, hash]
  end

  include T
  include Comparator.Make (T)

  let of_int n = n
  let to_int t = t
end

module Pr_number = struct
  type t = int [@@deriving show, eq, ord, sexp_of, compare, hash]

  let of_int n = n
  let to_int t = t
end

module Session_id = struct
  type t = string [@@deriving show, eq, ord, sexp_of, compare, hash]

  let of_string s = s
  let to_string t = t
end

module Branch = struct
  type t = string [@@deriving show, eq, ord, sexp_of, compare, hash]

  let of_string s = s
  let to_string t = t
end

module Operation_kind = struct
  type t = Rebase | Human | Merge_conflict | Ci | Review_comments
  [@@deriving show, eq, ord, sexp_of, compare, hash]
end

module Comment = struct
  type t = { body : string; path : string option; line : int option }
  [@@deriving show, eq, sexp_of, compare]
end

module Patch = struct
  type t = {
    id : Patch_id.t;
    title : string;
    branch : Branch.t;
    dependencies : Patch_id.t list;
  }
  [@@deriving show, eq, sexp_of, compare]
end

let%test "Patch_id.comparator allows Map.M(Patch_id)" =
  let m = Map.empty (module Patch_id) in
  let m = Map.set m ~key:(Patch_id.of_int 1) ~data:"a" in
  let m = Map.set m ~key:(Patch_id.of_int 2) ~data:"b" in
  String.equal (Map.find_exn m (Patch_id.of_int 1)) "a"
  && String.equal (Map.find_exn m (Patch_id.of_int 2)) "b"

let%test "Patch_id.comparator: key ordering is correct" =
  let m = Map.empty (module Patch_id) in
  let m = Map.set m ~key:(Patch_id.of_int 3) ~data:3 in
  let m = Map.set m ~key:(Patch_id.of_int 1) ~data:1 in
  let m = Map.set m ~key:(Patch_id.of_int 2) ~data:2 in
  let keys = Map.keys m in
  List.equal Patch_id.equal keys
    [ Patch_id.of_int 1; Patch_id.of_int 2; Patch_id.of_int 3 ]