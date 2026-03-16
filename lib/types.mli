open Base

module Patch_id : sig
  type t = private string [@@deriving show, eq, ord, sexp_of, compare, hash]

  include Comparator.S with type t := t

  val of_string : string -> t
  val to_string : t -> string
end

module Pr_number : sig
  type t = private int [@@deriving show, eq, ord, sexp_of, compare, hash]

  val of_int : int -> t
  val to_int : t -> int
end

module Session_id : sig
  type t = private string [@@deriving show, eq, ord, sexp_of, compare, hash]

  val of_string : string -> t
  val to_string : t -> string
end

module Branch : sig
  type t = private string [@@deriving show, eq, ord, sexp_of, compare, hash]

  val of_string : string -> t
  val to_string : t -> string
end

module Operation_kind : sig
  type t = Rebase | Human | Merge_conflict | Ci | Review_comments
  [@@deriving show, eq, ord, sexp_of, compare, hash]
end

module Comment_id : sig
  type t = private int [@@deriving show, eq, ord, sexp_of, compare, hash]

  include Comparator.S with type t := t

  val of_int : int -> t
  val to_int : t -> int
end

module Comment : sig
  type t = { body : string; path : string option; line : int option }
  [@@deriving show, eq, sexp_of, compare]

  include Comparator.S with type t := t
end

module Patch : sig
  type t = {
    id : Patch_id.t;
    title : string;
    branch : Branch.t;
    dependencies : Patch_id.t list;
  }
  [@@deriving show, eq, sexp_of, compare]
end

module Ci_check : sig
  type t = {
    name : string;
    conclusion : string;
    details_url : string option;
    description : string option;
  }
  [@@deriving show, eq, sexp_of, compare]
end

module Stream_event : sig
  type t =
    | Text_delta of string
    | Tool_use of { name : string; input : string }
    | Final_result of { text : string; stop_reason : string }
    | Error of string
  [@@deriving show, eq, sexp_of, compare]
end

module Gameplan : sig
  type t = {
    project_name : string;
    problem_statement : string;
    solution_summary : string;
    patches : Patch.t list;
  }
  [@@deriving show, eq, sexp_of, compare]
end
