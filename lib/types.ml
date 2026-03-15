open Base

module Patch_id = struct
  module T = struct
    type t = string [@@deriving show, eq, ord, sexp_of, compare, hash]
  end

  include T
  include Comparator.Make (T)

  let of_string s = s
  let to_string t = t
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
  module T = struct
    type t = { body : string; path : string option; line : int option }
    [@@deriving show, eq, sexp_of, compare]
  end

  include T
  include Comparator.Make (T)
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

module Ci_check = struct
  type t = {
    name : string;
    conclusion : string;
    details_url : string option;
    description : string option;
  }
  [@@deriving show, eq, sexp_of, compare]
end

module Gameplan = struct
  type t = {
    project_name : string;
    problem_statement : string;
    solution_summary : string;
    patches : Patch.t list;
  }
  [@@deriving show, eq, sexp_of, compare]
end
