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

module Comment_id = struct
  module T = struct
    type t = int [@@deriving show, eq, ord, sexp_of, compare, hash]
  end

  include T
  include Comparator.Make (T)

  let of_int n = n
  let to_int t = t

  let next_synthetic =
    let counter = Atomic.make 0 in
    fun () ->
      let n = Atomic.fetch_and_add counter (-1) - 1 in
      n
end

module Comment = struct
  module T = struct
    type t = {
      id : Comment_id.t;
      body : string;
      path : string option;
      line : int option;
    }
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

module Pr_url = struct
  module T = struct
    type t = string [@@deriving show, eq, ord, sexp_of, compare, hash]
  end

  include T
  include Comparator.Make (T)

  let of_string s = s
  let to_string t = t
end

module Stop_reason = struct
  type t =
    | End_turn
    | Tool_use
    | Max_tokens
    | Stop_sequence
    | Pause_turn
    | Refusal
    | Model_context_window_exceeded
  [@@deriving show, eq, sexp_of, compare]

  let of_string = function
    | "end_turn" -> Some End_turn
    | "tool_use" -> Some Tool_use
    | "max_tokens" -> Some Max_tokens
    | "stop_sequence" -> Some Stop_sequence
    | "pause_turn" -> Some Pause_turn
    | "refusal" -> Some Refusal
    | "model_context_window_exceeded" -> Some Model_context_window_exceeded
    | _ -> None
end

(* Models server-sent events from Claude Code's NDJSON stdout
   (--output-format json), not the raw Anthropic streaming API. *)
module Stream_event = struct
  type t =
    | Text_delta of string
    | Tool_use of { name : string; input : string }
    | Final_result of { text : string; stop_reason : Stop_reason.t }
    | Error of string
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
