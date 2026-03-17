open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Patch_id = struct
  module T = struct
    type t = string [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]
  end

  include T
  include Comparator.Make (T)

  let of_string s = s
  let to_string t = t
end

module Pr_number = struct
  type t = int [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  let of_int n = n
  let to_int t = t
end

module Session_id = struct
  type t = string [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  let of_string s = s
  let to_string t = t
end

module Branch = struct
  type t = string [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]

  let of_string s = s
  let to_string t = t
end

module Operation_kind = struct
  type t = Rebase | Human | Merge_conflict | Ci | Review_comments
  [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]
end

module Comment_id = struct
  module T = struct
    type t = int [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]
  end

  include T
  include Comparator.Make (T)

  let of_int n = n
  let to_int t = t
  let counter = Atomic.make 0

  let next_synthetic () =
    (* Atomically claim the next synthetic ID. fetch_and_add returns the old
       counter value and decrements it, so counter always equals the last issued
       ID after each call. Subtract 1 from the old value to get the new value;
       this ensures the first call returns -1 (counter: 0 → -1, result: 0-1=-1)
       and 0 is never issued. seed_synthetic_counter relies on this: seeding to
       min_id causes the next call to return min_id-1, safely below all existing
       IDs. *)
    Atomic.fetch_and_add counter (-1) - 1

  let seed_synthetic_counter ids =
    let min_id =
      List.fold ids ~init:0 ~f:(fun acc id -> Int.min acc (to_int id))
    in
    let rec try_seed () =
      let current = Atomic.get counter in
      if min_id < current then
        if not (Atomic.compare_and_set counter current min_id) then try_seed ()
    in
    try_seed ()
end

module Comment = struct
  module T = struct
    type t = {
      id : Comment_id.t;
      thread_id : string option;
      body : string;
      path : string option;
      line : int option;
    }
    [@@deriving show, eq, sexp_of, compare, yojson]
  end

  include T
  include Comparator.Make (T)
end

module Patch = struct
  type t = {
    id : Patch_id.t;
    title : string;
    description : string;
    branch : Branch.t;
    dependencies : Patch_id.t list;
    spec : string; [@yojson.default ""]
    acceptance_criteria : string list; [@yojson.default []]
    files : string list; [@yojson.default []]
  }
  [@@deriving show, eq, sexp_of, compare, yojson]
end

module Ci_check = struct
  type t = {
    name : string;
    conclusion : string;
    details_url : string option;
    description : string option;
  }
  [@@deriving show, eq, sexp_of, compare, yojson]
end

module Pr_url = struct
  module T = struct
    type t = string [@@deriving show, eq, ord, sexp_of, compare, hash, yojson]
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
  [@@deriving show, eq, sexp_of, compare, yojson]

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

(* Models events from Claude Code's NDJSON stdout
   (--output-format stream-json), not the raw Anthropic streaming API. *)
module Stream_event = struct
  type t =
    | Text_delta of string
    | Tool_use of { name : string; input : string }
    | Final_result of { text : string; stop_reason : Stop_reason.t }
    | Error of string
  [@@deriving show, eq, sexp_of, compare, yojson]
end

module Gameplan = struct
  type t = {
    project_name : string;
    problem_statement : string;
    solution_summary : string;
    design_decisions : string;
    patches : Patch.t list;
  }
  [@@deriving show, eq, sexp_of, compare, yojson]
end
