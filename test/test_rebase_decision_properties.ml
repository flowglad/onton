open Base
open Onton_core

(** Property tests for {!Rebase_decision.upstream}.

    The pure decision answers: "what [<upstream>] should I pass to
    [git rebase --onto <new_base> <upstream>]?" When the orchestrator recorded
    the previous base's SHA at the last successful rebase / start, that SHA is
    the right anchor — anything older than it is shared with the old base and
    gets dropped, anything newer is the patch's own work. Without the SHA, we
    fall back to the supplied branch name (today's behavior). Properties:

    - {b Totality}: never raises over arbitrary input.
    - {b Some-wins}: a non-empty SHA always beats the fallback.
    - {b None-falls-back}: [None] and empty / whitespace-only SHAs use the
      fallback verbatim.
    - {b Idempotence}: the result of [upstream] fed back as the SHA returns
      itself. *)

module Gen = QCheck2.Gen
module Test = QCheck2.Test

let gen_sha = Gen.string_size ~gen:(Gen.char_range 'a' 'f') (Gen.int_range 7 40)
let gen_branch = Gen.string_size ~gen:Gen.printable (Gen.int_range 1 40)

let prop_total =
  Test.make ~count:500 ~name:"RD-1: upstream is total over arbitrary input"
    (Gen.pair
       (Gen.option (Gen.string_size ~gen:Gen.printable (Gen.int_range 0 60)))
       (Gen.string_size ~gen:Gen.printable (Gen.int_range 0 60)))
    (fun (prev_base_sha, fallback) ->
      try
        let _ = Rebase_decision.upstream ~prev_base_sha ~fallback in
        true
      with _ -> false)

let prop_some_sha_wins =
  Test.make ~name:"RD-2: Some <non-empty SHA> wins over fallback" ~count:200
    (Gen.pair gen_sha gen_branch) (fun (sha, fallback) ->
      String.equal
        (Rebase_decision.upstream ~prev_base_sha:(Some sha) ~fallback)
        sha)

let prop_none_uses_fallback =
  Test.make ~name:"RD-3: None falls back to the supplied branch name" ~count:200
    gen_branch (fun fallback ->
      String.equal
        (Rebase_decision.upstream ~prev_base_sha:None ~fallback)
        fallback)

let prop_empty_sha_uses_fallback =
  Test.make
    ~name:"RD-4: Some empty / whitespace SHA falls back to the branch name"
    (Gen.pair (Gen.oneof_list [ ""; " "; "  \t  "; "\n" ]) gen_branch)
    (fun (sha, fallback) ->
      String.equal
        (Rebase_decision.upstream ~prev_base_sha:(Some sha) ~fallback)
        fallback)

let prop_idempotent_when_well_formed =
  Test.make
    ~name:
      "RD-5: upstream is idempotent under re-feeding the result, for \
       well-formed SHAs / fallbacks (no surrounding whitespace)"
    ~count:300
    (Gen.pair (Gen.option gen_sha) gen_branch)
    (fun (prev_base_sha, fallback) ->
      (* gen_sha and gen_branch don't generate whitespace-only strings, so
         the strip-then-empty branch never triggers — the result fed back as
         a SHA always survives unchanged. *)
      let r1 = Rebase_decision.upstream ~prev_base_sha ~fallback in
      let r2 = Rebase_decision.upstream ~prev_base_sha:(Some r1) ~fallback in
      (* gen_branch includes printable chars that may surround whitespace
         after a strip operation inside [upstream], so guard the equality
         with a strip-aware check. *)
      String.equal (String.strip r1) (String.strip r2))

let () =
  List.iter
    ~f:(fun t -> QCheck2.Test.check_exn t)
    [
      prop_total;
      prop_some_sha_wins;
      prop_none_uses_fallback;
      prop_empty_sha_uses_fallback;
      prop_idempotent_when_well_formed;
    ];
  Stdlib.print_endline "Rebase_decision: all properties passed"
