open Base
open Onton

(** Unit tests for the merge-conflict prompt's Recovery section.

    Pin the verbatim text the patch agent receives so an accidental change to
    the prompt is caught at test time rather than discovered when an agent fails
    to recover from a lost rebase state. *)

let assert_contains label haystack ~substring =
  if not (String.is_substring haystack ~substring) then (
    Stdlib.print_endline ("FAIL: " ^ label);
    Stdlib.print_endline ("  expected substring: " ^ substring);
    Stdlib.print_endline "  ----- prompt -----";
    Stdlib.print_endline haystack;
    Stdlib.print_endline "  ----- end prompt -----";
    Stdlib.exit 1)

let assert_not_contains label haystack ~substring =
  if String.is_substring haystack ~substring then (
    Stdlib.print_endline ("FAIL: " ^ label);
    Stdlib.print_endline ("  unexpected substring: " ^ substring);
    Stdlib.exit 1)

(* Property #11: legacy byte-equal regression. With no conflict_info, the
   output must match what callers got before the recovery section existed. *)
let () =
  let with_ci_none =
    Prompt.render_merge_conflict_prompt ~project_name:"" ~base_branch:"main" ()
  in
  let with_ci_explicit_none =
    Prompt.render_merge_conflict_prompt ~project_name:"" ~base_branch:"main"
      ?conflict_info:None ()
  in
  if not (String.equal with_ci_none with_ci_explicit_none) then (
    Stdlib.print_endline
      "FAIL: omitting conflict_info != passing ?conflict_info:None";
    Stdlib.exit 1);
  (* No Recovery section in the no-info form. *)
  assert_not_contains "no conflict_info -> no Recovery section" with_ci_none
    ~substring:"## Recovery"

(* Property #12: Onto strategy. Recovery section contains the verbatim
   `git rebase --onto <target> <old_base>` command, both inputs, and one
   bullet per unique commit (oldest first). *)
let () =
  let ci : Worktree.conflict_info =
    Worktree.
      {
        target = "origin/main";
        old_base = "deadbeefcafef00d";
        unique_commits =
          [
            { sha = "newest1abc"; subject = "[proj] Patch 7: head" };
            { sha = "middle2def"; subject = "[proj] Patch 7: middle" };
            { sha = "oldest3ghi"; subject = "[proj] Patch 7: tail" };
          ];
        strategy = Onto;
        orig_head = "abcdef0123456789abcdef0123456789abcdef01";
      }
  in
  let prompt =
    Prompt.render_merge_conflict_prompt ~project_name:"" ~base_branch:"main"
      ~conflict_info:ci ()
  in
  assert_contains "Onto: contains Recovery header" prompt
    ~substring:"## Recovery (if rebase state is lost)";
  assert_contains "Onto: instructs agent to fetch first" prompt
    ~substring:"git fetch origin";
  assert_contains "Onto: contains exact --onto command against origin/main"
    prompt ~substring:"git rebase --onto origin/main deadbeefcafef00d";
  assert_contains "Onto: surfaces orig_head reset command" prompt
    ~substring:"git reset --hard abcdef0123456789abcdef0123456789abcdef01";
  assert_contains "Onto: lists oldest commit first (oldest3)" prompt
    ~substring:"oldest3 [proj] Patch 7: tail";
  assert_contains "Onto: lists middle commit (middle2)" prompt
    ~substring:"middle2 [proj] Patch 7: middle";
  assert_contains "Onto: lists newest commit last (newest1)" prompt
    ~substring:"newest1 [proj] Patch 7: head";
  (* Order check: oldest must appear before newest in the rendered text *)
  let oldest_idx =
    Option.value_exn
      (String.substr_index prompt ~pattern:"oldest3 [proj] Patch 7: tail")
  in
  let newest_idx =
    Option.value_exn
      (String.substr_index prompt ~pattern:"newest1 [proj] Patch 7: head")
  in
  if not (oldest_idx < newest_idx) then (
    Stdlib.print_endline
      "FAIL: Onto: commits should be rendered oldest-first, but newest \
       appeared first";
    Stdlib.exit 1)

(* Property #13: Plain strategy. Recovery section recommends plain
   [git rebase <target>] and explicitly notes that no per-patch commit list
   is available. *)
let () =
  let ci : Worktree.conflict_info =
    Worktree.
      {
        target = "origin/release";
        old_base = "";
        unique_commits = [];
        strategy = Plain;
        orig_head = "";
      }
  in
  let prompt =
    Prompt.render_merge_conflict_prompt ~project_name:"" ~base_branch:"release"
      ~conflict_info:ci ()
  in
  (* Slice at the Recovery header so substring checks don't accidentally match
     legacy body text that also mentions --onto / the target branch. *)
  let recovery_section =
    match
      String.substr_index prompt
        ~pattern:"## Recovery (if rebase state is lost)"
    with
    | Some i -> String.subo prompt ~pos:i
    | None -> ""
  in
  assert_contains "Plain: contains Recovery header" recovery_section
    ~substring:"## Recovery (if rebase state is lost)";
  assert_contains "Plain: instructs agent to fetch first" recovery_section
    ~substring:"git fetch origin";
  assert_contains "Plain: contains plain rebase command against origin/release"
    recovery_section ~substring:"git rebase origin/release";
  assert_not_contains "Plain: Recovery section must NOT contain --onto"
    recovery_section ~substring:"--onto";
  assert_not_contains "Plain: empty orig_head -> no reset command in Recovery"
    recovery_section ~substring:"git reset --hard";
  assert_contains "Plain: notes no per-patch commit list" recovery_section
    ~substring:"No per-patch commit list could be isolated"

let () = Stdlib.print_endline "All prompt-recovery tests passed."
