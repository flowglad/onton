(* @archlint.module core
   @archlint.domain comment-responses *)

open Base
open Types

type entry = { comment_id : int; response : string }
[@@deriving show, eq, sexp_of, compare]

type action = {
  comment_id : Comment_id.t;
  thread_id : string option;
  response : string;
}
[@@deriving show, eq, sexp_of, compare]

type outcome = { actions : action list; unanswered : Comment_id.t list }
[@@deriving show, eq, sexp_of, compare]

(* The filename stem is the comment id: "12345.md" -> 12345. A single
   extension is stripped; "12345" bare works too. Anything whose stem is not
   all digits (with optional leading '-') is rejected — the caller logs those
   filenames as unrecognized. *)
let comment_id_of_filename filename =
  let base = Stdlib.Filename.basename filename in
  let stem = Stdlib.Filename.remove_extension base in
  let is_digits s =
    (not (String.is_empty s)) && String.for_all s ~f:Char.is_digit
  in
  let unsigned =
    match String.chop_prefix stem ~prefix:"-" with Some s -> s | None -> stem
  in
  if is_digits unsigned then Stdlib.int_of_string_opt stem else None

let entry_of_file ~filename ~contents : entry option =
  match comment_id_of_filename filename with
  | None -> None
  | Some comment_id ->
      let response = String.strip contents in
      if String.is_empty response then None else Some { comment_id; response }

(* True when the thread's last word is onton's own posted reply, so a
   re-delivery only needs its resolve retried — replying again would be a
   duplicate. Purely positional: only a viewer-authored *reply* counts.
   Openers are always reviewer feedback, even when authored by the viewer's
   login (a human co-reviewing from the token's account) — co-reviewers open
   threads, they never correspond mid-thread, so position disambiguates
   authorship where login cannot. An unknown viewer fails open to false: a
   possible duplicate reply beats silently never replying. *)
let is_resolve_retry ~(viewer_login : string option) (c : Comment.t) : bool =
  match viewer_login with
  | None -> false
  | Some viewer ->
      Option.equal String.equal c.Comment.last_reply_author (Some viewer)

let plan ~(delivered : Comment.t list) ~(entries : entry list) : outcome =
  (* Last entry per comment_id wins. Duplicate stems for one id (e.g. "12.md"
     and "12.txt") are pathological; feeding entries in sorted-filename order
     makes the pick deterministic. *)
  let index =
    List.fold entries
      ~init:(Map.empty (module Int))
      ~f:(fun acc e -> Map.set acc ~key:e.comment_id ~data:e)
  in
  (* One action per distinct delivered comment id, in delivered order. GitHub
     comment ids are unique, but a duplicate in [delivered] must not produce a
     duplicate reply. *)
  let _, rev_actions, rev_unanswered =
    List.fold delivered
      ~init:(Set.empty (module Int), [], [])
      ~f:(fun (seen, actions, unanswered) (c : Comment.t) ->
        let raw_id = Comment_id.to_int c.Comment.id in
        if Set.mem seen raw_id then (seen, actions, unanswered)
        else
          let seen = Set.add seen raw_id in
          match Map.find index raw_id with
          | Some e ->
              let action =
                {
                  comment_id = c.Comment.id;
                  thread_id = c.Comment.thread_id;
                  response = e.response;
                }
              in
              (seen, action :: actions, unanswered)
          | None -> (seen, actions, c.Comment.id :: unanswered))
  in
  { actions = List.rev rev_actions; unanswered = List.rev rev_unanswered }

let unmatched_entries ~(delivered : Comment.t list) ~(entries : entry list) :
    entry list =
  let delivered_ids =
    Set.of_list
      (module Int)
      (List.map delivered ~f:(fun c -> Comment_id.to_int c.Comment.id))
  in
  List.filter entries ~f:(fun e -> not (Set.mem delivered_ids e.comment_id))

(* {2 Inline tests} *)

let comment ?(thread_id = None) ?(last_reply_author = None) id : Comment.t =
  {
    Comment.id = Comment_id.of_int id;
    thread_id;
    body = "body";
    path = None;
    line = None;
    commit_sha = None;
    original_commit_sha = None;
    outdated = false;
    last_reply_author;
  }

let%test "is_resolve_retry: viewer-authored last reply" =
  is_resolve_retry ~viewer_login:(Some "onton-bot")
    (comment ~last_reply_author:(Some "onton-bot") 1)

let%test "is_resolve_retry: someone else's last reply is not a retry" =
  not
    (is_resolve_retry ~viewer_login:(Some "onton-bot")
       (comment ~last_reply_author:(Some "alice") 1))

let%test "is_resolve_retry: opener-only thread is never a retry" =
  (* Even a viewer-authored opener is reviewer feedback (a human co-reviewing
     from the token's account) — only a *reply* position marks onton's own
     response. *)
  not (is_resolve_retry ~viewer_login:(Some "onton-bot") (comment 1))

let%test "is_resolve_retry: unknown viewer fails open to false" =
  not
    (is_resolve_retry ~viewer_login:None
       (comment ~last_reply_author:(Some "onton-bot") 1))

let%test "entry_of_file: id.md with content" =
  match entry_of_file ~filename:"12345.md" ~contents:"fixed in abc123\n" with
  | Some { comment_id = 12345; response = "fixed in abc123" } -> true
  | _ -> false

let%test "entry_of_file: bare id, no extension" =
  match entry_of_file ~filename:"7.txt" ~contents:"r" with
  | Some { comment_id = 7; _ } -> (
      match entry_of_file ~filename:"7" ~contents:"r" with
      | Some { comment_id = 7; _ } -> true
      | _ -> false)
  | _ -> false

let%test "entry_of_file: absolute path uses basename" =
  match
    entry_of_file ~filename:"/data/artifacts/p1/comment_responses/42.md"
      ~contents:"done"
  with
  | Some { comment_id = 42; response = "done" } -> true
  | _ -> false

let%test "entry_of_file: negative (synthetic) ids parse" =
  match entry_of_file ~filename:"-3.md" ~contents:"r" with
  | Some { comment_id = -3; _ } -> true
  | _ -> false

let%test "entry_of_file: non-numeric stem rejected" =
  Option.is_none (entry_of_file ~filename:"notes.md" ~contents:"r")
  && Option.is_none (entry_of_file ~filename:"comment-12.md" ~contents:"r")
  && Option.is_none (entry_of_file ~filename:".md" ~contents:"r")
  && Option.is_none (entry_of_file ~filename:"" ~contents:"r")

let%test "entry_of_file: blank content rejected" =
  Option.is_none (entry_of_file ~filename:"12.md" ~contents:"  \n\t ")
  && Option.is_none (entry_of_file ~filename:"12.md" ~contents:"")

let%test "plan: joins entries to delivered comments in delivered order" =
  let delivered =
    [ comment ~thread_id:(Some "T1") 1; comment ~thread_id:(Some "T2") 2 ]
  in
  let entries =
    [
      { comment_id = 2; response = "second" };
      { comment_id = 1; response = "first" };
    ]
  in
  let o = plan ~delivered ~entries in
  equal_outcome o
    {
      actions =
        [
          {
            comment_id = Comment_id.of_int 1;
            thread_id = Some "T1";
            response = "first";
          };
          {
            comment_id = Comment_id.of_int 2;
            thread_id = Some "T2";
            response = "second";
          };
        ];
      unanswered = [];
    }

let%test "plan: last entry wins for a duplicated comment_id" =
  let delivered = [ comment 1 ] in
  let entries =
    [
      { comment_id = 1; response = "draft" };
      { comment_id = 1; response = "final" };
    ]
  in
  let o = plan ~delivered ~entries in
  match o.actions with [ { response = "final"; _ } ] -> true | _ -> false

let%test "plan: reports unanswered comments; unmatched_entries reports strays" =
  let delivered = [ comment 1; comment 2 ] in
  let entries =
    [ { comment_id = 2; response = "r" }; { comment_id = 99; response = "s" } ]
  in
  let o = plan ~delivered ~entries in
  equal_outcome o
    {
      actions =
        [
          { comment_id = Comment_id.of_int 2; thread_id = None; response = "r" };
        ];
      unanswered = [ Comment_id.of_int 1 ];
    }
  && equal_entry
       (List.hd_exn (unmatched_entries ~delivered ~entries))
       { comment_id = 99; response = "s" }

let%test "plan: duplicate delivered ids produce a single action" =
  let delivered = [ comment 1; comment 1 ] in
  let entries = [ { comment_id = 1; response = "once" } ] in
  let o = plan ~delivered ~entries in
  List.length o.actions = 1
