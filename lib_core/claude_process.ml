open Base

(** A Claude session's status. *)
type session_status = Idle | Busy | Failed
[@@deriving show, eq, sexp_of, compare]

(** State of a Claude process associated with a patch. The key invariant from
    the spec: sessions are never lost. Once a patch has a session, [has_session]
    always returns true. A session may transition between Idle, Busy, and
    Failed, but it cannot be removed. *)
type t =
  | No_session
  | Has_session of { id : Types.Session_id.t; status : session_status }
[@@deriving show, eq, sexp_of, compare]

let no_session = No_session
let start session_id = Has_session { id = session_id; status = Idle }
let has_session = function No_session -> false | Has_session _ -> true

let is_busy = function
  | No_session -> false
  | Has_session { status = Busy; _ } -> true
  | Has_session { status = Idle | Failed; _ } -> false

let is_failed = function
  | No_session -> false
  | Has_session { status = Failed; _ } -> true
  | Has_session { status = Idle | Busy; _ } -> false

let session_id = function
  | No_session -> None
  | Has_session { id; _ } -> Some id

(** Mark a session as busy. Requires an existing session. *)
let mark_busy = function
  | No_session -> No_session
  | Has_session s -> Has_session { s with status = Busy }

(** Mark a session as idle (finished work). Requires an existing session. *)
let mark_idle = function
  | No_session -> No_session
  | Has_session s -> Has_session { s with status = Idle }

(** Mark a session as failed. Requires an existing session. The session is NOT
    removed — sessions are never lost. *)
let mark_failed = function
  | No_session -> No_session
  | Has_session s -> Has_session { s with status = Failed }

(** Restart a failed session with a new session ID. Preserves the "sessions are
    never lost" invariant — replaces the failed session rather than removing it.
*)
let restart session_id = function
  | No_session -> No_session
  | Has_session { status = Failed; _ } ->
      Has_session { id = session_id; status = Idle }
  | Has_session { status = Idle | Busy; _ } as session -> session

(** Check the "sessions are never lost" invariant: if [before] has a session,
    [after] must also have a session. *)
let check_session_preservation ~before ~after =
  (not (has_session before)) || has_session after

let%test "no_session has no session" = not (has_session no_session)

let%test "start creates a session" =
  has_session (start (Types.Session_id.of_string "s1"))

let%test "sessions are never lost — mark_busy" =
  let s = start (Types.Session_id.of_string "s1") in
  check_session_preservation ~before:s ~after:(mark_busy s)

let%test "sessions are never lost — mark_failed" =
  let s = start (Types.Session_id.of_string "s1") in
  check_session_preservation ~before:s ~after:(mark_failed s)

let%test "sessions are never lost — mark_idle" =
  let s = start (Types.Session_id.of_string "s1") in
  check_session_preservation ~before:s ~after:(mark_idle s)

let%test "sessions are never lost — restart" =
  let s = start (Types.Session_id.of_string "s1") in
  let s = mark_failed s in
  let s' = restart (Types.Session_id.of_string "s2") s in
  check_session_preservation ~before:s ~after:s'

let%test "busy detection" =
  let s = start (Types.Session_id.of_string "s1") in
  (not (is_busy s)) && is_busy (mark_busy s)

let%test "failed detection" =
  let s = start (Types.Session_id.of_string "s1") in
  (not (is_failed s)) && is_failed (mark_failed s)

let%test "restart replaces session id on failed session" =
  let s = start (Types.Session_id.of_string "s1") |> mark_failed in
  let s' = restart (Types.Session_id.of_string "s2") s in
  match session_id s' with
  | Some id -> String.equal (Types.Session_id.to_string id) "s2"
  | None -> false

let%test "restart is no-op on idle session" =
  let s = start (Types.Session_id.of_string "s1") in
  let s' = restart (Types.Session_id.of_string "s2") s in
  match session_id s' with
  | Some id -> String.equal (Types.Session_id.to_string id) "s1"
  | None -> false

let%test "restart is no-op on busy session" =
  let s = start (Types.Session_id.of_string "s1") |> mark_busy in
  let s' = restart (Types.Session_id.of_string "s2") s in
  match session_id s' with
  | Some id -> String.equal (Types.Session_id.to_string id) "s1"
  | None -> false

let%test "no_session operations are no-ops" =
  let n = no_session in
  equal n (mark_busy n)
  && equal n (mark_idle n)
  && equal n (mark_failed n)
  && equal n (restart (Types.Session_id.of_string "s1") n)
