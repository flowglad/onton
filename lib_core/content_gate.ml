(* @archlint.module core
   @archlint.domain content-gate *)

open Base

(** One-shot gate that fires the first time the LLM has *committed* a
    conversation turn to its [.jsonl] (the only thing claude can resume).

    Subtlety this guards against: claude's stream-json carries text chunks
    ([Text_delta]) and tool-call announcements ([Tool_use]) as they're
    generated, but claude flushes them to the conversation file only when the
    turn *completes*. If the API errors mid-turn, the streamed chunks are gone
    and the [.jsonl] stays at its 124-byte [last-prompt] header. Persisting the
    session-id sidecar at that point would poison every later [--resume] with
    "No conversation found". [Final_result] (turn completed) is the earliest
    event that proves at least one complete turn is on disk; the failure path
    emits [Error] from the terminating "result" event instead, so the gate stays
    closed.

    [should_persist] returns the next gate state plus a decision. The decision
    is [true] exactly once — on the first [Final_result] event — and [false] for
    everything else, including all events after the first persist. *)

type t = Not_persisted | Persisted

let create () = Not_persisted

let should_persist t (event : Types.Stream_event.t) =
  match (t, event) with
  | Persisted, _ -> (Persisted, false)
  | Not_persisted, Types.Stream_event.Final_result _ -> (Persisted, true)
  | ( Not_persisted,
      ( Types.Stream_event.Turn_started | Types.Stream_event.Session_init _
      | Types.Stream_event.Text_delta _ | Types.Stream_event.Tool_use _
      | Types.Stream_event.Error _ ) ) ->
      (Not_persisted, false)

let has_persisted = function Persisted -> true | Not_persisted -> false

let test_session_init =
  Types.Stream_event.Session_init
    {
      session_id = "x";
      api_key_source = None;
      model = None;
      claude_code_version = None;
      permission_mode = None;
    }

let%test "Session_init never triggers persist" =
  let g = create () in
  let g, persist = should_persist g test_session_init in
  (not persist) && not (has_persisted g)

let%test "Turn_started never triggers persist" =
  let g = create () in
  let g, persist = should_persist g Types.Stream_event.Turn_started in
  (not persist) && not (has_persisted g)

let%test
    "Text_delta never triggers persist (chunks unflushed until Final_result)" =
  let g = create () in
  let g, persist = should_persist g (Types.Stream_event.Text_delta "hi") in
  (not persist) && not (has_persisted g)

let%test "Tool_use never triggers persist (turn still in progress)" =
  let g = create () in
  let ev =
    Types.Stream_event.Tool_use { name = "Bash"; input = "{}"; status = None }
  in
  let g, persist = should_persist g ev in
  (not persist) && not (has_persisted g)

let%test "Error never triggers persist" =
  let g = create () in
  let _, persist = should_persist g (Types.Stream_event.Error "boom") in
  not persist

let%test "first Final_result triggers; subsequent calls return false" =
  let g = create () in
  let ev =
    Types.Stream_event.Final_result
      { text = "done"; stop_reason = Types.Stop_reason.End_turn }
  in
  let g, first = should_persist g ev in
  let g, second = should_persist g ev in
  first && (not second) && has_persisted g

let%test "Final_result after a flurry of in-flight events triggers" =
  let g = create () in
  let g, _ = should_persist g test_session_init in
  let g, _ = should_persist g Types.Stream_event.Turn_started in
  let g, _ = should_persist g (Types.Stream_event.Text_delta "streamed") in
  let g, _ =
    should_persist g
      (Types.Stream_event.Tool_use
         { name = "Bash"; input = "{}"; status = None })
  in
  let ev =
    Types.Stream_event.Final_result
      { text = "done"; stop_reason = Types.Stop_reason.End_turn }
  in
  let g, persist = should_persist g ev in
  persist && has_persisted g
