(* @archlint.module shell
   @archlint.domain activity-log *)

open Base
open Types

let merged_log_entries ~(log : Activity_log.t) ~limit ~compare
    ~(map_event : Activity_log.Event.t -> 'a)
    ~(map_transition : Activity_log.Transition_entry.t -> 'a)
    ~(map_stream : Activity_log.Stream_entry.t -> 'a) =
  let events =
    List.map (Activity_log.recent_events log ~limit) ~f:(fun e ->
        (e.Activity_log.Event.timestamp, map_event e))
  in
  let transitions =
    List.map (Activity_log.recent_transitions log ~limit) ~f:(fun t ->
        (t.Activity_log.Transition_entry.timestamp, map_transition t))
  in
  let stream =
    List.map (Activity_log.recent_stream_entries log ~limit) ~f:(fun s ->
        (s.Activity_log.Stream_entry.timestamp, map_stream s))
  in
  List.sort (events @ transitions @ stream) ~compare

let format_stream_kind (kind : Activity_log.Stream_entry.kind) =
  match kind with
  | Activity_log.Stream_entry.Tool_use (name, input) ->
      if String.length input > 0 then Printf.sprintf "Tool %s — %s" name input
      else Printf.sprintf "Tool %s" name
  | Activity_log.Stream_entry.Text_chunk text -> text
  | Activity_log.Stream_entry.Finished reason ->
      Printf.sprintf "Finished — %s" reason
  | Activity_log.Stream_entry.Stream_error msg ->
      Printf.sprintf "Stream error — %s" msg

let format_time ts =
  let t = Unix.localtime ts in
  Printf.sprintf "%02d:%02d:%02d" t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec

let format_event (e : Activity_log.Event.t) =
  let pid_str =
    match e.Activity_log.Event.patch_id with
    | Some pid -> Printf.sprintf "[%s] " (Patch_id.to_string pid)
    | None -> ""
  in
  pid_str ^ e.Activity_log.Event.message

let format_transition (t : Activity_log.Transition_entry.t) =
  Printf.sprintf "[%s] %s -> %s"
    (Patch_id.to_string t.Activity_log.Transition_entry.patch_id)
    (Tui.label t.Activity_log.Transition_entry.from_status)
    (Tui.label t.Activity_log.Transition_entry.to_status)

module Headless_env = struct
  module type S = sig
    val runtime : Runtime.t
    val clock : float Eio.Time.clock_ty Eio.Time.clock
    val stdout : Eio_unix.sink_ty Eio.Resource.t
  end
end

module Make
    (_ : Forge.S with type error = Github.error)
    (_ : Worktree.S)
    (Env : Headless_env.S) =
struct
  let run () =
    let seen = Stdlib.Hashtbl.create 256 in
    let rec loop () =
      let entries =
        Runtime.read Env.runtime (fun snap ->
            merged_log_entries ~log:snap.Runtime.activity_log ~limit:500
              ~compare:(fun (t1, _) (t2, _) -> Float.ascending t1 t2)
              ~map_event:format_event ~map_transition:format_transition
              ~map_stream:(fun (s : Activity_log.Stream_entry.t) ->
                Printf.sprintf "[%s] %s"
                  (Patch_id.to_string s.Activity_log.Stream_entry.patch_id)
                  (format_stream_kind s.Activity_log.Stream_entry.kind)))
      in
      List.iter entries ~f:(fun (ts, msg) ->
          let key = (ts, msg) in
          if not (Stdlib.Hashtbl.mem seen key) then (
            Stdlib.Hashtbl.replace seen key true;
            Eio.Flow.copy_string
              (Printf.sprintf "%s %s\n" (format_time ts) msg)
              Env.stdout));
      if Stdlib.Hashtbl.length seen > 2000 then (
        let current = Stdlib.Hashtbl.create 256 in
        List.iter entries ~f:(fun key ->
            Stdlib.Hashtbl.replace current key true);
        Stdlib.Hashtbl.reset seen;
        Stdlib.Hashtbl.iter (fun k v -> Stdlib.Hashtbl.replace seen k v) current);
      Eio.Time.sleep Env.clock 1.0;
      loop ()
    in
    loop ()
end
