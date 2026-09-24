(* @archlint.module shell
   @archlint.domain activity-log *)

open Base
open Types

let merged_log_entries ~(log : Activity_log.t) ~limit ~compare
    ~(map_event : Activity_log.Event.t -> 'a)
    ~(map_transition : Activity_log.Transition_entry.t -> 'a) =
  let events =
    List.map (Activity_log.recent_events log ~limit) ~f:(fun e ->
        (e.Activity_log.Event.timestamp, map_event e))
  in
  let transitions =
    List.map (Activity_log.recent_transitions log ~limit) ~f:(fun t ->
        (t.Activity_log.Transition_entry.timestamp, map_transition t))
  in
  List.sort (events @ transitions) ~compare

let format_event (e : Activity_log.Event.t) = e.Activity_log.Event.message

let format_transition (t : Activity_log.Transition_entry.t) =
  Printf.sprintf "%s -> %s"
    (Tui.label t.Activity_log.Transition_entry.from_status)
    (Tui.label t.Activity_log.Transition_entry.to_status)

module Headless_env = struct
  module type S = sig
    val runtime : Runtime.t
    val clock : float Eio.Time.clock_ty Eio.Time.clock
    val stdout : Eio_unix.sink_ty Eio.Resource.t
    val transcript_updates : (Patch_id.t, string) Stdlib.Hashtbl.t
    val initial_transcript_positions : (Patch_id.t, int) Stdlib.Hashtbl.t
    val include_transcripts : bool
  end
end

module Make (_ : Forge.S) (_ : Worktree.S) (Env : Headless_env.S) = struct
  let max_transcript_chunk = 4 * 1024

  let write_record ~source ~timestamp ?patch_id message =
    let fields =
      [
        ("timestamp", `Float timestamp);
        ("source", `String source);
        ("message", `String message);
      ]
      @
      match patch_id with
      | None -> []
      | Some id -> [ ("patch_id", `String (Patch_id.to_string id)) ]
    in
    Eio.Flow.copy_string
      (Yojson.Safe.to_string (`Assoc fields) ^ "\n")
      Env.stdout

  let run () =
    let seen = Stdlib.Hashtbl.create 256 in
    let transcript_positions = Env.initial_transcript_positions in
    let rec loop () =
      let entries =
        Runtime.read Env.runtime (fun snap ->
            merged_log_entries ~log:snap.Runtime.activity_log ~limit:500
              ~compare:(fun (t1, _) (t2, _) -> Float.ascending t1 t2)
              ~map_event:(fun event ->
                (event.Activity_log.Event.patch_id, format_event event))
              ~map_transition:(fun transition ->
                ( Some transition.Activity_log.Transition_entry.patch_id,
                  format_transition transition )))
      in
      List.iter entries ~f:(fun (ts, (patch_id, msg)) ->
          let key = (ts, patch_id, msg) in
          if not (Stdlib.Hashtbl.mem seen key) then (
            Stdlib.Hashtbl.replace seen key true;
            write_record ~source:"activity" ~timestamp:ts ?patch_id msg));
      if Env.include_transcripts then (
        (* Drain without yielding. Session fibers replace entries in this table,
           so the snapshot contains only producers changed since the last poll. *)
        let updates =
          Stdlib.Hashtbl.fold
            (fun patch_id transcript acc -> (patch_id, transcript) :: acc)
            Env.transcript_updates []
        in
        Stdlib.Hashtbl.clear Env.transcript_updates;
        List.iter updates ~f:(fun (patch_id, transcript) ->
            let length = String.length transcript in
            let previous =
              Stdlib.Hashtbl.find_opt transcript_positions patch_id
              |> Option.value ~default:0
            in
            let offset = if previous <= length then previous else 0 in
            let rec emit offset =
              if offset < length then (
                let stop = min length (offset + max_transcript_chunk) in
                let stop =
                  if stop = length then stop
                  else
                    let rec boundary n =
                      if n <= offset then stop
                      else if Char.to_int transcript.[n] land 0xc0 <> 0x80 then
                        n
                      else boundary (n - 1)
                    in
                    boundary stop
                in
                write_record ~source:"transcript"
                  ~timestamp:(Unix.gettimeofday ()) ~patch_id
                  (String.sub transcript ~pos:offset ~len:(stop - offset));
                emit stop)
            in
            emit offset;
            Stdlib.Hashtbl.replace transcript_positions patch_id length));
      if Stdlib.Hashtbl.length seen > 2000 then (
        let current = Stdlib.Hashtbl.create 256 in
        List.iter entries ~f:(fun (ts, (patch_id, msg)) ->
            Stdlib.Hashtbl.replace current (ts, patch_id, msg) true);
        Stdlib.Hashtbl.reset seen;
        Stdlib.Hashtbl.iter (fun k v -> Stdlib.Hashtbl.replace seen k v) current);
      Eio.Time.sleep Env.clock 1.0;
      loop ()
    in
    loop ()
end
