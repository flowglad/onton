open Base

module Persistence_env = struct
  module type S = sig
    val runtime : Runtime.t
    val clock : float Eio.Time.clock_ty Eio.Time.clock
    val project_name : string
  end
end

module Make (Env : Persistence_env.S) = struct
  let log_event = Runtime_logging.log_event

  let run ~transcripts () =
    let path = Project_store.snapshot_path Env.project_name in
    Project_store.ensure_dir (Stdlib.Filename.dirname path);
    let rec loop () =
      Eio.Time.sleep Env.clock 5.0;
      Runtime.update Env.runtime (fun snap ->
          let t = snap.Runtime.transcripts in
          Base.Hashtbl.clear t;
          Stdlib.Hashtbl.iter
            (fun k v -> Base.Hashtbl.set t ~key:k ~data:v)
            transcripts;
          snap);
      let snap = Runtime.snapshot_unsync Env.runtime in
      (match Persistence.save ~path snap with
      | Ok () -> ()
      | Error msg ->
          log_event Env.runtime
            (Printf.sprintf "Persistence save failed — %s" msg));
      loop ()
    in
    loop ()
end
