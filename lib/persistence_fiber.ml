(* @archlint.module shell
   @archlint.domain runtime-logging *)

open Onton_core.Types

let log_event runtime ?patch_id msg =
  Runtime_logging.log_event runtime ?patch_id msg

module Persistence_env = struct
  module type S = sig
    val runtime : Runtime.t
    val clock : float Eio.Time.clock_ty Eio.Time.clock
    val project_name : string
    val transcripts : (Patch_id.t, string) Stdlib.Hashtbl.t
  end
end

module Make
    (_ : Forge.S with type error = Github.error)
    (_ : Worktree.S)
    (Env : Persistence_env.S) =
struct
  let run () =
    let path = Project_store.snapshot_path Env.project_name in
    Project_store.ensure_dir (Stdlib.Filename.dirname path);
    let rec loop () =
      Eio.Time.sleep Env.clock 5.0;
      Runtime.update Env.runtime (fun snap ->
          let t = snap.Runtime.transcripts in
          Base.Hashtbl.clear t;
          Stdlib.Hashtbl.iter
            (fun k v -> Base.Hashtbl.set t ~key:k ~data:v)
            Env.transcripts;
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
