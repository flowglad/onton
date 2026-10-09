(* @archlint.module test
   @archlint.domain project-lifecycle *)

open Onton
module L = Project_lifecycle

let check message value = if not value then QCheck2.Test.fail_report message

let get = function
  | Ok value -> value
  | Error error -> failwith (L.error_message error)

let busy = function
  | Error (L.Busy _) -> true
  | Ok _ | Error (L.Io_error _) -> false

let write path text =
  Out_channel.with_open_bin path (fun channel -> output_string channel text)

let read path = In_channel.with_open_bin path In_channel.input_all

let rec wait pid =
  try snd (Unix.waitpid [] pid)
  with Unix.Unix_error (Unix.EINTR, _, _) -> wait pid

let child_checks message f =
  match Unix.fork () with
  | 0 -> ( try exit (if f () then 0 else 2) with _ -> exit 3)
  | child -> check message (wait child = Unix.WEXITED 0)

let () =
  let root = Filename.temp_dir "onton-lifecycle-" "" in
  let before = Sys.getenv_opt "ONTON_DATA_DIR" in
  Unix.putenv "ONTON_DATA_DIR" root;
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "ONTON_DATA_DIR" (Option.value before ~default:""))
    (fun () ->
      let registration = get (L.acquire_registration ()) in
      check "same-process nested acquisition cannot release a held POSIX lock"
        (busy (L.acquire_registration ()));
      child_checks
        "registration excludes another process and ignores inherited releases"
        (fun () ->
          L.release_registration registration;
          busy (L.acquire_registration ()));
      let use = get (L.acquire_use registration ~project_name:"example") in
      check "same-process retirement cannot upgrade a use lease"
        (busy (L.acquire_retirement registration ~project_name:"example"));
      L.release_registration registration;
      L.release_registration registration;
      child_checks
        "shared lifetime admits another reader but excludes retirement"
        (fun () ->
          let r = get (L.acquire_registration ()) in
          let other = get (L.acquire_use r ~project_name:"example") in
          L.release_use other;
          let blocked = busy (L.acquire_retirement r ~project_name:"example") in
          L.release_registration r;
          blocked);
      L.release_use use;
      L.release_use use;
      let registration = get (L.acquire_registration ()) in
      let retirement =
        get (L.acquire_retirement registration ~project_name:"example")
      in
      let original = Project_store.project_dir "example" in
      Unix.mkdir original 0o700;
      write (Filename.concat original "old") "retired data";
      let outside = Filename.concat root "outside" in
      write outside "unrelated data";
      Unix.symlink outside (Filename.concat original "outside-link");
      get (L.retire registration retirement);
      check "retirement removes only the old pathname atomically"
        (not (Sys.file_exists original));
      L.release_retirement retirement;
      L.release_registration registration;
      (* Simulate death after rename: fresh registration and a new project at
         the same path must survive cleanup of the previous incarnation. *)
      let registration = get (L.acquire_registration ()) in
      let fresh = get (L.acquire_use registration ~project_name:"example") in
      Unix.mkdir original 0o700;
      write (Filename.concat original "new") "fresh data";
      get (L.cleanup_retired registration);
      get (L.cleanup_retired registration);
      check "restart cleanup cannot traverse a recreated project"
        (read (Filename.concat original "new") = "fresh data");
      check "retired payload symlinks cannot redirect deletion"
        (read outside = "unrelated data");
      let retired_root =
        Filename.concat (Project_store.lifecycle_dir ()) "retired"
      in
      check "recognized retirement journals are reclaimed"
        (Array.length (Sys.readdir retired_root) = 0);
      let interrupted_empty =
        Filename.temp_dir ~temp_dir:retired_root "retired-" ""
      in
      get (L.cleanup_retired registration);
      check "interruption after marker removal does not strand an empty journal"
        (not (Sys.file_exists interrupted_empty));
      let failed_name = "failed-retirement" in
      let failed =
        get (L.acquire_retirement registration ~project_name:failed_name)
      in
      check "missing source fails retirement after preparing its journal"
        (Result.is_error (L.retire registration failed));
      check "failed rename leaves a prepared retirement journal"
        (Array.length (Sys.readdir retired_root) = 1);
      L.release_retirement failed;
      let replacement =
        get (L.acquire_use registration ~project_name:failed_name)
      in
      let replacement_dir = Project_store.project_dir failed_name in
      Unix.mkdir replacement_dir 0o700;
      write
        (Filename.concat replacement_dir "new")
        "replacement after failed rename";
      get (L.cleanup_retired registration);
      check
        "prepared journal cannot select a newly created project for deletion"
        (read (Filename.concat replacement_dir "new")
        = "replacement after failed rename");
      check "prepared journal without a payload is eventually reclaimed"
        (Array.length (Sys.readdir retired_root) = 0);
      L.release_use replacement;
      let unknown = Filename.concat retired_root "unrecognized" in
      Unix.mkdir unknown 0o700;
      write (Filename.concat unknown "manifest") "not an onton manifest";
      write (Filename.concat unknown "keep") "unrelated";
      get (L.cleanup_retired registration);
      check "unrecognized retirement data is preserved"
        (Sys.file_exists (Filename.concat unknown "keep"));
      L.release_use fresh;
      L.release_registration registration;
      check "released registration cannot authorize new ownership"
        (Result.is_error (L.acquire_use registration ~project_name:"other"));
      (* A killed worker releases its lifetime lease without a stale-PID guess. *)
      let ready_r, ready_w = Unix.pipe ~cloexec:true () in
      let finish_r, finish_w = Unix.pipe ~cloexec:true () in
      let child =
        match Unix.fork () with
        | 0 ->
            Unix.close ready_r;
            Unix.close finish_w;
            let r = get (L.acquire_registration ()) in
            let _use = get (L.acquire_writer r ~project_name:"crash") in
            L.release_registration r;
            ignore (Unix.write ready_w (Bytes.of_string "R") 0 1);
            Unix.close ready_w;
            ignore (Unix.read finish_r (Bytes.create 1) 0 1);
            exit 0
        | pid -> pid
      in
      Unix.close ready_w;
      Unix.close finish_r;
      check "worker acquired lease before crash injection"
        (Unix.read ready_r (Bytes.create 1) 0 1 = 1);
      Unix.close ready_r;
      let r = get (L.acquire_registration ()) in
      check "live worker blocks retirement without an exclusive project lock"
        (busy (L.acquire_retirement r ~project_name:"crash"));
      check "live writer excludes another writer"
        (busy (L.acquire_writer r ~project_name:"crash"));
      check "live writer excludes shared lifetime readers"
        (busy (L.acquire_use r ~project_name:"crash"));
      Unix.kill child Sys.sigkill;
      ignore (wait child);
      Unix.close finish_w;
      let replacement = get (L.acquire_writer r ~project_name:"crash") in
      L.release_use replacement;
      let retired = get (L.acquire_retirement r ~project_name:"crash") in
      L.release_retirement retired;
      L.release_registration r;
      let retired_project = Project_store.project_dir "killed-retirement" in
      Unix.mkdir retired_project 0o700;
      write (Filename.concat retired_project "old") "captured";
      let ready_r, ready_w = Unix.pipe ~cloexec:true () in
      let finish_r, finish_w = Unix.pipe ~cloexec:true () in
      let child =
        match Unix.fork () with
        | 0 ->
            Unix.close ready_r;
            Unix.close finish_w;
            let r = get (L.acquire_registration ()) in
            let retirement =
              get (L.acquire_retirement r ~project_name:"killed-retirement")
            in
            get (L.retire r retirement);
            ignore (Unix.write ready_w (Bytes.of_string "R") 0 1);
            Unix.close ready_w;
            ignore (Unix.read finish_r (Bytes.create 1) 0 1);
            exit 0
        | pid -> pid
      in
      Unix.close ready_w;
      Unix.close finish_r;
      check "retirement rename completed before process termination"
        (Unix.read ready_r (Bytes.create 1) 0 1 = 1
        && not (Sys.file_exists retired_project));
      Unix.close ready_r;
      Unix.kill child Sys.sigkill;
      ignore (wait child);
      Unix.close finish_w;
      let r = get (L.acquire_registration ()) in
      Unix.mkdir retired_project 0o700;
      write (Filename.concat retired_project "fresh") "new incarnation";
      get (L.cleanup_retired r);
      check
        "cleanup after a killed retiring process preserves the next incarnation"
        (read (Filename.concat retired_project "fresh") = "new incarnation");
      L.release_registration r;
      (* Real CLI startup must not overwrite config or prepare a checkout before
         discovering an existing supervisor's exclusive project lock. *)
      let cli = Sys.argv.(1) in
      let name = "locked" in
      let project_dir = Project_store.project_dir name in
      Project_store.ensure_dir project_dir;
      let config_path = Project_store.config_path name in
      write config_path "sentinel: must not be read or overwritten";
      let plan = Filename.concat root "gameplan.yaml" in
      write plan
        "projectName: locked\n\
         owner: owner\n\
         repo: repo\n\
         problemStatement: test\n\
         solutionSummary: test\n\
         patches:\n\
        \  - number: 1\n\
        \    title: patch\n\
        \    description: patch\n\
        \    dependsOn: []\n\
         dependencyGraph:\n\
        \  - patch: 1\n\
        \    dependsOn: []\n";
      let start ?(no_lock = false) ?(env_no_lock = false) () =
        let devnull = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
        let env =
          Unix.environment () |> Array.to_list
          |> List.filter (fun entry ->
              not (String.starts_with ~prefix:"ONTON_NO_LOCK=" entry))
          |> fun entries ->
          Array.of_list
            (if env_no_lock then "ONTON_NO_LOCK=1" :: entries else entries)
        in
        let child =
          Unix.create_process_env cli
            (Array.of_list
               ([ cli; "--gameplan"; plan ]
               @ if no_lock then [ "--no-lock" ] else []))
            env devnull devnull devnull
        in
        Unix.close devnull;
        wait child
      in
      let registration = get (L.acquire_registration ()) in
      check "CLI startup is serialized with pruning registration"
        (start () = Unix.WEXITED 75);
      L.release_registration registration;
      let lock =
        match Project_lock.acquire ~project_dir ~on_stale:(fun _ -> ()) with
        | Ok lock -> lock
        | Error _ -> failwith "fixture lock failed"
      in
      Fun.protect
        ~finally:(fun () -> Project_lock.release lock)
        (fun () ->
          check "CLI rejects contention before reading invalid prior config"
            (start () = Unix.WEXITED 75);
          let registration = get (L.acquire_registration ()) in
          let use = get (L.acquire_writer registration ~project_name:name) in
          L.release_registration registration;
          Fun.protect
            ~finally:(fun () -> L.release_use use)
            (fun () ->
              check "--no-lock cannot bypass the project lifetime owner"
                (start ~no_lock:true () = Unix.WEXITED 75);
              check "ONTON_NO_LOCK cannot bypass the project lifetime owner"
                (start ~env_no_lock:true () = Unix.WEXITED 75));
          check "CLI leaves existing configuration intact"
            (read config_path = "sentinel: must not be read or overwritten");
          check
            "CLI does not provision while another supervisor owns the project"
            (not (Sys.file_exists (Project_store.managed_repo_dir name)))));
  print_endline "project lifecycle, retirement and startup exclusion: OK"
