(* @archlint.module state
   @archlint.domain project-lifecycle *)

open Base

type error = Project_retirement.error = Busy of string | Io_error of string

type lease = {
  fd : Unix.file_descr;
  path : string;
  pid : int;
  guard : string option;
  mutable released : bool;
}

type registration = { lease : lease; directory : string }
type use = lease

type retirement = {
  lease : lease;
  directory : string;
  manifest : Project_retirement.t;
}

(* POSIX record locks are process-scoped. Never open/close a second descriptor
   for a lock held by this process: closing it would release the first lock. *)
let held = Hashtbl.create (module String)
let mutex = Stdlib.Mutex.create ()

let synchronized f =
  Stdlib.Mutex.lock mutex;
  Stdlib.Fun.protect ~finally:(fun () -> Stdlib.Mutex.unlock mutex) f

let valid lease = (not lease.released) && lease.pid = Unix.getpid ()

let acquire ?guard path command =
  synchronized (fun () ->
      match Hashtbl.find held path with
      | Some lease when valid lease -> Error (Busy path)
      | Some _ | None -> (
          Hashtbl.remove held path;
          try
            let fd =
              Unix.openfile path
                [ Unix.O_CREAT; Unix.O_RDWR; Unix.O_CLOEXEC ]
                0o600
            in
            try
              Unix.lockf fd command 0;
              (* A dead runtime releases its primary lock before its commands
                 finish draining. Supervisors retain shared locks on this
                 separate inode until the entire command group is reaped. *)
              Option.iter guard ~f:(fun path ->
                  let drain =
                    Unix.openfile path
                      [ Unix.O_CREAT; Unix.O_RDWR; Unix.O_CLOEXEC ]
                      0o600
                  in
                  Stdlib.Fun.protect
                    ~finally:(fun () -> Unix.close drain)
                    (fun () -> Unix.lockf drain Unix.F_TLOCK 0));
              let lease =
                { fd; path; pid = Unix.getpid (); guard; released = false }
              in
              Hashtbl.add_exn held ~key:path ~data:lease;
              Ok lease
            with exn -> (
              Unix.close fd;
              match exn with
              | Unix.Unix_error (code, _, _)
                when Poly.equal code Unix.EAGAIN || Poly.equal code Unix.EACCES
                ->
                  Error (Busy path)
              | _ -> Error (Io_error (Exn.to_string exn)))
          with exn -> Error (Io_error (Exn.to_string exn))))

let release lease =
  synchronized (fun () ->
      if valid lease then (
        lease.released <- true;
        Hashtbl.remove held lease.path;
        (try Unix.lockf lease.fd Unix.F_ULOCK 0 with _ -> ());
        try Unix.close lease.fd with _ -> ()))

let command_guards () =
  synchronized (fun () ->
      Hashtbl.data held
      |> List.filter_map ~f:(fun lease ->
          if valid lease then lease.guard else None)
      |> List.dedup_and_sort ~compare:String.compare)

let acquire_registration () =
  try
    let directory = Project_store.lifecycle_dir () in
    Project_store.ensure_dir directory;
    let directory = Unix.realpath directory in
    Result.map
      (acquire (Stdlib.Filename.concat directory "registry.lock") Unix.F_TLOCK)
      ~f:(fun lease -> { lease; directory })
  with exn -> Error (Io_error (Exn.to_string exn))

let release_registration (registration : registration) =
  release registration.lease

let acquire_project (registration : registration) ~project_name command =
  if not (valid registration.lease) then
    Error (Io_error "project registration lease is no longer held")
  else
    match
      Project_retirement.make ~slug:(Project_store.slugify project_name)
    with
    | None ->
        Error (Io_error "project name has an empty or invalid storage slug")
    | Some manifest ->
        let path =
          Stdlib.Filename.concat registration.directory
            ("use-" ^ Project_retirement.slug manifest ^ ".lock")
        in
        Result.map
          (acquire ~guard:(path ^ ".drain") path command)
          ~f:(fun lease -> (lease, manifest))

let acquire_use registration ~project_name =
  Result.map (acquire_project registration ~project_name Unix.F_TRLOCK) ~f:fst

let acquire_writer registration ~project_name =
  Result.map (acquire_project registration ~project_name Unix.F_TLOCK) ~f:fst

let release_use = release

let acquire_retirement registration ~project_name =
  Result.map (acquire_project registration ~project_name Unix.F_TLOCK)
    ~f:(fun (lease, manifest) ->
      { lease; directory = registration.directory; manifest })

let release_retirement (retirement : retirement) = release retirement.lease
let retirement_root directory = Stdlib.Filename.concat directory "retired"
let marker_name = "manifest"
let payload_name = "project"

let retire (registration : registration) (retirement : retirement) =
  if
    (not (valid registration.lease && valid retirement.lease))
    || not (String.equal registration.directory retirement.directory)
  then Error (Io_error "project retirement requires matching live leases")
  else
    try
      let root = retirement_root registration.directory in
      Project_store.ensure_dir root;
      let journal = Stdlib.Filename.temp_dir ~temp_dir:root "retired-" "" in
      let marker = Stdlib.Filename.concat journal marker_name in
      Stdlib.Out_channel.with_open_bin marker (fun channel ->
          Stdlib.output_string channel
            (Project_retirement.encode retirement.manifest);
          Stdlib.flush channel;
          Unix.fsync (Unix.descr_of_out_channel channel));
      let project_dir =
        Stdlib.Filename.concat
          (Stdlib.Filename.dirname registration.directory)
          (Project_retirement.slug retirement.manifest)
      in
      Unix.rename project_dir (Stdlib.Filename.concat journal payload_name);
      Ok ()
    with exn -> Error (Io_error (Exn.to_string exn))

let rec remove path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | { Unix.st_kind = Unix.S_DIR; _ } ->
      Stdlib.Sys.readdir path
      |> Array.iter ~f:(fun name -> remove (Stdlib.Filename.concat path name));
      Unix.rmdir path
  | {
   Unix.st_kind =
     ( Unix.S_REG | Unix.S_LNK | Unix.S_CHR | Unix.S_BLK | Unix.S_FIFO
     | Unix.S_SOCK );
   _;
  } ->
      Unix.unlink path

let cleanup_retired (registration : registration) =
  if not (valid registration.lease) then
    Error (Io_error "project registration lease is no longer held")
  else
    try
      let root = retirement_root registration.directory in
      Project_store.ensure_dir root;
      Stdlib.Sys.readdir root
      |> Array.iter ~f:(fun name ->
          let journal = Stdlib.Filename.concat root name in
          match Unix.lstat journal with
          | { Unix.st_kind = Unix.S_DIR; _ } -> (
              let marker = Stdlib.Filename.concat journal marker_name in
              let manifest =
                match Unix.lstat marker with
                | { Unix.st_kind = Unix.S_REG; _ } ->
                    Stdlib.In_channel.with_open_bin marker
                      Stdlib.In_channel.input_all
                    |> Project_retirement.decode
                | {
                 Unix.st_kind =
                   ( Unix.S_DIR | Unix.S_LNK | Unix.S_CHR | Unix.S_BLK
                   | Unix.S_FIFO | Unix.S_SOCK );
                 _;
                } ->
                    None
                | exception Unix.Unix_error (Unix.ENOENT, _, _) -> None
              in
              match manifest with
              | None ->
                  if
                    String.is_prefix name ~prefix:"retired-"
                    && Array.is_empty (Stdlib.Sys.readdir journal)
                  then Unix.rmdir journal
              | Some _ ->
                  (* Keep the marker until payload cleanup finishes, so any
                    interrupted deletion remains recognizable on restart. *)
                  remove (Stdlib.Filename.concat journal payload_name);
                  Unix.unlink marker;
                  Unix.rmdir journal)
          | {
           Unix.st_kind =
             ( Unix.S_REG | Unix.S_LNK | Unix.S_CHR | Unix.S_BLK | Unix.S_FIFO
             | Unix.S_SOCK );
           _;
          } ->
              ());
      Ok ()
    with exn -> Error (Io_error (Exn.to_string exn))

let error_message = Project_retirement.error_message
