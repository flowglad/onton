(* @archlint.module state
   @archlint.domain worktree-lifecycle *)

open Base
open Worktree_lifecycle

type checkout = {
  path : string;
  owner : config;
  repo_id : string;
  branch : Types.Branch.t;
}

let path (t : checkout) = t.path
let owner t = t.owner

module type S = sig
  val inspect :
    path:string -> branch:Types.Branch.t -> (checkout option, string) Result.t

  val materialize :
    path:string ->
    branch:Types.Branch.t ->
    expected_local:string option ->
    Start_point_plan.action ->
    checkout * bool

  val list : unit -> (string * Types.Branch.t) list
  val remove : discard:bool -> checkout -> unit
  val prune_stale_for_branch : Types.Branch.t -> unit
  val reconcile : unit -> unit
end

let rec cancelled = function
  | Eio.Cancel.Cancelled _ -> true
  | Eio.Exn.Multiple es -> List.exists es ~f:(fun (e, _) -> cancelled e)
  | _ -> false

let protect_result f =
  try Ok (f ()) with
  | e when cancelled e -> raise e
  | e -> Error (Exn.to_string e)

let get = function Ok x -> x | Error msg -> failwith msg

let read_file path =
  let ic = Stdlib.open_in_bin path in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_in ic)
    (fun () -> Stdlib.In_channel.input_all ic)

let canonical path =
  (* The final component may not exist yet, including an unmounted overlay.
     Resolve the existing prefix so /var and /private/var identify one checkout. *)
  let rec resolve p =
    try Unix.realpath p
    with Unix.Unix_error (Unix.ENOENT, _, _) ->
      let parent = Stdlib.Filename.dirname p in
      if String.equal p parent then p
      else Stdlib.Filename.concat (resolve parent) (Stdlib.Filename.basename p)
  in
  resolve
    (if Stdlib.Filename.is_relative path then
       Stdlib.Filename.concat (Stdlib.Sys.getcwd ()) path
     else path)

let make ~fs ~clock ~process_mgr ~repo_root ~(config : config) ~timeout_seconds
    =
  if not (Float.is_finite timeout_seconds && Float.(timeout_seconds > 0.)) then
    invalid_arg "worktree timeout must be finite and positive";
  let run ?(cwd = repo_root) args =
    let stdout = Buffer.create 256 and stderr = Buffer.create 256 in
    let result =
      Eio.Time.with_timeout clock timeout_seconds (fun () ->
          Ok
            (Eio.Switch.run (fun sw ->
                 let cwd = Eio.Path.(fs / cwd) in
                 let child =
                   Eio.Process.spawn ~sw process_mgr ~cwd
                     ~env:(Git_env.clean_env ())
                     ~stdin:(Eio.Flow.string_source "")
                     ~stdout:(Eio.Flow.buffer_sink stdout)
                     ~stderr:(Eio.Flow.buffer_sink stderr)
                     args
                 in
                 match Eio.Process.await child with
                 | `Exited c -> c
                 | `Signaled s -> 128 + s)))
    in
    match result with
    | Error `Timeout ->
        failwith ("Worktree command timed out: " ^ String.concat ~sep:" " args)
    | Ok code ->
        ( code,
          String.strip (Buffer.contents stdout),
          String.strip (Buffer.contents stderr) )
  in
  let checked ?cwd args =
    match run ?cwd args with
    | 0, out, _ -> out
    | code, out, err ->
        failwith
          (Printf.sprintf "Worktree command failed (%d): %s\n%s\n%s" code
             (String.concat ~sep:" " args)
             err out)
  in
  let git args = checked ("git" :: "-C" :: repo_root :: args) in
  let common =
    canonical
      (git [ "rev-parse"; "--path-format=absolute"; "--git-common-dir" ])
  in
  let registry = Stdlib.Filename.concat common "onton-worktrees" in
  let checked_executables = Hashtbl.create (module String) in
  let resolve_executable name =
    let candidates =
      if String.contains name '/' then [ name ]
      else
        String.split (Option.value (Sys.getenv "PATH") ~default:"") ~on:':'
        |> List.map ~f:(fun dir -> Stdlib.Filename.concat dir name)
    in
    List.find_map candidates ~f:(fun path ->
        try
          Unix.access path [ Unix.X_OK ];
          if Stdlib.Sys.is_directory path then None else Some (canonical path)
        with Unix.Unix_error _ -> None)
  in
  let probe name =
    match Hashtbl.find checked_executables name with
    | Some executable -> Ok executable
    | None ->
        protect_result (fun () ->
            let executable =
              match resolve_executable name with
              | Some path -> path
              | None -> failwith ("Executable unavailable: " ^ name)
            in
            get (doctor_identity (checked [ executable; "--json"; "doctor" ]));
            Hashtbl.add_exn checked_executables ~key:name ~data:executable;
            executable)
  in
  let discover () =
    match probe "simgit" with
    | Ok executable -> Ok executable
    | Error first -> (
        match probe "sg" with
        | Ok executable -> Ok executable
        | Error second -> Error (first ^ "\n" ^ second))
  in
  let resolve_config c =
    match c.backend with
    | Git -> c
    | Simgit ->
        let executable =
          get
            (match c.executable with
            | Some name -> probe name
            | None -> discover ())
        in
        get (configure ~backend:"simgit" ~executable:(Some executable))
  in
  let config = resolve_config config in
  let preflight c = ignore (resolve_config c : config) in
  let executable c =
    match (resolve_config c).executable with
    | Some name -> name
    | None -> failwith "Resolved backend has no executable"
  in
  let sg c args = checked (executable c :: args) in
  let metadata_path path =
    Stdlib.Filename.concat registry
      (Stdlib.Digest.to_hex (Stdlib.Digest.string (canonical path)) ^ ".json")
  in
  let write_owner ~path ~branch ~phase c =
    (if not (Stdlib.Sys.file_exists registry) then
       try Unix.mkdir registry 0o700
       with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    let json =
      ownership_json ~path:(canonical path)
        ~branch:(Types.Branch.to_string branch)
        ~phase c
    in
    let target = metadata_path path in
    let tmp, oc =
      Stdlib.Filename.open_temp_file ~temp_dir:registry "owner-" ".tmp"
    in
    Stdlib.Fun.protect
      ~finally:(fun () ->
        Stdlib.close_out_noerr oc;
        if Stdlib.Sys.file_exists tmp then Unix.unlink tmp)
      (fun () ->
        Stdlib.output_string oc (Yojson.Safe.to_string json);
        Stdlib.close_out oc;
        Unix.rename tmp target)
  in
  let read_owner ~path ~branch =
    let file = metadata_path path in
    if not (Stdlib.Sys.file_exists file) then None
    else
      let json = Yojson.Safe.from_string (read_file file) in
      Some
        (get
           (parse_ownership ~path:(canonical path)
              ~branch:(Types.Branch.to_string branch)
              json))
  in
  let read_owner_for_path ~path =
    let file = metadata_path path in
    if not (Stdlib.Sys.file_exists file) then None
    else
      let json = Yojson.Safe.from_string (read_file file) in
      Some (get (parse_ownership_for_path ~path:(canonical path) json))
  in
  let git_list () =
    let out = git [ "worktree"; "list"; "--porcelain"; "-z" ] in
    get (parse_git_list out)
  in
  let git_registrations_for_branch branch =
    let exact_ref = "refs/heads/" ^ branch in
    git
      [ "for-each-ref"; "--format=%(refname)%00%(worktreepath)%00"; exact_ref ]
    |> String.split_lines
    |> List.filter_map ~f:(fun line ->
        match String.lsplit2 line ~on:'\000' with
        | Some (refname, encoded_path) when String.equal refname exact_ref -> (
            match String.chop_suffix encoded_path ~suffix:"\000" with
            | Some path when not (String.is_empty path) ->
                Some { path; branch = Some branch; mode = None }
            | Some _ | None -> None)
        | Some _ | None -> None)
  in
  let admin_for path =
    let dirs = Stdlib.Filename.concat common "worktrees" in
    if not (Stdlib.Sys.file_exists dirs) then None
    else
      Stdlib.Sys.readdir dirs |> Array.to_list
      |> List.find_map ~f:(fun name ->
          let admin = Stdlib.Filename.concat dirs name in
          let gitdir = Stdlib.Filename.concat admin "gitdir" in
          if
            Stdlib.Sys.file_exists gitdir
            && String.equal
                 (canonical (String.strip (read_file gitdir)))
                 (Stdlib.Filename.concat (canonical path) ".git")
          then Some admin
          else None)
  in
  let simgit_config () =
    if equal_backend config.backend Simgit then Some config
    else
      match discover () with
      | Error _ -> None
      | Ok executable ->
          Some (get (configure ~backend:"simgit" ~executable:(Some executable)))
  in
  let legacy_owner path =
    match simgit_config () with
    | None ->
        (* Git's registration alone cannot identify an unrecorded simgit owner.
           Internal simgit marker files are not a supported ownership API. *)
        if Option.is_some (admin_for path) then
          failwith
            ("Cannot determine legacy checkout ownership without simgit; \
              restore simgit before adopting: " ^ path)
        else Worktree_lifecycle.git
    | Some c -> (
        let entries = get (parse_simgit_list (sg c [ "list"; "--json" ])) in
        match
          List.find entries ~f:(fun (e : registration) ->
              String.equal e.path (canonical path))
        with
        | Some e when equal_backend (registration_backend e) Simgit -> c
        | Some _ | None -> Worktree_lifecycle.git)
  in
  let validate ~path ~branch =
    let at args = checked ("git" :: "-C" :: path :: args) in
    if
      not
        (String.equal
           (canonical (at [ "rev-parse"; "--show-toplevel" ]))
           (canonical path))
    then failwith ("Not a checkout root: " ^ path);
    if
      not
        (String.equal
           (canonical
              (at [ "rev-parse"; "--path-format=absolute"; "--git-common-dir" ]))
           common)
    then failwith ("Checkout belongs to another repository: " ^ path);
    let head =
      match
        run ("git" :: "-C" :: path :: [ "symbolic-ref"; "--quiet"; "HEAD" ])
      with
      | 0, head, _ -> head
      | _ -> (
          let admin = at [ "rev-parse"; "--absolute-git-dir" ] in
          match
            List.find_map [ "rebase-merge/head-name"; "rebase-apply/head-name" ]
              ~f:(fun name ->
                let p = Stdlib.Filename.concat admin name in
                if Stdlib.Sys.file_exists p then
                  Some (String.strip (read_file p))
                else None)
          with
          | Some name -> name
          | None -> failwith ("Checkout has detached HEAD: " ^ path))
    in
    if not (String.equal head ("refs/heads/" ^ Types.Branch.to_string branch))
    then failwith ("Checkout branch differs from requested branch: " ^ path)
  in
  let owning ~path ~branch =
    match read_owner_for_path ~path with
    | None -> (legacy_owner path, Ready)
    | Some (recorded, c, phase)
      when String.equal recorded (Types.Branch.to_string branch) ->
        (c, phase)
    | Some (_, c, phase)
      when equal_phase phase Ready && equal_backend c.backend Git ->
        let registered =
          List.exists (git_list ()) ~f:(fun (e : registration) ->
              String.equal (canonical e.path) (canonical path)
              && Option.equal String.equal e.branch
                   (Some (Types.Branch.to_string branch)))
        in
        if (not registered) || Option.is_none (admin_for path) then
          failwith "Checkout branch changed without a linked Git registration";
        validate ~path ~branch;
        write_owner ~path ~branch ~phase:Ready c;
        (c, Ready)
    | Some _ ->
        failwith
          "Checkout ownership conflicts with the requested path or branch, or \
           lacks publication state"
  in
  let cleanup ~path c =
    ignore (sg c [ "unlock"; path; "--json" ] : string);
    ignore (sg c [ "remove"; path; "--json" ] : string);
    Unix.unlink (metadata_path path)
  in
  let inspect_impl ~creation_finished ~path ~branch =
    protect_result (fun () ->
        let c, phase = owning ~path ~branch in
        if equal_phase phase Cleanup_pending then (
          cleanup ~path c;
          None)
        else (
          if equal_phase phase Preparing && not creation_finished then
            failwith
              ("Checkout creation did not finish; preserve its files and \
                verify the creator has stopped before recovery: " ^ path);
          let registered =
            List.exists (git_list ()) ~f:(fun (e : registration) ->
                String.equal (canonical e.path) (canonical path))
          in
          if equal_backend c.backend Simgit && registered then (
            preflight c;
            let code, output, _err = run [ executable c; "repair"; "--json" ] in
            get (repair_error ~code ~path:(canonical path) output));
          if not (Stdlib.Sys.file_exists path) then (
            if equal_backend c.backend Simgit && registered then
              failwith ("Simgit checkout remains unavailable: " ^ path);
            None)
          else (
            if not registered then
              failwith
                ("Path exists but is not a registered linked checkout: " ^ path);
            if Option.is_none (admin_for path) then
              failwith "Cannot adopt the main checkout";
            validate ~path ~branch;
            if Option.is_none (read_owner ~path ~branch) then
              write_owner ~path ~branch ~phase:Ready c;
            Some { path; owner = c; repo_id = common; branch })))
  in
  let inspect = inspect_impl ~creation_finished:false in
  let ref_sha branch =
    match
      run
        [
          "git";
          "-C";
          repo_root;
          "rev-parse";
          "--verify";
          "--quiet";
          "refs/heads/" ^ branch;
        ]
    with
    | 0, sha, _ -> Some sha
    | 1, _, _ -> None
    | _, _, err -> failwith err
  in
  let materialize ~path ~branch ~expected_local action =
    match get (inspect ~path ~branch) with
    | Some checkout -> (checkout, false)
    | None ->
        let c =
          Option.value
            (Option.map (read_owner ~path ~branch) ~f:fst)
            ~default:config
        in
        preflight c;
        let branch_str = Types.Branch.to_string branch in
        if
          List.exists (git_list ()) ~f:(fun (e : registration) ->
              Option.equal String.equal e.branch (Some branch_str))
        then
          failwith "Branch is already checked out; rediscover before retrying";
        if not (Option.equal String.equal (ref_sha branch_str) expected_local)
        then
          failwith
            "Branch changed during worktree preparation; rediscover before \
             retrying";
        write_owner ~path ~branch ~phase:Preparing c;
        let target =
          match action with
          | Start_point_plan.Use_local_branch_unchanged { local_sha } ->
              local_sha
          | Reset_and_use_remote_tracking { remote_sha } -> remote_sha
          | Create_new_branch_from_base { base_branch } ->
              git [ "rev-parse"; "--verify"; base_branch ^ "^{commit}" ]
        in
        let attach () =
          match c.backend with
          | Git -> ignore (git [ "worktree"; "add"; path; branch_str ] : string)
          | Simgit ->
              ignore
                (sg c
                   [ "run"; branch_str; "--path"; path; "--"; "/usr/bin/true" ]
                  : string)
        in
        let published = ref false in
        let reset_applied = ref false in
        Stdlib.Fun.protect
          ~finally:(fun () ->
            if !reset_applied && not !published then
              Eio.Cancel.protect (fun () ->
                  match ref_sha branch_str with
                  | current
                    when Option.equal String.equal current expected_local ->
                      ()
                  | Some current when String.equal current target ->
                      let args =
                        match expected_local with
                        | Some old ->
                            [
                              "update-ref";
                              "refs/heads/" ^ branch_str;
                              old;
                              target;
                            ]
                        | None ->
                            [
                              "update-ref";
                              "-d";
                              "refs/heads/" ^ branch_str;
                              target;
                            ]
                      in
                      ignore (git args : string)
                  | _ ->
                      failwith
                        "Branch changed during failed creation; refusing to \
                         overwrite concurrent history");
            if (not !published) && equal_backend c.backend Simgit then
              Eio.Cancel.protect (fun () ->
                  match
                    protect_result (fun () ->
                        (* This child has been reaped and ref rollback completed.
                           Persist that fact so cleanup can resume after a restart. *)
                        write_owner ~path ~branch ~phase:Cleanup_pending c;
                        cleanup ~path c)
                  with
                  | Ok () -> ()
                  | Error message ->
                      Eio.traceln
                        "onton: unfinished checkout preserved at %s: %s" path
                        message))
          (fun () ->
            (match (c.backend, action) with
            | Simgit, Create_new_branch_from_base _ ->
                ignore
                  (sg c
                     [
                       "add";
                       branch_str;
                       "--path";
                       path;
                       "--base";
                       target;
                       "--json";
                     ]
                    : string)
            | ( Git,
                ( Use_local_branch_unchanged _ | Reset_and_use_remote_tracking _
                | Create_new_branch_from_base _ ) )
            | ( Simgit,
                (Use_local_branch_unchanged _ | Reset_and_use_remote_tracking _)
              ) ->
                (match action with
                | Use_local_branch_unchanged _ -> ()
                | Reset_and_use_remote_tracking _
                | Create_new_branch_from_base _ ->
                    let old =
                      Option.value expected_local
                        ~default:(String.make (String.length target) '0')
                    in
                    (* Only a successful CAS authorizes rollback. Keep its
                       acknowledgement and recording together under external
                       cancellation; a failed CAS may observe another writer's
                       update to the very same target. *)
                    Eio.Cancel.protect (fun () ->
                        ignore
                          (git
                             [
                               "update-ref";
                               "refs/heads/" ^ branch_str;
                               target;
                               old;
                             ]
                            : string);
                        match action with
                        | Reset_and_use_remote_tracking _ ->
                            reset_applied := true
                        | Use_local_branch_unchanged _
                        | Create_new_branch_from_base _ ->
                            ()));
                attach ());
            let checkout =
              match
                get (inspect_impl ~creation_finished:true ~path ~branch)
              with
              | Some c -> c
              | None -> failwith "Worktree creation returned without a checkout"
            in
            let head = checked [ "git"; "-C"; path; "rev-parse"; "HEAD" ] in
            if not (String.equal head target) then
              failwith
                "Worktree HEAD changed during creation; preserving it for \
                 intervention";
            (if equal_backend c.backend Simgit then
               let entries =
                 get (parse_simgit_list (sg c [ "list"; "--json" ]))
               in
               List.iter entries ~f:(fun (e : registration) ->
                   if String.equal (canonical e.path) (canonical path) then
                     Eio.traceln "onton: simgit checkout %s (%s)" path
                       (Option.value e.mode ~default:"unknown mode")));
            write_owner ~path ~branch ~phase:Ready c;
            published := true;
            (checkout, true))
  in
  let list () =
    git_list ()
    |> List.filter ~f:(fun (e : registration) ->
        Option.is_some (admin_for e.path))
    |> List.filter_map ~f:(fun (e : registration) ->
        let branch =
          match e.branch with
          | Some _ as b -> b
          | None -> (
              match admin_for e.path with
              | None -> None
              | Some admin ->
                  List.find_map
                    [ "rebase-merge/head-name"; "rebase-apply/head-name" ]
                    ~f:(fun name ->
                      let file = Stdlib.Filename.concat admin name in
                      if Stdlib.Sys.file_exists file then
                        String.chop_prefix
                          (String.strip (read_file file))
                          ~prefix:"refs/heads/"
                      else None))
        in
        Option.map branch ~f:(fun b -> (e.path, Types.Branch.of_string b)))
  in
  let remove ~discard (checkout : checkout) =
    let { path; branch; owner = c; repo_id } = checkout in
    if not (String.equal repo_id common) then
      failwith "Cannot remove a checkout through another repository's backend";
    (match get (inspect ~path ~branch) with
    | Some current when equal_config current.owner c -> ()
    | Some _ -> failwith "Checkout ownership changed before removal"
    | None ->
        failwith
          "Checkout disappeared before removal; reconcile before retrying");
    let flags = if discard then [ "--force" ] else [] in
    (match c.backend with
    | Git -> ignore (git ([ "worktree"; "remove" ] @ flags @ [ path ]) : string)
    | Simgit ->
        let flags = if discard then [ "--discard-dirty" ] else [] in
        ignore (sg c ([ "remove" ] @ flags @ [ path ]) : string));
    let metadata = metadata_path path in
    if Stdlib.Sys.file_exists metadata then Unix.unlink metadata
  in
  let registration_owner (e : registration) =
    match e.branch with
    | Some branch ->
        fst (owning ~path:e.path ~branch:(Types.Branch.of_string branch))
    | None -> legacy_owner e.path
  in
  let prune_stale_for_branch requested =
    let requested = Types.Branch.to_string requested in
    List.iter (git_registrations_for_branch requested)
      ~f:(fun (e : registration) ->
        if not (Stdlib.Sys.file_exists e.path) then
          let owner =
            match
              protect_result (fun () -> read_owner_for_path ~path:e.path)
            with
            | Ok (Some (_, owner, _)) -> owner
            | Ok None | Error _ -> Worktree_lifecycle.git
          in
          match owner.backend with
          | Git ->
              ignore (git [ "worktree"; "remove"; "--force"; e.path ] : string);
              let metadata = metadata_path e.path in
              if Stdlib.Sys.file_exists metadata then Unix.unlink metadata
          | Simgit -> cleanup ~path:e.path owner)
  in
  let reconcile () =
    let owners =
      config :: List.map (git_list ()) ~f:registration_owner
      |> List.dedup_and_sort ~compare:compare_config
    in
    List.iter owners ~f:(fun c ->
        match c.backend with
        | Git -> ()
        | Simgit ->
            preflight c;
            ignore (sg c [ "prune" ] : string));
    (* Native prune must service Git registrations without destroying the admin
       entries of unmounted overlays. Preserve existing locks, and release only
       the temporary locks we acquired, including on failure/cancellation. *)
    let locked = ref [] in
    Stdlib.Fun.protect
      ~finally:(fun () ->
        Eio.Cancel.protect (fun () ->
            List.iter !locked ~f:(fun path ->
                ignore (git [ "worktree"; "unlock"; path ] : string))))
      (fun () ->
        List.iter (git_list ()) ~f:(fun e ->
            if equal_backend (registration_owner e).backend Simgit then
              match admin_for e.path with
              | Some admin
                when not
                       (Stdlib.Sys.file_exists
                          (Stdlib.Filename.concat admin "locked")) ->
                  Eio.Cancel.protect (fun () ->
                      ignore (git [ "worktree"; "lock"; e.path ] : string);
                      locked := e.path :: !locked)
              | Some _ | None -> ());
        ignore (git [ "worktree"; "prune" ] : string))
  in

  let mutex = Eio.Mutex.create () in
  (module struct
    let inspect ~path ~branch =
      Eio.Mutex.use_ro mutex (fun () -> inspect ~path ~branch)

    let materialize ~path ~branch ~expected_local action =
      Eio.Mutex.use_ro mutex (fun () ->
          materialize ~path ~branch ~expected_local action)

    let list () = Eio.Mutex.use_ro mutex list

    let remove ~discard checkout =
      Eio.Mutex.use_ro mutex (fun () -> remove ~discard checkout)

    let prune_stale_for_branch branch =
      Eio.Mutex.use_ro mutex (fun () -> prune_stale_for_branch branch)

    let reconcile () = Eio.Mutex.use_ro mutex reconcile
  end : S)
