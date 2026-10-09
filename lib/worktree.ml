(* @archlint.module shell
   @archlint.domain worktree-parser *)

open Base

type t = {
  patch_id : Types.Patch_id.t;
  branch : Types.Branch.t;
  path : string;
  newly_created : bool;
}
[@@deriving show, eq, sexp_of, compare]

(* Pure parsers and decision functions live in [Worktree_parser] (lib_core/).
   This file is the effectful handler — git subprocess driver, FS operations,
   per-worktree mutex pool — that calls into the pure side. *)

let normalize_path path =
  Worktree_parser.normalize_path ~cwd:(Stdlib.Sys.getcwd ()) path

let worktree_dir ~project_name ~patch_id =
  let home =
    match Stdlib.Sys.getenv_opt "HOME" with Some h -> h | None -> "."
  in
  let id_str = Types.Patch_id.to_string patch_id in
  Stdlib.Filename.concat
    (Stdlib.Filename.concat home ("worktrees/" ^ project_name))
    ("patch-" ^ id_str)

let has_cancellation = Process_tree.has_cancellation
let is_transient_spawn_failure = Process_tree.is_transient_spawn_failure
let retry_transient_spawn = Process_tree.retry_transient_spawn

(* Legacy read helpers retain their failure mapping, but delegate process
   ownership, cancellation and output capture to the shared runner. *)
let run_command process_mgr ~env ?stdout ?stderr args =
  let code, _out, err =
    Process_tree.run ~process_mgr ~env ?stdout ?stderr args
  in
  if code <> 0 then failwith (Printf.sprintf "Command exited %d: %s" code err)

let clean_git_env = Stdlib.Lazy.from_fun Git_env.clean_env

let ref_exists ~process_mgr ~repo_root ref_path =
  let buf = Buffer.create 16 in
  match
    run_command process_mgr
      ~env:(Stdlib.Lazy.force clean_git_env)
      ~stdout:buf ~stderr:(Buffer.create 16)
      [ "git"; "-C"; repo_root; "rev-parse"; "--verify"; ref_path ]
  with
  | () -> true
  | exception e when has_cancellation e -> raise e
  | exception _ -> false

let remote_branch_exists ~process_mgr ~repo_root branch_str =
  ref_exists ~process_mgr ~repo_root ("refs/remotes/origin/" ^ branch_str)

let resolve_main_root ~process_mgr ~repo_root =
  let buf = Buffer.create 128 in
  let stderr_buf = Buffer.create 64 in
  match
    run_command process_mgr
      ~env:(Stdlib.Lazy.force clean_git_env)
      ~stdout:buf ~stderr:stderr_buf
      [
        "git";
        "-C";
        repo_root;
        "rev-parse";
        "--path-format=absolute";
        "--git-common-dir";
      ]
  with
  | () ->
      let common_git_dir = String.strip (Buffer.contents buf) in
      (* The common git dir is the .git directory of the main working tree.
         Its parent is the main working tree root. *)
      Stdlib.Filename.dirname common_git_dir
  | exception e when has_cancellation e -> raise e
  | exception _ -> repo_root

let is_checked_out_in_repo_root ~process_mgr ~repo_root branch =
  let main_root = resolve_main_root ~process_mgr ~repo_root in
  let buf = Buffer.create 128 in
  let stderr_buf = Buffer.create 64 in
  match
    run_command process_mgr
      ~env:(Stdlib.Lazy.force clean_git_env)
      ~stdout:buf ~stderr:stderr_buf
      [ "git"; "-C"; main_root; "rev-parse"; "--abbrev-ref"; "HEAD" ]
  with
  | () ->
      let current = String.strip (Buffer.contents buf) in
      String.equal current (Types.Branch.to_string branch)
  | exception e when has_cancellation e -> raise e
  | exception _ -> false

(* Run a git command and capture (exit_code, stdout, stderr) without raising.
   Defined here, ahead of its first use in [read_repo_ref_sha] and
   [compute_ancestry], so the worktree-creation planner inputs can be gathered
   before the type re-exports and the heavier rebase/push functions below. *)
let run_git_exit_code ~process_mgr args =
  Process_tree.run ~process_mgr ~env:(Stdlib.Lazy.force clean_git_env) args

(* Absence is a successful ref observation. Process/object failures remain
   errors so provisioning cannot turn an unavailable ref into a new branch. *)
let read_repo_ref_sha ~process_mgr ~repo_root ~ref_name =
  let git args =
    run_git_exit_code ~process_mgr ([ "git"; "-C"; repo_root ] @ args)
  in
  let symbolic, _, stderr = git [ "symbolic-ref"; "-q"; ref_name ] in
  if symbolic = 0 then Error "symbolic branch ref cannot authorize provisioning"
  else if symbolic <> 1 then Error ("ref identity probe failed: " ^ stderr)
  else
    let code, _, stderr = git [ "show-ref"; "--verify"; "--quiet"; ref_name ] in
    match code with
    | 1 -> Ok None
    | 0 -> (
        let code, stdout, stderr = git [ "rev-parse"; "--verify"; ref_name ] in
        if code <> 0 then Error ("ref revision probe failed: " ^ stderr)
        else
          match Branch_reconcile.Commit.make (String.strip stdout) with
          | None -> Error "ref revision probe returned an invalid revision"
          | Some revision ->
              let sha = Branch_reconcile.Commit.to_string revision in
              let code, kind, stderr = git [ "cat-file"; "-t"; sha ] in
              if code = 0 && String.equal (String.strip kind) "commit" then
                Ok (Some sha)
              else Error ("ref is not a readable commit: " ^ stderr))
    | _ -> Error ("ref existence probe failed: " ^ stderr)

(* Compute the ancestor relationship between [local] and [remote] using two
   [git merge-base --is-ancestor] probes. Returns [Unknown] if either probe
   fails. Pure inputs are SHAs already known to exist at the time of call —
   caller is expected to gather them via [read_repo_ref_sha] first. *)
let compute_repo_ancestry ~process_mgr ~repo_root ~local ~remote :
    Start_point_plan.ancestry =
  let is_ancestor a b =
    let code, _, _ =
      run_git_exit_code ~process_mgr
        [ "git"; "-C"; repo_root; "merge-base"; "--is-ancestor"; a; b ]
    in
    match code with 0 -> Some true | 1 -> Some false | _ -> None
  in
  match (is_ancestor local remote, is_ancestor remote local) with
  | Some true, Some true -> Start_point_plan.Equal
  | Some true, Some false -> Start_point_plan.Remote_ahead
  | Some false, Some true -> Start_point_plan.Local_ahead
  | Some false, Some false -> Start_point_plan.Diverged
  | _ -> Start_point_plan.Unknown

(* Fetch a single branch from origin into the corresponding remote-tracking
   ref. Returns a typed [fetch_branch_result] so callers can distinguish
   the routine "brand-new branch — no upstream yet" case from genuine
   fetch failures (network, auth, ref-lock contention). The planner
   correctly handles [remote_ref = None] either way; the distinction is
   load-bearing only for log clarity.

   Operates on [repo_root]; worktrees share the ref store with the main repo,
   so this updates [refs/remotes/origin/<branch>] for all workers. The
   [fetch_lock] mutex must be the same one shared with [fetch_origin] above. *)
let fetch_origin_branch ~fetch_lock ~process_mgr ~repo_root ~branch_str =
  Eio.Mutex.use_ro fetch_lock (fun () ->
      try
        let code, _stdout, stderr =
          run_git_exit_code ~process_mgr
            [
              "git";
              "-C";
              repo_root;
              "fetch";
              "origin";
              "+refs/heads/" ^ branch_str ^ ":refs/remotes/origin/" ^ branch_str;
            ]
        in
        Worktree_parser.classify_fetch_branch_result ~code ~stderr
      with
      | exn when has_cancellation exn -> raise exn
      | exn ->
          Worktree_parser.Fetch_branch_error
            (Printf.sprintf "git fetch origin %s crashed: %s" branch_str
               (Exn.to_string exn)))

let branch_prefixes = Worktree_parser.branch_prefixes
let find_ci_ref_collision = Worktree_parser.find_ci_ref_collision

let check_case_insensitive_ref_collision ~process_mgr ~repo_root branch_str =
  let buf = Buffer.create 512 in
  let existing_branches =
    match
      run_command process_mgr
        ~env:(Stdlib.Lazy.force clean_git_env)
        ~stdout:buf ~stderr:(Buffer.create 16)
        [
          "git";
          "-C";
          repo_root;
          "for-each-ref";
          "--format=%(refname:short)";
          "refs/heads/";
        ]
    with
    | () -> String.split_lines (Buffer.contents buf)
    | exception e when has_cancellation e -> raise e
    | exception _ ->
        Eio.traceln
          "warning: git for-each-ref failed; case-insensitive ref collision \
           check skipped for %s"
          branch_str;
        []
  in
  match find_ci_ref_collision ~existing_branches branch_str with
  | Some colliding ->
      failwith
        (Printf.sprintf
           "Cannot create branch %s: existing branch %s conflicts on \
            case-insensitive filesystem (macOS). Delete or rename the \
            conflicting branch with: git branch -D %s"
           branch_str colliding colliding)
  | None -> ()

(* Creation wiring separates start-point decisions from backend lifecycle effects. *)
type create_io = {
  worktree_exists : path:string -> bool;
      (** True only for a validated, ready checkout of the requested branch. *)
  check_ref_collision : branch_str:string -> unit;
      (** Case-insensitive ref-collision guard; raises to abort creation. *)
  read_ref : ref_name:string -> (string option, string) Result.t;
      (** Resolve a commit ref; only verified absence returns [Ok None]. *)
  ancestry : local:string -> remote:string -> Start_point_plan.ancestry;
      (** Two-way ancestry between an existing local and remote SHA. *)
  execute_action :
    path:string ->
    branch_str:string ->
    expected_local:string option ->
    Start_point_plan.action ->
    bool;
      (** Realize the approved action; return true only for a new checkout. *)
}

(* Wiring of [create], parameterised over its effects. Given [io], it reads the
   local/remote refs, computes ancestry only when both sides exist, consults the
   pure {!Start_point_plan.plan}, and either executes the action or surfaces the
   refusal. [make] supplies validated backend effects; the split exists
   so the control flow (including the short-circuit and the refusal mapping) can
   be unit-tested without spawning git.

   [branch_checked_out_in_main_root] and [existing_worktree_path] are checked by
   [Worktree_setup.ensure_worktree] before this point — we pass [false]/[None]
   so the planner's totality contract is preserved without redoing the work. *)
let create_with_io ~io ~project_name ~patch_id ~branch ~base_ref :
    (t, Start_point_plan.refusal) Result.t =
  let path = worktree_dir ~project_name ~patch_id in
  let branch_str = Types.Branch.to_string branch in
  if io.worktree_exists ~path then
    Result.Ok { patch_id; branch; path; newly_created = false }
  else (
    io.check_ref_collision ~branch_str;
    let read_ref ~ref_name =
      match io.read_ref ~ref_name with
      | Ok revision -> revision
      | Error reason -> failwith ("Worktree ref observation failed: " ^ reason)
    in
    let local_ref = read_ref ~ref_name:("refs/heads/" ^ branch_str) in
    let remote_ref = read_ref ~ref_name:("refs/remotes/origin/" ^ branch_str) in
    let ancestry =
      match (local_ref, remote_ref) with
      | Some l, Some r -> io.ancestry ~local:l ~remote:r
      | _ -> Start_point_plan.Unknown
    in
    let decision =
      Start_point_plan.plan ~local_ref ~remote_ref ~ancestry
        ~base_branch:base_ref ~branch_checked_out_in_main_root:false
        ~existing_worktree_path:None
    in
    match decision with
    | Refuse refusal -> Result.Error refusal
    | Plan action ->
        let newly_created =
          io.execute_action ~path ~branch_str ~expected_local:local_ref action
        in
        Result.Ok { patch_id; branch; path; newly_created })

let detect_branch ~process_mgr ~path =
  let buf = Buffer.create 128 in
  let path = normalize_path path in
  let stderr_buf = Buffer.create 64 in
  (match
     run_command process_mgr
       ~env:(Stdlib.Lazy.force clean_git_env)
       ~stdout:buf ~stderr:stderr_buf
       [ "git"; "-C"; path; "rev-parse"; "--abbrev-ref"; "HEAD" ]
   with
  | () -> ()
  | exception e when has_cancellation e -> raise e
  | exception exn ->
      let msg = Buffer.contents stderr_buf in
      failwith
        (Printf.sprintf "detect_branch failed at %s: %s\ngit stderr: %s" path
           (Exn.to_string exn) msg));
  let raw = Buffer.contents buf in
  let branch_str = String.strip raw in
  if String.is_empty branch_str then
    failwith ("detect_branch: git rev-parse returned empty output at " ^ path);
  if String.equal branch_str "HEAD" then
    failwith ("Worktree at " ^ path ^ " has detached HEAD; cannot detect branch");
  Types.Branch.of_string branch_str

let parse_porcelain ~repo_root raw =
  Worktree_parser.parse_porcelain ~cwd:(Stdlib.Sys.getcwd ()) ~repo_root raw

let classify_fetch_result = Worktree_parser.classify_fetch_result

type fetch_branch_result = Worktree_parser.fetch_branch_result =
  | Fetch_branch_ok
  | Fetch_branch_no_remote_ref
  | Fetch_branch_error of string
[@@deriving show, eq, sexp_of, compare]

let classify_fetch_branch_result = Worktree_parser.classify_fetch_branch_result

let fetch_origin ~fetch_lock ~process_mgr ~path =
  (* Serialize concurrent fetches across worktrees of the same repo. All
     worktrees share the main repo's ref store, so simultaneous
     [git fetch origin] processes race on the compare-and-swap update of
     [refs/remotes/origin/*], producing
     "cannot lock ref ...: is at X but expected Y" in the loser. The mutex
     eliminates that race by construction. *)
  Eio.Mutex.use_ro fetch_lock (fun () ->
      try
        let code, _stdout, stderr =
          run_git_exit_code ~process_mgr
            [ "git"; "-C"; path; "fetch"; "origin" ]
        in
        classify_fetch_result ~code ~stderr
      with
      | exn when has_cancellation exn -> raise exn
      | exn ->
          Result.Error
            (Printf.sprintf "git fetch origin crashed: %s" (Exn.to_string exn)))

let git_status ~process_mgr ~path =
  let code, stdout, _ =
    run_git_exit_code ~process_mgr [ "git"; "-C"; path; "status" ]
  in
  if code <> 0 then "" else String.strip stdout

let has_uncommitted_changes ~process_mgr ~path =
  let code, stdout, stderr =
    run_git_exit_code ~process_mgr
      [
        "git";
        "-C";
        path;
        "status";
        "--porcelain=v1";
        "-z";
        "--untracked-files=all";
      ]
  in
  if code = 0 then
    let observation =
      Git_observation.of_porcelain ~branch:None ~head:""
        ~sequencer:Git_observation.None_active stdout
    in
    if observation.Git_observation.valid then
      Result.Ok (not (Git_observation.clean observation))
    else Result.Error ("Malformed git status in " ^ path)
  else
    Result.Error
      (Printf.sprintf "git status failed in %s (exit %d): %s" path code
         (String.strip stderr))

let conflict_diff ~process_mgr ~path =
  let code, stdout, _ =
    run_git_exit_code ~process_mgr
      [ "git"; "-C"; path; "diff"; "--diff-filter=U" ]
  in
  if code <> 0 then ""
  else
    let s = String.strip stdout in
    (* Truncate to avoid blowing up the prompt *)
    if String.length s > 4000 then String.prefix s 4000 ^ "\n[truncated]" else s

(** Read a ref from the checkout. Missing refs or failed observations return
    [None]; owner commands independently verify their mutation preconditions. *)
let read_branch_sha ~process_mgr ~path ~ref_name =
  let code, stdout, _ =
    run_git_exit_code ~process_mgr
      [ "git"; "-C"; path; "rev-parse"; "--verify"; ref_name ]
  in
  if code = 0 then
    let s = String.strip stdout in
    if String.is_empty s then None else Some s
  else None

let is_ancestor ~process_mgr ~path ~ancestor ~descendant =
  let code, _, _ =
    run_git_exit_code ~process_mgr
      [ "git"; "-C"; path; "merge-base"; "--is-ancestor"; ancestor; descendant ]
  in
  code = 0

type push_result = Worktree_parser.push_result =
  | Push_ok
  | Push_up_to_date
  | Push_rejected of Push_reject_classify.rejection
  | Push_error of string
[@@deriving show, eq, sexp_of, compare]

let parse_push_porcelain = Worktree_parser.parse_push_porcelain
let parse_commit_count = Worktree_parser.parse_commit_count

type push_gate = Worktree_parser.push_gate = Proceed | Skip_no_commits
[@@deriving show, eq, sexp_of]

let push_gate_from_count = Worktree_parser.push_gate_from_count
let classify_push_result = Worktree_parser.classify_push_result
let path t = t.path
let patch_id t = t.patch_id
let branch t = t.branch
let newly_created t = t.newly_created

let commit_gameplan ~clock ~process_mgr ~path ~publication ~message :
    (unit, string) Result.t =
  let relative_path = Gameplan_publication.path publication in
  let content = Gameplan_publication.content publication in
  let git args =
    let code, stdout, stderr =
      run_git_exit_code ~process_mgr ([ "git"; "-C"; path ] @ args)
    in
    if code <> 0 then
      failwith
        (Printf.sprintf "git %s exited %d: %s"
           (String.concat ~sep:" " args)
           code stderr);
    stdout
  in
  try
    Eio.Time.with_timeout_exn clock 120. (fun () ->
        (* This checkout is supervisor-owned. Refuse unrelated staged or dirty
           files rather than including or discarding them in a generated commit. *)
        List.iter
          [
            [ "diff"; "--name-only"; "-z" ];
            [ "diff"; "--cached"; "--name-only"; "-z" ];
            [ "ls-files"; "--others"; "--exclude-standard"; "-z" ];
          ]
          ~f:(fun args ->
            git args |> String.split ~on:'\000'
            |> List.iter ~f:(fun entry ->
                if
                  not (String.is_empty entry || String.equal entry relative_path)
                then
                  failwith
                    ("Unrelated worktree change prevents gameplan publication: "
                   ^ entry)));
        let components = String.split relative_path ~on:'/' in
        let rec prepare parent = function
          | [] -> failwith "Empty publication path"
          | [ filename ] -> Stdlib.Filename.concat parent filename
          | dir :: rest ->
              let next = Stdlib.Filename.concat parent dir in
              (match Unix.lstat next with
              | stat when Poly.equal stat.Unix.st_kind Unix.S_DIR -> ()
              | _ ->
                  failwith
                    ("Publication directory is not a real directory: " ^ next)
              | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
                  Unix.mkdir next 0o755);
              prepare next rest
        in
        let destination = prepare path components in
        (match Unix.lstat destination with
        | stat when Poly.equal stat.Unix.st_kind Unix.S_REG ->
            let existing =
              Stdlib.In_channel.with_open_bin destination
                Stdlib.In_channel.input_all
            in
            if not (String.equal existing content) then
              failwith
                ("Gameplan destination already contains different content: "
               ^ relative_path)
        | _ ->
            failwith
              ("Gameplan destination is not a regular file: " ^ relative_path)
        | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
            Stdlib.Out_channel.with_open_bin destination (fun out ->
                Stdlib.Out_channel.output_string out content));
        ignore (git [ "add"; "--"; relative_path ]);
        if
          not
            (String.is_empty
               (git [ "diff"; "--cached"; "--name-only"; "--"; relative_path ]))
        then (
          let before = String.strip (git [ "rev-parse"; "HEAD" ]) in
          ignore
            (git [ "commit"; "--only"; "-m"; message; "--"; relative_path ]);
          let committed_paths =
            git [ "diff"; "--name-only"; "-z"; before; "HEAD" ]
            |> String.split ~on:'\000'
            |> List.filter ~f:(fun path -> not (String.is_empty path))
          in
          if not (List.equal String.equal committed_paths [ relative_path ])
          then
            failwith
              "Commit hook changed files outside the gameplan; publication was \
               not pushed");
        let committed = git [ "show"; "HEAD:" ^ relative_path ] in
        if not (String.equal committed content) then
          failwith "Commit hook changed the supplied gameplan";
        Result.Ok ())
  with
  | exn when has_cancellation exn -> raise exn
  | exn -> Result.Error (Stdlib.Printexc.to_string exn)

module type S = sig
  val resolve_main_root : unit -> string
  val is_checked_out_in_repo_root : Types.Branch.t -> bool
  val remote_branch_exists : string -> bool

  val create :
    project_name:string ->
    patch_id:Types.Patch_id.t ->
    branch:Types.Branch.t ->
    base_ref:string ->
    (t, Start_point_plan.refusal) Result.t

  val fetch_origin_branch :
    fetch_lock:Eio.Mutex.t -> branch:string -> fetch_branch_result
  (** Fetch a single branch from origin into the corresponding remote-tracking
      ref. Returns [Fetch_branch_no_remote_ref] for the routine brand-new-branch
      case (no upstream yet — not a failure); [Fetch_branch_error msg] for real
      fetch failures. Caller in [Worktree_setup.ensure_worktree] runs this
      before [create] so the planner sees a fresh view of [origin/<branch>]. *)

  val remove : discard:bool -> t -> unit
  val detect_branch : path:string -> Types.Branch.t
  val list_with_branches : unit -> (string * Types.Branch.t) list
  val find_for_branch : Types.Branch.t -> string option
  val prune_stale_for_branch : Types.Branch.t -> unit

  val inspect_existing :
    path:string -> branch:Types.Branch.t -> (bool, string) Result.t

  val ensure_ready :
    path:string -> branch:Types.Branch.t -> (bool, string) Result.t

  val run_hook :
    clock:_ Eio.Time.clock ->
    script:string ->
    cwd:Eio.Fs.dir_ty Eio.Path.t ->
    env:(string * string) list ->
    unit ->
    (unit, string) Result.t

  val fetch_origin :
    fetch_lock:Eio.Mutex.t -> path:string -> (unit, string) Result.t

  val git_status : path:string -> string
  val has_uncommitted_changes : path:string -> (bool, string) Result.t
  val conflict_diff : path:string -> string

  val read_branch_sha : path:string -> ref_name:string -> string option
  (** Resolve [ref_name] to a SHA in the worktree at [path]. [None] on any error
      (missing ref, git failure). *)

  val is_ancestor : path:string -> ancestor:string -> descendant:string -> bool

  val commit_gameplan :
    path:string ->
    publication:Gameplan_publication.t ->
    message:string ->
    (unit, string) Result.t

  val materialization :
    path:string ->
    project_name:string ->
    branch:Types.Branch.t ->
    (Branch_reconcile.materialization option, string) Result.t

  val reconcile :
    path:string ->
    project_name:string ->
    branch:Types.Branch.t ->
    operation:Branch_reconcile.operation ->
    Branch_reconcile.command ->
    Branch_reconcile.result
end

type client = (module S)

let make ~fs ~config ~clock ~process_mgr ~repo_root =
  let module B =
    (val Worktree_backend.make ~fs ~clock ~process_mgr ~repo_root ~config
           ~timeout_seconds:120.)
  in
  let ready ~path ~branch =
    Result.map (B.inspect ~path ~branch) ~f:Option.is_some
  in
  (module struct
    let resolve_main_root () = resolve_main_root ~process_mgr ~repo_root

    let is_checked_out_in_repo_root branch =
      is_checked_out_in_repo_root ~process_mgr ~repo_root branch

    let remote_branch_exists branch_str =
      remote_branch_exists ~process_mgr ~repo_root branch_str

    let create ~project_name ~patch_id ~branch ~base_ref =
      let io =
        {
          worktree_exists =
            (fun ~path ->
              match ready ~path ~branch with
              | Ok b -> b
              | Error msg -> failwith msg);
          check_ref_collision =
            (fun ~branch_str ->
              check_case_insensitive_ref_collision ~process_mgr ~repo_root
                branch_str);
          read_ref =
            (fun ~ref_name ->
              read_repo_ref_sha ~process_mgr ~repo_root ~ref_name);
          ancestry =
            (fun ~local ~remote ->
              compute_repo_ancestry ~process_mgr ~repo_root ~local ~remote);
          execute_action =
            (fun ~path ~branch_str:_ ~expected_local action ->
              let action, expected_head =
                match action with
                | Start_point_plan.Create_new_branch_from_base { base_branch }
                  -> (
                    let io =
                      Branch_reconcile_executor.make_io ~process_mgr ~clock
                        ~path:repo_root
                    in
                    match
                      Branch_reconcile_executor.capture_commit ~io
                        ~ref_name:base_branch
                    with
                    | Error message -> failwith message
                    | Ok sha ->
                        let prefix =
                          Branch_reconcile.recovery_prefix ~project:project_name
                            ~branch:(Types.Branch.to_string branch)
                        in
                        let sha =
                          match
                            Branch_reconcile_executor.pin_materialization_intent
                              ~io ~prefix sha
                          with
                          | Ok pinned -> pinned
                          | Error message -> failwith message
                        in
                        ( Start_point_plan.Create_new_branch_from_base
                            {
                              base_branch =
                                Branch_reconcile.Commit.to_string sha;
                            },
                          Some sha ))
                | Start_point_plan.Reset_and_use_remote_tracking _
                | Start_point_plan.Use_local_branch_unchanged _ ->
                    (action, None)
              in
              let _, created =
                B.materialize ~path ~branch ~expected_local action
              in
              let io =
                Branch_reconcile_executor.make_io ~process_mgr ~clock ~path
              in
              let prefix =
                Branch_reconcile.recovery_prefix ~project:project_name
                  ~branch:(Types.Branch.to_string branch)
              in
              (match
                 Branch_reconcile_executor.record_materialization ~io ~prefix
                   ~branch:(Types.Branch.to_string branch)
                   ~new_branch_from:expected_head
               with
              | Ok _ -> ()
              | Error message -> failwith message);
              created);
        }
      in
      create_with_io ~io ~project_name ~patch_id ~branch ~base_ref

    let remove ~discard t =
      match B.inspect ~path:t.path ~branch:t.branch with
      | Ok (Some checkout) -> B.remove ~discard checkout
      | Ok None -> ()
      | Error msg -> failwith msg

    let detect_branch ~path = detect_branch ~process_mgr ~path
    let list_with_branches () = B.list ()

    let find_for_branch branch =
      List.find_map (B.list ()) ~f:(fun (path, b) ->
          if Types.Branch.equal branch b then Some path else None)

    let prune_stale_for_branch branch = B.prune_stale_for_branch branch

    let inspect_existing ~path ~branch =
      Result.map (B.inspect_existing ~path ~branch) ~f:Option.is_some

    let ensure_ready = ready

    let run_hook ~clock ~script ~cwd ~env () =
      User_config.run_hook ~process_mgr ~clock ~script ~cwd ~env ()

    let fetch_origin ~fetch_lock ~path =
      fetch_origin ~fetch_lock ~process_mgr ~path

    let fetch_origin_branch ~fetch_lock ~branch =
      fetch_origin_branch ~fetch_lock ~process_mgr ~repo_root ~branch_str:branch

    let git_status ~path = git_status ~process_mgr ~path

    let has_uncommitted_changes ~path =
      has_uncommitted_changes ~process_mgr ~path

    let conflict_diff ~path = conflict_diff ~process_mgr ~path

    let read_branch_sha ~path ~ref_name =
      read_branch_sha ~process_mgr ~path ~ref_name

    let is_ancestor ~path ~ancestor ~descendant =
      is_ancestor ~process_mgr ~path ~ancestor ~descendant

    let commit_gameplan ~path ~publication ~message =
      commit_gameplan ~clock ~process_mgr ~path ~publication ~message

    let materialization ~path ~project_name ~branch =
      let io = Branch_reconcile_executor.make_io ~process_mgr ~clock ~path in
      let prefix =
        Branch_reconcile.recovery_prefix ~project:project_name
          ~branch:(Types.Branch.to_string branch)
      in
      Branch_reconcile_executor.recover_materialization ~io ~prefix
        ~branch:(Types.Branch.to_string branch)

    let reconcile ~path ~project_name ~branch ~operation command =
      let io = Branch_reconcile_executor.make_io ~process_mgr ~clock ~path in
      let prefix =
        Branch_reconcile.recovery_prefix ~project:project_name
          ~branch:(Types.Branch.to_string branch)
      in
      Branch_reconcile_executor.execute ~io ~prefix
        ~branch:(Types.Branch.to_string branch)
        ~operation command
  end : S)
