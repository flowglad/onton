(* @archlint.module stateTest
   @archlint.domain worktree-lifecycle *)

open Onton
open Onton_core
module G = Onton_test_support.Git_env
module L = Worktree_lifecycle

let check label condition = if not condition then failwith label
let get = function Ok x -> x | Error msg -> failwith msg
let branch = Types.Branch.of_string

let write path text =
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc text)

let rejects f =
  try
    f ();
    false
  with Failure _ -> true

let remove (module B : Worktree_backend.S) ~discard ~path ~branch =
  match get (B.inspect ~path ~branch) with
  | Some checkout -> B.remove ~discard checkout
  | None -> ()

let fixture env config =
  let config =
    match (config.L.backend, config.L.executable) with
    | L.Simgit, Some path ->
        get
          (L.configure ~backend:"simgit"
             ~executable:(Some (Unix.realpath path)))
    | L.Git, _ | L.Simgit, None -> config
  in
  G.with_temp_repo (fun repo ->
      write (Filename.concat repo "file") "base\n";
      G.run_git ~cwd:repo [ "add"; "file" ];
      G.run_git ~cwd:repo [ "commit"; "-qm"; "base" ];
      let base = G.git_capture ~cwd:repo [ "rev-parse"; "HEAD" ] in
      let make config =
        Worktree_backend.make ~fs:(Eio.Stdenv.fs env)
          ~clock:(Eio.Stdenv.clock env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~repo_root:repo ~config ~timeout_seconds:30.
      in
      let module B = (val make config) in
      let p name = Filename.concat repo ("checkout-" ^ name) in
      let add name expected action =
        B.materialize ~path:(p name) ~branch:(branch name)
          ~expected_local:expected action
      in
      let new_action =
        Start_point_plan.Create_new_branch_from_base { base_branch = base }
      in
      Fun.protect
        ~finally:(fun () ->
          (* Use the owning tool for teardown, including when an assertion fails. *)
          List.iter
            (fun (path, b) ->
              try remove (module B) ~discard:true ~path ~branch:b with _ -> ())
            (B.list ()))
        (fun () ->
          let checkout, created = add "new" None new_action in
          check "new checkout" created;
          check "owner"
            (L.equal_config (Worktree_backend.owner checkout) config);
          check "new HEAD"
            (G.git_capture ~cwd:(p "new") [ "rev-parse"; "HEAD" ] = base);
          let (_, first), (_, second) =
            Eio.Fiber.pair
              (fun () -> add "concurrent" None new_action)
              (fun () -> add "concurrent" None new_action)
          in
          check "concurrent requests create only once" (first <> second);
          write (Filename.concat (p "new") "untracked") "retain me";
          let _, created = add "new" None new_action in
          check "idempotent create" (not created);
          check "dirty files retained"
            (Sys.file_exists (Filename.concat (p "new") "untracked"));
          check "clean-only removal refuses edits"
            (rejects (fun () ->
                 remove
                   (module B)
                   ~discard:false ~path:(p "new") ~branch:(branch "new")));
          let module Restart = (val make L.git) in
          let adopted =
            match
              get (Restart.inspect ~path:(p "new") ~branch:(branch "new"))
            with
            | Some c -> c
            | None -> failwith "lost checkout on restart"
          in
          check "owner survives default change"
            (L.equal_config (Worktree_backend.owner adopted) config);
          remove
            (module Restart)
            ~discard:true ~path:(p "new") ~branch:(branch "new");
          check "branch retained on removal"
            (G.git_capture ~cwd:repo [ "rev-parse"; "new" ] = base);

          G.run_git ~cwd:repo [ "branch"; "existing"; base ];
          let _, created =
            add "existing" (Some base)
              (Start_point_plan.Use_local_branch_unchanged { local_sha = base })
          in
          check "existing branch attached" created;
          check "branch mismatch refused"
            (Result.is_error
               (B.inspect ~path:(p "existing") ~branch:(branch "other")));
          check "main checkout refused"
            (Result.is_error (B.inspect ~path:repo ~branch:(branch "main")));
          check "occupied subdirectory refused"
            (Result.is_error
               (B.inspect
                  ~path:(Filename.concat repo ".git")
                  ~branch:(branch "main")));

          write (Filename.concat repo "file") "remote ahead\n";
          G.run_git ~cwd:repo [ "commit"; "-qam"; "ahead" ];
          let ahead = G.git_capture ~cwd:repo [ "rev-parse"; "HEAD" ] in
          G.run_git ~cwd:repo [ "branch"; "stale"; base ];
          ignore
            (add "stale" (Some base)
               (Start_point_plan.Reset_and_use_remote_tracking
                  { remote_sha = ahead }));
          check "remote reset pinned to approved SHA"
            (G.git_capture ~cwd:(p "stale") [ "rev-parse"; "HEAD" ] = ahead);
          G.run_git ~cwd:repo [ "branch"; "raced"; ahead ];
          check "changed branch refused"
            (rejects (fun () ->
                 ignore
                   (add "raced" (Some base)
                      (Start_point_plan.Reset_and_use_remote_tracking
                         { remote_sha = base }))));
          check "changed branch preserved"
            (G.git_capture ~cwd:repo [ "rev-parse"; "raced" ] = ahead);

          Unix.mkdir (p "partial") 0o700;
          check "partial directory preserved"
            (rejects (fun () -> ignore (add "partial" None new_action)));
          check "partial directory remains" (Sys.file_exists (p "partial"));
          G.with_temp_repo (fun other ->
              G.run_git ~cwd:other [ "commit"; "--allow-empty"; "-qm"; "other" ];
              G.run_git ~cwd:other
                [ "worktree"; "add"; "-b"; "foreign"; p "foreign" ];
              Fun.protect
                ~finally:(fun () ->
                  G.run_git ~cwd:other
                    [ "worktree"; "remove"; "--force"; p "foreign" ])
                (fun () ->
                  check "foreign repository refused"
                    (Result.is_error
                       (B.inspect ~path:(p "foreign") ~branch:(branch "foreign")))));

          (* A real conflicting rebase has detached HEAD but still belongs to its branch. *)
          write (Filename.concat (p "existing") "file") "patch change\n";
          G.run_git ~cwd:(p "existing") [ "commit"; "-qam"; "patch" ];
          check "rebase conflicts"
            (G.git_exit_code ~cwd:(p "existing") [ "rebase"; "main" ] <> 0);
          check "in-progress rebase remains adoptable"
            (Option.is_some
               (get
                  (B.inspect ~path:(p "existing") ~branch:(branch "existing"))));
          G.run_git ~cwd:(p "existing") [ "rebase"; "--abort" ];

          (* Native legacy checkouts have no Onton ownership metadata. *)
          G.run_git ~cwd:repo
            [ "worktree"; "add"; "-b"; "legacy"; p "legacy"; base ];
          (match B.inspect ~path:(p "legacy") ~branch:(branch "legacy") with
          | Ok (Some legacy) ->
              check "legacy native ownership"
                (L.equal_backend (Worktree_backend.owner legacy).backend L.Git)
          | Error msg when L.equal_backend config.backend L.Git ->
              check "unknown legacy owner requires simgit"
                (Base.String.is_substring msg
                   ~substring:"Cannot determine legacy checkout ownership");
              G.run_git ~cwd:repo [ "worktree"; "remove"; p "legacy" ]
          | Ok None -> failwith "legacy missing"
          | Error msg -> failwith msg);
          (match (config.backend, config.executable) with
          | L.Simgit, Some executable ->
              let old_path = Sys.getenv "PATH" in
              Fun.protect
                ~finally:(fun () -> Unix.putenv "PATH" old_path)
                (fun () ->
                  let bin = Filename.concat repo "discovery-bin" in
                  Unix.mkdir bin 0o700;
                  Unix.symlink executable (Filename.concat bin "simgit");
                  Unix.putenv "PATH" (bin ^ ":" ^ old_path);
                  G.sh ~dir:repo
                    (Filename.quote executable ^ " add external --path "
                    ^ Filename.quote (p "external"));
                  let module Native = (val make L.git) in
                  let external_checkout =
                    match
                      get
                        (Native.inspect ~path:(p "external")
                           ~branch:(branch "external"))
                    with
                    | Some checkout -> checkout
                    | None -> failwith "external simgit checkout missing"
                  in
                  check "Git default discovers unrecorded simgit ownership"
                    (L.equal_backend
                       (Worktree_backend.owner external_checkout).backend
                       L.Simgit);
                  Native.remove ~discard:false external_checkout)
          | L.Git, _ | L.Simgit, None -> ());
          B.reconcile ();
          check "reconcile retains healthy checkout"
            (Option.is_some
               (get (B.inspect ~path:(p "stale") ~branch:(branch "stale"))));
          (match config.backend with
          | L.Simgit -> ()
          | L.Git ->
              ignore (add "deleted" None new_action);
              G.sh ~dir:repo ("rm -rf " ^ Filename.quote (p "deleted"));
              check "missing native checkout distinguished from failure"
                (get (B.inspect ~path:(p "deleted") ~branch:(branch "deleted"))
                = None);
              B.reconcile ();
              let _, created =
                add "deleted" (Some base)
                  (Start_point_plan.Use_local_branch_unchanged
                     { local_sha = base })
              in
              check "deleted checkout reconstructed" created);
          Printf.printf "%s lifecycle integration: OK\n%!"
            (L.backend_name config.backend)))

let renamed_checkout env =
  G.with_temp_repo (fun repo ->
      G.run_git ~cwd:repo [ "commit"; "--allow-empty"; "-qm"; "base" ];
      let base = G.git_capture ~cwd:repo [ "rev-parse"; "HEAD" ] in
      let path = Filename.concat repo "checkout" in
      let module B =
        (val Worktree_backend.make ~fs:(Eio.Stdenv.fs env)
               ~clock:(Eio.Stdenv.clock env)
               ~process_mgr:(Eio.Stdenv.process_mgr env)
               ~repo_root:repo ~config:L.git ~timeout_seconds:5.)
      in
      let old_branch = branch "old" in
      let new_branch = branch "new" in
      ignore
        (B.materialize ~path ~branch:old_branch ~expected_local:None
           (Start_point_plan.Create_new_branch_from_base { base_branch = base }));
      Fun.protect
        ~finally:(fun () ->
          ignore
            (G.git_exit_code ~cwd:repo
               [ "worktree"; "remove"; "--force"; path ]))
        (fun () ->
          G.run_git ~cwd:path [ "switch"; "-c"; "new" ];
          check "unrelated branch request cannot retarget ownership"
            (Result.is_error (B.inspect ~path ~branch:(branch "unrelated")));
          B.reconcile ();
          check "reconcile adopts renamed ready Git checkout"
            (Option.is_some (get (B.inspect ~path ~branch:new_branch)));
          check "old branch no longer owns checkout"
            (Result.is_error (B.inspect ~path ~branch:old_branch));
          B.reconcile ();
          B.remove ~discard:false
            (Option.get (get (B.inspect ~path ~branch:new_branch))));
      print_endline "renamed Git checkout reconciliation: OK")

let failures env =
  G.with_temp_repo (fun repo ->
      let script = Filename.concat repo "fake-sg" in
      let make ?(timeout = 5.) executable =
        Worktree_backend.make ~fs:(Eio.Stdenv.fs env)
          ~clock:(Eio.Stdenv.clock env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~repo_root:repo
          ~config:
            (get (L.configure ~backend:"simgit" ~executable:(Some executable)))
          ~timeout_seconds:timeout
      in
      check "missing executable rejected"
        (try
           ignore (make (Filename.concat repo "missing"));
           false
         with _ -> true);
      write script
        "#!/bin/sh\necho 'unrecognized subcommand worktree' >&2\nexit 2\n";
      Unix.chmod script 0o700;
      check "wrong sg executable rejected"
        (rejects (fun () -> ignore (make script)));
      write script "#!/bin/sh\nexec sleep 10\n";
      check "timeout rejected"
        (rejects (fun () -> ignore (make ~timeout:0.02 script)));
      let cancelled = ref false in
      Eio.Fiber.first
        (fun () ->
          try ignore (make script)
          with Eio.Cancel.Cancelled _ as exn ->
            cancelled := true;
            raise exn)
        (fun () -> Eio.Time.sleep (Eio.Stdenv.clock env) 0.02);
      check "cancellation propagated" !cancelled;
      print_endline "backend preflight failures and cancellation: OK")

let interrupted_creation env =
  G.with_temp_repo (fun repo ->
      G.run_git ~cwd:repo [ "commit"; "--allow-empty"; "-qm"; "base" ];
      let base = G.git_capture ~cwd:repo [ "rev-parse"; "HEAD" ] in
      let script = Filename.concat repo "interrupted-sg" in
      let script_body ending =
        {|#!/bin/sh
[ "$1" = "--json" ] && [ "$2" = "doctor" ] && { echo '{"identity":"simgit","version":"0.3.0"}'; exit 0; }
[ "$1" = "list" ] && { echo '[]'; exit 0; }
[ "$1" = "unlock" ] && exit 0
[ "$1" = "remove" ] && { git worktree remove "$2"; exit $?; }
[ "$1" = "add" ] || exit 2
shift
branch=$1
shift
while [ "$#" -gt 0 ]; do
  case "$1" in
    --path) checkout_path=$2; shift 2 ;;
    --base) base_ref=$2; shift 2 ;;
    --json) shift ;;
    *) exit 2 ;;
  esac
done
git worktree add -b "$branch" "$checkout_path" "$base_ref" || exit 3
echo preserve > "$checkout_path/finished"
|}
        ^ ending ^ "\n"
      in
      write script (script_body "exit 1");
      Unix.chmod script 0o700;
      let config =
        get (L.configure ~backend:"simgit" ~executable:(Some script))
      in
      let make config =
        Worktree_backend.make ~fs:(Eio.Stdenv.fs env)
          ~clock:(Eio.Stdenv.clock env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~repo_root:repo ~config ~timeout_seconds:10.
      in
      let module B = (val make config) in
      let module Restart = (val make L.git) in
      let create name =
        B.materialize
          ~path:(Filename.concat repo name)
          ~branch:(branch name) ~expected_local:None
          (Start_point_plan.Create_new_branch_from_base { base_branch = base })
      in
      List.iter
        (fun name ->
          let path = Filename.concat repo name in
          Fun.protect
            ~finally:(fun () ->
              ignore
                (G.git_exit_code ~cwd:repo
                   [ "worktree"; "remove"; "--force"; path ]))
            (fun () ->
              if name = "failed" then
                check "failed command reported"
                  (rejects (fun () -> ignore (create name)))
              else (
                write script (script_body "exec sleep 10");
                let cancelled = ref false in
                Eio.Fiber.first
                  (fun () ->
                    try ignore (create name)
                    with Eio.Cancel.Cancelled _ as exn ->
                      cancelled := true;
                      raise exn)
                  (fun () ->
                    let rec await_files () =
                      if not (Sys.file_exists (Filename.concat path "finished"))
                      then (
                        Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                        await_files ())
                    in
                    await_files ());
                check "creation cancellation propagated" !cancelled);
              check "unpublished checkout refuses adoption"
                (Result.is_error (B.inspect ~path ~branch:(branch name)));
              check "publication state survives restart and default change"
                (Result.is_error (Restart.inspect ~path ~branch:(branch name)));
              check "retry does not reuse unpublished checkout"
                (rejects (fun () -> ignore (create name)));
              check "failed creation retains edits"
                (Sys.file_exists (Filename.concat path "finished"));
              check "failed creation retains branch"
                (G.git_capture ~cwd:repo [ "rev-parse"; name ] = base);
              Unix.unlink (Filename.concat path "finished");
              check "restart resumes cleanup after dirty files are resolved"
                (get (Restart.inspect ~path ~branch:(branch name)) = None);
              check "cleanup retry remains idempotent"
                (get (Restart.inspect ~path ~branch:(branch name)) = None)))
        [ "failed"; "cancelled" ];
      print_endline "failed and cancelled provisioning remains unpublished: OK")

let failed_reset env =
  G.with_temp_repo (fun repo ->
      G.run_git ~cwd:repo [ "commit"; "--allow-empty"; "-qm"; "base" ];
      let base = G.git_capture ~cwd:repo [ "rev-parse"; "HEAD" ] in
      G.run_git ~cwd:repo [ "commit"; "--allow-empty"; "-qm"; "ahead" ];
      let ahead = G.git_capture ~cwd:repo [ "rev-parse"; "HEAD" ] in
      let script = Filename.concat repo "reset-sg" in
      let body ending =
        {|#!/bin/sh
[ "$1" = "--json" ] && [ "$2" = "doctor" ] && { echo '{"identity":"simgit","version":"0.3.0"}'; exit 0; }
[ "$1" = "list" ] && { echo '[]'; exit 0; }
[ "$1" = "unlock" ] && exit 0
[ "$1" = "remove" ] && { git worktree remove "$2"; exit $?; }
[ "$1" = "run" ] || exit 2
git worktree add "$4" "$2" || exit 3
touch "$4/attached"
|}
        ^ ending ^ "\n"
      in
      write script (body "exit 1");
      Unix.chmod script 0o700;
      let module B =
        (val Worktree_backend.make ~fs:(Eio.Stdenv.fs env)
               ~clock:(Eio.Stdenv.clock env)
               ~process_mgr:(Eio.Stdenv.process_mgr env)
               ~repo_root:repo
               ~config:
                 (get (L.configure ~backend:"simgit" ~executable:(Some script)))
               ~timeout_seconds:5.)
      in
      List.iter
        (fun name ->
          G.run_git ~cwd:repo [ "branch"; name; base ];
          let path = Filename.concat repo name in
          let create () =
            ignore
              (B.materialize ~path ~branch:(branch name)
                 ~expected_local:(Some base)
                 (Start_point_plan.Reset_and_use_remote_tracking
                    { remote_sha = ahead }))
          in
          Fun.protect
            ~finally:(fun () ->
              G.run_git ~cwd:repo [ "worktree"; "remove"; "--force"; path ])
            (fun () ->
              if name = "reset-failed" then
                check "reset attachment fails" (rejects create)
              else (
                write script (body "exec sleep 10");
                let cancelled = ref false in
                Eio.Fiber.first
                  (fun () ->
                    try create ()
                    with exn when Worktree.has_cancellation exn ->
                      cancelled := true;
                      raise exn)
                  (fun () ->
                    let rec wait () =
                      if not (Sys.file_exists (Filename.concat path "attached"))
                      then (
                        Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                        wait ())
                    in
                    wait ());
                check "reset cancellation propagated" !cancelled);
              check "failed reset restores old branch"
                (G.git_capture ~cwd:repo [ "rev-parse"; name ] = base);
              check "failed reset remains unpublished"
                (Result.is_error (B.inspect ~path ~branch:(branch name)));
              check "retry refuses partial checkout" (rejects create);
              check "retry preserves old branch"
                (G.git_capture ~cwd:repo [ "rev-parse"; name ] = base)))
        [ "reset-failed"; "reset-cancelled" ])

let concurrent_reset env =
  G.with_temp_repo (fun repo ->
      G.run_git ~cwd:repo [ "commit"; "--allow-empty"; "-qm"; "base" ];
      let base = G.git_capture ~cwd:repo [ "rev-parse"; "HEAD" ] in
      G.run_git ~cwd:repo [ "commit"; "--allow-empty"; "-qm"; "ahead" ];
      let ahead = G.git_capture ~cwd:repo [ "rev-parse"; "HEAD" ] in
      G.run_git ~cwd:repo [ "branch"; "race"; base ];
      let old_path = Sys.getenv "PATH" in
      let real_git =
        String.split_on_char ':' old_path
        |> List.map (fun dir -> Filename.concat dir "git")
        |> List.find (fun path ->
            try
              Unix.access path [ Unix.X_OK ];
              not (Sys.is_directory path)
            with Unix.Unix_error _ -> false)
        |> Unix.realpath
      in
      let bin = Filename.concat repo "bin" in
      Unix.mkdir bin 0o700;
      let wrapper = Filename.concat bin "git" in
      let marker = Filename.concat repo "concurrent-update" in
      (* A competing writer performs the same target update immediately before
         our compare-and-swap. The second real Git command must then fail. *)
      write wrapper
        ("#!/bin/sh\n\
          if [ \"$3\" = update-ref ] && [ \"$4\" = refs/heads/race ] && [ \
          \"$5\" = " ^ Filename.quote ahead ^ " ]; then\n"
       ^ Filename.quote real_git ^ " \"$@\" || exit 3\n" ^ "touch "
       ^ Filename.quote marker ^ "\nfi\nexec " ^ Filename.quote real_git
       ^ " \"$@\"\n");
      Unix.chmod wrapper 0o700;
      Fun.protect
        ~finally:(fun () -> Unix.putenv "PATH" old_path)
        (fun () ->
          Unix.putenv "PATH" (bin ^ ":" ^ old_path);
          let module B =
            (val Worktree_backend.make ~fs:(Eio.Stdenv.fs env)
                   ~clock:(Eio.Stdenv.clock env)
                   ~process_mgr:(Eio.Stdenv.process_mgr env)
                   ~repo_root:repo ~config:L.git ~timeout_seconds:5.)
          in
          let path = Filename.concat repo "checkout" in
          check "competing update causes reset CAS failure"
            (rejects (fun () ->
                 ignore
                   (B.materialize ~path ~branch:(branch "race")
                      ~expected_local:(Some base)
                      (Start_point_plan.Reset_and_use_remote_tracking
                         { remote_sha = ahead }))));
          check "competing writer ran" (Sys.file_exists marker);
          check "failed CAS preserves competing target update"
            (G.git_capture ~cwd:repo [ "rev-parse"; "race" ] = ahead);
          check "failed CAS does not publish checkout"
            (Result.is_error (B.inspect ~path ~branch:(branch "race")))))

let mixed_reconcile env =
  G.with_temp_repo (fun repo ->
      let repo = Unix.realpath repo in
      G.run_git ~cwd:repo [ "commit"; "--allow-empty"; "-qm"; "base" ];
      let make config =
        Worktree_backend.make ~fs:(Eio.Stdenv.fs env)
          ~clock:(Eio.Stdenv.clock env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~repo_root:repo ~config ~timeout_seconds:5.
      in
      let paths =
        List.map
          (fun name ->
            let path = Filename.concat repo name in
            if name = "native" then
              let module Native = (val make L.git) in
              ignore
                (Native.materialize ~path ~branch:(branch name)
                   ~expected_local:None
                   (Start_point_plan.Create_new_branch_from_base
                      { base_branch = "main" }))
            else G.run_git ~cwd:repo [ "worktree"; "add"; "-b"; name; path ];
            path)
          [ "sg-one"; "sg-two"; "native" ]
      in
      Fun.protect
        ~finally:(fun () ->
          List.iter
            (fun path ->
              ignore (G.git_exit_code ~cwd:repo [ "worktree"; "unlock"; path ]);
              ignore
                (G.git_exit_code ~cwd:repo
                   [ "worktree"; "remove"; "--force"; path ]))
            paths)
        (fun () ->
          List.iter
            (fun name ->
              let path = Filename.concat repo name in
              let script = Filename.concat repo (name ^ "-bin") in
              let log = script ^ ".log" in
              let listing =
                Yojson.Safe.to_string
                  (`List
                     [
                       `Assoc
                         [
                           ("worktree", `String path);
                           ("mode", `String "overlay");
                         ];
                     ])
              in
              write script
                ("#!/bin/sh\n"
               ^ "if [ \"$1\" = \"--json\" ] && [ \"$2\" = \"doctor\" ]; then \
                  echo '{\"identity\":\"simgit\",\"version\":\"0.3.0\"}'; exit \
                  0; fi\n" ^ "case \"$1\" in\n" ^ "list) echo "
               ^ Filename.quote listing ^ ";;\n"
               ^ "repair) echo '{\"failed\":[]}' ;;\n" ^ "prune) touch "
               ^ Filename.quote log ^ ";;\n*) exit 2;;\nesac\n");
              Unix.chmod script 0o700;
              let module B =
                (val make
                       (get
                          (L.configure ~backend:"simgit"
                             ~executable:(Some script))))
              in
              check "adopt simulated overlay"
                (Option.is_some (get (B.inspect ~path ~branch:(branch name)))))
            [ "sg-one"; "sg-two" ];
          G.run_git ~cwd:repo
            [ "worktree"; "lock"; Filename.concat repo "sg-two" ];
          List.iter
            (fun path -> G.sh ~dir:repo ("rm -rf " ^ Filename.quote path))
            paths;
          let module B = (val make L.git) in
          B.reconcile ();
          List.iter
            (fun name ->
              check "all simgit owners reconciled"
                (Sys.file_exists (Filename.concat repo (name ^ "-bin.log"))))
            [ "sg-one"; "sg-two" ];
          let listed = List.map fst (B.list ()) in
          check "stale Git registration pruned"
            (not (List.mem (Filename.concat repo "native") listed));
          List.iter
            (fun name ->
              let path = Filename.concat repo name in
              check "unmounted overlay registration retained"
                (List.mem path listed);
              let admin = Filename.concat repo (".git/worktrees/" ^ name) in
              check "temporary locks released and existing locks preserved"
                (Sys.file_exists (Filename.concat admin "locked")
                = (name = "sg-two")))
            [ "sg-one"; "sg-two" ]))

let targeted_prune_ignores_unrelated_owner env =
  G.with_temp_repo (fun repo ->
      let repo = Unix.realpath repo in
      G.run_git ~cwd:repo [ "commit"; "--allow-empty"; "-qm"; "base" ];
      let path = Filename.concat repo "checkout" in
      G.run_git ~cwd:repo [ "worktree"; "add"; "-b"; "old"; path ];
      let stale_path = Filename.concat repo "stale-requested" in
      G.run_git ~cwd:repo [ "worktree"; "add"; "-b"; "requested"; stale_path ];
      let unrelated_stale_path = Filename.concat repo "stale-unrelated" in
      G.run_git ~cwd:repo
        [ "worktree"; "add"; "-b"; "unrelated"; unrelated_stale_path ];
      let simgit_stale_path = Filename.concat repo "stale-simgit" in
      G.run_git ~cwd:repo
        [ "worktree"; "add"; "-b"; "simgit-requested"; simgit_stale_path ];
      let nested_stale_path = Filename.concat repo "stale-prefix-nested" in
      G.run_git ~cwd:repo
        [ "worktree"; "add"; "-b"; "prefix/nested"; nested_stale_path ];
      let whitespace_stale_path = Filename.concat repo " stale-whitespace " in
      G.run_git ~cwd:repo
        [ "worktree"; "add"; "-b"; "whitespace"; whitespace_stale_path ];
      let locked_stale_path = Filename.concat repo "stale-locked" in
      G.run_git ~cwd:repo
        [ "worktree"; "add"; "-b"; "locked-requested"; locked_stale_path ];
      G.run_git ~cwd:repo [ "worktree"; "lock"; locked_stale_path ];
      Fun.protect
        ~finally:(fun () ->
          ignore (G.git_exit_code ~cwd:repo [ "worktree"; "unlock"; path ]);
          ignore
            (G.git_exit_code ~cwd:repo
               [ "worktree"; "remove"; "--force"; path ]))
        (fun () ->
          G.run_git ~cwd:path [ "switch"; "-c"; "new" ];
          let common = Filename.concat repo ".git" in
          let registry = Filename.concat common "onton-worktrees" in
          Unix.mkdir registry 0o700;
          let metadata =
            Filename.concat registry
              (Digest.to_hex (Digest.string path) ^ ".json")
          in
          let unavailable_simgit =
            get
              (L.configure ~backend:"simgit"
                 ~executable:(Some (Filename.concat repo "missing-simgit")))
          in
          write metadata
            (Yojson.Safe.to_string
               (L.ownership_json ~path ~branch:"old" ~phase:L.Ready
                  unavailable_simgit));
          let simgit_log = Filename.concat repo "simgit-remove.log" in
          let simgit = Filename.concat repo "simgit" in
          write simgit
            ("#!/bin/sh\n" ^ "case \"$1:$2\" in\n"
           ^ "--json:doctor) echo \
              '{\"identity\":\"simgit\",\"version\":\"0.3.0\"}' ;;\n"
           ^ "unlock:*) exit 0 ;;\n" ^ "remove:*) git -C " ^ Filename.quote repo
           ^ " worktree remove --force \"$2\" && touch "
           ^ Filename.quote simgit_log ^ " ;;\n" ^ "*) exit 2 ;;\n" ^ "esac\n");
          Unix.chmod simgit 0o700;
          let simgit_owner =
            get (L.configure ~backend:"simgit" ~executable:(Some simgit))
          in
          let simgit_metadata =
            Filename.concat registry
              (Digest.to_hex (Digest.string simgit_stale_path) ^ ".json")
          in
          write simgit_metadata
            (Yojson.Safe.to_string
               (L.ownership_json ~path:simgit_stale_path
                  ~branch:"renamed-simgit" ~phase:L.Ready simgit_owner));
          G.sh ~dir:repo ("rm -rf " ^ Filename.quote stale_path);
          G.sh ~dir:repo ("rm -rf " ^ Filename.quote unrelated_stale_path);
          G.sh ~dir:repo ("rm -rf " ^ Filename.quote simgit_stale_path);
          G.sh ~dir:repo ("rm -rf " ^ Filename.quote nested_stale_path);
          G.sh ~dir:repo ("rm -rf " ^ Filename.quote whitespace_stale_path);
          G.sh ~dir:repo ("rm -rf " ^ Filename.quote locked_stale_path);
          let module B =
            (val Worktree_backend.make ~fs:(Eio.Stdenv.fs env)
                   ~clock:(Eio.Stdenv.clock env)
                   ~process_mgr:(Eio.Stdenv.process_mgr env)
                   ~repo_root:repo ~config:L.git ~timeout_seconds:5.)
          in
          B.prune_stale_for_branch (branch "requested");
          check "requested stale registration is pruned"
            (not (List.mem stale_path (List.map fst (B.list ()))));
          B.prune_stale_for_branch (branch "simgit-requested");
          check "requested stale simgit registration is pruned"
            (not (List.mem simgit_stale_path (List.map fst (B.list ()))));
          check "simgit removal was used" (Sys.file_exists simgit_log);
          check "stale simgit ownership metadata is removed"
            (not (Sys.file_exists simgit_metadata));
          B.prune_stale_for_branch (branch "whitespace");
          check "whitespace path registration is pruned without normalization"
            (not (List.mem whitespace_stale_path (List.map fst (B.list ()))));
          B.prune_stale_for_branch (branch "locked-requested");
          check "locked unavailable registration is preserved"
            (List.mem locked_stale_path (List.map fst (B.list ())));
          B.prune_stale_for_branch (branch "prefix");
          check "nested branch registration is not pruned by prefix"
            (List.mem nested_stale_path (List.map fst (B.list ())));
          check "unrelated stale registration is not pruned"
            (List.mem unrelated_stale_path (List.map fst (B.list ())));
          check "inconsistent unrelated owner remains registered"
            (List.mem path (List.map fst (B.list ()))));
      print_endline "targeted prune ignores unrelated ownership: OK")

let discovery env =
  G.with_temp_repo (fun repo ->
      let bin = Filename.concat repo "bin" in
      Unix.mkdir bin 0o700;
      let old_path = Sys.getenv "PATH" in
      let make ?executable () =
        Worktree_backend.make ~fs:(Eio.Stdenv.fs env)
          ~clock:(Eio.Stdenv.clock env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~repo_root:repo
          ~config:(get (L.configure ~backend:"simgit" ~executable))
          ~timeout_seconds:5.
      in
      let script name body =
        let path = Filename.concat bin name in
        write path ("#!/bin/sh\n" ^ body ^ "\n");
        Unix.chmod path 0o700;
        path
      in
      let good = "echo '{\"identity\":\"simgit\",\"version\":\"0.3.0\"}'" in
      Fun.protect
        ~finally:(fun () -> Unix.putenv "PATH" old_path)
        (fun () ->
          Unix.putenv "PATH" (bin ^ ":/usr/bin:/bin");
          let sg = script "sg" good in
          ignore (make ());
          let canonical = script "simgit" good in
          ignore (script "sg" "exit 77");
          ignore (make ());
          ignore
            (script "simgit"
               "echo '{\"identity\":\"ast-grep\",\"version\":\"1.0.0\"}'");
          ignore (script "sg" good);
          ignore (make ());
          check "explicit wrong identity does not fall back"
            (rejects (fun () -> ignore (make ~executable:canonical ())));
          Unix.unlink canonical;
          ignore
            (script "sg"
               "echo '{\"identity\":\"ast-grep\",\"version\":\"1.0.0\"}'");
          check "colliding alias rejected"
            (rejects (fun () -> ignore (make ())));
          Unix.unlink sg;
          check "no candidates rejected" (rejects (fun () -> ignore (make ())))));
  print_endline "executable discovery and identity: OK"

let unavailable_legacy_owner env =
  G.with_temp_repo (fun repo ->
      let repo = Unix.realpath repo in
      G.run_git ~cwd:repo [ "commit"; "--allow-empty"; "-qm"; "base" ];
      let path = Filename.concat repo "legacy" in
      G.run_git ~cwd:repo [ "worktree"; "add"; "-b"; "legacy"; path ];
      let bin = Filename.concat repo "bin" in
      Unix.mkdir bin 0o700;
      let script = Filename.concat bin "simgit" in
      let alias = Filename.concat bin "sg" in
      List.iter
        (fun file ->
          write file "#!/bin/sh\nexit 1\n";
          Unix.chmod file 0o700)
        [ script; alias ];
      let old_path = Sys.getenv "PATH" in
      Fun.protect
        ~finally:(fun () -> Unix.putenv "PATH" old_path)
        (fun () ->
          Unix.putenv "PATH" (bin ^ ":" ^ old_path);
          let make () =
            Worktree_backend.make ~fs:(Eio.Stdenv.fs env)
              ~clock:(Eio.Stdenv.clock env)
              ~process_mgr:(Eio.Stdenv.process_mgr env)
              ~repo_root:repo ~config:L.git ~timeout_seconds:5.
          in
          let module Missing = (val make ()) in
          check "unavailable legacy owner refuses adoption"
            (Result.is_error (Missing.inspect ~path ~branch:(branch "legacy")));
          check "unavailable owner preserves files" (Sys.file_exists path);
          let listing =
            Yojson.Safe.to_string
              (`List
                 [
                   `Assoc
                     [
                       ("worktree", `String path);
                       ("branch", `String "legacy");
                       ("mode", `String "cow-clone");
                     ];
                 ])
          in
          let removed = Filename.concat repo "removed-by-simgit" in
          write script
            ("#!/bin/sh\n\
              case \"$1\" in\n\
              --json) echo '{\"identity\":\"simgit\",\"version\":\"0.3.0\"}' ;;\n\
              list) echo " ^ Filename.quote listing
           ^ ";;\n\
              repair) echo '{\"failed\":[]}' ;;\n\
              remove) git worktree remove \"$2\" && touch "
           ^ Filename.quote removed ^ ";;\n*) exit 2;;\nesac\n");
          let module Restored = (val make ()) in
          let checkout =
            match get (Restored.inspect ~path ~branch:(branch "legacy")) with
            | Some c -> c
            | None -> failwith "legacy checkout lost"
          in
          check "restored tool recovers simgit ownership"
            (L.equal_backend (Worktree_backend.owner checkout).backend L.Simgit);
          Restored.remove ~discard:false checkout;
          check "removal uses owning backend" (Sys.file_exists removed)))

let clean_failed_creation env =
  G.with_temp_repo (fun repo ->
      G.run_git ~cwd:repo [ "commit"; "--allow-empty"; "-qm"; "base" ];
      let script = Filename.concat repo "fake-simgit" in
      let log = Filename.concat repo "cleanup.log" in
      let path = Filename.concat repo "checkout" in
      let make ending =
        write script
          ("#!/bin/sh\n"
         ^ {|[ "$1" = "--json" ] && [ "$2" = "doctor" ] && { echo '{"identity":"simgit","version":"0.3.0"}'; exit 0; }
case "$1" in
list) echo '[]';;
unlock) echo unlock >> |}
         ^ Filename.quote log ^ {|; exit 0;;
remove) echo remove >> |}
         ^ Filename.quote log
         ^ {|;
  if [ -e "$2" ]; then git worktree remove "$2" || exit 1; fi;;
add) |}
         ^ ending ^ {|;;
*) exit 2;;
esac
|});
        Unix.chmod script 0o700;
        Worktree_backend.make ~fs:(Eio.Stdenv.fs env)
          ~clock:(Eio.Stdenv.clock env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~repo_root:repo
          ~config:
            (get (L.configure ~backend:"simgit" ~executable:(Some script)))
          ~timeout_seconds:5.
      in
      List.iter
        (fun ending ->
          write log "";
          let module B = (val make ending) in
          let expected_local =
            if
              G.git_exit_code ~cwd:repo
                [ "show-ref"; "--verify"; "refs/heads/failed" ]
              = 0
            then Some (G.git_capture ~cwd:repo [ "rev-parse"; "failed" ])
            else None
          in
          check "provisioning error reported"
            (rejects (fun () ->
                 ignore
                   (B.materialize ~path ~branch:(branch "failed")
                      ~expected_local
                      (Start_point_plan.Create_new_branch_from_base
                         { base_branch = "main" }))));
          check "clean or absent target recovered"
            (get (B.inspect ~path ~branch:(branch "failed")) = None);
          let ic = open_in log in
          let calls =
            Fun.protect
              ~finally:(fun () -> close_in ic)
              (fun () -> In_channel.input_all ic)
          in
          check "cleanup always unlocks before removing"
            (calls = "unlock\nremove\n"))
        [ "exit 1"; "git worktree add -b \"$2\" \"$4\" main || exit 3; exit 1" ]);
  print_endline "failed provisioning cleanup: OK"

let lifecycle_sequences env =
  QCheck2.Test.make
    ~name:"checkout readiness follows generated lifecycle operations" ~count:25
    QCheck2.Gen.(list_size (int_range 1 12) (int_range 0 3))
    (fun operations ->
      try
        G.with_temp_repo (fun repo ->
            G.run_git ~cwd:repo [ "commit"; "--allow-empty"; "-qm"; "base" ];
            let base = G.git_capture ~cwd:repo [ "rev-parse"; "HEAD" ] in
            G.run_git ~cwd:repo [ "branch"; "sequence"; base ];
            let path = Filename.concat repo "checkout" in
            let requested = branch "sequence" in
            let module B =
              (val Worktree_backend.make ~fs:(Eio.Stdenv.fs env)
                     ~clock:(Eio.Stdenv.clock env)
                     ~process_mgr:(Eio.Stdenv.process_mgr env)
                     ~repo_root:repo ~config:L.git ~timeout_seconds:5.)
            in
            let ready = ref false in
            Fun.protect
              ~finally:(fun () ->
                remove (module B) ~discard:true ~path ~branch:requested)
              (fun () ->
                List.for_all
                  (fun operation ->
                    (match operation with
                    | 0 ->
                        let checkout, created =
                          B.materialize ~path ~branch:requested
                            ~expected_local:(Some base)
                            (Start_point_plan.Use_local_branch_unchanged
                               { local_sha = base })
                        in
                        check "creation is idempotent" (created = not !ready);
                        check "owner retained"
                          (L.equal_config
                             (Worktree_backend.owner checkout)
                             L.git);
                        ready := true
                    | 1 ->
                        remove (module B) ~discard:false ~path ~branch:requested;
                        ready := false
                    | 2 -> B.reconcile ()
                    | _ -> ignore (get (B.inspect ~path ~branch:requested)));
                    let registrations =
                      get
                        (L.parse_git_list
                           (G.git_capture ~cwd:repo
                              [ "worktree"; "list"; "--porcelain"; "-z" ]))
                    in
                    let registered =
                      List.exists
                        (fun (entry : L.registration) ->
                          entry.branch = Some "sequence")
                        registrations
                    in
                    registered = !ready
                    && Option.is_some (get (B.inspect ~path ~branch:requested))
                       = !ready
                    && G.git_capture ~cwd:repo [ "rev-parse"; "sequence" ]
                       = base)
                  operations))
      with _ -> false)

let () =
  Eio_main.run (fun env ->
      QCheck2.Test.check_exn (lifecycle_sequences env);
      fixture env L.git;
      renamed_checkout env;
      failures env;
      discovery env;
      unavailable_legacy_owner env;
      clean_failed_creation env;
      failed_reset env;
      concurrent_reset env;
      mixed_reconcile env;
      targeted_prune_ignores_unrelated_owner env;
      interrupted_creation env;
      match Sys.getenv_opt "ONTON_TEST_SIMGIT" with
      | None ->
          print_endline
            "simgit integration: skipped (set ONTON_TEST_SIMGIT to a simgit \
             executable)"
      | Some executable ->
          fixture env
            (get (L.configure ~backend:"simgit" ~executable:(Some executable))))
