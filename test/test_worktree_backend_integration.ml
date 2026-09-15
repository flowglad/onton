(* @archlint.module test
   @archlint.domain worktree-backend *)

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
          let legacy =
            match
              get (B.inspect ~path:(p "legacy") ~branch:(branch "legacy"))
            with
            | Some c -> c
            | None -> failwith "legacy missing"
          in
          check "legacy native ownership"
            (L.equal_backend (Worktree_backend.owner legacy).backend L.Git);
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
for arg in "$@"; do [ "$arg" = "--help" ] && exit 0; done
[ "$1" = "worktree" ] && [ "$2" = "add" ] || exit 2
shift 2
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
              G.run_git ~cwd:repo [ "worktree"; "remove"; "--force"; path ])
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
                (G.git_capture ~cwd:repo [ "rev-parse"; name ] = base)))
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
for arg in "$@"; do [ "$arg" = "--help" ] && exit 0; done
[ "$2" = "run" ] || exit 2
git worktree add "$5" "$3" || exit 3
touch "$5/attached"
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
            G.run_git ~cwd:repo [ "worktree"; "add"; "-b"; name; path ];
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
              write script
                ("#!/bin/sh\n\
                  for arg in \"$@\"; do [ \"$arg\" = \"--help\" ] && exit 0; \
                  done\n"
               ^ "case \"$2\" in\n\
                  repair) echo '{\"failed\":[]}' ;;\n\
                  prune) touch " ^ Filename.quote log
               ^ ";;\n*) exit 2;;\nesac\n");
              Unix.chmod script 0o700;
              let admin =
                G.git_capture ~cwd:path [ "rev-parse"; "--absolute-git-dir" ]
              in
              write (Filename.concat admin "simgit-mode") "overlay";
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

let () =
  Eio_main.run (fun env ->
      fixture env L.git;
      failures env;
      failed_reset env;
      mixed_reconcile env;
      interrupted_creation env;
      match Sys.getenv_opt "ONTON_TEST_SIMGIT" with
      | None ->
          print_endline
            "simgit integration: skipped (set ONTON_TEST_SIMGIT to a simgit \
             executable)"
      | Some executable ->
          fixture env
            (get (L.configure ~backend:"simgit" ~executable:(Some executable))))
