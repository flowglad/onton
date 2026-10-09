(* @archlint.module test
   @archlint.domain worktree *)
open Base
open Onton
open Onton_core
open Types
module Git = Onton_test_support.Git_env
module B = Branch_reconcile

let check label value = if not value then failwith label

(* Inject a failed probe through the process boundary while all other commands
   operate on real Git state. *)
let failing_conflict_probe_mgr (type tag)
    (mgr : tag Eio.Process.mgr_ty Eio.Resource.t) probed =
  let (Eio.Resource.T (state, ops)) = mgr in
  let module Original = (val Eio.Resource.get ops Eio.Process.Pi.Mgr) in
  let merge_started = ref false in
  let module Faults = struct
    include Original

    let spawn t ~sw ?cwd ?stdin ?stdout ?stderr ?env ?executable args =
      if List.mem args "merge" ~equal:String.equal then merge_started := true;
      match args with
      | _
        when !merge_started
             && List.mem args "--porcelain=v1" ~equal:String.equal ->
          probed := true;
          Original.spawn t ~sw ?cwd ?stdin ?stdout ?stderr ?env
            ~executable:"/bin/sh"
            [ "/bin/sh"; "-c"; "echo index-probe-stderr >&2; exit 23" ]
      | _ ->
          Original.spawn t ~sw ?cwd ?stdin ?stdout ?stderr ?env ?executable args
  end in
  Eio.Resource.T (state, Eio.Process.Pi.mgr (module Faults))

let write path content =
  let oc = Stdlib.open_out path in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_out oc)
    (fun () -> Stdlib.output_string oc content)

let commit dir file content =
  write (Stdlib.Filename.concat dir file) content;
  Git.run_git ~cwd:dir [ "add"; file ];
  Git.run_git ~cwd:dir [ "commit"; "-q"; "-m"; file ];
  Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]

let integrated state =
  match (B.phase state, B.operation state) with
  | Some B.Settled, Some { candidate = Some sha; _ } -> B.Commit.to_string sha
  | None, _
  | _, None
  | Some B.Settled, Some { candidate = None; _ }
  | ( Some
        ( B.Preparing | Integrating | Repairing _ | Publishing | Confirming
        | Waiting _ | Recovering | Intervention _ ),
      Some _ ) ->
      failwith
        ("integration did not settle: "
        ^ Yojson.Safe.to_string (B.yojson_of_t state))

let empty_gameplan =
  Gameplan.
    {
      project_name = "git-test";
      repo_owner = "";
      repo_name = "";
      problem_statement = "";
      architecture_design = None;
      solution_summary = "";
      final_state_spec = "";
      patches = [];
      operational_considerations = "";
      required_changes = "";
      ordering_constraints = [];
      current_state_analysis = "";
      explicit_opinions = "";
      acceptance_criteria = [];
      open_questions = [];
      functional_changes = [];
      context_resources = [];
      publication = None;
      reachability_traces = [];
    }

let () =
  Eio_main.run (fun env ->
      Git.with_temp_repo (fun dir ->
          let bare = dir ^ "/remote.git" in
          Git.run_git ~cwd:dir [ "init"; "--bare"; "-q"; bare ];
          Git.run_git ~cwd:dir [ "remote"; "add"; "origin"; bare ];
          let base = commit dir "shared" "base\n" in
          Git.run_git ~cwd:dir [ "push"; "-q"; "origin"; "main" ];
          Git.run_git ~cwd:dir [ "checkout"; "-q"; "-b"; "root" ];
          let initial = commit dir "root-file" "root\n" in
          Git.run_git ~cwd:dir [ "push"; "-q"; "origin"; "root" ];
          Git.run_git ~cwd:dir [ "checkout"; "-q"; "main" ];
          let root_path = dir ^ "/root-checkout" in
          Git.run_git ~cwd:dir [ "worktree"; "add"; "-q"; root_path; "root" ];
          let child name file content =
            Git.run_git ~cwd:dir [ "checkout"; "-q"; "-b"; name; initial ];
            let sha = commit dir file content in
            Git.run_git ~cwd:dir [ "push"; "-q"; "origin"; name ];
            Git.run_git ~cwd:dir [ "checkout"; "-q"; "main" ];
            sha
          in
          let head2 = child "child2" "two" "two\n" in
          let head3 = child "child3" "three" "three\n" in
          let conflict_head = child "conflict" "shared" "child\n" in
          let next_head = child "next" "next-file" "next\n" in
          let client =
            Worktree.make ~fs:(Eio.Stdenv.fs env) ~config:Worktree_lifecycle.git
              ~clock:(Eio.Stdenv.clock env)
              ~process_mgr:(Eio.Stdenv.process_mgr env)
              ~repo_root:dir
          in
          let module W = (val client) in
          let drive (module Client : Worktree.S) checkpoint event =
            let rec execute remaining state =
              if remaining = 0 then
                failwith "root reconciliation did not settle";
              let state =
                match B.decode (B.yojson_of_t state) with
                | Ok restored -> restored
                | Error reason -> failwith reason
              in
              checkpoint := state;
              match (B.operation state, B.pending state) with
              | Some operation, Some command ->
                  let result =
                    Client.reconcile ~path:root_path ~project_name:"git-test"
                      ~branch:(Branch.of_string "root") ~operation command
                  in
                  execute (remaining - 1)
                    (fst
                       (B.step state
                          (B.Result
                             { token = command.B.token; at = 100.; result })))
              | None, _ | _, None -> state
            in
            checkpoint := execute 100 (fst (B.step !checkpoint event))
          in
          let rt =
            Runtime.create ~gameplan:empty_gameplan
              ~main_branch:(Branch.of_string "main") ()
          in
          let publication = ref B.empty in
          let contribution name head_sha =
            let revision =
              match B.Commit.make head_sha with
              | Some revision -> revision
              | None -> failwith "invalid fixture revision"
            in
            B.Request
              {
                base = "main";
                policy = B.Preserve_ancestry;
                purpose = B.Integrate_revision { contributor = name; revision };
              }
          in
          let integrate name head_sha =
            Runtime.with_root_write rt (fun () ->
                drive (module W) publication (contribution name head_sha);
                !publication)
          in
          let lost_confirmation = ref false in
          let module Lost_confirmation = struct
            include W

            let reconcile ~path ~project_name ~branch ~operation command =
              let result =
                W.reconcile ~path ~project_name ~branch ~operation command
              in
              match command.B.kind with
              | B.Confirm _ when not !lost_confirmation ->
                  lost_confirmation := true;
                  raise Stdlib.Exit
              | B.Observe | Inspect | Verify_recovery | Pin _ | Integrate _
              | Continue _ | Commit_merge _ | Plan_remote_replay _
              | Checkout_remote _ | Publish _ | Confirm _ ->
                  result
          end in
          let results =
            Eio.Fiber.List.map
              (fun (name, sha) ->
                Runtime.with_root_write rt (fun () ->
                    if String.equal name "child3" then (
                      (try
                         drive
                           (module Lost_confirmation)
                           publication (contribution name sha)
                       with Stdlib.Exit -> ());
                      check "lost confirmation exercised" !lost_confirmation;
                      let published =
                        Git.git_capture ~cwd:bare [ "rev-parse"; "root" ]
                      in
                      drive (module W) publication B.Recover;
                      check
                        "lost acknowledgement retains exact published commit"
                        (String.equal published (integrated !publication)))
                    else drive (module W) publication (contribution name sha);
                    integrated !publication))
              [ ("child2", head2); ("child3", head3) ]
          in
          let final = Git.git_capture ~cwd:bare [ "rev-parse"; "root" ] in
          List.iter [ base; initial; head2; head3 ] ~f:(fun ancestor ->
              check "ancestry preserved"
                (Git.git_exit_code ~cwd:dir
                   [ "merge-base"; "--is-ancestor"; ancestor; final ]
                = 0));
          check "normal two-parent merge"
            (List.length
               (String.split
                  (Git.git_capture ~cwd:dir
                     [ "show"; "-s"; "--format=%P"; final ])
                  ~on:' ')
            = 2);
          check "siblings serialized"
            (List.length (List.dedup_and_sort results ~compare:String.compare)
            = 2);
          check "duplicate contribution keeps published commit"
            (String.equal (integrated (integrate "child2" head2)) final);
          List.iter [ "untracked"; "staged"; "unstaged"; "mixed" ]
            ~f:(fun shape ->
              let name = "dirty-" ^ shape in
              let file = name ^ "-file" in
              let dirty_head = child name file "dirty contribution\n" in
              let before = Git.git_capture ~cwd:bare [ "rev-parse"; "root" ] in
              let staged =
                String.equal shape "staged" || String.equal shape "mixed"
              in
              let unstaged =
                String.equal shape "unstaged" || String.equal shape "mixed"
              in
              let untracked =
                String.equal shape "untracked" || String.equal shape "mixed"
              in
              if staged then (
                write (root_path ^ "/shared") "staged user work\n";
                Git.run_git ~cwd:root_path [ "add"; "shared" ]);
              if unstaged then
                write (root_path ^ "/shared") "unstaged user work\n";
              if untracked then
                write (root_path ^ "/user-change") "untracked user work\n";
              let index = Git.git_capture ~cwd:root_path [ "write-tree" ] in
              let status =
                Git.git_capture ~cwd:root_path
                  [ "status"; "--porcelain=v1"; "--untracked-files=all" ]
              in
              check
                ("dirty root requests recovery: " ^ shape)
                (match B.phase (integrate name dirty_head) with
                | Some (B.Repairing _) -> true
                | None
                | Some
                    ( B.Preparing | Integrating | Publishing | Confirming
                    | Waiting _ | Recovering | Settled | Intervention _ ) ->
                    false);
              check "root index remains byte-equivalent"
                (String.equal index
                   (Git.git_capture ~cwd:root_path [ "write-tree" ]));
              check "root status remains unchanged"
                (String.equal status
                   (Git.git_capture ~cwd:root_path
                      [ "status"; "--porcelain=v1"; "--untracked-files=all" ]));
              let read file =
                let channel = Stdlib.open_in_bin (root_path ^ "/" ^ file) in
                Stdlib.Fun.protect
                  ~finally:(fun () -> Stdlib.close_in_noerr channel)
                  (fun () -> Stdlib.In_channel.input_all channel)
              in
              check "root working-tree bytes survive"
                (String.equal (read "shared")
                   (if unstaged then "unstaged user work\n"
                    else if staged then "staged user work\n"
                    else "base\n"));
              if untracked then
                check "untracked bytes survive"
                  (String.equal (read "user-change") "untracked user work\n");
              check
                "dirty contribution changes neither checkout head nor remote"
                (String.equal before
                   (Git.git_capture ~cwd:bare [ "rev-parse"; "root" ])
                && String.equal before
                     (Git.git_capture ~cwd:root_path [ "rev-parse"; "HEAD" ]));
              (* Only the fixture removes its injected edits before explicit resume. *)
              Git.run_git ~cwd:root_path
                [
                  "restore";
                  "--source=HEAD";
                  "--staged";
                  "--worktree";
                  "--";
                  "shared";
                ];
              if untracked then Unix.unlink (root_path ^ "/user-change");
              drive (module W) publication B.Recover;
              ignore (integrated !publication);
              check "explicit resume publishes the retained contribution"
                (Git.git_exit_code ~cwd:bare
                   [ "merge-base"; "--is-ancestor"; dirty_head; "root" ]
                = 0));
          let captured = child "moving" "captured" "captured revision\n" in
          Git.run_git ~cwd:dir [ "checkout"; "-q"; "moving" ];
          let moved = commit dir "later" "unchecked revision\n" in
          Git.run_git ~cwd:dir [ "push"; "-q"; "origin"; "moving" ];
          Git.run_git ~cwd:dir [ "checkout"; "-q"; "main" ];
          let pinned = integrated (integrate "moving" captured) in
          check "captured contributor revision survives branch movement"
            (Git.git_exit_code ~cwd:dir
               [ "merge-base"; "--is-ancestor"; captured; pinned ]
            = 0);
          check "new unchecked contributor revision is not substituted"
            (Git.git_exit_code ~cwd:dir
               [ "merge-base"; "--is-ancestor"; moved; pinned ]
            = 1);
          let local = commit root_path "unpublished" "local\n" in
          let extra = child "extra" "extra-file" "extra contribution\n" in
          let combined_local = integrated (integrate "extra" extra) in
          check "unpublished root work retained during contribution"
            (Git.git_exit_code ~cwd:dir
               [ "merge-base"; "--is-ancestor"; local; combined_local ]
            = 0);
          drive
            (module W)
            publication
            (B.Request
               B.
                 {
                   base = "main";
                   policy = Preserve_ancestry;
                   purpose = Publish_session "root-publication-fixture";
                 });
          check "root normal publication"
            (Option.equal B.equal_phase (B.phase !publication) (Some B.Settled));
          check "root publication reaches destination"
            (String.equal combined_local
               (Git.git_capture ~cwd:bare [ "rev-parse"; "root" ]));
          let _ = commit root_path "shared" "root\n" in
          Git.run_git ~cwd:root_path [ "push"; "-q"; "origin"; "root" ];
          let before_conflict =
            Git.git_capture ~cwd:bare [ "rev-parse"; "root" ]
          in
          check "conflict reported for staged repair"
            (match B.phase (integrate "conflict" conflict_head) with
            | Some (B.Repairing { mode = B.Content_repair; _ }) -> true
            | None
            | Some
                ( B.Preparing | Integrating
                | Repairing { mode = Diagnosis _ | History_recovery _; _ }
                | Publishing | Confirming | Waiting _ | Recovering | Settled
                | Intervention _ ) ->
                false);
          check "conflict retains captured target and unresolved file"
            (String.equal
               (Git.git_capture ~cwd:root_path [ "rev-parse"; "MERGE_HEAD" ])
               conflict_head
            && String.equal
                 (Git.git_capture ~cwd:root_path
                    [ "diff"; "--name-only"; "--diff-filter=U" ])
                 "shared");
          check "conflict does not publish"
            (String.equal before_conflict
               (Git.git_capture ~cwd:bare [ "rev-parse"; "root" ]));
          write (root_path ^ "/shared") "root and child resolved\n";
          Git.run_git ~cwd:root_path [ "add"; "shared" ];
          drive (module W) publication B.Recover;
          let resolved = integrated !publication in
          List.iter [ before_conflict; conflict_head ] ~f:(fun ancestor ->
              check "descendant repair preserves both parents"
                (Git.git_exit_code ~cwd:dir
                   [ "merge-base"; "--is-ancestor"; ancestor; resolved ]
                = 0));
          check "descendant repair publishes staged content"
            (String.equal
               (Git.git_capture ~cwd:bare [ "show"; "root:shared" ])
               "root and child resolved");
          (* Race the publication using the remote's pre-receive hook. The race commit
     is already in the remote object store, so updating its ref is legal. *)
          Git.run_git ~cwd:dir [ "checkout"; "-q"; "-b"; "race"; "root" ];
          let race = commit dir "raced" "remote update\n" in
          Git.run_git ~cwd:dir [ "push"; "-q"; "origin"; "race" ];
          Git.run_git ~cwd:dir [ "checkout"; "-q"; "main" ];
          let hook = bare ^ "/hooks/pre-receive" in
          Git.run_git ~cwd:bare [ "config"; "core.hooksPath"; bare ^ "/hooks" ];
          write hook
            ("#!/bin/sh\n\
              unset GIT_QUARANTINE_PATH GIT_OBJECT_DIRECTORY \
              GIT_ALTERNATE_OBJECT_DIRECTORIES\n\
              git update-ref refs/heads/root " ^ race ^ "\n");
          Unix.chmod hook 0o755;
          (* Local transport inherits the client environment; model an
             independent remote server's hook configuration explicitly. *)
          Git.run_git ~cwd:dir
            [
              "config";
              "remote.origin.receivepack";
              "git -c core.hooksPath="
              ^ Stdlib.Filename.quote (bare ^ "/hooks")
              ^ " receive-pack";
            ];
          check "remote race rejected"
            (match B.phase (integrate "next" next_head) with
            | Some (B.Waiting _) -> true
            | None
            | Some
                ( B.Preparing | Integrating | Repairing _ | Publishing
                | Confirming | Recovering | Settled | Intervention _ ) ->
                false);
          check "race commit preserved"
            (String.equal
               (Git.git_capture ~cwd:bare [ "rev-parse"; "root" ])
               race);
          Unix.unlink hook;
          drive (module W) publication B.Recover;
          let combined = integrated !publication in
          check "retry includes raced commit"
            (Git.git_exit_code ~cwd:dir
               [ "merge-base"; "--is-ancestor"; race; combined ]
            = 0);
          (* Exercise the production reconciliation command API, including its
             durable recovery path, rather than a second merge implementation. *)
          let owner = publication in
          let request label base =
            B.Request
              B.
                {
                  base;
                  policy = B.Preserve_ancestry;
                  purpose = B.Reconcile_request label;
                }
          in
          let settled label state =
            check label
              (Option.equal B.equal_phase (B.phase !state) (Some B.Settled))
          in
          let waiting label state substring =
            check label
              (match B.phase !state with
              | Some (B.Waiting { reason; _ }) ->
                  String.is_substring reason ~substring
              | None
              | Some
                  ( B.Preparing | B.Integrating | B.Repairing _ | B.Publishing
                  | B.Confirming | B.Recovering | B.Settled | B.Intervention _
                    ) ->
                  false)
          in
          let main_tip = commit dir "upstream" "main update\n" in
          Git.run_git ~cwd:dir [ "push"; "-q"; "origin"; "main" ];
          drive (module W) owner (request "upstream" "main");
          settled "root merges and publishes main" owner;
          let root_tip =
            Git.git_capture ~cwd:root_path [ "rev-parse"; "HEAD" ]
          in
          List.iter [ combined; main_tip ] ~f:(fun ancestor ->
              check "root merge preserves parents"
                (Git.git_exit_code ~cwd:dir
                   [ "merge-base"; "--is-ancestor"; ancestor; root_tip ]
                = 0));
          let conflicting_tip = commit dir "shared" "upstream conflict\n" in
          Git.run_git ~cwd:dir [ "push"; "-q"; "origin"; "main" ];
          let probed = ref false in
          let module Failed_probe =
            (val Worktree.make ~fs:(Eio.Stdenv.fs env)
                   ~config:Worktree_lifecycle.git ~clock:(Eio.Stdenv.clock env)
                   ~process_mgr:
                     (failing_conflict_probe_mgr
                        (Eio.Stdenv.process_mgr env)
                        probed)
                   ~repo_root:dir)
          in
          drive
            (module Failed_probe)
            owner
            (request "conflicting-upstream" "main");
          check "failed index probe exercised" !probed;
          waiting "failed index probe waits without aborting" owner
            "index-probe-stderr";
          check "root conflict retains merge state and unmerged file"
            (String.equal
               (Git.git_capture ~cwd:root_path [ "rev-parse"; "MERGE_HEAD" ])
               conflicting_tip
            && String.equal
                 (Git.git_capture ~cwd:root_path
                    [ "diff"; "--name-only"; "--diff-filter=U" ])
                 "shared");
          drive (module W) owner B.Recover;
          check "root conflict classified for repair"
            (match B.phase !owner with
            | Some (B.Repairing _) -> true
            | None
            | Some
                ( B.Preparing | B.Integrating | B.Publishing | B.Confirming
                | B.Waiting _ | B.Recovering | B.Settled | B.Intervention _ ) ->
                false);
          check "root conflict preserves HEAD"
            (String.equal
               (Git.git_capture ~cwd:root_path [ "rev-parse"; "HEAD" ])
               root_tip);
          let before_retry = W.conflict_diff ~path:root_path in
          let module Restarted =
            (val Worktree.make ~fs:(Eio.Stdenv.fs env)
                   ~config:Worktree_lifecycle.git ~clock:(Eio.Stdenv.clock env)
                   ~process_mgr:(Eio.Stdenv.process_mgr env)
                   ~repo_root:dir)
          in
          drive (module Restarted) owner B.Recover;
          check "retry preserves conflict contents"
            (String.equal before_retry (W.conflict_diff ~path:root_path));
          write (root_path ^ "/shared") "root and upstream resolved\n";
          Git.run_git ~cwd:root_path [ "add"; "shared" ];
          (* Onton consumes the staged repair itself against the captured target. *)
          drive (module Restarted) owner B.Recover;
          settled "staged repair completes and publishes" owner;
          let repaired =
            Git.git_capture ~cwd:root_path [ "rev-parse"; "HEAD" ]
          in
          check "repair preserves both parents"
            (String.equal
               (Git.git_capture ~cwd:root_path
                  [ "show"; "-s"; "--format=%P"; "HEAD" ])
               (root_tip ^ " " ^ conflicting_tip));
          check "repair preserves staged resolution"
            (String.equal
               (Git.git_capture ~cwd:root_path [ "show"; "HEAD:shared" ])
               "root and upstream resolved");
          drive (module W) owner B.Recover;
          settled "completed repair remains settled" owner;
          check "completed repair does not rewrite"
            (String.equal repaired
               (Git.git_capture ~cwd:root_path [ "rev-parse"; "HEAD" ]));
          check "remote contains repaired merge"
            (String.equal
               (Git.git_capture ~cwd:bare [ "rev-parse"; "root" ])
               repaired);
          let missing = ref B.empty in
          drive (module W) missing (request "missing-target" "missing-target");
          waiting "invalid target has command diagnostics" missing
            "missing-target";
          (* Onton-owned integration bypasses local hooks without changing config. *)
          let hook_tip = commit dir "hook-upstream" "upstream\n" in
          Git.run_git ~cwd:dir [ "push"; "-q"; "origin"; "main" ];
          let merge_hook = dir ^ "/.git/hooks/pre-merge-commit" in
          Git.run_git ~cwd:dir
            [ "config"; "core.hooksPath"; dir ^ "/.git/hooks" ];
          write merge_hook
            "#!/bin/sh\n\
             echo merge-hook-stdout\n\
             echo merge-hook-stderr >&2\n\
             exit 1\n";
          Unix.chmod merge_hook 0o755;
          drive (module W) owner (request "hook-upstream" "main");
          settled "Onton integration bypasses rejecting local hooks" owner;
          check "integration leaves the checkout clean"
            (String.is_empty
               (Git.git_capture ~cwd:root_path [ "status"; "--porcelain" ])
            && Git.git_exit_code ~cwd:root_path [ "rev-parse"; "MERGE_HEAD" ]
               <> 0);
          Unix.unlink merge_hook;
          check "hook recovery preserves both parents"
            (String.equal
               (Git.git_capture ~cwd:root_path
                  [ "show"; "-s"; "--format=%P"; "HEAD" ])
               (repaired ^ " " ^ hook_tip));
          check "no temporary worktree remains"
            (not
               (String.is_substring
                  (Git.git_capture ~cwd:dir
                     [ "worktree"; "list"; "--porcelain" ])
                  ~substring:"onton-integration-"));
          let marker = dir ^ "/integration-hook-started" in
          let filter_pid = dir ^ "/smudge-pid" in
          let helper_pid = dir ^ "/smudge-helper-pid" in
          let filter = dir ^ "/slow-smudge" in
          let stalled_script =
            "#!/bin/sh\necho $$ > "
            ^ Stdlib.Filename.quote filter_pid
            ^ "\nsleep 30 &\necho $! > "
            ^ Stdlib.Filename.quote helper_pid
            ^ "\ntouch "
            ^ Stdlib.Filename.quote marker
            ^ "\nwait\n"
          in
          write filter stalled_script;
          Unix.chmod filter 0o755;
          List.iteri
            [ (false, false); (false, false); (false, true); (true, false) ]
            ~f:(fun index (during_push, timeout) ->
              let name = "cancel-" ^ Int.to_string index in
              let revision = child name name (name ^ "\n") in
              if during_push then (
                write hook stalled_script;
                Unix.chmod hook 0o755)
              else (
                write
                  (dir ^ "/.git/info/attributes")
                  (name ^ " filter=integration-cancel\n");
                Git.run_git ~cwd:dir
                  [ "config"; "filter.integration-cancel.smudge"; filter ]);
              let before = Git.git_capture ~cwd:bare [ "rev-parse"; "root" ] in
              let cancelled = ref false in
              let run () =
                try
                  ignore (integrate name revision);
                  failwith "integration finished before cancellation"
                with exn when Worktree.has_cancellation exn ->
                  cancelled := true;
                  raise exn
              in
              if timeout then (
                let clock = Eio.Stdenv.clock env in
                let started = Eio.Time.now clock in
                let result =
                  Eio.Time.with_timeout clock 5. (fun () -> Ok (run ()))
                in
                check "stalled merge deadline propagates"
                  (match result with Error `Timeout -> true | Ok _ -> false);
                check "deadline reaps without waiting for sleeper"
                  Float.(Eio.Time.now clock -. started < 10.))
              else
                Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                    Eio.Fiber.first run (fun () ->
                        while not (Stdlib.Sys.file_exists marker) do
                          Eio.Time.sleep (Eio.Stdenv.clock env) 0.001
                        done));
              check "integration cancellation propagates" !cancelled;
              check "selected Git phase reached" (Stdlib.Sys.file_exists marker);
              List.iter [ filter_pid; helper_pid ] ~f:(fun file ->
                  let pid =
                    Stdlib.In_channel.with_open_bin file
                      Stdlib.In_channel.input_all
                    |> String.strip |> Int.of_string
                  in
                  check "cancelled process tree reaped"
                    (match Unix.kill pid 0 with
                    | () -> false
                    | exception Unix.Unix_error (Unix.ESRCH, _, _) -> true));
              check "root lock released"
                (Runtime.with_root_write rt (fun () -> true));
              check "cancellation preserves remote"
                (String.equal before
                   (Git.git_capture ~cwd:bare [ "rev-parse"; "root" ]));
              if during_push then Unix.unlink hook
              else (
                Unix.unlink (dir ^ "/.git/info/attributes");
                Git.run_git ~cwd:dir
                  [ "config"; "--unset"; "filter.integration-cancel.smudge" ]);
              drive (module Restarted) publication B.Recover;
              let recovered = integrated !publication in
              List.iter [ before; revision ] ~f:(fun ancestor ->
                  check "cancelled contribution resumes without lost ancestry"
                    (Git.git_exit_code ~cwd:dir
                       [ "merge-base"; "--is-ancestor"; ancestor; recovered ]
                    = 0));
              List.iter [ filter_pid; helper_pid; marker ] ~f:Unix.unlink);
          Stdlib.print_endline
            "PASS local Git feature integration, races, recovery and ancestry"))
