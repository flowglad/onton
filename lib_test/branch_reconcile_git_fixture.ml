(* @archlint.module exempt
   @archlint.exempt-reason effect-boundary *)

module Git = Git_env
open Onton
open Onton_core
module B = Branch_reconcile
module E = Branch_reconcile_executor
module R = Branch_reconcile_runner

exception Simulated_stop

let get = function Ok v -> v | Error e -> failwith e
let check msg b = if not b then failwith msg

let commit dir file content message =
  let oc = open_out_bin (Filename.concat dir file) in
  output_string oc content;
  close_out oc;
  Git.run_git ~cwd:dir [ "add"; file ];
  Git.run_git ~cwd:dir [ "commit"; "-q"; "-m"; message ];
  Git.git_capture ~cwd:dir [ "rev-parse"; "HEAD" ]

let sha text =
  match B.Commit.make text with
  | Some sha -> sha
  | None -> failwith "invalid fixture SHA"

let id = Types.Patch_id.of_string "1"

let gameplan =
  (get
     (Gameplan_parser.parse_string
        "projectName: demo\n\
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
        \    dependsOn: []\n"))
    .Gameplan_parser.gameplan

let make_runtime () =
  Runtime.create ~gameplan ~main_branch:(Types.Branch.of_string "main") ()

let state runtime =
  Runtime.read runtime (fun s ->
      (Orchestrator.agent s.Runtime.orchestrator id)
        .Patch_agent.branch_reconcile)

let prefix = "refs/onton/reconcile/test/patch"
let intent = B.{ base = "main"; policy = Rewrite; purpose = Reconcile_base }

let execute io ~operation command =
  E.execute ~io ~prefix ~branch:"patch" ~operation command

let run runtime persist io event =
  R.run ~runtime ~persist ~patch_id:id
    ~now:(fun () -> 100.)
    ~execute:(execute io) event

let persist path = Persistence.save_snapshot ~path

let prepare remote dir =
  Git.run_git ~cwd:dir [ "remote"; "add"; "origin"; remote ];
  Git.run_git ~cwd:dir [ "fetch"; "-q"; "origin" ];
  Git.run_git ~cwd:dir [ "checkout"; "-q"; "-b"; "patch"; "origin/main" ]

(* Every case reports both entry and completion, so a timeout identifies the
   exact scenario rather than only the last monolithic test executable. *)
let timed ~name f =
  let started = Unix.gettimeofday () in
  Printf.eprintf "[start] %s\n%!" name;
  match f () with
  | result ->
      Printf.eprintf "[pass] %s (%.3fs)\n%!" name
        (Unix.gettimeofday () -. started);
      result
  | exception exn ->
      Printf.eprintf "[fail] %s (%.3fs): %s\n%!" name
        (Unix.gettimeofday () -. started)
        (Printexc.to_string exn);
      raise exn

let iter_cases ~name f cases =
  let count = List.length cases in
  List.iteri
    (fun index case ->
      timed
        ~name:(Printf.sprintf "%s/%d-of-%d" name (index + 1) count)
        (fun () -> f case))
    cases
