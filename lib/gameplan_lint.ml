open Base

module Severity = struct
  type t = Error | Warning [@@deriving show, eq, compare, sexp_of]

  let to_label (s : t) = match s with Error -> "error" | Warning -> "warning"
  let rank (s : t) = match s with Error -> 0 | Warning -> 1
end

module Issue = struct
  type t = {
    severity : Severity.t;
    patch_id : Types.Patch_id.t option;
    message : string;
  }
  [@@deriving show, eq, compare, sexp_of]
end

let known_classifications = [ "INFRA"; "GATED"; "BEHAVIOR" ]
let issue ?patch_id severity message = { Issue.severity; patch_id; message }

(** Collect duplicates from [items] using [key] as the hash key. Returns the
    duplicated keys in first-seen order, each appearing once. *)
let duplicates ~key items =
  let seen = Hashtbl.create (module String) in
  let dups = ref [] in
  List.iter items ~f:(fun item ->
      let k = key item in
      match Hashtbl.find seen k with
      | None -> Hashtbl.set seen ~key:k ~data:()
      | Some () ->
          if not (List.mem !dups k ~equal:String.equal) then dups := k :: !dups);
  List.rev !dups

(** DFS-based cycle detector: finds at least one cycle and returns the nodes
    participating in it (in the order a -> ... -> a). Returns [None] when the
    graph is acyclic. Self-edges and unknown deps are excluded before building
    the adjacency list. *)
let find_cycle ~known_ids (patches : Types.Patch.t list) =
  let adj = Hashtbl.create (module Types.Patch_id) in
  List.iter patches ~f:(fun p ->
      let real_deps =
        List.filter p.dependencies ~f:(fun d ->
            (not (Types.Patch_id.equal d p.id)) && Set.mem known_ids d)
      in
      Hashtbl.set adj ~key:p.id ~data:real_deps);
  let color = Hashtbl.create (module Types.Patch_id) in
  let stack : Types.Patch_id.t list ref = ref [] in
  let cycle : Types.Patch_id.t list option ref = ref None in
  let rec visit node =
    if Option.is_some !cycle then ()
    else
      match Hashtbl.find color node with
      | Some `Black -> ()
      | Some `Gray ->
          let in_cycle =
            List.take_while !stack ~f:(fun n ->
                not (Types.Patch_id.equal n node))
          in
          cycle := Some (node :: List.rev in_cycle)
      | Some `White | None ->
          Hashtbl.set color ~key:node ~data:`Gray;
          stack := node :: !stack;
          let neighbours = Hashtbl.find adj node |> Option.value ~default:[] in
          List.iter neighbours ~f:visit;
          (stack := match !stack with _ :: rest -> rest | [] -> []);
          Hashtbl.set color ~key:node ~data:`Black
  in
  List.iter patches ~f:(fun p -> visit p.id);
  !cycle

(** Patch-level checks that do not depend on the rest of the gameplan. *)
let lint_patch ~known_ids (p : Types.Patch.t) =
  let acc = ref [] in
  let add sev msg = acc := issue ~patch_id:p.id sev msg :: !acc in
  if String.is_empty (String.strip p.title) then
    add Severity.Error "title is empty";
  if String.is_empty (String.strip (Types.Branch.to_string p.branch)) then
    add Severity.Error "branch name is empty";
  if String.is_empty (String.strip p.spec) then
    add Severity.Warning "spec is empty";
  if List.is_empty p.acceptance_criteria then
    add Severity.Warning "no acceptance criteria";
  let cls = String.strip p.classification in
  if
    (not (String.is_empty cls))
    && not (List.mem known_classifications cls ~equal:String.equal)
  then
    add Severity.Warning
      (Printf.sprintf "unknown classification %S (expected one of: %s)" cls
         (String.concat ~sep:", " known_classifications));
  if List.mem p.dependencies p.id ~equal:Types.Patch_id.equal then
    add Severity.Error "patch depends on itself";
  let non_self_deps =
    List.filter p.dependencies ~f:(fun d -> not (Types.Patch_id.equal d p.id))
  in
  let dep_dups = duplicates non_self_deps ~key:Types.Patch_id.to_string in
  List.iter dep_dups ~f:(fun d ->
      add Severity.Warning (Printf.sprintf "duplicate dependency %s" d));
  let unique_non_self_deps =
    List.dedup_and_sort non_self_deps ~compare:Types.Patch_id.compare
  in
  List.iter unique_non_self_deps ~f:(fun d ->
      if not (Set.mem known_ids d) then
        add Severity.Error
          (Printf.sprintf "depends on unknown patch %s"
             (Types.Patch_id.to_string d)));
  !acc

(** Gameplan-level checks: cross-patch invariants and missing top-level fields.
*)
let lint_gameplan_globals (g : Types.Gameplan.t) =
  let acc = ref [] in
  let add sev msg = acc := issue sev msg :: !acc in
  if String.is_empty (String.strip g.project_name) then
    add Severity.Error "projectName is empty";
  (if List.is_empty g.patches then add Severity.Error "gameplan has no patches"
   else
     let patch_id_strs =
       List.map g.patches ~f:(fun p -> Types.Patch_id.to_string p.id)
     in
     List.iter (duplicates patch_id_strs ~key:Fn.id) ~f:(fun id ->
         add Severity.Error (Printf.sprintf "duplicate patch id %s" id));
     let branches =
       List.map g.patches ~f:(fun p -> Types.Branch.to_string p.branch)
     in
     List.iter (duplicates branches ~key:Fn.id) ~f:(fun b ->
         add Severity.Error (Printf.sprintf "duplicate branch name %s" b));
     let known_ids =
       List.map g.patches ~f:(fun p -> p.id)
       |> Set.of_list (module Types.Patch_id)
     in
     let has_dup_ids = List.length g.patches <> Set.length known_ids in
     if not has_dup_ids then
       match find_cycle ~known_ids g.patches with
       | None -> ()
       | Some chain ->
           let rendered =
             List.map chain ~f:Types.Patch_id.to_string
             |> String.concat ~sep:" -> "
           in
           add Severity.Error
             (Printf.sprintf "dependency cycle: %s -> %s" rendered
                (Types.Patch_id.to_string (List.hd_exn chain))));
  !acc

let compare_issue a b =
  let by_severity =
    Int.compare
      (Severity.rank a.Issue.severity)
      (Severity.rank b.Issue.severity)
  in
  if by_severity <> 0 then by_severity
  else
    let pid_key = function
      | None -> ""
      | Some p -> Types.Patch_id.to_string p
    in
    let by_patch = String.compare (pid_key a.patch_id) (pid_key b.patch_id) in
    if by_patch <> 0 then by_patch else String.compare a.message b.message

let lint (g : Types.Gameplan.t) =
  let known_ids =
    List.map g.patches ~f:(fun p -> p.Types.Patch.id)
    |> Set.of_list (module Types.Patch_id)
  in
  let global = lint_gameplan_globals g in
  let per_patch = List.concat_map g.patches ~f:(lint_patch ~known_ids) in
  List.sort (global @ per_patch) ~compare:compare_issue

let has_errors issues =
  List.exists issues ~f:(fun i ->
      Severity.equal i.Issue.severity Severity.Error)

let format_issues issues =
  if List.is_empty issues then ""
  else
    let lines =
      List.map issues ~f:(fun i ->
          let scope =
            match i.Issue.patch_id with
            | None -> "gameplan"
            | Some p -> Printf.sprintf "patch %s" (Types.Patch_id.to_string p)
          in
          Printf.sprintf "[%s] %s: %s"
            (Severity.to_label i.severity)
            scope i.message)
    in
    String.concat ~sep:"\n" lines ^ "\n"

(* -------------------------------------------------------------------------- *)
(* Inline tests                                                                *)
(* -------------------------------------------------------------------------- *)

let%test_module "gameplan_lint" =
  (module struct
    let pid = Types.Patch_id.of_string
    let br = Types.Branch.of_string

    let mk_patch ?(deps = []) ?(spec = "spec body")
        ?(acceptance = [ "criterion" ]) ?(classification = "BEHAVIOR")
        ?(title = "Some title") ?(branch = None) id =
      let branch = Option.value branch ~default:(br ("branch-" ^ id)) in
      {
        Types.Patch.id = pid id;
        title;
        description = "";
        branch;
        dependencies = List.map deps ~f:pid;
        spec;
        acceptance_criteria = acceptance;
        files = [];
        classification;
        changes = [];
        test_stubs_introduced = [];
        test_stubs_implemented = [];
      }

    let mk_gameplan ?(name = "demo") patches =
      {
        Types.Gameplan.project_name = name;
        problem_statement = "";
        solution_summary = "";
        final_state_spec = "";
        patches;
        current_state_analysis = "";
        explicit_opinions = "";
        acceptance_criteria = [];
        open_questions = [];
      }

    let%test "clean gameplan produces no issues" =
      let g = mk_gameplan [ mk_patch "a"; mk_patch ~deps:[ "a" ] "b" ] in
      List.is_empty (lint g)

    let%test "self-dependency is an error" =
      let g = mk_gameplan [ mk_patch ~deps:[ "a" ] "a" ] in
      List.exists (lint g) ~f:(fun i ->
          Severity.equal i.severity Error
          && String.is_substring i.message ~substring:"itself")

    let%test "missing dep target is an error" =
      let g = mk_gameplan [ mk_patch ~deps:[ "ghost" ] "a" ] in
      List.exists (lint g) ~f:(fun i ->
          Severity.equal i.severity Error
          && String.is_substring i.message ~substring:"unknown patch")

    let%test "duplicate patch id is an error" =
      let g = mk_gameplan [ mk_patch "a"; mk_patch "a" ] in
      List.exists (lint g) ~f:(fun i ->
          Severity.equal i.severity Error
          && String.is_substring i.message ~substring:"duplicate patch id")

    let%test "duplicate branch is an error" =
      let g =
        mk_gameplan
          [
            mk_patch ~branch:(Some (br "shared")) "a";
            mk_patch ~branch:(Some (br "shared")) "b";
          ]
      in
      List.exists (lint g) ~f:(fun i ->
          Severity.equal i.severity Error
          && String.is_substring i.message ~substring:"duplicate branch")

    let%test "cycle is detected" =
      let g =
        mk_gameplan [ mk_patch ~deps:[ "b" ] "a"; mk_patch ~deps:[ "a" ] "b" ]
      in
      List.exists (lint g) ~f:(fun i ->
          Severity.equal i.severity Error
          && String.is_substring i.message ~substring:"dependency cycle")

    let%test "empty spec is a warning, not an error" =
      let g = mk_gameplan [ mk_patch ~spec:"" "a" ] in
      let issues = lint g in
      (not (has_errors issues))
      && List.exists issues ~f:(fun i ->
          Severity.equal i.severity Warning
          && String.is_substring i.message ~substring:"spec is empty")

    let%test "missing acceptance criteria is a warning" =
      let g = mk_gameplan [ mk_patch ~acceptance:[] "a" ] in
      let issues = lint g in
      (not (has_errors issues))
      && List.exists issues ~f:(fun i ->
          Severity.equal i.severity Warning
          && String.is_substring i.message ~substring:"acceptance")

    let%test "unknown classification is a warning" =
      let g = mk_gameplan [ mk_patch ~classification:"WONKY" "a" ] in
      List.exists (lint g) ~f:(fun i ->
          Severity.equal i.severity Warning
          && String.is_substring i.message ~substring:"unknown classification")

    let%test "empty classification is tolerated silently" =
      let g = mk_gameplan [ mk_patch ~classification:"" "a" ] in
      not
        (List.exists (lint g) ~f:(fun i ->
             String.is_substring i.message ~substring:"classification"))

    let%test "duplicate dependency is a warning" =
      let g = mk_gameplan [ mk_patch "a"; mk_patch ~deps:[ "a"; "a" ] "b" ] in
      let issues = lint g in
      (not (has_errors issues))
      && List.exists issues ~f:(fun i ->
          Severity.equal i.severity Warning
          && String.is_substring i.message ~substring:"duplicate dependency")

    let%test "empty patch list is an error" =
      let g = mk_gameplan [] in
      has_errors (lint g)

    let%test "errors come before warnings in output" =
      let g = mk_gameplan [ mk_patch ~spec:"" ~deps:[ "ghost" ] "a" ] in
      match lint g with
      | first :: _ -> Severity.equal first.severity Error
      | [] -> false

    let%test "format_issues is empty when there are no issues" =
      String.is_empty (format_issues [])

    let%test "format_issues ends with newline when non-empty" =
      let s =
        format_issues [ issue Severity.Error "looks bad" ~patch_id:(pid "a") ]
      in
      String.is_suffix s ~suffix:"\n"
      && String.is_substring s ~substring:"[error]"
      && String.is_substring s ~substring:"patch a"
  end)
