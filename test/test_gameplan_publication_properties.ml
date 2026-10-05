(* @archlint.module test
   @archlint.domain graph *)

open Onton_core
open Types

let get = function Ok value -> value | Error error -> failwith error
let pid = Patch_id.of_string

let source =
  {|{"projectName":"demo","owner":"owner","repo":"repo","problemStatement":"problem","solutionSummary":"solution","patches":[{"number":1,"title":"First","description":"First","dependsOn":[]},{"number":2,"title":"Second","description":"Second","dependsOn":[1]}],"dependencyGraph":[{"patch":1,"dependsOn":[]},{"patch":2,"dependsOn":[1]}]}|}

let gameplan =
  (get (Gameplan_parser.parse_string source)).Gameplan_parser.gameplan

let publication =
  get
    (Gameplan_publication.create ~directory:"/gameplans/" ~project_name:"demo"
       ~yaml:true ~content:source)

let published = get (Gameplan.publish gameplan publication)

let totality =
  QCheck2.Test.make ~name:"publication creation and decoding are total"
    ~count:500
    QCheck2.Gen.(triple string string string)
    (fun (directory, project_name, content) ->
      try
        ignore
          (Gameplan_publication.create ~directory ~project_name ~yaml:true
             ~content);
        ignore
          (Gameplan_publication.of_yojson
             (`Assoc
                [ ("path", `String directory); ("content", `String content) ]));
        ignore (Gameplan_publication.of_yojson (`List [ `String content ]));
        ignore
          (Gameplan_publication.persisted_of_yojson (`List [ `String content ]));
        ignore
          (Gameplan_publication.persisted_of_yojson
             (`Assoc
                [ ("path", `String directory); ("content", `String content) ]));
        true
      with _ -> false)

let roundtrip =
  QCheck2.Test.make ~name:"publication preserves arbitrary source bytes"
    ~count:300
    QCheck2.Gen.(pair bool string)
    (fun (yaml, content) ->
      try
        let p =
          get
            (Gameplan_publication.create ~directory:"gameplans"
               ~project_name:"demo" ~yaml ~content)
        in
        Gameplan_publication.content p = content
        && (Gameplan_publication.path p
           = "gameplans/demo/gameplan." ^ if yaml then "yaml" else "json")
        && Gameplan_publication.equal p
             (get
                (Gameplan_publication.of_yojson
                   (Gameplan_publication.yojson_of_t p)))
      with _ -> false)

let normalization =
  QCheck2.Test.make ~name:"publication directory normalization is idempotent"
    ~count:300 QCheck2.Gen.string (fun raw ->
      try
        match Gameplan_publication.normalize_directory raw with
        | Error _ -> true
        | Ok normalized ->
            Gameplan_publication.normalize_directory normalized = Ok normalized
      with _ -> false)

let merge_barrier =
  QCheck2.Test.make
    ~name:"merge-required edges stay blocked under PR/merge interleavings"
    ~count:400
    QCheck2.Gen.(list (pair bool bool))
    (fun steps ->
      try
        let graph = Graph.of_gameplan published in
        let rec check = function
          | [] -> true
          | (merged, has_pr) :: rest ->
              let has_merged id = Patch_id.equal id (pid "0") && merged in
              let has_pr _ = has_pr in
              Graph.deps_satisfied graph (pid "1") ~has_merged ~has_pr = merged
              && Graph.merge_deps_satisfied graph (pid "2") ~has_merged = merged
              && check rest
        in
        check steps
      with _ -> false)

let upgrade_is_sticky =
  QCheck2.Test.make
    ~name:"merge requirements cannot be downgraded by duplicate edges"
    ~count:300
    QCheck2.Gen.(list bool)
    (fun steps ->
      try
        let graph = Graph.of_patches gameplan.Gameplan.patches in
        let graph =
          Graph.add_dependency ~requirement:Graph.Merged graph (pid "2")
            ~dep:(pid "1")
        in
        let graph =
          List.fold_left
            (fun graph strict ->
              Graph.add_dependency
                ~requirement:(if strict then Graph.Merged else Graph.Stackable)
                graph (pid "2") ~dep:(pid "1"))
            graph steps
        in
        (not
           (Graph.deps_satisfied graph (pid "2")
              ~has_merged:(fun _ -> false)
              ~has_pr:(fun _ -> true)))
        && Graph.deps_satisfied graph (pid "2")
             ~has_merged:(fun _ -> true)
             ~has_pr:(fun _ -> false)
      with _ -> false)

let boundaries () =
  List.iter
    (fun directory ->
      assert (
        Result.is_error (Gameplan_publication.normalize_directory directory)))
    [
      "";
      "/";
      "../gameplans";
      "gameplans/../outside";
      "gameplans/.git";
      "gameplans\\outside";
      "gameplans\000outside";
    ];
  assert (Gameplan_publication.path publication = "gameplans/demo/gameplan.yaml");
  assert (
    List.map
      (fun patch -> Patch_id.to_string patch.Patch.id)
      published.Gameplan.patches
    = [ "0"; "1"; "2" ]);
  assert (
    List.map Patch_id.to_string
      (Graph.deps (Graph.of_gameplan published) (pid "2"))
    = [ "0"; "1" ]);
  assert (Result.is_error (Gameplan.publish published publication));
  let graph = Graph.of_patches gameplan.Gameplan.patches in
  assert (
    Graph.deps_satisfied graph (pid "2")
      ~has_merged:(fun _ -> false)
      ~has_pr:(fun _ -> true));
  let graph = Graph.remove_patch (Graph.of_gameplan published) (pid "0") in
  assert (Graph.merge_required_deps graph (pid "1") = [])

let () =
  boundaries ();
  List.iter
    (fun test -> QCheck2.Test.check_exn test)
    [ totality; roundtrip; normalization; merge_barrier; upgrade_is_sticky ]
