(* @archlint.module test
   @archlint.domain worktree-lifecycle *)

open Onton_core
module W = Worktree_lifecycle
module Q = QCheck2

let total =
  Q.Test.make ~name:"worktree decoders are total" ~count:1000 Q.Gen.string
    (fun s ->
      try
        ignore (W.configure ~backend:s ~executable:(Some s));
        ignore (W.of_json (`String s));
        ignore (W.parse_simgit_list s);
        ignore (W.doctor_identity s);
        ignore (W.parse_git_list s);
        ignore (W.parse_ownership ~path:s ~branch:s (`String s));
        ignore
          (W.resolve ~backend:(Some s) ~executable:(Some s) ~stored_backend:None
             ~stored_executable:None ~repo:None);
        ignore (W.repair_error ~path:s s);
        true
      with _ -> false)

let roundtrip =
  Q.Test.make ~name:"backend configuration roundtrips" ~count:300 Q.Gen.string
    (fun executable ->
      try
        match W.configure ~backend:"simgit" ~executable:(Some executable) with
        | Error _ -> true
        | Ok config -> (
            match W.of_json (W.to_json config) with
            | Ok decoded -> W.equal_config config decoded
            | Error _ -> false)
      with _ -> false)

let list_roundtrip =
  Q.Test.make ~name:"simgit listing preserves paths and branch names" ~count:300
    Q.Gen.(pair (string_size (int_range 1 100)) string)
    (fun (path, branch) ->
      try
        let raw =
          Yojson.Safe.to_string
            (`List
               [
                 `Assoc
                   [
                     ("worktree", `String path);
                     ("branch", `String ("refs/heads/" ^ branch));
                     ("mode", `String "cow-clone");
                   ];
               ])
        in
        match W.parse_simgit_list raw with
        | Ok [ { path = p; branch = Some b; mode = Some "cow-clone" } ] ->
            p = path && b = branch
        | Ok _ | Error _ -> false
      with _ -> false)

let sticky_failure =
  Q.Test.make
    ~name:"repair failure stays an error across interleaved unrelated failures"
    ~count:500
    Q.Gen.(list bool)
    (fun flags ->
      try
        let failure path =
          `Assoc
            [ ("worktree", `String path); ("error", `String "mount failed") ]
        in
        let entries =
          List.map
            (fun target -> failure (if target then "/target" else "/other"))
            flags
        in
        let result =
          W.repair_error ~path:"/target"
            (Yojson.Safe.to_string (`Assoc [ ("failed", `List entries) ]))
        in
        Result.is_error result = List.exists Fun.id flags
      with _ -> false)

let boundaries =
  Q.Test.make ~name:"backend defaults and malformed schemas" ~count:1 Q.Gen.unit
    (fun () ->
      try
        W.configure ~backend:"git" ~executable:None = Ok W.git
        && W.parse_optional None = Ok None
        && Result.is_error (W.configure ~backend:"simgit" ~executable:(Some ""))
        && Result.is_error (W.configure ~backend:"unknown" ~executable:None)
        && Result.is_error (W.configure ~backend:"git" ~executable:(Some "sg"))
        && Result.is_error (W.parse_simgit_list "[{}]")
        && Result.is_error (W.repair_error ~path:"/target" "{}")
        && W.repair_error ~path:"/target" {|{"failed":[]}|} = Ok ()
        && Result.is_error
             (W.repair_error ~code:1 ~path:"/target" {|{"failed":[]}|})
        && W.repair_error ~code:1 ~path:"/target"
             {|{"failed":[{"worktree":"/other","error":"mount failed"}]}|}
           = Ok ()
        &&
        match
          Repo_config.parse_string ~known_backends:[]
            {|{"worktree":{"backend":"simgit","executable":"/opt/simgit/sg"}}|}
        with
        | Ok { worktree = Some c; _ } ->
            c.executable = Some "/opt/simgit/sg"
            && W.equal_backend c.backend W.Simgit
        | Ok _ | Error _ -> false
      with _ -> false)

let resolution =
  Q.Test.make ~name:"configuration precedence and backend switching" ~count:1
    Q.Gen.unit (fun () ->
      let sg path =
        match W.configure ~backend:"simgit" ~executable:(Some path) with
        | Ok c -> c
        | Error msg -> failwith msg
      in
      let resolve ?backend ?executable ?stored_backend ?stored_executable ?repo
          () =
        W.resolve ~backend ~executable ~stored_backend ~stored_executable ~repo
      in
      try
        resolve () = Ok W.git
        && resolve ~repo:(sg "/repo/sg") () = Ok (sg "/repo/sg")
        && resolve ~stored_backend:"git" ~stored_executable:"git"
             ~repo:(sg "/repo/sg") ()
           = Ok W.git
        && resolve ~stored_backend:"simgit" ~stored_executable:"/stored/sg"
             ~repo:(sg "/repo/sg") ()
           = Ok (sg "/stored/sg")
        && resolve ~backend:"git" ~stored_backend:"simgit"
             ~stored_executable:"/stored/sg" ~repo:(sg "/repo/sg") ()
           = Ok W.git
        && resolve ~backend:"simgit" ~stored_backend:"git"
             ~stored_executable:"git" ~repo:(sg "/repo/sg") ()
           = Ok (sg "/repo/sg")
        && resolve ~backend:"simgit" ~executable:"/cli/sg"
             ~stored_backend:"simgit" ~stored_executable:"/stored/sg" ()
           = Ok (sg "/cli/sg")
        && resolve ~stored_backend:"simgit" ~repo:(sg "/repo/sg") ()
           = W.configure ~backend:"simgit" ~executable:None
        && Result.is_error (resolve ~stored_backend:"unknown" ())
      with _ -> false)

let native_paths =
  Q.Test.make
    ~name:"native porcelain preserves unusual paths and detached registrations"
    ~count:300 Q.Gen.string (fun suffix ->
      try
        let path =
          "/tmp/" ^ String.concat "" (String.split_on_char '\000' suffix)
        in
        let raw =
          "worktree " ^ path
          ^ "\000HEAD abc\000detached\000\000worktree /second\000branch \
             refs/heads/feature\000\000"
        in
        match W.parse_git_list raw with
        | Ok
            [
              { path = p; branch = None; _ };
              { path = "/second"; branch = Some "feature"; _ };
            ] ->
            p = path
        | Ok _ | Error _ -> false
      with _ -> false)

let publication =
  Q.Test.make
    ~name:"ownership preserves publication state and rejects another checkout"
    ~count:300
    Q.Gen.(pair string (oneof_list [ W.Preparing; W.Cleanup_pending; W.Ready ]))
    (fun (name, phase) ->
      try
        let owner =
          match W.configure ~backend:"simgit" ~executable:None with
          | Ok c -> c
          | Error msg -> failwith msg
        in
        let json = W.ownership_json ~path:name ~branch:name ~phase owner in
        (match W.parse_ownership ~path:name ~branch:name json with
          | Ok (decoded_owner, state) ->
              W.equal_config decoded_owner owner && W.equal_phase phase state
          | Error _ -> false)
        && Result.is_error
             (W.parse_ownership ~path:(name ^ "/other") ~branch:name json)
        && Result.is_error
             (W.parse_ownership ~path:name ~branch:(name ^ "-other") json)
      with _ -> false)

let repository_executable =
  Q.Test.make
    ~name:
      "repository executables require absolute paths; trusted overrides remain \
       valid"
    ~count:300
    Q.Gen.(string_size (int_range 1 100))
    (fun suffix ->
      try
        let relative =
          "./" ^ String.map (fun c -> if c = '\000' then 'x' else c) suffix
        in
        let json executable =
          `Assoc
            [
              ("backend", `String "simgit"); ("executable", `String executable);
            ]
        in
        Result.is_error (W.parse_optional (Some (json relative)))
        && Result.is_error
             (Repo_config.parse_string ~known_backends:[]
                (Yojson.Safe.to_string (`Assoc [ ("worktree", json relative) ])))
        && Result.is_ok (W.parse_optional (Some (json ("/opt/" ^ relative))))
        && Result.is_ok
             (W.resolve ~backend:(Some "simgit") ~executable:(Some relative)
                ~stored_backend:None ~stored_executable:None ~repo:None)
        && Result.is_ok
             (W.resolve ~backend:None ~executable:None
                ~stored_backend:(Some "simgit")
                ~stored_executable:(Some relative) ~repo:None)
        && Result.is_error (W.parse_optional (Some (json "sg")))
        && Result.is_ok
             (W.parse_optional
                (Some (`Assoc [ ("backend", `String "simgit") ])))
      with _ -> false)

let discovery_contract =
  Q.Test.make
    ~name:"automatic discovery survives persistence and ownership publication"
    ~count:1 Q.Gen.unit (fun () ->
      try
        let automatic = W.configure ~backend:"simgit" ~executable:None in
        match automatic with
        | Error _ -> false
        | Ok config ->
            config.executable = None
            && W.of_json (W.to_json config) = automatic
            && W.resolve ~backend:None ~executable:None
                 ~stored_backend:(Some "simgit") ~stored_executable:None
                 ~repo:None
               = automatic
            && W.doctor_identity {|{"identity":"simgit","version":"0.3.0"}|}
               = Ok ()
            && List.for_all
                 (fun json -> Result.is_error (W.doctor_identity json))
                 [
                   "{}";
                   "[]";
                   {|{"identity":"ast-grep","version":"1"}|};
                   {|{"identity":"simgit"}|};
                   {|{"identity":"simgit","version":""}|};
                 ]
      with _ -> false)

let ownership_modes =
  Q.Test.make
    ~name:"all reported modes stay simgit-owned across arbitrary listings"
    ~count:300
    Q.Gen.(list (option string))
    (fun modes ->
      try
        let raw =
          Yojson.Safe.to_string
            (`List
               (List.mapi
                  (fun i mode ->
                    `Assoc
                      [
                        ("worktree", `String ("/checkout/" ^ string_of_int i));
                        ( "mode",
                          match mode with None -> `Null | Some s -> `String s );
                      ])
                  modes))
        in
        match W.parse_simgit_list raw with
        | Error _ -> false
        | Ok entries ->
            List.map W.registration_backend entries
            = List.map (function None -> W.Git | Some _ -> W.Simgit) modes
      with _ -> false)

let backend_names =
  Q.Test.make ~name:"backend names match the selected backend" ~count:100
    Q.Gen.bool (fun simgit ->
      W.backend_name (if simgit then W.Simgit else W.Git)
      = if simgit then "simgit" else "git")

let () =
  QCheck_base_runner.run_tests_main
    [
      discovery_contract;
      ownership_modes;
      repository_executable;
      backend_names;
      total;
      roundtrip;
      list_roundtrip;
      sticky_failure;
      boundaries;
      resolution;
      native_paths;
      publication;
    ]
