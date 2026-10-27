(* @archlint.module test
   @archlint.domain pi-event-parser *)

open Onton_core

let pi_build_args_include_session_dir =
  QCheck2.Test.make ~name:"pi args include patch-scoped session dir" ~count:200
    QCheck2.Gen.string_small (fun patch_id ->
      let args =
        Pi_event_parser.build_args ~model:None ~effort:None
          ~cwd_path:"/tmp/work" ~patch_id ~prompt:"do work" ~resume_session:None
      in
      List.mem ("/tmp/work/.pi-sessions/" ^ patch_id) args)

let pi_effort_levels =
  [ "off"; "minimal"; "low"; "medium"; "high"; "xhigh"; "max" ]

let pi_thinking_args =
  QCheck2.Test.make
    ~name:"pi forwards effort as --thinking, including on resume" ~count:200
    QCheck2.Gen.(
      pair (option (oneof_list pi_effort_levels)) (option string_small))
    (fun (effort, resume_session) ->
      let args =
        Pi_event_parser.build_args ~model:None ~effort ~cwd_path:"/tmp/work"
          ~patch_id:"1" ~prompt:"do work" ~resume_session
      in
      let expected =
        [ "pi"; "-p"; "do work"; "--mode"; "json" ]
        @ (match effort with None -> [] | Some e -> [ "--thinking"; e ])
        @ (match resume_session with
          | None -> []
          | Some s -> [ "--session"; s ])
        @ [ "--session-dir"; "/tmp/work/.pi-sessions/1" ]
      in
      args = expected)

let pi_auto_with_effort =
  QCheck2.Test.make
    ~name:"pi auto accepts configured thinking levels at every tier" ~count:200
    QCheck2.Gen.(
      pair (oneof_list pi_effort_levels) (option (int_range (-5) 10)))
    (fun (effort, complexity) ->
      let repo_config =
        { Repo_config.empty with Repo_config.default_effort = Some effort }
      in
      let decision =
        Backend_routing.decide ~repo_config ~default_backend:"pi"
          ~effective_model:(Some "auto") ~complexity
        |> fun d ->
        Backend_routing.resolve_auto d ~auto_model:Pi_event_parser.auto_model
          ~complexity
      in
      decision.Backend_routing.backend = "pi"
      && decision.Backend_routing.model = Pi_event_parser.auto_model ~complexity
      && decision.Backend_routing.effort = Some effort
      && Backend_routing.unsupported_efforts ~repo_config ~default_backend:"pi"
           ~effective_model:(Some "auto")
         = [])

let pi_route_effort =
  QCheck2.Test.make ~name:"pi route inherits, clears, or overrides thinking"
    ~count:200 (QCheck2.Gen.oneof_list pi_effort_levels) (fun level ->
      List.for_all
        (fun (effort, expected) ->
          let repo_config =
            {
              Repo_config.empty with
              Repo_config.default_effort = Some "medium";
              Repo_config.complexity_routes =
                [ (1, { Repo_config.backend = "pi"; model = None; effort }) ];
            }
          in
          let decision =
            Backend_routing.decide ~repo_config ~default_backend:"pi"
              ~effective_model:(Some "auto") ~complexity:(Some 1)
          in
          decision.Backend_routing.effort = expected
          && Backend_routing.unsupported_efforts ~repo_config
               ~default_backend:"pi" ~effective_model:(Some "auto")
             = [])
        [
          (Repo_config.Inherit, Some "medium");
          (Repo_config.Provider_default, None);
          (Repo_config.Level level, Some level);
        ])

let pi_effort_config =
  QCheck2.Test.make ~name:"pi effort levels parse from repository config"
    ~count:100 (QCheck2.Gen.oneof_list pi_effort_levels) (fun level ->
      let raw =
        Printf.sprintf
          {|{"default":{"backend":"pi","model":"auto","effort":"%s"}}|} level
      in
      match Repo_config.parse_string ~known_backends:[ "pi" ] raw with
      | Ok config -> config.Repo_config.default_effort = Some level
      | Error _ -> false)

let pi_parse_event_is_total =
  QCheck2.Test.make ~name:"pi parse_event is total" ~count:200
    QCheck2.Gen.string_small (fun line ->
      let events = Pi_event_parser.parse_event line in
      List.length events >= 0)

let pi_auto_tiers =
  QCheck2.Test.make ~name:"pi auto tiers pass model and thinking to CLI"
    ~count:200
    QCheck2.Gen.(
      oneof
        [
          return None;
          return (Some 1);
          return (Some 2);
          map (fun complexity -> Some complexity) int;
        ])
    (fun complexity ->
      let model, effort =
        match complexity with
        | Some 1 -> ("gpt-6-luna", "medium")
        | Some 2 -> ("gpt-6-luna", "xhigh")
        | Some _ | None -> ("gpt-6-sol", "medium")
      in
      let decision =
        Backend_routing.decide ~repo_config:Repo_config.empty
          ~default_backend:"pi" ~effective_model:(Some "auto") ~complexity
        |> fun d ->
        Backend_routing.resolve_auto d ~auto_model:Pi_event_parser.auto_model
          ~complexity
      in
      Pi_event_parser.build_args ~model:decision.Backend_routing.model
        ~effort:decision.Backend_routing.effort ~cwd_path:"/tmp/work"
        ~patch_id:"1" ~prompt:"do work" ~resume_session:None
      = [
          "pi";
          "-p";
          "do work";
          "--mode";
          "json";
          "--model";
          model;
          "--thinking";
          effort;
          "--session-dir";
          "/tmp/work/.pi-sessions/1";
        ])

let pi_session_headers =
  QCheck2.Test.make ~count:500
    ~name:
      "Pi session headers retain exact nonblank IDs for context-free resumption"
    QCheck2.Gen.string (fun id ->
      try
        let header =
          Yojson.Safe.to_string
            (`Assoc [ ("type", `String "session"); ("id", `String id) ])
        in
        let events = Pi_event_parser.parse_event header in
        if String.trim id = "" then events = []
        else
          events
          = [
              Types.Stream_event.Session_init
                {
                  session_id = id;
                  api_key_source = None;
                  model = None;
                  claude_code_version = None;
                  permission_mode = None;
                };
            ]
          && Session_prompt.render ~resume_session:(Some id)
               (Session_prompt.create ~context:"FULL PATCH CONTEXT"
                  ~turn:"next turn")
             = "next turn"
          && Pi_event_parser.build_args ~model:None ~effort:None
               ~cwd_path:"/tmp/work" ~patch_id:"1" ~prompt:"next turn"
               ~resume_session:(Some id)
             = [
                 "pi";
                 "-p";
                 "next turn";
                 "--mode";
                 "json";
                 "--session";
                 id;
                 "--session-dir";
                 "/tmp/work/.pi-sessions/1";
               ]
      with _ -> false)

let pi_malformed_session_headers =
  QCheck2.Test.make ~count:1
    ~name:"Pi malformed session headers cannot establish a conversation"
    QCheck2.Gen.unit (fun () ->
      List.for_all
        (fun json -> Pi_event_parser.parse_event json = [])
        [
          {|{"type":"session"}|};
          {|{"type":"session","id":null}|};
          {|{"type":"session","id":42}|};
          {|{"type":"session","id":""}|};
          {|{"type":"session","id":"  "}|};
        ])

let pi_parser_public_surface_is_linked =
  QCheck2.Test.make ~name:"pi parser public surface is linked" QCheck2.Gen.unit
    (fun () ->
      ignore Pi_event_parser.auto_model;
      ignore Pi_event_parser.parse_event;
      true)

let () =
  QCheck2.Test.check_exn pi_build_args_include_session_dir;
  QCheck2.Test.check_exn pi_thinking_args;
  QCheck2.Test.check_exn pi_auto_with_effort;
  QCheck2.Test.check_exn pi_route_effort;
  QCheck2.Test.check_exn pi_effort_config;
  QCheck2.Test.check_exn pi_parse_event_is_total;
  QCheck2.Test.check_exn pi_auto_tiers;
  QCheck2.Test.check_exn pi_session_headers;
  QCheck2.Test.check_exn pi_malformed_session_headers;
  QCheck2.Test.check_exn pi_parser_public_surface_is_linked
