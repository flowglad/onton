open Base
open Onton_core

(** QCheck2 property-based tests for [Backend_routing.decide].

    The function is fully pure: ([Repo_config.t], default_backend, cli_model,
    complexity) -> [decision]. The properties below exhaust the four branches of
    the precedence rules and the case-insensitive [auto] detection. *)

let known_backends = [ "claude"; "codex"; "opencode"; "pi"; "gemini" ]
let gen_backend_name = QCheck2.Gen.oneof_list known_backends

(* Mix in some plausible non-auto model strings, [None] (no flag), and
   pathological strings. We exclude any string equal to "auto" (in any case
   variant) so the property applies cleanly. *)
let gen_non_auto_cli_model : string option QCheck2.Gen.t =
  let open QCheck2.Gen in
  oneof
    [
      return None;
      map
        (fun s -> Some s)
        (oneof_list
           [
             "sonnet";
             "opus";
             "haiku";
             "gpt-5.5";
             "gpt-5.4-mini";
             "gemini-3-pro-preview";
             "anthropic/claude-opus-4-7";
             "";
           ]);
    ]

let gen_auto_variant : string QCheck2.Gen.t =
  QCheck2.Gen.oneof_list [ "auto"; "AUTO"; "Auto"; "aUTo"; "auTO" ]

let gen_complexity : int option QCheck2.Gen.t =
  let open QCheck2.Gen in
  oneof
    [
      return None;
      map (fun k -> Some k) (oneof_list [ 1; 2; 3 ]);
      (* Out-of-range values shouldn't crash — surface them too. *)
      map (fun k -> Some k) (oneof_list [ 0; 4; 5; -1; 99 ]);
    ]

let gen_route_model : string option QCheck2.Gen.t =
  let open QCheck2.Gen in
  oneof
    [
      return None;
      map
        (fun s -> Some s)
        (oneof_list
           [
             "haiku"; "sonnet"; "opus"; "gpt-5.5"; "gpt-5.4"; "gemini-2.5-flash";
           ]);
    ]

let gen_route : Repo_config.route QCheck2.Gen.t =
  let open QCheck2.Gen in
  let* backend = gen_backend_name in
  let* model = gen_route_model in
  return { Repo_config.backend; model }

(* Generate a repo config with a random subset of complexity tiers populated.
   The empty config (no routes) is one possible value. *)
let gen_repo_config : Repo_config.t QCheck2.Gen.t =
  let open QCheck2.Gen in
  let* keys =
    let* size = int_range 0 3 in
    let* sub = list_size (return size) (oneof_list [ 1; 2; 3 ]) in
    return (List.dedup_and_sort sub ~compare:Int.compare)
  in
  let* routes = list_size (return (List.length keys)) gen_route in
  let complexity_routes = List.zip_exn keys routes in
  return { Repo_config.complexity_routes }

let pp_decision (d : Backend_routing.decision) =
  Printf.sprintf "{ backend = %s; model = %s }" d.backend
    (match d.model with Some s -> Printf.sprintf "Some %S" s | None -> "None")

let () =
  let open QCheck2 in
  let prop_explicit_model_passes_through =
    Test.make
      ~name:
        "decide: non-auto cli_model passes through unchanged on default backend"
      ~count:500
      (Gen.tup4 gen_repo_config gen_backend_name gen_non_auto_cli_model
         gen_complexity)
      (fun (repo_config, default_backend, cli_model, complexity) ->
        let d =
          Backend_routing.decide ~repo_config ~default_backend ~cli_model
            ~complexity
        in
        String.equal d.backend default_backend
        && Option.equal String.equal d.model cli_model)
  in

  let prop_explicit_ignores_repo_config =
    Test.make ~name:"decide: non-auto cli_model is unaffected by repo_config"
      ~count:500
      (Gen.tup4 gen_repo_config gen_backend_name gen_non_auto_cli_model
         gen_complexity)
      (fun (repo_config, default_backend, cli_model, complexity) ->
        let with_cfg =
          Backend_routing.decide ~repo_config ~default_backend ~cli_model
            ~complexity
        in
        let without_cfg =
          Backend_routing.decide ~repo_config:Repo_config.empty ~default_backend
            ~cli_model ~complexity
        in
        String.equal with_cfg.backend without_cfg.backend
        && Option.equal String.equal with_cfg.model without_cfg.model)
  in

  let prop_auto_with_route_uses_route =
    Test.make ~name:"decide: auto + matching route -> route's (backend, model)"
      ~count:500
      (Gen.tup4 gen_repo_config gen_backend_name gen_auto_variant gen_complexity)
      (fun (repo_config, default_backend, auto, complexity) ->
        match Repo_config.route_for_complexity repo_config ~complexity with
        | None -> true (* not in scope for this property *)
        | Some route ->
            let d =
              Backend_routing.decide ~repo_config ~default_backend
                ~cli_model:(Some auto) ~complexity
            in
            String.equal d.backend route.backend
            && Option.equal String.equal d.model route.model)
  in

  let prop_auto_without_route_falls_back =
    Test.make
      ~name:
        "decide: auto + no matching route -> { default_backend; Some \"auto\" }"
      ~count:500
      (Gen.tup4 gen_repo_config gen_backend_name gen_auto_variant gen_complexity)
      (fun (repo_config, default_backend, auto, complexity) ->
        match Repo_config.route_for_complexity repo_config ~complexity with
        | Some _ -> true (* not in scope for this property *)
        | None ->
            let d =
              Backend_routing.decide ~repo_config ~default_backend
                ~cli_model:(Some auto) ~complexity
            in
            (* The fallback canonicalises to lowercase "auto" regardless of
               how the user typed [--model], so registry deduplication works. *)
            String.equal d.backend default_backend
            && Option.equal String.equal d.model (Some "auto"))
  in

  let prop_auto_none_complexity_falls_back =
    Test.make
      ~name:
        "decide: auto + complexity=None -> { default_backend; Some \"auto\" }"
      ~count:200 (Gen.tup3 gen_repo_config gen_backend_name gen_auto_variant)
      (fun (repo_config, default_backend, auto) ->
        let d =
          Backend_routing.decide ~repo_config ~default_backend
            ~cli_model:(Some auto) ~complexity:None
        in
        String.equal d.backend default_backend
        && Option.equal String.equal d.model (Some "auto"))
  in

  let prop_auto_case_insensitive =
    Test.make
      ~name:"decide: auto detection is case-insensitive (all variants agree)"
      ~count:300 (Gen.tup3 gen_repo_config gen_backend_name gen_complexity)
      (fun (repo_config, default_backend, complexity) ->
        let variants = [ "auto"; "AUTO"; "Auto"; "aUTo"; "auTO" ] in
        let decisions =
          List.map variants ~f:(fun v ->
              Backend_routing.decide ~repo_config ~default_backend
                ~cli_model:(Some v) ~complexity)
        in
        match decisions with
        | [] -> true
        | first :: rest -> (
            (* All variants must agree on BOTH backend and model. The fallback
               arm canonicalises model to [Some "auto"] (lowercase), so the
               backend-only check that used to live here would have allowed
               case-divergent model fields to slip through and fragment the
               [Backend_registry] cache. *)
            let route =
              Repo_config.route_for_complexity repo_config ~complexity
            in
            match route with
            | Some r ->
                List.for_all decisions ~f:(fun d ->
                    String.equal d.backend r.backend
                    && Option.equal String.equal d.model r.model)
            | None ->
                List.for_all rest ~f:(fun d ->
                    String.equal d.backend first.backend
                    && Option.equal String.equal d.model first.model)))
  in

  let prop_total =
    Test.make
      ~name:"decide: total — never raises and yields a non-empty backend"
      ~count:1000
      (Gen.tup4 gen_repo_config gen_backend_name
         (Gen.oneof [ Gen.return None; Gen.map (fun s -> Some s) Gen.string ])
         gen_complexity)
      (fun (repo_config, default_backend, cli_model, complexity) ->
        try
          let d =
            Backend_routing.decide ~repo_config ~default_backend ~cli_model
              ~complexity
          in
          not (String.is_empty d.backend)
        with _ -> false)
  in

  let prop_is_auto_model_case_insensitive =
    Test.make ~name:"is_auto_model: case-insensitive on 'auto'" ~count:200
      Gen.string (fun s ->
        let detected = Backend_routing.is_auto_model (Some s) in
        let expected = String.equal (String.lowercase s) "auto" in
        Bool.equal detected expected)
  in

  let prop_is_auto_model_none =
    Test.make ~name:"is_auto_model: None -> false" ~count:1 (Gen.return ())
      (fun () -> not (Backend_routing.is_auto_model None))
  in

  let suite =
    [
      prop_explicit_model_passes_through;
      prop_explicit_ignores_repo_config;
      prop_auto_with_route_uses_route;
      prop_auto_without_route_falls_back;
      prop_auto_none_complexity_falls_back;
      prop_auto_case_insensitive;
      prop_total;
      prop_is_auto_model_case_insensitive;
      prop_is_auto_model_none;
    ]
  in
  let errcode = QCheck_base_runner.run_tests ~verbose:true suite in
  if errcode <> 0 then Stdlib.exit errcode;
  ignore pp_decision
