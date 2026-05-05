open Base

(* Pure decoder + cost-tracking decision logic lives in [Codex_event_parser]
   (lib_core/). This file is the effectful handler — env-var probing, process
   spawning — that drives the parser over [codex exec --json] output. *)

type cost_state = Codex_cost.cost_state

let initial_cost_state = Codex_cost.initial_cost_state

let parse_event_with_cost_tracking =
  Codex_event_parser.parse_event_with_cost_tracking

let parse_event = Codex_event_parser.parse_event
let build_args = Codex_event_parser.build_args
let auto_model = Codex_event_parser.auto_model

let budget_cap_nano_usd_from_env () =
  match
    Sys.getenv "ONTON_BUDGET_CAP_USD"
    |> Option.map ~f:(fun value -> String.strip value)
  with
  | None | Some "" -> None
  | Some raw -> (
      match Float.of_string_opt raw with
      | Some cap when Float.(cap > 0.) ->
          Some (Int64.of_float (Float.round_nearest (cap *. 1_000_000_000.0)))
      | Some _ | None -> None)

let run_streaming ~model ~process_mgr ~clock ~timeout ~setsid_exec ~project_name
    ~cwd ~patch_id ~prompt ~resume_session ~complexity ~on_event =
  let model = Llm_backend.resolve_auto_model ~model ~complexity ~auto_model in
  let cwd_path = snd cwd in
  let args = build_args ~model ~cwd_path ~prompt ~resume_session in
  let env =
    Spawn_env.merge_env ~base_env:(Unix.environment ())
      ~overrides:(Spawn_env.per_patch_env ~project_name ~patch_id)
  in
  let budget_cap_nano_usd = budget_cap_nano_usd_from_env () in
  let cost_state = ref initial_cost_state in
  let process_line line =
    let trimmed = String.strip line in
    if String.is_empty trimmed then []
    else
      let events, next_cost_state =
        parse_event_with_cost_tracking ~model ~budget_cap_nano_usd
          ~cost_state:!cost_state trimmed
      in
      cost_state := next_cost_state;
      events
  in
  Llm_backend.spawn_and_stream ~process_mgr ~clock ~timeout ~cwd ~env
    ~setsid_exec ~args ~process_line ~on_event

let create ~model ~process_mgr ~clock ~timeout ~setsid_exec : Llm_backend.t =
  {
    name = "Codex";
    run_streaming =
      (fun ~project_name
        ~cwd
        ~patch_id
        ~prompt
        ~resume_session
        ~complexity
        ~on_event
      ->
        run_streaming ~model ~process_mgr ~clock ~timeout ~setsid_exec ~cwd
          ~project_name ~patch_id ~prompt ~resume_session ~complexity ~on_event);
  }
