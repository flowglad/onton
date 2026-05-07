open Base

type t = {
  factory : backend:string -> model:string option -> kind;
  cache : (string * string option, kind) Hashtbl.t;
}

and kind = Ephemeral of Llm_backend.t | Long_lived of Llm_backend_long_lived.t

let display_name_of_claude_model = function
  | Some m when Backend_routing.is_auto_model (Some m) ->
      (* Under [--model auto] the registry caches one backend per
         [(backend, "auto")] key and [run_streaming] resolves the actual
         Claude alias from [complexity] at call time, so any single label
         here would be wrong for at least some patches. Drop the
         parenthetical: showing "Claude" matches the no-flag default and
         avoids the literal "Claude (auto)" string treating the sentinel
         as if it were a Claude model name. *)
      "Claude"
  | Some m -> Printf.sprintf "Claude (%s)" m
  | None -> "Claude"

let make_factory ~(process_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t) ~clock
    ~timeout ~setsid_exec : backend:string -> model:string option -> kind =
 fun ~backend ~model ->
  match backend with
  | "claude" ->
      Ephemeral
        (Claude_backend.create
           ~name:(display_name_of_claude_model model)
           ~model ~process_mgr ~clock ~timeout ~setsid_exec)
  | "codex" ->
      Ephemeral
        (Codex_backend.create ~model ~process_mgr ~clock ~timeout ~setsid_exec)
  | "opencode" ->
      Ephemeral
        (Opencode_backend.create ~model ~process_mgr ~clock ~timeout
           ~setsid_exec)
  | "pi" ->
      Ephemeral
        (Pi_backend.create ~model ~process_mgr ~clock ~timeout ~setsid_exec)
  | "gemini" ->
      Ephemeral
        (Gemini_backend.create ~model ~process_mgr ~clock ~timeout ~setsid_exec)
  | "patch-agent" ->
      Long_lived
        (Patch_agent_backend.create ~process_mgr ~clock ~timeout
           ~binary_path:"patch-agent" ~setsid_exec)
  | other ->
      invalid_arg
        (Printf.sprintf "Backend_registry.get: unknown backend %S" other)

let create ~(process_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t) ~clock
    ~timeout ~setsid_exec =
  {
    factory = make_factory ~process_mgr ~clock ~timeout ~setsid_exec;
    cache = Hashtbl.Poly.create ();
  }

let auto_model ~backend ~complexity =
  match backend with
  | "claude" -> Claude_runner.auto_model ~complexity
  | "codex" -> Codex_backend.auto_model ~complexity
  | "opencode" -> Opencode_backend.auto_model ~complexity
  | "pi" -> Pi_backend.auto_model ~complexity
  | "gemini" -> Gemini_backend.auto_model ~complexity
  | "patch-agent" -> Some "claude-sonnet-4-5"
  | other ->
      invalid_arg
        (Printf.sprintf "Backend_registry.auto_model: unknown backend %S" other)

let resolve_model ~backend ~model ~complexity =
  let model =
    Llm_backend.resolve_auto_model ~model ~complexity
      ~auto_model:(auto_model ~backend)
  in
  match (backend, model) with
  | "patch-agent", None -> auto_model ~backend ~complexity
  | _ -> model

let get t ~backend ~model =
  let key = (backend, model) in
  match Hashtbl.find t.cache key with
  | Some b -> b
  | None ->
      let b = t.factory ~backend ~model in
      (* [set] (not [add_exn]): the runner forks one daemon fiber per patch
         action, so two fibers may both miss [find] and race to insert the
         same key. Constructing a duplicate backend is harmless — they hold
         the same {process_mgr, clock, timeout, setsid_exec} closure — so
         losing the race just means one of the two backend records is
         garbage. [add_exn] would raise [Duplicate] on the loser. *)
      Hashtbl.set t.cache ~key ~data:b;
      b
