(* @archlint.module shell
   @archlint.domain backend-routing *)

open Base

type t = {
  factory :
    backend:string ->
    model:string option ->
    effort:string option ->
    Llm_backend.t;
  cache : (string * string option * string option, Llm_backend.t) Hashtbl.t;
}

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
    ~timeout ~setsid_exec ~extras :
    backend:string ->
    model:string option ->
    effort:string option ->
    Llm_backend.t =
 fun ~backend ~model ~effort ->
  match backend with
  | "claude" ->
      Claude_backend.create
        ~name:(display_name_of_claude_model model)
        ~model ~effort ~process_mgr ~clock ~timeout ~setsid_exec
  | "codex" ->
      Codex_backend.create ~model ~effort ~extras ~process_mgr ~clock ~timeout
        ~setsid_exec
  | "opencode" ->
      Opencode_backend.create ~model ~process_mgr ~clock ~timeout ~setsid_exec
  | "pi" -> Pi_backend.create ~model ~process_mgr ~clock ~timeout ~setsid_exec
  | "gemini" ->
      Gemini_backend.create ~model ~process_mgr ~clock ~timeout ~setsid_exec
  | other ->
      invalid_arg
        (Printf.sprintf "Backend_registry.get: unknown backend %S" other)

let create ~(process_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t) ~clock
    ~timeout ~setsid_exec ~extras =
  {
    factory = make_factory ~process_mgr ~clock ~timeout ~setsid_exec ~extras;
    cache = Hashtbl.Poly.create ();
  }

let auto_model ~backend ~complexity =
  match backend with
  | "claude" -> Claude_runner.auto_model ~complexity
  | "codex" -> Codex_backend.auto_model ~complexity
  | "opencode" -> Opencode_backend.auto_model ~complexity
  | "pi" -> Pi_backend.auto_model ~complexity
  | "gemini" -> Gemini_backend.auto_model ~complexity
  | other ->
      invalid_arg
        (Printf.sprintf "Backend_registry.auto_model: unknown backend %S" other)

let resolve_model ~backend ~model ~complexity =
  Llm_backend.resolve_auto_model ~model ~complexity
    ~auto_model:(auto_model ~backend)

let get t ~backend ~model ~effort =
  let key = (backend, model, effort) in
  match Hashtbl.find t.cache key with
  | Some b -> b
  | None ->
      let b = t.factory ~backend ~model ~effort in
      (* [set] (not [add_exn]): the runner forks one daemon fiber per patch
         action, so two fibers may both miss [find] and race to insert the
         same key. Constructing a duplicate backend is harmless — they hold
         the same {process_mgr, clock, timeout, setsid_exec} closure — so
         losing the race just means one of the two backend records is
         garbage. [add_exn] would raise [Duplicate] on the loser. *)
      Hashtbl.set t.cache ~key ~data:b;
      b
