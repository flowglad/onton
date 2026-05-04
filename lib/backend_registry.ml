open Base

type t = {
  factory : backend:string -> model:string option -> Llm_backend.t;
  cache : (string * string option, Llm_backend.t) Hashtbl.t;
}

let display_name_of_claude_model = function
  | Some m -> Printf.sprintf "Claude (%s)" m
  | None -> "Claude"

let make_factory ~process_mgr ~clock ~timeout ~setsid_exec :
    backend:string -> model:string option -> Llm_backend.t =
 fun ~backend ~model ->
  match backend with
  | "claude" ->
      Claude_backend.create
        ~name:(display_name_of_claude_model model)
        ~model ~process_mgr ~clock ~timeout ~setsid_exec
  | "codex" ->
      Codex_backend.create ~model ~process_mgr ~clock ~timeout ~setsid_exec
  | "opencode" ->
      Opencode_backend.create ~model ~process_mgr ~clock ~timeout ~setsid_exec
  | "pi" -> Pi_backend.create ~model ~process_mgr ~clock ~timeout ~setsid_exec
  | "gemini" ->
      Gemini_backend.create ~model ~process_mgr ~clock ~timeout ~setsid_exec
  | other ->
      invalid_arg
        (Printf.sprintf "Backend_registry.get: unknown backend %S" other)

let create ~process_mgr ~clock ~timeout ~setsid_exec =
  {
    factory = make_factory ~process_mgr ~clock ~timeout ~setsid_exec;
    cache = Hashtbl.Poly.create ();
  }

let get t ~backend ~model =
  let key = (backend, model) in
  match Hashtbl.find t.cache key with
  | Some b -> b
  | None ->
      let b = t.factory ~backend ~model in
      Hashtbl.add_exn t.cache ~key ~data:b;
      b
