open Base

type result = Llm_backend.result = {
  exit_code : int;
  stdout : string;
  stderr : string;
  got_events : bool;
  saw_final_result : bool;
  timed_out : bool;
}
[@@deriving show, eq, sexp_of, compare]

type start_config = {
  project_name : string;
  worktree : Eio.Fs.dir_ty Eio.Path.t;
  patch_id : Types.Patch_id.t;
  provider : string;
  model : string;
  effort : string;
  gameplan_prompt : string;
  patch_prompt : string;
}

type t =
  | T : {
      name : string;
      timeout : float;
      start : sw:Eio.Switch.t -> start_config -> 'handle;
      prompt :
        'handle ->
        prompt:string ->
        timeout:float ->
        on_event:(Types.Stream_event.t -> unit) ->
        result;
      abort : 'handle -> unit;
      shutdown : 'handle -> unit;
    }
      -> t
