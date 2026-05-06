open Base

type handle = unit

type result = {
  exit_code : int;
  stdout : string;
  stderr : string;
  got_events : bool;
  saw_final_result : bool;
  timed_out : bool;
}
[@@deriving show, eq, sexp_of, compare]

type t = {
  name : string;
  start :
    process_mgr:[ `Process_mgr | `Platform of [ `Generic ] ] Eio.Resource.t ->
    clock:[ `Clock of float ] Eio.Resource.t ->
    timeout:float ->
    cwd:Eio.Fs.dir_ty Eio.Path.t ->
    env:string array ->
    project_name:string ->
    patch_id:Types.Patch_id.t ->
    gameplan:string ->
    patch:string ->
    resume_session:string option ->
    complexity:int option ->
    handle;
  prompt :
    handle ->
    process_mgr:[ `Process_mgr | `Platform of [ `Generic ] ] Eio.Resource.t ->
    clock:[ `Clock of float ] Eio.Resource.t ->
    timeout:float ->
    cwd:Eio.Fs.dir_ty Eio.Path.t ->
    env:string array ->
    prompt:string ->
    on_event:(Types.Stream_event.t -> unit) ->
    result;
  abort : handle -> unit;
  shutdown : handle -> unit;
}

let not_implemented () = raise (Failure "not implemented; see patch 4")

let placeholder ~name =
  {
    name;
    start =
      (fun ~process_mgr:_
        ~clock:_
        ~timeout:_
        ~cwd:_
        ~env:_
        ~project_name:_
        ~patch_id:_
        ~gameplan:_
        ~patch:_
        ~resume_session:_
        ~complexity:_
      -> not_implemented ());
    prompt =
      (fun _
        ~process_mgr:_
        ~clock:_
        ~timeout:_
        ~cwd:_
        ~env:_
        ~prompt:_
        ~on_event:_
      -> not_implemented ());
    abort = (fun _ -> not_implemented ());
    shutdown = (fun _ -> not_implemented ());
  }
