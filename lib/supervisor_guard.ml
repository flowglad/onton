(* @archlint.module shell
   @archlint.domain supervisor *)

open Base

exception
  Fatal_supervisor_error of {
    name : string;
    reason : Supervisor_decision.fatal_reason;
    message : string;
  }

let raise_fatal ~log ~name reason =
  let message = Supervisor_decision.message ~name reason in
  log message;
  raise (Fatal_supervisor_error { name; reason; message })

let handle_decision ~log = function
  | Supervisor_decision.Normal_return | Normal_quit -> ()
  | Propagate_cancel -> ()
  | Fatal { name; reason } -> raise_fatal ~log ~name reason

let wrap ?(quit_is_normal = false) ?(return_is_normal = false) ~name
    ~is_normal_quit ~log f () =
  let return_policy =
    if return_is_normal then Supervisor_decision.Return_is_normal
    else Return_is_fatal
  in
  match f () with
  | () ->
      Supervisor_decision.classify ~name ~quit_is_normal ~return_policy
        Supervisor_decision.Returned
      |> handle_decision ~log
  | exception exn when is_normal_quit exn && quit_is_normal -> raise exn
  | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
  | exception exn ->
      Supervisor_decision.classify ~name ~quit_is_normal ~return_policy
        (Supervisor_decision.Raised (Exn.to_string exn))
      |> handle_decision ~log
