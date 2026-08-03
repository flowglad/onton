(* @archlint.module core
   @archlint.domain resolved-config *)

let automerge_timeout_error automerge_timeout =
  if
    (not (Float.is_finite automerge_timeout))
    || Float.compare automerge_timeout 0. <= 0
  then
    Some
      (Printf.sprintf "--automerge-timeout must be finite and > 0 (got %g)"
         automerge_timeout)
  else None
