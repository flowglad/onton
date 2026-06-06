(* @archlint.module interface
   @archlint.domain debug-upload *)

(** Upload project debug state for remote troubleshooting.

    Collects snapshot, event log, config, and gameplan; scrubs secrets; uploads
    to a presigned URL; prints a case ID. *)

val build_bundle : project_name:string -> version:string -> string
(** Build the scrubbed debug bundle JSON without uploading it. Exposed for
    tests. *)

val run : net:_ Eio.Net.t -> project_name:string -> version:string -> unit
