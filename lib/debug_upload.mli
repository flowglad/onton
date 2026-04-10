(** Upload project debug state for remote troubleshooting.

    Collects snapshot, event log, config, and gameplan; scrubs secrets; uploads
    to a presigned URL; prints a case ID. *)

val run : net:_ Eio.Net.t -> project_name:string -> version:string -> unit
