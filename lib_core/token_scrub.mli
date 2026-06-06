(* @archlint.module interface
   @archlint.domain token-scrub *)

val scrub_token_patterns : string -> string
val redact_env_value_by_name : name:string -> value:string -> string
val redact_env_entry : string -> string
