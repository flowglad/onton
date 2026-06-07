(* @archlint.module test
   @archlint.domain token-scrub *)

open Onton_core

let sensitive_env_entries_are_redacted =
  QCheck2.Test.make ~name:"sensitive env entries are redacted" ~count:200
    QCheck2.Gen.string_small (fun value ->
      String.equal
        (Token_scrub.redact_env_entry ("GITHUB_TOKEN=" ^ value))
        "GITHUB_TOKEN=<REDACTED>")

let token_scrub_public_surface_is_linked =
  QCheck2.Test.make ~name:"token scrub public surface is linked"
    QCheck2.Gen.unit (fun () ->
      ignore Token_scrub.redact_env_value_by_name;
      ignore Token_scrub.scrub_token_patterns;
      true)

let () =
  QCheck2.Test.check_exn sensitive_env_entries_are_redacted;
  QCheck2.Test.check_exn token_scrub_public_surface_is_linked
