(* @archlint.module test
   @archlint.domain token-scrub *)

open Onton_core

let sensitive_env_entries_are_redacted =
  QCheck2.Test.make ~name:"sensitive env entries are redacted" ~count:200
    QCheck2.Gen.string_small (fun value ->
      String.equal
        (Token_scrub.redact_env_entry ("GITHUB_TOKEN=" ^ value))
        "GITHUB_TOKEN=<REDACTED>")

let scrub_token_patterns_is_idempotent =
  QCheck2.Test.make ~name:"scrub_token_patterns is idempotent" ~count:200
    QCheck2.Gen.string_small (fun value ->
      let once = Token_scrub.scrub_token_patterns value in
      String.equal once (Token_scrub.scrub_token_patterns once))

let redact_env_value_by_name_masks_sensitive_names =
  QCheck2.Test.make ~name:"redact_env_value_by_name masks sensitive names"
    ~count:200 QCheck2.Gen.string_small (fun value ->
      String.equal
        (Token_scrub.redact_env_value_by_name ~name:"GITHUB_TOKEN" ~value)
        "<REDACTED>")

let () =
  QCheck2.Test.check_exn sensitive_env_entries_are_redacted;
  QCheck2.Test.check_exn scrub_token_patterns_is_idempotent;
  QCheck2.Test.check_exn redact_env_value_by_name_masks_sensitive_names
