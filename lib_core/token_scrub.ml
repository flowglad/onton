(* @archlint.module core
   @archlint.domain token-scrub *)

open Base

let redacted = "<REDACTED>"

let token_pattern_strings =
  [
    "github_pat_[A-Za-z0-9_]+";
    "ghp_[A-Za-z0-9_]+";
    "gho_[A-Za-z0-9_]+";
    "ghu_[A-Za-z0-9_]+";
    "ghs_[A-Za-z0-9_]+";
    "glpat-[A-Za-z0-9_-]+";
    "sk-ant-oat01-[A-Za-z0-9_-]+";
    "sk-ant-api03-[A-Za-z0-9_-]+";
    "sk-ant-[A-Za-z0-9_-]+";
    "sk-[A-Za-z0-9_-]{20,}";
    "xox[baprs]-[A-Za-z0-9-]+";
    "xapp-[A-Za-z0-9-]+";
    "AKIA[0-9A-Z]+";
  ]

let compiled_patterns =
  lazy
    (List.map token_pattern_strings ~f:(fun pattern ->
         try Some (Re.Perl.compile_pat pattern) with _ -> None))

let scrub_token_patterns text =
  List.fold (Lazy.force compiled_patterns) ~init:text ~f:(fun acc pattern ->
      match pattern with
      | None -> acc
      | Some compiled -> Re.replace_string compiled ~all:true ~by:redacted acc)

let sensitive_env_markers = [ "KEY"; "TOKEN"; "SECRET" ]

let redact_env_value_by_name ~name ~value =
  let upper = String.uppercase name in
  if
    List.exists sensitive_env_markers ~f:(fun marker ->
        String.is_substring upper ~substring:marker)
  then redacted
  else scrub_token_patterns value

let redact_env_entry entry =
  match String.lsplit2 entry ~on:'=' with
  | None -> scrub_token_patterns entry
  | Some (name, value) -> name ^ "=" ^ redact_env_value_by_name ~name ~value

let%test "scrub_token_patterns redacts known token formats" =
  let input =
    String.concat ~sep:" "
      [
        "ghp_abcdef123";
        "gho_abcdef123";
        "ghu_abcdef123";
        "ghs_abcdef123";
        "github_pat_abcdef123";
        "glpat-abcdef123";
        "sk-ant-oat01-secret";
        "sk-ant-api03-secret";
        "sk-ant-secret";
        "sk-abcdefghijklmnopqrstuvwxyz123456";
        "xoxb-123-456";
        "xapp-123-456";
        "AKIAABCDEF123456";
      ]
  in
  let output = scrub_token_patterns input in
  String.is_substring output ~substring:redacted
  && (not (String.is_substring output ~substring:"ghp_abcdef123"))
  && (not (String.is_substring output ~substring:"gho_abcdef123"))
  && (not (String.is_substring output ~substring:"ghu_abcdef123"))
  && (not (String.is_substring output ~substring:"ghs_abcdef123"))
  && (not (String.is_substring output ~substring:"github_pat_abcdef123"))
  && (not (String.is_substring output ~substring:"glpat-abcdef123"))
  && (not (String.is_substring output ~substring:"sk-ant-oat01-secret"))
  && (not (String.is_substring output ~substring:"sk-ant-api03-secret"))
  && (not (String.is_substring output ~substring:"sk-ant-secret"))
  && (not
        (String.is_substring output
           ~substring:"sk-abcdefghijklmnopqrstuvwxyz123456"))
  && (not (String.is_substring output ~substring:"xoxb-123-456"))
  && (not (String.is_substring output ~substring:"xapp-123-456"))
  && not (String.is_substring output ~substring:"AKIAABCDEF123456")

let%test "scrub_token_patterns leaves non-token sk-prefix strings alone" =
  String.equal (scrub_token_patterns "sk-engineering") "sk-engineering"

let%test "redact_env_value_by_name masks values by sensitive env name" =
  String.equal
    (redact_env_value_by_name ~name:"ANTHROPIC_API_KEY" ~value:"plain")
    redacted

let%test "redact_env_entry masks values by sensitive env name" =
  String.equal
    (redact_env_entry "CLAUDE_CODE_OAUTH_TOKEN=plain")
    ("CLAUDE_CODE_OAUTH_TOKEN=" ^ redacted)

let%test "leaves ordinary env values intact" =
  String.equal (redact_env_entry "PATH=/bin") "PATH=/bin"
