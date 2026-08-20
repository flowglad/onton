(* @archlint.module exempt
   @archlint.exempt-reason effect-boundary *)

open Base

type configured_credentials = Github of string | Sourcehut of string * string

let configured_credentials = ref None

let set_github_token token =
  let token = String.strip token in
  if not (String.is_empty token) then
    configured_credentials := Some (Github token)

let set_sourcehut_token ~username token =
  let username = String.strip username in
  let token = String.strip token in
  if not (String.is_empty username || String.is_empty token) then
    configured_credentials := Some (Sourcehut (username, token))

let askpass_script =
  Stdlib.Lazy.from_fun (fun () ->
      let path, oc =
        Stdlib.Filename.open_temp_file
          ~temp_dir:(Stdlib.Filename.get_temp_dir_name ())
          "onton-git-askpass-" ".sh"
      in
      let body =
        {|#!/bin/sh
case "$1" in
  *git.sr.ht*)
    case "$1" in
      *Username*)
        printf '%s\n' "${SRHT_USERNAME:-oauth2}"
        ;;
      *Password*)
        printf '%s\n' "$SRHT_TOKEN"
        ;;
      *)
        printf '\n'
        ;;
    esac
    ;;
  *Username*)
    printf '%s\n' 'x-access-token'
    ;;
  *Password*)
    if [ -n "$GITHUB_TOKEN" ]; then
      printf '%s\n' "$GITHUB_TOKEN"
    elif [ -n "$GH_TOKEN" ]; then
      printf '%s\n' "$GH_TOKEN"
    else
      GH_PROMPT_DISABLED=1 gh auth token 2>/dev/null
    fi
    ;;
  *)
    printf '\n'
    ;;
esac
|}
      in
      Stdlib.Fun.protect
        ~finally:(fun () -> Stdlib.close_out_noerr oc)
        (fun () -> Stdlib.output_string oc body);
      Unix.chmod path 0o700;
      path)

let is_env_binding name s = String.is_prefix s ~prefix:(name ^ "=")

let with_configured_tokens env =
  match !configured_credentials with
  | None -> env
  | Some credentials -> (
      let env =
        List.filter env ~f:(fun s ->
            not
              (is_env_binding "GITHUB_TOKEN" s
              || is_env_binding "GH_TOKEN" s
              || is_env_binding "SRHT_USERNAME" s
              || is_env_binding "SRHT_TOKEN" s))
      in
      match credentials with
      | Github token -> ("GITHUB_TOKEN=" ^ token) :: env
      | Sourcehut (username, token) ->
          ("SRHT_USERNAME=" ^ username) :: ("SRHT_TOKEN=" ^ token) :: env)

let clean_env () =
  let askpass = Stdlib.Lazy.force askpass_script in
  let fixed_env_names =
    [
      "GIT_TERMINAL_PROMPT";
      "GIT_ASKPASS";
      "GCM_INTERACTIVE";
      "GH_PROMPT_DISABLED";
    ]
  in
  Unix.environment () |> Array.to_list
  |> List.filter ~f:(fun s -> not (String.is_prefix s ~prefix:"GIT_"))
  |> List.filter ~f:(fun s ->
      not (List.exists fixed_env_names ~f:(fun name -> is_env_binding name s)))
  |> with_configured_tokens
  (* These fixed bindings are installed after the broad GIT_* scrub so the git
     subprocess receives only the noninteractive Git variables we install. *)
  |> List.append
       [
         "GIT_TERMINAL_PROMPT=0";
         "GIT_ASKPASS=" ^ askpass;
         "GCM_INTERACTIVE=never";
         "GH_PROMPT_DISABLED=1";
       ]
  |> Array.of_list
