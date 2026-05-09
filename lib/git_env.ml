open Base

let configured_github_token = ref None

let set_github_token token =
  let token = String.strip token in
  if not (String.is_empty token) then configured_github_token := Some token

let askpass_script =
  Stdlib.Lazy.from_fun (fun () ->
      let path =
        Stdlib.Filename.concat
          (Stdlib.Filename.get_temp_dir_name ())
          "onton-git-askpass.sh"
      in
      let body =
        {|#!/bin/sh
case "$1" in
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
      let oc = Stdlib.open_out path in
      Stdlib.Fun.protect
        ~finally:(fun () -> Stdlib.close_out_noerr oc)
        (fun () -> Stdlib.output_string oc body);
      (try Unix.chmod path 0o700 with _ -> ());
      path)

let is_env_binding name s = String.is_prefix s ~prefix:(name ^ "=")

let with_configured_token env =
  match !configured_github_token with
  | None -> env
  | Some token ->
      let env =
        List.filter env ~f:(fun s -> not (is_env_binding "GITHUB_TOKEN" s))
      in
      ("GITHUB_TOKEN=" ^ token) :: env

let clean_env () =
  let askpass = Stdlib.Lazy.force askpass_script in
  Unix.environment () |> Array.to_list
  |> List.filter ~f:(fun s -> not (String.is_prefix s ~prefix:"GIT_"))
  |> with_configured_token
  |> List.append
       [
         "GIT_TERMINAL_PROMPT=0";
         "GIT_ASKPASS=" ^ askpass;
         "GCM_INTERACTIVE=never";
         "GH_PROMPT_DISABLED=1";
       ]
  |> Array.of_list
