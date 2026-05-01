open Base

let clean_env () =
  Unix.environment () |> Array.to_list
  |> List.filter ~f:(fun s -> not (String.is_prefix s ~prefix:"GIT_"))
  |> Array.of_list
