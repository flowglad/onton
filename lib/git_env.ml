open Base

let stripped_prefixes =
  [
    "GIT_DIR=";
    "GIT_WORK_TREE=";
    "GIT_INDEX_FILE=";
    "GIT_COMMON_DIR=";
    "GIT_OBJECT_DIRECTORY=";
  ]

let clean_env () =
  Unix.environment () |> Array.to_list
  |> List.filter ~f:(fun s ->
      not
        (List.exists stripped_prefixes ~f:(fun p ->
             String.is_prefix s ~prefix:p)))
  |> Array.of_list
