(* @archlint.module test
   @archlint.domain repo-config *)

open Onton_core

let known_backends = [ "claude"; "codex" ]

let parse_is_total =
  QCheck2.Test.make ~name:"repo config parsing is total" ~count:1000
    QCheck2.Gen.string (fun raw ->
      try
        ignore (Repo_config.parse_string ~known_backends raw);
        true
      with _ -> false)

let positive_automerge_timeout_round_trips =
  QCheck2.Test.make
    ~name:"positive finite automerge timeouts parse without changing value"
    ~count:500
    QCheck2.Gen.(float_range 0.001 10000.)
    (fun timeout ->
      try
        let raw = Printf.sprintf {|{"automerge_timeout":%.17g}|} timeout in
        match Repo_config.parse_string ~known_backends raw with
        | Ok config ->
            Option.equal Float.equal config.Repo_config.automerge_timeout
              (Some timeout)
        | Error _ -> false
      with _ -> false)

let () =
  QCheck2.Test.check_exn parse_is_total;
  QCheck2.Test.check_exn positive_automerge_timeout_round_trips
