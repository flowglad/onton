(* @archlint.module test
   @archlint.domain failure-subkind *)

open Base
open Onton_core

let gen_init_info =
  let open QCheck2.Gen in
  let gen_opt_string = option (string_small_of printable) in
  let* api_key_source = gen_opt_string in
  let* model = gen_opt_string in
  let* claude_code_version = gen_opt_string in
  return Failure_subkind.{ api_key_source; model; claude_code_version }

let gen_run_classification =
  let open QCheck2.Gen in
  oneof
    [
      map
        (fun msg -> Run_classification.Process_error msg)
        (string_small_of printable);
      pure Run_classification.No_session_to_resume;
      pure Run_classification.Timed_out;
      map
        (fun stream_errors -> Run_classification.Success { stream_errors })
        (string_small_of printable);
      map2
        (fun exit_code detail ->
          Run_classification.Session_failed { exit_code; detail })
        int_small
        (string_small_of printable);
    ]

let prop_classify_total =
  QCheck2.Test.make ~name:"Failure_subkind.classify is total" ~count:500
    QCheck2.Gen.(
      quad gen_run_classification gen_init_info
        (string_small_of printable)
        (string_small_of printable))
    (fun (classification, init, text_tail, stderr_tail) ->
      try
        let _ =
          Failure_subkind.classify ~classification ~init ~text_tail ~stderr_tail
        in
        true
      with _ -> false)

let () =
  let errcode =
    QCheck_base_runner.run_tests ~verbose:true [ prop_classify_total ]
  in
  if errcode <> 0 then Stdlib.exit errcode
