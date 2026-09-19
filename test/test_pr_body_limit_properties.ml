(* @archlint.module test
   @archlint.domain pr-body-limit *)

open Base
open Onton_core
module Gen = QCheck2.Gen
module Test = QCheck2.Test

let gen_body =
  Gen.map2 (fun n c -> String.make n c) (Gen.int_range 0 80_000) Gen.char

let properties =
  [
    Test.make ~count:200 ~name:"PR body limit is total and idempotent" gen_body
      (fun body ->
        try
          let limited = Pr_body_limit.limit body in
          String.length limited <= Pr_body_limit.max_bytes
          && String.equal (Pr_body_limit.limit limited) limited
        with _ -> false);
    Test.make ~count:200 ~name:"short PR bodies are unchanged" gen_body
      (fun body ->
        if String.length body > Pr_body_limit.max_bytes then true
        else String.equal (Pr_body_limit.limit body) body);
    Test.make ~count:200 ~name:"file summaries retain order and count"
      (Gen.list_size (Gen.int_range 0 700)
         (Gen.string_size ~gen:Gen.printable (Gen.int_range 0 30)))
      (fun files ->
        let summary = Pr_body_limit.summarize_files files in
        let visible = Int.min 30 (List.length files) in
        List.equal String.equal
          (List.take summary visible)
          (List.take files visible)
        && List.length summary
           = visible + if List.length files > 30 then 1 else 0);
  ]

let () = List.iter properties ~f:(fun property -> Test.check_exn property)

let () =
  let body = "a" ^ String.concat (List.init 40_000 ~f:(fun _ -> "é")) in
  let limited = Pr_body_limit.limit body in
  if String.length limited > Pr_body_limit.max_bytes then failwith "oversize";
  let prefix_length =
    String.substr_index_exn limited ~pattern:"\n\n[PR description truncated"
  in
  if prefix_length % 2 <> 1 then failwith "split UTF-8 character";
  let end_pos =
    Pr_body_limit.max_bytes
    - String.length
        "\n\n[PR description truncated; see the gameplan for full details.]"
  in
  let mixed_body =
    String.make (end_pos - 2) 'a' ^ "\xffé" ^ String.make 100 'z'
  in
  let mixed_limited = Pr_body_limit.limit mixed_body in
  let mixed_prefix_length =
    String.substr_index_exn mixed_limited
      ~pattern:"\n\n[PR description truncated"
  in
  if mixed_prefix_length <> end_pos - 1 then
    failwith "split UTF-8 character after malformed byte";
  if Char.to_int (String.get mixed_limited (mixed_prefix_length - 1)) <> 0xff
  then failwith "malformed byte was not preserved";
  let fenced_body =
    String.make (end_pos - 20) 'a' ^ "\n```\n" ^ String.make 100 'x'
  in
  let fenced_limited = Pr_body_limit.limit fenced_body in
  let fenced_prefix_length =
    String.substr_index_exn fenced_limited
      ~pattern:"\n\n[PR description truncated"
  in
  if fenced_prefix_length <> end_pos - 19 then
    failwith "truncated inside an open Markdown fence";
  if String.is_substring fenced_limited ~substring:"```" then
    failwith "open Markdown fence retained";
  let files = List.init 616 ~f:(fun i -> Printf.sprintf "file-%d" i) in
  let summary = Pr_body_limit.summarize_files files in
  if List.length summary <> 31 then failwith "missing file summary";
  if not (String.is_substring (List.last_exn summary) ~substring:"586 more")
  then failwith "incorrect omitted count";
  let singular_summary =
    Pr_body_limit.summarize_files
      (List.init 31 ~f:(fun i -> Printf.sprintf "file-%d" i))
  in
  if
    not
      (String.is_substring
         (List.last_exn singular_summary)
         ~substring:"1 more file;")
  then failwith "incorrect singular file summary"
