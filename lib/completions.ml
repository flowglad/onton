open Base

type t = { display : string; full : string }

(* Split buffer into leading digits and the rest *)
let split_digits buf =
  let len = String.length buf in
  let rec find_end i =
    if i >= len then i
    else if Char.is_digit (String.get buf i) then find_end (i + 1)
    else i
  in
  let n = find_end 0 in
  (String.prefix buf n, String.drop_prefix buf n)

let complete ~buffer ~patch_ids =
  if String.is_empty buffer then
    let patch_completions =
      List.map patch_ids ~f:(fun id ->
          { display = id ^ "> "; full = id ^ "> " })
    in
    [
      { display = "+<PR number>"; full = "+" };
      { display = "w <path>"; full = "w " };
      { display = "-"; full = "-" };
    ]
    @ patch_completions
  else if String.equal buffer "+" then
    [ { display = "+<PR number>"; full = "+" } ]
  else if String.equal buffer "w" then [ { display = "w <path>"; full = "w " } ]
  else if String.is_prefix buffer ~prefix:"w " then []
  else if String.equal buffer "-" then []
  else
    let digits, rest = split_digits buffer in
    if String.is_empty digits then
      (* No leading digits — not a recognized pattern *)
      []
    else if String.is_empty rest then
      (* Pure digits: offer matching patch IDs, exact match first *)
      let exact, others =
        List.partition_tf patch_ids ~f:(fun id -> String.equal id digits)
      in
      List.filter_map (exact @ others) ~f:(fun id ->
          if String.is_prefix id ~prefix:digits then
            let tail = String.drop_prefix id (String.length digits) in
            Some { display = tail ^ "> "; full = id ^ "> " }
          else None)
    else if String.equal rest ">" then
      (* "N>" — complete to "N> " only if N is a valid patch ID *)
      if List.mem patch_ids digits ~equal:String.equal then
        [ { display = " "; full = digits ^ "> " } ]
      else []
    else if String.is_prefix rest ~prefix:"> " then
      (* "N> something" — free-form, no completions *)
      []
    else []

let accept_first ~buffer ~completions =
  match completions with [] -> buffer | first :: _ -> first.full

let%test_unit "complete digit prefix matches patch ids" =
  let result = complete ~buffer:"1" ~patch_ids:[ "1"; "10"; "2" ] in
  let fulls = List.map result ~f:(fun c -> c.full) in
  [%test_eq: string list] fulls [ "1> "; "10> " ]

let%test_unit "complete with message after redirect returns empty" =
  let result = complete ~buffer:"1> hello" ~patch_ids:[ "1" ] in
  [%test_eq: string list] (List.map result ~f:(fun c -> c.full)) []

let%test_unit "accept_first replaces buffer" =
  let result =
    accept_first ~buffer:"1" ~completions:[ { display = "> "; full = "1> " } ]
  in
  [%test_eq: string] result "1> "

let%test_unit "accept_first returns buffer when no completions" =
  [%test_eq: string] (accept_first ~buffer:"hello" ~completions:[]) "hello"

let%test_unit "empty buffer offers all patch ids" =
  let result = complete ~buffer:"" ~patch_ids:[ "1"; "2" ] in
  let fulls = List.map result ~f:(fun c -> c.full) in
  assert (List.mem fulls "1> " ~equal:String.equal);
  assert (List.mem fulls "2> " ~equal:String.equal)

let%test_unit "plus sign shows hint" =
  match complete ~buffer:"+" ~patch_ids:[] with
  | [ first ] -> [%test_eq: string] first.full "+"
  | _ -> failwith "expected exactly one completion"

let%test_unit "w shows hint" =
  match complete ~buffer:"w" ~patch_ids:[] with
  | [ first ] -> [%test_eq: string] first.full "w "
  | _ -> failwith "expected exactly one completion"

let%test_unit "w with path returns empty" =
  let result = complete ~buffer:"w src/foo.ml" ~patch_ids:[] in
  [%test_eq: string list] (List.map result ~f:(fun c -> c.full)) []

let%test_unit "dash returns empty" =
  let result = complete ~buffer:"-" ~patch_ids:[] in
  [%test_eq: string list] (List.map result ~f:(fun c -> c.full)) []

let%test_unit "N> completes to N> with space" =
  let result = complete ~buffer:"5>" ~patch_ids:[ "5" ] in
  [%test_eq: string list] (List.map result ~f:(fun c -> c.full)) [ "5> " ]

let%test_unit "N> with invalid patch id returns empty" =
  let result = complete ~buffer:"99>" ~patch_ids:[ "1"; "2" ] in
  [%test_eq: string list] (List.map result ~f:(fun c -> c.full)) []

let%test_unit "accept_first on empty buffer returns command hint not patch id" =
  let completions = complete ~buffer:"" ~patch_ids:[ "1"; "2" ] in
  [%test_eq: string] (accept_first ~buffer:"" ~completions) "+"

let%test_unit "exact patch id sorts before longer prefix" =
  let result = complete ~buffer:"1" ~patch_ids:[ "10"; "1" ] in
  let fulls = List.map result ~f:(fun c -> c.full) in
  [%test_eq: string list] fulls [ "1> "; "10> " ]
