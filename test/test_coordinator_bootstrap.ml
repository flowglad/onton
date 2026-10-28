(* @archlint.module test
   @archlint.domain coordinator *)

open Onton

[@@@warning "-40-42"]

module F = Onton_test_support.Coordinator_store_fixture

let check yes why = if not yes then failwith why
let id = ref 0

let next () =
  incr id;
  "bootstrap-" ^ string_of_int !id

let () =
  let f = F.fixture () in
  let opens = ref 0 in
  let transport ~argv ~input =
    let fields =
      match Yojson.Safe.from_string input with `Assoc xs -> xs | _ -> []
    in
    match F.string "op" fields with
    | "open" ->
        incr opens;
        check (F.string "config_digest" fields = "cfg") "changed open digest";
        Ok
          ( 0,
            {|{"status":"ok","reason":"none","run_id":"r","owner_epoch":1,"revision":1}|},
            "" )
    | "takeover" ->
        check (F.string "owner_epoch" fields = "1") "wrong predecessor epoch";
        check (F.string "revision" fields = "1") "wrong predecessor revision";
        Ok
          ( 0,
            {|{"status":"ok","reason":"none","run_id":"r","owner_epoch":1,"revision":1}|},
            "" )
    | _ -> F.transport f ~argv ~input
  in
  let client = Resource_client.create ~helper:"fixture" ~transport in
  let start ?(config = "config") ?(fresh = true) ?(guards = None) () =
    Coordinator_bootstrap.start ~client ~run_id:"r" ~coordinator_id:"c"
      ~config_id:"cfg" ~plan_id:"plan" ~plan_bytes:"plan" ~config_bytes:config
      ~capacity:2 ~fresh ~takeover_guards:guards ~next_id:next
  in
  let first = match start () with Ok r -> r | Error why -> failwith why in
  check
    (Coordinator_store.version first.envelope = 1L)
    "initial write not confirmed";
  check (List.length f.writes = 1) "initial state not committed exactly once";
  check (Result.is_ok (start ())) "same run did not restore";
  check (List.length f.writes = 1) "restart rewrote state";
  check
    (Result.is_error (start ~config:"changed" ()))
    "config mismatch accepted";
  check
    (Result.is_error
       (Coordinator_bootstrap.start ~client ~run_id:"r" ~coordinator_id:"c"
          ~config_id:"cfg" ~plan_id:"plan" ~plan_bytes:"plan"
          ~config_bytes:"config" ~capacity:3 ~fresh:true ~takeover_guards:None
          ~next_id:next))
    "changed capacity accepted";
  check (Result.is_error (start ~fresh:false ())) "unguarded takeover accepted";
  check
    (Result.is_ok (start ~fresh:false ~guards:(Some (1L, 1L)) ()))
    "confirmed guarded takeover lost existing state";
  check
    (Result.is_error (start ~guards:(Some (1L, 1L)) ()))
    "open accepted takeover guards";
  f.payload <- None;
  f.version <- 0L;
  f.revision <- 1;
  f.writes <- [];
  f.drop_write <- true;
  check
    (Result.is_ok (start ()))
    "committed lost response not resolved by receipt/readback";
  f.payload <- None;
  f.version <- 0L;
  f.revision <- 1;
  f.writes <- [];
  f.refuse_receipt <- true;
  check (Result.is_error (start ())) "unconfirmed write receipt accepted";
  check (!opens = 6) "helper open not used for every fresh admission";
  print_endline
    "coordinator bootstrap: exact identity, confirmed initial commit, guarded \
     takeover"
