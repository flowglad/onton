(* @archlint.module core
   @archlint.domain resource-protocol *)

open Base

type operation =
  | Open
  | Inspect
  | Takeover
  | Launch
  | Observe
  | Stop
  | Replace
  | State_read
  | State_write
  | Write_status
  | Close
  | Log

type request = {
  operation : operation;
  bytes : string;
  request_id : string;
  run_id : string;
}

type status = Ok | Rejected | Pending | Attention

type reason =
  | None_reason
  | Invalid_request
  | Not_found
  | Not_owner
  | Stale_guard
  | Changed_request
  | Closed
  | Closing
  | Chain_busy
  | Predecessor_live
  | Launch_uncertain
  | Observation_exhausted
  | State_version
  | Storage_uncertain
  | Corrupt_object
  | Cleanup_pending
  | Invariant_violation

type response = {
  status : status;
  reason : reason;
  run_id : string option;
  revision : int64 option;
  owner_epoch : int64 option;
  resource_id : string option;
  state_version : int64 option;
  receipt_state_version : int64 option;
  payload : string option;
}

let max_envelope = 16 * 1024 * 1024

let op_name = function
  | Open -> "open"
  | Inspect -> "inspect"
  | Takeover -> "takeover"
  | Launch -> "launch"
  | Observe -> "observe"
  | Stop -> "stop"
  | Replace -> "replace"
  | State_read -> "state-read"
  | State_write -> "state-write"
  | Write_status -> "write-status"
  | Close -> "close"
  | Log -> "log"

let operation = function
  | "open" -> Some Open
  | "inspect" -> Some Inspect
  | "takeover" -> Some Takeover
  | "launch" -> Some Launch
  | "observe" -> Some Observe
  | "stop" -> Some Stop
  | "replace" -> Some Replace
  | "state-read" -> Some State_read
  | "state-write" -> Some State_write
  | "write-status" -> Some Write_status
  | "close" -> Some Close
  | "log" -> Some Log
  | _ -> None

let unique xs =
  let keys = List.map xs ~f:fst in
  List.length keys
  = List.length (List.dedup_and_sort keys ~compare:String.compare)

let rec no_duplicates = function
  | `Assoc fields ->
      unique fields && List.for_all fields ~f:(fun (_, v) -> no_duplicates v)
  | `List xs -> List.for_all xs ~f:no_duplicates
  | _ -> true

let utf8_valid s =
  let rec loop i =
    if i = String.length s then true
    else
      let decoded = Stdlib.String.get_utf_8_uchar s i in
      if not (Stdlib.Uchar.utf_decode_is_valid decoded) then false
      else loop (i + Stdlib.Uchar.utf_decode_length decoded)
  in
  loop 0

let parse s =
  if String.length s > max_envelope then Error "envelope too large"
  else if not (utf8_valid s) then Error "invalid UTF-8"
  else
    try
      let j = Yojson.Safe.from_string s in
      if no_duplicates j then Ok j else Error "duplicate key"
    with _ -> Error "malformed JSON"

let field fields key = List.Assoc.find fields ~equal:String.equal key

let str fields key =
  match field fields key with Some (`String s) -> Some s | _ -> None

let identifier s =
  let n = String.length s in
  n > 0 && n <= 256
  && not
       (String.exists s ~f:(fun c ->
            Char.equal c '\000' || Char.equal c '\r' || Char.equal c '\n'))

let counter s =
  let n = String.length s in
  n > 0
  && (String.equal s "0"
     || ((not (Char.equal s.[0] '0')) && String.for_all s ~f:Char.is_digit))
  && Option.is_some (Int64.of_string_opt s)
  &&
  match Int64.of_string_opt s with
  | Some i -> Int64.(i >= 0L)
  | None -> false

let keys fields allowed =
  List.for_all fields ~f:(fun (k, _) -> List.mem allowed k ~equal:String.equal)

let b64_size s =
  let n = String.length s in
  if n % 4 <> 0 then None
  else
    let pad =
      if n > 0 && Char.equal s.[n - 1] '=' then
        if n > 1 && Char.equal s.[n - 2] '=' then 2 else 1
      else 0
    in
    if
      String.for_alli s ~f:(fun i c ->
          if i >= n - pad then Char.equal c '='
          else Char.is_alphanum c || Char.equal c '+' || Char.equal c '/')
    then
      let size = (n / 4 * 3) - pad in
      (* Reject noncanonical unused bits. *)
      let value c =
        match c with
        | 'A' .. 'Z' -> Char.to_int c - 65
        | 'a' .. 'z' -> Char.to_int c - 71
        | '0' .. '9' -> Char.to_int c + 4
        | '+' -> 62
        | '/' -> 63
        | _ -> 0
      in
      if
        (pad = 1 && value s.[n - 2] land 3 <> 0)
        || (pad = 2 && value s.[n - 3] land 15 <> 0)
      then None
      else Some size
    else None

let valid_manifest = function
  | `Assoc f
    when unique f && keys f [ "executable"; "args"; "preparation"; "inputs" ] ->
      let args =
        match field f "args" with
        | None -> true
        | Some (`List xs) ->
            List.length xs <= 128
            && List.for_all xs ~f:(function
              | `String s -> String.length s <= 4096
              | _ -> false)
        | _ -> false
      in
      let inputs =
        match field f "inputs" with
        | None -> true
        | Some (`List xs) ->
            let paths = ref [] and size = ref 0 in
            List.length xs <= 256
            && List.for_all xs ~f:(function
              | `Assoc entry when unique entry && keys entry [ "path"; "data" ]
                -> (
                  match (str entry "path", str entry "data") with
                  | Some p, Some data -> (
                      let parts = String.split p ~on:'/' in
                      let clean =
                        String.length p > 0
                        && (not (String.is_prefix p ~prefix:"/"))
                        && List.for_all parts ~f:(fun x ->
                            not
                              (String.is_empty x || String.equal x "."
                             || String.equal x ".." || String.contains x '\000'
                              ))
                        && not (List.mem !paths p ~equal:String.equal)
                      in
                      paths := p :: !paths;
                      match b64_size data with
                      | Some n ->
                          size := !size + n;
                          clean && !size <= 8 * 1024 * 1024
                      | None -> false)
                  | _ -> false)
              | _ -> false)
        | _ -> false
      in
      (match str f "executable" with Some s -> identifier s | _ -> false)
      && (match field f "preparation" with
        | None -> true
        | Some (`String s) -> identifier s
        | _ -> false)
      && args && inputs
  | _ -> false

let make_request bytes =
  match parse bytes with
  | Error e -> Error e
  | Ok (`Assoc f) -> (
      let required =
        [ "version"; "op"; "run_id"; "request_id"; "coordinator_id" ]
      in
      match str f "op" |> Option.bind ~f:operation with
      | None -> Error "unknown operation"
      | Some op ->
          let extra =
            match op with
            | Open -> [ "config_digest" ]
            | Launch ->
                [
                  "chain_id";
                  "patch_id";
                  "resource_id";
                  "launch_digest";
                  "manifest";
                ]
            | Replace ->
                [
                  "chain_id";
                  "patch_id";
                  "resource_id";
                  "predecessor_id";
                  "launch_digest";
                  "manifest";
                ]
            | Observe | Stop -> [ "resource_id" ]
            | State_write -> [ "write_id"; "state_version"; "payload" ]
            | Write_status -> [ "write_id" ]
            | Log -> [ "message"; "patch_id"; "resource_id" ]
            | Inspect | Takeover | State_read | Close -> []
          in
          let required_extra =
            List.filter extra ~f:(fun x ->
                not
                  (String.equal x "payload"
                  || Poly.equal op Log
                     && List.mem
                          [ "patch_id"; "resource_id" ]
                          x ~equal:String.equal))
          in
          let guards =
            if Poly.equal op Open then [] else [ "owner_epoch"; "revision" ]
          in
          let ids =
            List.filter (required @ required_extra) ~f:(fun x ->
                not
                  (List.mem
                     [ "version"; "op"; "manifest"; "message"; "state_version" ]
                     x ~equal:String.equal))
          in
          let valid =
            keys f (required @ extra @ guards)
            && List.for_all ids ~f:(fun k ->
                Option.value_map (str f k) ~default:false ~f:identifier)
            && List.for_all guards ~f:(fun k ->
                Option.value_map (str f k) ~default:false ~f:counter)
            && String.equal
                 (Option.value (str f "version") ~default:"")
                 "resource-v1"
            && (if Poly.equal op State_write then
                  Option.value_map (str f "state_version") ~default:false
                    ~f:counter
                  &&
                  match field f "payload" with
                  | None -> true
                  | Some (`String s) ->
                      Option.value_map (b64_size s) ~default:false ~f:(fun n ->
                          n <= 8 * 1024 * 1024)
                  | _ -> false
                else true)
            && (if Poly.equal op Launch || Poly.equal op Replace then
                  Option.value_map (field f "manifest") ~default:false
                    ~f:valid_manifest
                else true)
            &&
            if Poly.equal op Log then
              Option.value_map (str f "message") ~default:false ~f:(fun s ->
                  String.length s > 0 && String.length s <= 4096)
              && List.for_all [ "patch_id"; "resource_id" ] ~f:(fun k ->
                  match field f k with
                  | None -> true
                  | Some (`String s) -> identifier s
                  | _ -> false)
            else true
          in
          if valid then
            Ok
              {
                operation = op;
                bytes;
                request_id = Option.value (str f "request_id") ~default:"";
                run_id = Option.value (str f "run_id") ~default:"";
              }
          else Error "invalid request")
  | Ok _ -> Error "expected object"

let encode_request r = Result.map (make_request r.bytes) ~f:(fun _ -> r.bytes)

let status = function
  | "ok" -> Some (Ok : status)
  | "rejected" -> Some Rejected
  | "pending" -> Some Pending
  | "attention" -> Some Attention
  | _ -> None

let reason = function
  | "none" -> Some None_reason
  | "invalid_request" -> Some Invalid_request
  | "not_found" -> Some Not_found
  | "not_owner" -> Some Not_owner
  | "stale_guard" -> Some Stale_guard
  | "changed_request" -> Some Changed_request
  | "closed" -> Some Closed
  | "closing" -> Some Closing
  | "chain_busy" -> Some Chain_busy
  | "predecessor_live" -> Some Predecessor_live
  | "launch_uncertain" -> Some Launch_uncertain
  | "observation_exhausted" -> Some Observation_exhausted
  | "state_version" -> Some State_version
  | "storage_uncertain" -> Some Storage_uncertain
  | "corrupt_object" -> Some Corrupt_object
  | "cleanup_pending" -> Some Cleanup_pending
  | "invariant_violation" -> Some Invariant_violation
  | _ -> None

let number = function
  | `Int n when n >= 0 -> Some (Int64.of_int n)
  | `Intlit s when counter s -> Int64.of_string_opt s
  | _ -> None

let decode_response bytes =
  match parse bytes with
  | Error e -> Error e
  | Ok (`Assoc f) -> (
      let allowed =
        [
          "status";
          "reason";
          "run_id";
          "revision";
          "owner_epoch";
          "resource_id";
          "state_version";
          "receipt_state_version";
          "payload";
        ]
      in
      let nums =
        [ "revision"; "owner_epoch"; "state_version"; "receipt_state_version" ]
      in
      let valid =
        keys f allowed
        && List.for_all nums ~f:(fun k ->
            match field f k with
            | None -> true
            | Some x -> Option.is_some (number x))
        && List.for_all [ "run_id"; "resource_id" ] ~f:(fun k ->
            match field f k with
            | None -> true
            | Some (`String s) -> identifier s
            | _ -> false)
        &&
        match field f "payload" with
        | None -> true
        | Some (`String s) ->
            Option.value_map (b64_size s) ~default:false ~f:(fun n ->
                n <= 8 * 1024 * 1024)
        | _ -> false
      in
      match
        ( str f "status" |> Option.bind ~f:status,
          str f "reason" |> Option.bind ~f:reason )
      with
      | Some status, Some reason
        when valid
             &&
             match field f "payload" with
             | None -> true
             | Some _ -> Poly.equal status Ok ->
          let num k = Option.bind (field f k) ~f:number in
          Ok
            {
              status;
              reason;
              run_id = str f "run_id";
              revision = num "revision";
              owner_epoch = num "owner_epoch";
              resource_id = str f "resource_id";
              state_version = num "state_version";
              receipt_state_version = num "receipt_state_version";
              payload = str f "payload";
            }
      | _ -> Error "invalid response")
  | Ok _ -> Error "expected object"

let retirement_evidence op ~process_exit ~matching r =
  process_exit = 0 && matching
  && Poly.equal r.status (Ok : status)
  && (Poly.equal op Stop || Poly.equal op Replace)
