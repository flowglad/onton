(* @archlint.module core
   @archlint.domain scheduler-recovery *)
open Base
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type kind =
  | Forge_revision
  | Ci_evidence
  | Queue_removal
  | Native_stack
  | Pr_base
  | Pr_draft
  | Merge_attempt
[@@deriving eq, compare, sexp_of, yojson]

type scope = {
  head : string option;
  base : string option;
  pr : int option;
  publication : string option;
  queue : string option; [@yojson.default None]
}
[@@deriving eq, compare, sexp_of, yojson]

type ticket = { id : int; kind : kind; scope : scope }
[@@deriving eq, compare, sexp_of, yojson]

type phase =
  | Waiting
  | Requested of ticket
  | Running of ticket
  | Repaired
  | Attention
[@@deriving eq, compare, sexp_of, yojson]

type entry = {
  kind : kind;
  scope : scope;
  since : float;
  failures : int;
  progress : string option; [@yojson.default None]
  phase : phase;
}
[@@deriving eq, compare, sexp_of, yojson]

type t = { next : int; now : float; entries : entry list }
[@@deriving eq, compare, sexp_of, yojson]

let empty = { next = 1; now = 0.; entries = [] }
let finite x = if Float.is_finite x then Float.max 0. x else 0.

let grace = function
  | Ci_evidence -> 900.
  | Native_stack | Merge_attempt -> 0.
  | Forge_revision | Queue_removal | Pr_base | Pr_draft -> 120.

let label = function
  | Forge_revision -> "forge revision/publication evidence"
  | Ci_evidence -> "missing or stalled CI evidence"
  | Queue_removal -> "unresolved merge queue removal"
  | Native_stack -> "unsupported native stack"
  | Pr_base -> "PR base update"
  | Pr_draft -> "PR draft update"
  | Merge_attempt -> "repeated merge API failures"

let prompt kind =
  "Repair the scheduler blockage: " ^ label kind
  ^ ". Inspect the current Git and forge state, diagnose and repair the cause, \
     and validate the result. Preserve existing work and user settings. Do not \
     fabricate CI, review, conflict, or revision evidence. For unsupported \
     native stacks, repair the stack using supported forge operations or \
     explain the exact action and permission the operator needs. For stalled \
     CI, inspect workflow triggers and jobs and rerun or repair them as \
     appropriate. For queue removal, diagnose the removal and make this PR \
     ready to retry. Report the repair and any remaining external requirement. \
     Your accepted completion is retained; the scheduler will not require the \
     old commit ancestry to be recreated."

let pending t =
  List.find_map t.entries ~f:(fun e ->
      match e.phase with
      | Requested ticket -> Some ticket
      | Waiting | Running _ | Repaired | Attention -> None)

let attention t =
  List.find_map t.entries ~f:(fun e ->
      if equal_phase e.phase Attention then
        Some
          ("scheduler_repair_required: " ^ label e.kind
         ^ "; send repair guidance to retry")
      else None)

(* Track provider-visible run progress, not poll frequency or mutable links and
   descriptions. Unknown conclusions never get a renewed grace from another job.
   The observed interval also covers missing/malformed provider timestamps. *)
let ci_progress checks =
  let active check =
    List.mem
      [ "pending"; "queued"; "in_progress"; "waiting"; "requested" ]
      check.Types.Ci_check.conclusion ~equal:String.equal
  in
  if
    (not (List.is_empty checks))
    && List.for_all checks ~f:(fun check ->
        active check || Types.Ci_check.is_success check)
  then
    let progress =
      List.map checks ~f:(fun check ->
          ( check.Types.Ci_check.name,
            check.Types.Ci_check.id,
            check.Types.Ci_check.check_suite_id,
            check.Types.Ci_check.conclusion,
            check.Types.Ci_check.started_at ))
      |> List.dedup_and_sort
           ~compare:
             [%compare:
               string * int option * int option * string * string option]
    in
    Some
      (Sexp.to_string
         ([%sexp_of:
            (string * int option * int option * string * string option) list]
            progress))
  else None

let observe ?(ci_checks = []) t ~now ~scope ~blocked =
  let ci_progress = ci_progress ci_checks in
  let now = finite now in
  let kinds = List.dedup_and_sort blocked ~compare:compare_kind in
  let entries, next =
    List.fold kinds ~init:([], t.next) ~f:(fun (entries, next) kind ->
        let prior = List.find t.entries ~f:(fun e -> equal_kind e.kind kind) in
        let e =
          match prior with
          | None ->
              {
                kind;
                scope;
                since = now;
                failures = 0;
                progress = None;
                phase = Waiting;
              }
          | Some e when equal_scope e.scope scope -> e
          | Some e
            when equal_phase e.phase Repaired
                 && Option.equal Int.equal e.scope.pr scope.pr
                 && Option.equal String.equal e.scope.base scope.base
                 && Option.equal String.equal e.scope.queue scope.queue
                 && Option.is_some e.scope.publication
                 && Option.equal String.equal e.scope.publication scope.head
                 && Option.is_none scope.publication ->
              (* Acknowledging the repair's published revision is the same
                 result, rather than a new external revision to diagnose. *)
              { e with scope }
          | Some e
            when equal_phase e.phase Attention
                 && Option.equal Int.equal e.scope.pr scope.pr
                 && Option.equal String.equal e.scope.queue scope.queue ->
              { e with scope; since = now }
          | Some _ ->
              {
                kind;
                scope;
                since = now;
                failures = 0;
                progress = None;
                phase = Waiting;
              }
        in
        let e =
          if equal_kind kind Ci_evidence then
            let advanced =
              Option.is_some ci_progress
              && not (Option.equal String.equal e.progress ci_progress)
            in
            let since =
              match e.phase with
              | (Waiting | Repaired) when advanced -> now
              | Waiting | Requested _ | Running _ | Repaired | Attention ->
                  e.since
            in
            {
              e with
              since;
              progress = Option.first_some ci_progress e.progress;
            }
          else e
        in
        let e = { e with since = Float.min e.since now } in
        let delay =
          match e.phase with
          | Repaired -> Float.max 120. (grace kind)
          | Waiting | Requested _ | Running _ | Attention -> grace kind
        in
        let due = e.failures >= 2 || Float.(now -. e.since >= delay) in
        let e, next =
          match e.phase with
          | Waiting when due ->
              if next = Int.max_value then ({ e with phase = Attention }, next)
              else
                ( { e with phase = Requested { id = next; kind; scope } },
                  next + 1 )
          | Repaired when due -> ({ e with phase = Attention }, next)
          | Waiting | Requested _ | Running _ | Repaired | Attention -> (e, next)
        in
        (e :: entries, next))
  in
  let entries = List.rev entries in
  if next = t.next && List.equal equal_entry entries t.entries then t
  else { next; now; entries }

let failed t ~now ~scope ~kind =
  let fresh =
    {
      kind;
      scope;
      since = finite now;
      failures = 0;
      progress = None;
      phase = Waiting;
    }
  in
  let entries =
    if List.exists t.entries ~f:(fun e -> equal_kind e.kind kind) then
      List.map t.entries ~f:(fun e ->
          if equal_kind e.kind kind && not (equal_scope e.scope scope) then
            fresh
          else e)
    else fresh :: t.entries
  in
  let t = { t with entries } in
  {
    t with
    entries =
      List.map t.entries ~f:(fun e ->
          if equal_kind e.kind kind then
            { e with failures = Int.min 2 (e.failures + 1) }
          else e);
  }

let is_running t ticket =
  List.exists t.entries ~f:(fun e ->
      match e.phase with
      | Running current -> equal_ticket current ticket
      | Waiting | Requested _ | Repaired | Attention -> false)

let claim t ticket =
  {
    t with
    entries =
      List.map t.entries ~f:(fun e ->
          match e.phase with
          | Requested current when equal_ticket current ticket ->
              { e with phase = Running ticket }
          | Waiting | Requested _ | Running _ | Repaired | Attention -> e);
  }

let complete ?now ?scope t ticket ~success =
  if not (is_running t ticket) then t
  else
    let now = Option.value_map now ~default:t.now ~f:finite in
    {
      t with
      now;
      entries =
        List.map t.entries ~f:(fun e ->
            match e.phase with
            | Running current when equal_ticket current ticket ->
                {
                  e with
                  scope = Option.value scope ~default:e.scope;
                  since = now;
                  failures = 0;
                  phase = (if success then Repaired else Attention);
                }
            | Waiting | Requested _ | Running _ | Repaired | Attention -> e);
    }

let recover t ~running =
  {
    t with
    entries =
      List.map t.entries ~f:(fun e ->
          match e.phase with
          | Running ticket
            when not (Option.exists running ~f:(equal_ticket ticket)) ->
              { e with phase = Requested ticket }
          | Waiting | Requested _ | Running _ | Repaired | Attention -> e);
  }

let resolve t kind =
  {
    t with
    entries = List.filter t.entries ~f:(fun e -> not (equal_kind e.kind kind));
  }

let contains t kind = List.exists t.entries ~f:(fun e -> equal_kind e.kind kind)

let write_allowed t kind =
  match List.find t.entries ~f:(fun e -> equal_kind e.kind kind) with
  | None -> true
  | Some e -> (
      match e.phase with
      | Waiting | Repaired -> e.failures < 2
      | Requested _ | Running _ | Attention -> false)

let resume t =
  {
    t with
    entries =
      List.map t.entries ~f:(fun e ->
          if equal_phase e.phase Attention then
            { e with phase = Waiting; failures = 2 }
          else e);
  }

let valid_ticket ticket = ticket.id > 0

let decode_ticket json =
  try
    let ticket = ticket_of_yojson json in
    if valid_ticket ticket then Some ticket else None
  with _ -> None

let decode json =
  try
    let t = t_of_yojson json in
    let kinds = List.map t.entries ~f:(fun e -> e.kind) in
    if
      t.next > 0 && Float.is_finite t.now
      && Float.(t.now >= 0.)
      && List.length t.entries <= 7
      && (not (List.contains_dup kinds ~compare:compare_kind))
      && List.for_all t.entries ~f:(fun e ->
          Float.is_finite e.since
          && Float.(e.since >= 0.)
          && e.failures >= 0 && e.failures <= 2
          &&
          match e.phase with
          | Requested ticket | Running ticket ->
              valid_ticket ticket && ticket.id < t.next
              && equal_kind ticket.kind e.kind
              && equal_scope ticket.scope e.scope
          | Waiting | Repaired | Attention -> true)
    then t
    else empty
  with _ -> empty
