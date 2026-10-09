(* @archlint.module test
   @archlint.domain test-support *)
open Base
open Onton_core
module B = Branch_reconcile

let commit n =
  match B.Commit.make (Printf.sprintf "%040x" n) with
  | Some revision -> revision
  | None -> QCheck2.Test.fail_reportf "invalid fixture revision"

let step t event = fst (B.step t event)
let reply t result = Publication_fixture.reply ~step ~state:Fn.id t result

let restore t =
  match B.decode (B.yojson_of_t t) with
  | Ok t -> t
  | Error reason -> QCheck2.Test.fail_reportf "checkpoint rejected: %s" reason

let request ?(base = "main") ~name t =
  step t
    (B.Request { base; policy = Rewrite; purpose = Reconcile_request name })

let boundaries t =
  match B.operation t with
  | Some op -> B.observation_boundaries op
  | None -> QCheck2.Test.fail_reportf "missing owner operation"

let observation ~source ~target ~boundary ~noop =
  B.
    {
      head = source;
      source;
      target;
      remote = Some source;
      boundary;
      topology = Equal;
      clean = true;
      sequencer = None;
      conflicts = 0;
      target_included = noop;
      base_contains_source = false;
      completed_integration = false;
      destination = B.Remote_id.of_destination "fixture-origin";
    }

let observe ~source ~target ~boundary ~noop t =
  reply t (B.Observed (observation ~source ~target ~boundary ~noop))

let repair_inspection ~source ~target ~boundary ~sequencer ~conflicts =
  B.Inspected_active
    {
      observation =
        {
          (observation ~source ~target ~boundary ~noop:false) with
          sequencer = Some sequencer;
          conflicts;
          clean = false;
        };
      ready = conflicts = 0;
    }

let complete ~candidate ~noop t =
  let t = reply t B.Pinned in
  let t =
    if noop then t
    else reply t (B.Integrated candidate) |> fun t -> reply t B.Published
  in
  let t = reply t (B.Remote { sha = Some candidate; topology = Equal }) in
  assert (Option.equal B.equal_phase (B.phase t) (Some B.Settled));
  t

let integrate ?(base = "main") ~name ~source ~target ~candidate ~boundary ~noop
    t =
  request ~base ~name t
  |> observe ~source ~target ~boundary ~noop
  |> complete ~candidate ~noop

let selected ~reachable candidates =
  let evidence = List.map reachable ~f:(fun revision -> (revision, true)) in
  let rec choose evidence =
    match B.choose_boundary ~candidates ~evidence with
    | B.Chosen boundary -> boundary
    | B.Probe revision -> choose ((revision, false) :: evidence)
  in
  choose evidence
