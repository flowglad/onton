(* @archlint.module core
   @archlint.domain sourcehut-builds *)

open Base

type job = {
  id : int;
  status : string;
  note : string;
  tags : string list;
  created : string option;
  owner : string;
  manifest : string;
  visibility : string;
  logs : string list;
}
[@@deriving show, eq]

let member name = function
  | `Assoc fields -> List.Assoc.find fields ~equal:String.equal name
  | _ -> None

let string = function Some (`String value) -> value | _ -> ""
let int = function Some (`Int value) -> value | _ -> 0

let string_option = function
  | Some (`String value) -> Some value
  | Some `Null | None | Some _ -> None

let strings = function
  | Some (`List values) ->
      List.filter_map values ~f:(function
        | `String value -> Some value
        | _ -> None)
  | Some _ | None -> []

let log_of_json json =
  string (Option.bind (member "log" json) ~f:(member "last128KiB"))

let job_of_json json =
  let owner =
    string (Option.bind (member "owner" json) ~f:(member "canonicalName"))
  in
  let task_logs =
    match member "tasks" json with
    | Some (`List tasks) -> List.map tasks ~f:log_of_json
    | Some _ | None -> []
  in
  {
    id = int (member "id" json);
    status = string (member "status" json);
    note = string (member "note" json);
    tags = strings (member "tags" json);
    created = string_option (member "created" json);
    owner;
    manifest = string (member "manifest" json);
    visibility = string (member "visibility" json);
    logs =
      log_of_json json :: task_logs |> List.filter ~f:(Fn.non String.is_empty);
  }

let graphql_errors json =
  match member "errors" json with
  | Some (`List errors) ->
      List.filter_map errors ~f:(fun error ->
          match member "message" error with
          | Some (`String s) -> Some s
          | _ -> None)
  | Some _ | None -> []

let parse body =
  try Ok (Yojson.Safe.from_string body)
  with Yojson.Json_error message -> Error message

let jobs_of_response body =
  match parse body with
  | Error _ as error -> error
  | Ok json -> (
      match graphql_errors json with
      | _ :: _ as errors -> Error (String.concat errors ~sep:"; ")
      | [] -> (
          let jobs = Option.bind (member "data" json) ~f:(member "jobs") in
          let results = Option.bind jobs ~f:(member "results") in
          let cursor = Option.bind jobs ~f:(member "cursor") |> string_option in
          match results with
          | Some (`List values) -> Ok (List.map values ~f:job_of_json, cursor)
          | Some _ | None -> Error "response has no data.jobs.results list"))

let job_of_response body =
  match parse body with
  | Error _ as error -> error
  | Ok json -> (
      match graphql_errors json with
      | _ :: _ as errors -> Error (String.concat errors ~sep:"; ")
      | [] -> (
          match Option.bind (member "data" json) ~f:(member "job") with
          | Some (`Assoc _ as value) -> Ok (job_of_json value)
          | Some _ | None -> Error "response has no data.job object"))

let submit_id_of_response body =
  match parse body with
  | Error _ as error -> error
  | Ok json -> (
      match graphql_errors json with
      | _ :: _ as errors -> Error (String.concat errors ~sep:"; ")
      | [] -> (
          match
            Option.bind (member "data" json) ~f:(member "submit")
            |> Option.bind ~f:(member "id")
          with
          | Some (`Int id) when id > 0 -> Ok id
          | Some _ | None -> Error "response has no data.submit.id"))

let conclusion_of_status status =
  match String.uppercase (String.strip status) with
  | "SUCCESS" -> "success"
  | "FAILED" -> "failure"
  | "TIMEOUT" -> "timed_out"
  | "CANCELLED" -> "cancelled"
  | "RUNNING" -> "in_progress"
  | "QUEUED" -> "queued"
  | "PENDING" -> "pending"
  | _ -> "pending"

let checks_for_commit ~owner ~repo ~branch ~sha jobs =
  let branch = Types.Branch.to_string branch in
  let commit_path = Printf.sprintf "/~%s/%s/commit/%s" owner repo sha in
  let matching =
    jobs
    |> List.filter ~f:(fun job ->
        String.is_substring job.note ~substring:commit_path
        && List.mem job.tags repo ~equal:String.equal
        && List.mem job.tags "commits" ~equal:String.equal
        && List.mem job.tags branch ~equal:String.equal)
    |> List.sort ~compare:(fun left right -> Int.descending left.id right.id)
  in
  let _, latest =
    List.fold matching
      ~init:(Set.empty (module String), [])
      ~f:(fun (seen, kept) job ->
        let manifest =
          let value = String.strip job.manifest in
          if String.is_empty value then "build" else value
        in
        if Set.mem seen manifest then (seen, kept)
        else (Set.add seen manifest, job :: kept))
  in
  List.rev latest
  |> List.map ~f:(fun job ->
      let manifest_name =
        let value = String.strip job.manifest in
        if String.is_empty value then "build" else value
      in
      let account =
        String.chop_prefix job.owner ~prefix:"~"
        |> Option.value ~default:job.owner
      in
      {
        Types.Ci_check.name = "builds.sr.ht / " ^ manifest_name;
        conclusion = conclusion_of_status job.status;
        details_url =
          Some (Printf.sprintf "https://builds.sr.ht/~%s/job/%d" account job.id);
        description = Some (Printf.sprintf "SourceHut build #%d" job.id);
        started_at = job.created;
        id = Some job.id;
      })

let log_source job =
  {
    Ci_log_digest.annotations = [];
    log = Some (String.concat job.logs ~sep:"\n");
  }
