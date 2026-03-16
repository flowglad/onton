open Base
open Types

type pr_discovery = {
  patch_id : Patch_id.t;
  pr_number : Pr_number.t;
  merged : bool;
}
[@@deriving show, eq]

type t = { discovered : pr_discovery list } [@@deriving show, eq]

(** Query [gh pr list] for a branch, returning [(pr_number, merged)] if found.
*)
let discover_pr ~process_mgr ~token ~owner ~repo ~branch =
  let args =
    [
      "gh";
      "pr";
      "list";
      "--repo";
      Printf.sprintf "%s/%s" owner repo;
      "--head";
      Branch.to_string branch;
      "--state";
      "all";
      "--json";
      "number,state";
      "--limit";
      "1";
    ]
  in
  let base_env = Unix.environment () in
  let env = Array.append [| Printf.sprintf "GH_TOKEN=%s" token |] base_env in
  try
    let buf = Buffer.create 256 in
    Eio.Process.run ~stdout:(Eio.Flow.buffer_sink buf) ~env process_mgr args;
    let output = Buffer.contents buf in
    match Yojson.Basic.from_string output with
    | `List (`Assoc fields :: _) -> (
        match
          ( List.Assoc.find fields ~equal:String.equal "number",
            List.Assoc.find fields ~equal:String.equal "state" )
        with
        | Some (`Int n), Some (`String state) ->
            Ok (Some (Pr_number.of_int n, String.equal state "MERGED"))
        | _ -> Ok None)
    | `List [] -> Ok None
    | _ -> Error "unexpected JSON shape"
  with exn -> Error (Stdlib.Printexc.to_string exn)

let reconcile ~process_mgr ~token ~owner ~repo ~patches =
  let discovered =
    List.filter_map patches ~f:(fun (patch : Patch.t) ->
        match
          discover_pr ~process_mgr ~token ~owner ~repo ~branch:patch.branch
        with
        | Ok (Some (pr_number, merged)) ->
            Some { patch_id = patch.id; pr_number; merged }
        | Ok None | Error _ -> None)
  in
  { discovered }
