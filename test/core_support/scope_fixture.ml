(* @archlint.module test
   @archlint.domain test-support *)

open Onton_core
module B = Branch_reconcile
module S = Replay_scope

(* Protocol fixtures supply successful immutable Git observations. Scope's own
   properties and real-Git tests supply adversarial trees and histories. *)
let verified request revision =
  if request <> S.Unproven && not (S.valid_request request) then
    QCheck2.Test.fail_reportf "scope fixture requires valid captured revisions";
  let candidate = B.Commit.to_string revision in
  let tree = String.make 40 'f' in
  let chain source parent =
    if source = parent then "" else Printf.sprintf "%s\000%s\n" source parent
  in
  let plan, history =
    match request with
    | S.Unproven -> (Error "replay_scope_missing_boundary", "")
    | S.Identity source -> (S.prepare_identity ~source ~tree, "")
    | S.Replay { source; boundary; target } ->
        let plan =
          match
            S.capture ~boundary:(Some boundary) ~source (chain source boundary)
          with
          | Error reason -> Error (S.error_reason reason)
          | Ok scope -> S.prepare scope ~target ~status:0 (tree ^ "\000")
        in
        (plan, chain candidate target)
    | S.Merge { source; target } ->
        ( S.prepare_merge ~source ~target ~status:0 (tree ^ "\000"),
          if candidate = source || candidate = target then ""
          else Printf.sprintf "%s\000%s %s\n" candidate source target )
  in
  match plan with
  | Error reason -> B.Recovery_required reason
  | Ok plan -> (
      match S.verify plan ~candidate ~tree ~history ~changed_paths:"" with
      | Ok proof -> B.Scope_verified proof
      | Error reason -> B.Recovery_required reason)

let local_extension ~source ~candidate =
  let source = B.Commit.to_string source
  and candidate = B.Commit.to_string candidate in
  match
    S.capture ~boundary:(Some source) ~source:candidate
      (if source = candidate then ""
       else Printf.sprintf "%s\000%s\n" candidate source)
  with
  | Ok scope -> Some scope
  | Error _ -> None
