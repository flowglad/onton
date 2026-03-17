open Base
open Types

module Queue_key = struct
  module T = struct
    type t = Patch_id.t * Operation_kind.t [@@deriving sexp_of, compare, hash]
  end

  include T
  include Comparator.Make (T)
end

module Comment_patch_key = struct
  module T = struct
    type t = Comment.t * Patch_id.t [@@deriving sexp_of, compare]
  end

  include T
  include Comparator.Make (T)
end

module Patch_ctx = struct
  type t = {
    queue : bool Map.M(Queue_key).t;
    busy : bool Map.M(Patch_id).t;
    has_pr : bool Map.M(Patch_id).t;
    has_session : bool Map.M(Patch_id).t;
    needs_intervention : bool Map.M(Patch_id).t;
    merged : bool Map.M(Patch_id).t;
    approved : bool Map.M(Patch_id).t;
    ci_failure_count : int Map.M(Patch_id).t;
    base_branch : Branch.t Map.M(Patch_id).t;
  }
  [@@deriving sexp_of]

  let empty =
    {
      queue = Map.empty (module Queue_key);
      busy = Map.empty (module Patch_id);
      has_pr = Map.empty (module Patch_id);
      has_session = Map.empty (module Patch_id);
      needs_intervention = Map.empty (module Patch_id);
      merged = Map.empty (module Patch_id);
      approved = Map.empty (module Patch_id);
      ci_failure_count = Map.empty (module Patch_id);
      base_branch = Map.empty (module Patch_id);
    }

  let is_queued t ~patch_id ~kind =
    Map.find t.queue (patch_id, kind) |> Option.value ~default:false

  let set_queued t ~patch_id ~kind ~value =
    { t with queue = Map.set t.queue ~key:(patch_id, kind) ~data:value }

  let is_busy t ~patch_id =
    Map.find t.busy patch_id |> Option.value ~default:false

  let set_busy t ~patch_id ~value =
    { t with busy = Map.set t.busy ~key:patch_id ~data:value }

  let has_pr t ~patch_id =
    Map.find t.has_pr patch_id |> Option.value ~default:false

  let set_has_pr t ~patch_id ~value =
    { t with has_pr = Map.set t.has_pr ~key:patch_id ~data:value }

  let has_session t ~patch_id =
    Map.find t.has_session patch_id |> Option.value ~default:false

  let set_has_session t ~patch_id ~value =
    { t with has_session = Map.set t.has_session ~key:patch_id ~data:value }

  let needs_intervention t ~patch_id =
    Map.find t.needs_intervention patch_id |> Option.value ~default:false

  let set_needs_intervention t ~patch_id ~value =
    {
      t with
      needs_intervention =
        Map.set t.needs_intervention ~key:patch_id ~data:value;
    }

  let is_merged t ~patch_id =
    Map.find t.merged patch_id |> Option.value ~default:false

  let set_merged t ~patch_id ~value =
    { t with merged = Map.set t.merged ~key:patch_id ~data:value }

  let is_approved t ~patch_id =
    Map.find t.approved patch_id |> Option.value ~default:false

  let set_approved t ~patch_id ~value =
    { t with approved = Map.set t.approved ~key:patch_id ~data:value }

  let ci_failure_count t ~patch_id =
    Map.find t.ci_failure_count patch_id |> Option.value ~default:0

  let set_ci_failure_count t ~patch_id ~count =
    {
      t with
      ci_failure_count = Map.set t.ci_failure_count ~key:patch_id ~data:count;
    }

  let increment_ci_failure_count t ~patch_id =
    let current = ci_failure_count t ~patch_id in
    set_ci_failure_count t ~patch_id ~count:(current + 1)

  let base_branch t ~patch_id = Map.find t.base_branch patch_id

  let set_base_branch t ~patch_id ~branch =
    { t with base_branch = Map.set t.base_branch ~key:patch_id ~data:branch }

  let known_patch_ids t =
    let from_maps =
      [
        Map.keys t.busy;
        Map.keys t.has_pr;
        Map.keys t.has_session;
        Map.keys t.needs_intervention;
        Map.keys t.merged;
        Map.keys t.approved;
        Map.keys t.ci_failure_count;
        Map.keys t.base_branch;
      ]
    in
    let from_queue =
      Map.keys t.queue |> List.map ~f:(fun (pid, _kind) -> pid)
    in
    List.concat (from_queue :: from_maps)
    |> Set.of_list (module Patch_id)
    |> Set.to_list
end

module Comments = struct
  type t = {
    resolved : bool Map.M(Comment).t;
    pending : bool Map.M(Comment_patch_key).t;
  }
  [@@deriving sexp_of]

  let empty =
    {
      resolved = Map.empty (module Comment);
      pending = Map.empty (module Comment_patch_key);
    }

  let is_resolved t ~comment =
    Map.find t.resolved comment |> Option.value ~default:false

  let set_resolved t ~comment ~value =
    { t with resolved = Map.set t.resolved ~key:comment ~data:value }

  let is_pending t ~comment ~patch_id =
    Map.find t.pending (comment, patch_id) |> Option.value ~default:false

  let set_pending t ~comment ~patch_id ~value =
    { t with pending = Map.set t.pending ~key:(comment, patch_id) ~data:value }

  let all_resolved t =
    Map.fold t.resolved ~init:[] ~f:(fun ~key ~data acc ->
        if data then key :: acc else acc)

  let all_pending t =
    Map.fold t.pending ~init:[] ~f:(fun ~key ~data acc ->
        if data then key :: acc else acc)
end

type t = { patch_ctx : Patch_ctx.t; comments : Comments.t } [@@deriving sexp_of]

let empty = { patch_ctx = Patch_ctx.empty; comments = Comments.empty }
let update_patch_ctx t ~f = { t with patch_ctx = f t.patch_ctx }
let update_comments t ~f = { t with comments = f t.comments }
