open Base

type defer_reason =
  | Main_sha_unknown
  | Base_patch_busy_with_rebase of { base_branch : string }
  | Base_not_rebased_since_main_advanced of {
      base_branch : string;
      base_rebased_onto_sha : string option;
      main_sha : string;
    }
[@@deriving show, eq, sexp_of, compare]

type decision = Allow | Defer of defer_reason
[@@deriving show, eq, sexp_of, compare]

let decide ~base_is_main ~base_branch ~base_patch_merged
    ~base_patch_rebased_onto_sha ~base_patch_busy_rebasing ~main_sha =
  if base_is_main then Allow
  else if base_patch_merged then Allow
  else
    match main_sha with
    | None -> Defer Main_sha_unknown
    | Some main_sha -> (
        if base_patch_busy_rebasing then
          Defer (Base_patch_busy_with_rebase { base_branch })
        else
          match base_patch_rebased_onto_sha with
          | Some s when String.equal s main_sha -> Allow
          | _ ->
              Defer
                (Base_not_rebased_since_main_advanced
                   {
                     base_branch;
                     base_rebased_onto_sha = base_patch_rebased_onto_sha;
                     main_sha;
                   }))

let short_label = function
  | Allow -> "allow"
  | Defer Main_sha_unknown -> "defer_main_unknown"
  | Defer (Base_patch_busy_with_rebase _) -> "defer_base_busy_rebasing"
  | Defer (Base_not_rebased_since_main_advanced _) -> "defer_base_stale"
