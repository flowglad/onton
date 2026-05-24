open Base

let upstream ~prev_base_sha ~fallback =
  match prev_base_sha with
  | Some sha ->
      let sha = String.strip sha in
      if String.is_empty sha then fallback else sha
  | None -> fallback
