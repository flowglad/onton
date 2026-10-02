(* @archlint.module core
   @archlint.domain gameplan-document *)

open Base

type style = Plain | Quoted

let number =
  Re.Perl.compile_pat {|\A-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?\z|}

let decode style value : (Yojson.Safe.t, string) Result.t =
  match style with
  | Quoted -> Ok (`String value)
  | Plain -> (
      match value with
      | "" | "~" | "null" -> Ok `Null
      | "true" -> Ok (`Bool true)
      | "false" -> Ok (`Bool false)
      | value when Re.execp number value -> (
          try
            match Yojson.Safe.from_string value with
            | `Float f when not (Float.is_finite f) ->
                Error "YAML numbers must be finite"
            | json -> Ok json
          with exn -> Error (Exn.to_string exn))
      | value -> Ok (`String value))
