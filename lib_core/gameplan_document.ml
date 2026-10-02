(* @archlint.module shell
   @archlint.domain gameplan-document *)

open Base

exception Decode_error of string

let scalar style value =
  let style =
    match style with
    | `Single_quoted | `Double_quoted | `Literal | `Folded ->
        Gameplan_scalar.Quoted
    | `Any | `Plain -> Gameplan_scalar.Plain
    | `E _ -> raise (Decode_error "Unknown YAML scalar style")
  in
  match Gameplan_scalar.decode style value with
  | Ok json -> json
  | Error msg -> raise (Decode_error msg)

(* [Yaml.Stream] in yaml 3.2 reads scalars as C strings (truncating escaped
   NULs) and never deletes native events or parsers. Drive its libyaml bindings
   directly so each scalar uses its explicit length and each allocation has a
   cleanup path, including parse failures. *)
module B = Yaml_ffi.M
module T = Yaml_types.M

external delete_event : nativeint -> unit = "onton_yaml_event_delete"

type event =
  | Stream_start
  | Stream_end
  | Document_start
  | Document_end
  | Sequence_start
  | Sequence_end
  | Mapping_start
  | Mapping_end
  | Scalar of Yojson.Safe.t

let with_parser input f =
  let open Ctypes in
  let parser = allocate_n T.Parser.t ~count:1 in
  let event = allocate_n T.Event.t ~count:1 in
  let buf = CArray.of_string input in
  if B.parser_init parser <> 1 then
    raise (Decode_error "Cannot initialize YAML parser");
  Stdlib.Fun.protect
    ~finally:(fun () ->
      B.parser_delete parser;
      ignore (Sys.opaque_identity buf))
    (fun () ->
      B.parser_set_input_string parser (CArray.start buf)
        (Unsigned.Size_t.of_int (String.length input));
      let reject_metadata anchor tag =
        if Option.is_some anchor || Option.is_some tag then
          raise (Decode_error "YAML tags and anchors are not supported")
      in
      let next () =
        (if B.parser_parse parser event <> 1 then
           let msg =
             getf !@parser T.Parser.problem
             |> Option.value ~default:"Invalid YAML"
           in
           raise (Decode_error msg));
        Stdlib.Fun.protect
          ~finally:(fun () ->
            delete_event (raw_address_of_ptr (to_voidp event)))
          (fun () ->
            let data = getf !@event T.Event.data in
            match getf !@event T.Event._type with
            | `Stream_start -> Stream_start
            | `Stream_end -> Stream_end
            | `Document_start ->
                let doc = getf data T.Event.Data.document_start in
                let tags = getf doc T.Event.Document_start.tag_directives in
                if
                  ptr_compare
                    (getf tags T.Event.Document_start.Tag_directives.start)
                    (getf tags T.Event.Document_start.Tag_directives._end)
                  <> 0
                then
                  raise (Decode_error "YAML tag directives are not supported");
                Document_start
            | `Document_end -> Document_end
            | `Sequence_end -> Sequence_end
            | `Mapping_end -> Mapping_end
            | `Alias -> raise (Decode_error "YAML aliases are not supported")
            | `None | `E _ -> raise (Decode_error "Unknown YAML event")
            | `Scalar ->
                let s = getf data T.Event.Data.scalar in
                reject_metadata
                  (getf s T.Event.Scalar.anchor)
                  (getf s T.Event.Scalar.tag);
                let length =
                  getf s T.Event.Scalar.length |> Unsigned.Size_t.to_int
                in
                let ptr =
                  coerce (ptr string)
                    (ptr (ptr char))
                    (s @. T.Event.Scalar.value)
                  |> ( !@ )
                in
                let value = string_from_ptr ptr ~length in
                Scalar (scalar (getf s T.Event.Scalar.style) value)
            | `Sequence_start ->
                let s = getf data T.Event.Data.sequence_start in
                reject_metadata
                  (getf s T.Event.Sequence_start.anchor)
                  (getf s T.Event.Sequence_start.tag);
                Sequence_start
            | `Mapping_start ->
                let s = getf data T.Event.Data.mapping_start in
                reject_metadata
                  (getf s T.Event.Mapping_start.anchor)
                  (getf s T.Event.Mapping_start.tag);
                Mapping_start)
      in
      f next)

(* New event variants are rejected at the stream boundary by design. *)
let[@warning "-4"] of_yaml_string input =
  try
    with_parser input (fun next ->
        let rec node = function
          | Scalar value -> value
          | Sequence_start -> sequence []
          | Mapping_start -> mapping (Set.empty (module String)) []
          | _ -> raise (Decode_error "Expected a YAML value")
        and sequence acc =
          match next () with
          | Sequence_end -> `List (List.rev acc)
          | event ->
              let value = node event in
              sequence (value :: acc)
        and mapping keys acc =
          match next () with
          | Mapping_end -> `Assoc (List.rev acc)
          | event -> (
              match node event with
              | `String key ->
                  if String.equal key "<<" then
                    raise (Decode_error "YAML merge keys are not supported");
                  if Set.mem keys key then
                    raise (Decode_error ("Duplicate YAML key: " ^ key));
                  let value = node (next ()) in
                  mapping (Set.add keys key) ((key, value) :: acc)
              | _ -> raise (Decode_error "YAML mapping keys must be strings"))
        in
        (match next () with
        | Stream_start -> ()
        | _ -> raise (Decode_error "Expected a YAML stream"));
        (match next () with
        | Document_start -> ()
        | _ -> raise (Decode_error "Expected one YAML document"));
        let value = node (next ()) in
        (match next () with
        | Document_end -> ()
        | _ -> raise (Decode_error "Expected the end of the YAML document"));
        match next () with
        | Stream_end -> Ok value
        | _ -> Error "YAML gameplans must contain exactly one document")
  with
  | Decode_error msg -> Error ("YAML parse error: " ^ msg)
  | exn -> Error ("YAML parse error: " ^ Exn.to_string exn)

let of_string = of_yaml_string
