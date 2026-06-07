(* @archlint.module test
   @archlint.domain anchor *)

open Base
open Onton_core
open Onton_core.Types

(** Unit and property tests for {!Anchor} and {!Anchor_history}. *)

module Gen = QCheck2.Gen
module Test = QCheck2.Test

(* ── Generators ───────────────────────────────────────────────────────── *)

let gen_hex_sha : string Gen.t =
  Gen.string_size
    ~gen:Gen.(oneof [ char_range '0' '9'; char_range 'a' 'f' ])
    (Gen.return 40)

let gen_branch : Branch.t Gen.t =
  Gen.map Branch.of_string
    (Gen.string_size ~gen:(Gen.char_range 'a' 'z') (Gen.int_range 1 20))

let gen_anchor : Anchor.t Gen.t =
  Gen.map3
    (fun base sha observed_at_remote ->
      match Anchor.make ~base ~sha ~observed_at_remote with
      | Some a -> a
      | None -> assert false (* gen_hex_sha is always valid *))
    gen_branch gen_hex_sha Gen.bool

(* Anything that is not a 40-char lowercase hex string. *)
let gen_invalid_sha : string Gen.t =
  Gen.oneof
    [
      Gen.return "";
      Gen.return "   ";
      Gen.return "deadbeef" (* too short *);
      Gen.return (String.make 41 '0') (* too long *);
      Gen.return (String.make 40 'g') (* not hex *);
      Gen.return (String.make 40 'A') (* uppercase *);
      Gen.string_size ~gen:Gen.printable (Gen.int_range 0 60);
    ]

(* ── Anchor.make / validation ──────────────────────────────────────────── *)

let prop_make_accepts_hex40 =
  Test.make ~count:200 ~name:"Anchor.make accepts 40-char lowercase hex"
    (Gen.pair gen_branch gen_hex_sha) (fun (base, sha) ->
      Option.is_some (Anchor.make ~base ~sha ~observed_at_remote:false))

let prop_make_strips_whitespace =
  Test.make ~count:200
    ~name:"Anchor.make strips surrounding whitespace before validating"
    (Gen.pair gen_branch gen_hex_sha) (fun (base, sha) ->
      let padded = "  " ^ sha ^ "  \n" in
      match Anchor.make ~base ~sha:padded ~observed_at_remote:false with
      | Some a -> String.equal (Anchor.sha a) sha
      | None -> false)

let prop_make_rejects_invalid =
  Test.make ~count:500 ~name:"Anchor.make rejects malformed SHAs"
    (Gen.pair gen_branch gen_invalid_sha) (fun (base, sha) ->
      Option.is_none (Anchor.make ~base ~sha ~observed_at_remote:false))

let prop_make_preserves_fields =
  Test.make ~count:200 ~name:"Anchor accessors round-trip make"
    (Gen.triple gen_branch gen_hex_sha Gen.bool)
    (fun (base, sha, observed_at_remote) ->
      match Anchor.make ~base ~sha ~observed_at_remote with
      | None -> false
      | Some a ->
          Branch.equal (Anchor.base a) base
          && String.equal (Anchor.sha a) sha
          && Bool.equal (Anchor.is_remote a) observed_at_remote)

(* ── Anchor yojson round-trip ──────────────────────────────────────────── *)

let prop_anchor_yojson_roundtrip =
  Test.make ~count:300 ~name:"Anchor: yojson_of_t / t_of_yojson round-trip"
    gen_anchor (fun a ->
      let json = Anchor.yojson_of_t a in
      let a' = Anchor.t_of_yojson json in
      Anchor.equal a a')

let prop_anchor_of_yojson_opt_rejects_bad_sha =
  Test.make ~count:200
    ~name:"Anchor.of_yojson_opt returns None for invalid persisted SHA"
    (Gen.pair gen_branch gen_invalid_sha) (fun (base, sha) ->
      let json : Yojson.Safe.t =
        `Assoc
          [
            ("base", Branch.yojson_of_t base);
            ("sha", `String sha);
            ("observed_at_remote", `Bool false);
          ]
      in
      Option.is_none (Anchor.of_yojson_opt json))

(* ── Anchor_history.push semantics ─────────────────────────────────────── *)

let prop_push_puts_newest_first =
  Test.make ~count:300 ~name:"Anchor_history.push: newest entry is at head"
    (Gen.pair (Gen.list_size (Gen.int_range 0 12) gen_anchor) gen_anchor)
    (fun (seed, fresh) ->
      let h =
        List.fold seed ~init:Anchor_history.empty ~f:Anchor_history.push
      in
      let h' = Anchor_history.push h fresh in
      match Anchor_history.newest h' with
      | Some a -> Anchor.equal a fresh
      | None -> false)

let prop_push_caps_at_8 =
  Test.make ~count:200 ~name:"Anchor_history.push: length never exceeds cap"
    (Gen.list_size (Gen.int_range 0 30) gen_anchor)
    (fun seed ->
      let h =
        List.fold seed ~init:Anchor_history.empty ~f:Anchor_history.push
      in
      Anchor_history.length h <= Anchor_history.cap)

let prop_push_dedups_by_key =
  Test.make ~count:200
    ~name:
      "Anchor_history.push: re-pushing same (base, sha) moves it to head, does \
       not duplicate"
    (Gen.pair (Gen.list_size (Gen.int_range 0 6) gen_anchor) gen_anchor)
    (fun (seed, a) ->
      let h =
        List.fold seed ~init:Anchor_history.empty ~f:Anchor_history.push
      in
      let h1 = Anchor_history.push h a in
      let h2 = Anchor_history.push h1 a in
      let list1 = Anchor_history.to_list h1 in
      let list2 = Anchor_history.to_list h2 in
      let count_a xs =
        List.count xs ~f:(fun e ->
            Branch.equal (Anchor.base e) (Anchor.base a)
            && String.equal (Anchor.sha e) (Anchor.sha a))
      in
      count_a list1 = 1
      && count_a list2 = 1
      && List.equal Anchor.equal list1 list2)

let prop_push_monotone_membership =
  Test.make ~count:200
    ~name:"Anchor_history.push: pushed anchor is a member of the result"
    (Gen.pair (Gen.list_size (Gen.int_range 0 12) gen_anchor) gen_anchor)
    (fun (seed, fresh) ->
      let h =
        List.fold seed ~init:Anchor_history.empty ~f:Anchor_history.push
      in
      let h' = Anchor_history.push h fresh in
      List.exists (Anchor_history.to_list h') ~f:(Anchor.equal fresh))

(* ── Anchor_history yojson round-trip ─────────────────────────────────── *)

let prop_history_yojson_roundtrip =
  Test.make ~count:200
    ~name:"Anchor_history: yojson_of_t / t_of_yojson round-trip"
    (Gen.list_size (Gen.int_range 0 8) gen_anchor)
    (fun seed ->
      let h =
        List.fold seed ~init:Anchor_history.empty ~f:Anchor_history.push
      in
      let json = Anchor_history.yojson_of_t h in
      let h' = Anchor_history.t_of_yojson json in
      Anchor_history.equal h h')

let prop_history_load_truncates_to_cap =
  Test.make ~count:50
    ~name:"Anchor_history.of_yojson truncates over-cap legacy snapshots"
    (Gen.list_size (Gen.int_range 9 30) gen_anchor)
    (fun seed ->
      (* Build a raw JSON list longer than cap, bypassing push's truncation. *)
      let json : Yojson.Safe.t = `List (List.map seed ~f:Anchor.yojson_of_t) in
      let h = Anchor_history.t_of_yojson json in
      Anchor_history.length h <= Anchor_history.cap)

(* ── Anchor_history.empty / newest invariants ─────────────────────────── *)

let test_empty_invariants () =
  assert (Anchor_history.length Anchor_history.empty = 0);
  assert (Option.is_none (Anchor_history.newest Anchor_history.empty));
  assert (List.is_empty (Anchor_history.to_list Anchor_history.empty))

let () =
  test_empty_invariants ();
  List.iter
    ~f:(fun t -> QCheck2.Test.check_exn t)
    [
      prop_make_accepts_hex40;
      prop_make_strips_whitespace;
      prop_make_rejects_invalid;
      prop_make_preserves_fields;
      prop_anchor_yojson_roundtrip;
      prop_anchor_of_yojson_opt_rejects_bad_sha;
      prop_push_puts_newest_first;
      prop_push_caps_at_8;
      prop_push_dedups_by_key;
      prop_push_monotone_membership;
      prop_history_yojson_roundtrip;
      prop_history_load_truncates_to_cap;
    ];
  Stdlib.print_endline "Anchor / Anchor_history: all properties passed"
