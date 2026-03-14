open Base

type point = { x : float; y : float }
[@@deriving show, eq, ord, sexp_of, compare, hash]

let origin = { x = 0.0; y = 0.0 }

let distance p1 p2 =
  let dx = p1.x -. p2.x in
  let dy = p1.y -. p2.y in
  Float.sqrt ((dx *. dx) +. (dy *. dy))

let%test "origin is zero" = equal_point origin { x = 0.0; y = 0.0 }

let%test "distance from origin" =
  Float.equal (distance origin { x = 3.0; y = 4.0 }) 5.0
