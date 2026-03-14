let () =
  let p = Onton.{ x = 3.0; y = 4.0 } in
  Printf.printf "point: %s\n" (Onton.show_point p);
  Printf.printf "distance from origin: %f\n" (Onton.distance Onton.origin p)
