let () =
  let p = Onton.{ x = 1.0; y = 1.0 } in
  assert (Onton.equal_point p p);
  assert (Float.equal (Onton.distance Onton.origin p) (Float.sqrt 2.0));
  print_endline "all tests passed"
