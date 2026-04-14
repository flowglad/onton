(* CI test — third commit *)

let divide_scores scores = List.map (fun s -> 100 / s) scores

let process () =
  let scores = [ 10; 25; 0; 50 ] in
  let normalized = divide_scores scores in
  List.iter (fun n -> Printf.printf "%d\n" n) normalized
