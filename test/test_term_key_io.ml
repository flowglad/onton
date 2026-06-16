let with_stdin fd f =
  let saved = Unix.dup Unix.stdin in
  Fun.protect
    ~finally:(fun () ->
      Unix.dup2 saved Unix.stdin;
      Unix.close saved)
    (fun () ->
      Unix.dup2 fd Unix.stdin;
      f ())

let with_pipe f =
  let rd, wr = Unix.pipe () in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd;
      Unix.close wr)
    (fun () -> f ~rd ~wr)

let assert_poll_no_input () =
  with_pipe (fun ~rd ~wr:_ ->
      with_stdin rd (fun () ->
          Eio_main.run @@ fun _env ->
          match Onton.Term.Key_io.poll () with
          | Onton.Term.Key_io.No_input -> ()
          | Eof -> failwith "expected No_input, got Eof"
          | Key _ -> failwith "expected No_input, got Key"))

let assert_poll_key () =
  with_pipe (fun ~rd ~wr ->
      let written = Unix.write_substring wr "q" 0 1 in
      assert (written = 1);
      with_stdin rd (fun () ->
          Eio_main.run @@ fun _env ->
          match Onton.Term.Key_io.poll () with
          | Onton.Term.Key_io.Key key ->
              if not (Onton.Term.Key.equal key (Onton.Term.Key.Char 'q')) then
                failwith "expected Key(Char q), got different key"
          | No_input -> failwith "expected Key, got No_input"
          | Eof -> failwith "expected Key, got Eof"))

let () =
  assert_poll_no_input ();
  assert_poll_key ();
  Printf.printf "term_key_io: passed\n"
