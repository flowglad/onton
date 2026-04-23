type limits = { soft : int; hard : int }

external get_nofile_stub : unit -> int * int = "caml_onton_getrlimit_nofile"
external set_nofile_stub : int -> int -> unit = "caml_onton_setrlimit_nofile"

let get_nofile () =
  let soft, hard = get_nofile_stub () in
  { soft; hard }

let try_raise_nofile_soft ~target =
  let cur = get_nofile () in
  (* Never lower — this is [raise], not [set]. *)
  if target <= cur.soft then cur
  else
    let desired_soft = min target cur.hard in
    (try set_nofile_stub desired_soft cur.hard with _ -> ());
    get_nofile ()
