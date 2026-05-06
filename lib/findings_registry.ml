type entry = {
  backend : Onton_core.Review_backend.t;
  owner : string;
  repo : string;
  pr_number : int;
}

type t = { mutex : Eio.Mutex.t; table : (string, entry) Hashtbl.t }

let create () = { mutex = Eio.Mutex.create (); table = Hashtbl.create 64 }

let with_lock t f =
  Eio.Mutex.lock t.mutex;
  Fun.protect ~finally:(fun () -> Eio.Mutex.unlock t.mutex) f

let register t ~finding_id entry =
  with_lock t (fun () -> Hashtbl.replace t.table finding_id entry)

let find t ~finding_id =
  with_lock t (fun () -> Hashtbl.find_opt t.table finding_id)

let forget t ~finding_id =
  with_lock t (fun () -> Hashtbl.remove t.table finding_id)
