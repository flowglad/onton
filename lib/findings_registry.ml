type entry = {
  backend : Onton_core.Review_backend.t;
  owner : string;
  repo : string;
  pr_number : int;
  finding_id : string;
}

type t = { mutex : Eio.Mutex.t; table : (string, entry) Hashtbl.t }

let create () = { mutex = Eio.Mutex.create (); table = Hashtbl.create 64 }
let with_lock t f = Eio.Mutex.use_rw ~protect:true t.mutex f

let make_key ~backend_name ~owner ~repo ~pr_number ~finding_id =
  Printf.sprintf "%s/%s/%s#%d/%s" backend_name owner repo pr_number finding_id

let register t ~key entry =
  with_lock t (fun () -> Hashtbl.replace t.table key entry)

let find t ~key = with_lock t (fun () -> Hashtbl.find_opt t.table key)
let forget t ~key = with_lock t (fun () -> Hashtbl.remove t.table key)
