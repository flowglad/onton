type entry = {
  backend_name : string;
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

let remove_stale_for_scope t ~backend_name ~owner ~repo ~pr_number ~keep_keys =
  with_lock t (fun () ->
      let stale = ref [] in
      Hashtbl.iter
        (fun key (entry : entry) ->
          let same_scope =
            String.equal entry.backend_name backend_name
            && String.equal entry.owner owner
            && String.equal entry.repo repo
            && entry.pr_number = pr_number
          in
          if same_scope && not (List.exists (String.equal key) keep_keys) then
            stale := key :: !stale)
        t.table;
      List.iter (fun key -> Hashtbl.remove t.table key) !stale)

let find t ~key = with_lock t (fun () -> Hashtbl.find_opt t.table key)
let forget t ~key = with_lock t (fun () -> Hashtbl.remove t.table key)
