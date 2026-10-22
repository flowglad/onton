(* @archlint.module test
   @archlint.domain session-driver *)

(* Patch 5 replaces this scaffold with a fake backend at the actual session
   boundary. After lost-workspace setup, capture the invocation and observe
   resume_session=None plus requirements, outstanding operation, guidance,
   observed revisions and loss/inspection instructions. A valid warm session
   must still receive its ordinary resume ID. Wait, attention and merge setup
   outcomes must not invoke the backend. *)
let () = print_endline "PENDING patch 5: fresh recovery backend prompt"
