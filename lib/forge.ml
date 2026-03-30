(** Forge interface: abstract source of pull request / merge request state.

    Each forge implementation (GitHub, GitLab, etc.) satisfies this signature.
    Constructor functions are forge-specific and not part of the interface. *)

module type S = sig
  type t
  type error

  val show_error : error -> string

  val pr_state :
    net:_ Eio.Net.t -> t -> Types.Pr_number.t -> (Pr_state.t, error) Result.t
end
