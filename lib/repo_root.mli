(** Normalize a user-supplied repo-root path to the absolute main working tree.

    Users may invoke [onton] from any directory inside a git repository,
    including a worktree. This module resolves the main working tree via
    [git rev-parse --git-common-dir] so the persisted [repo_root] is always the
    main checkout, never a worktree path. *)

val normalize : string -> string
(** [normalize path] returns an absolute path to the main working tree of
    [path]'s git repository.

    - Relative paths are resolved against the current working directory.
    - Trailing slashes and [/.] suffixes are stripped.
    - If [path] is inside any worktree, the main checkout is returned.
    - If [path] is not inside any git repository, the absolute-but-unresolved
      path is returned — downstream code surfaces the non-repo error. *)
