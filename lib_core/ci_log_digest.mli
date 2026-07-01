type annotation = {
  path : string option;
  line : int option;
  level : string;
  message : string;
}

type source = { annotations : annotation list; log : string option }
type enrichment = { summary_md : string; teaser : string option; signal : int }

val summary_total_cap_bytes : int
val marker_context_lines : int
val error_window_lines : int
val collapse_signal_threshold : int
val strip_log : string -> string
val digest : source -> enrichment
