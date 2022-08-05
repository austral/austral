(** A CLI parser framework. *)

(** Represents a parsed argument list. *)
type arglist

(** Parse the CLI arguments into an arglist. *)
val parse_args : string list -> arglist

(** Number of arguments. *)
val arglist_size : arglist -> int

(** Pop a boolean flag (e.g. --help), returned the arglist without the flag if
    present, None otherwise. *)
val pop_bool_flag : arglist -> string -> arglist option

(** Pop the value of a flag (e.g. --foo=bar returns bar), returning the arglist
    without the flag and the value if present, None otherwise. *)
val pop_value_flag : arglist -> string -> (arglist * string) option

(** Take all positional arguments from the arglist, returning the arglist
    without the arguments and the list of positional arguments. **)
val pop_positional : arglist -> (arglist * string list)

(** Get positional arguments without removing them. **)
val get_positional : arglist -> string list

(** Update the list of positional arguments. *)
val adjust_positional : arglist -> string list -> arglist
