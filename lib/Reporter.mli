(** The reporter keeps track of the compiler's call tree, and is useful for
    debugging crashes and bugs. *)

(** Render the event list and dump it to a file. *)
val dump : unit -> unit

(** Run some code inside a named frame. *)
val with_frame : string -> (unit -> 'a) -> 'a

(** The type of property labels.*)
type label = string

(** Push a string property to the current frame. *)
val ps : (label * string) -> unit
