(** Utility code used throughout the compiler. *)

open Identifier

(** Explode a string into a list of characters. *)
val string_explode : string -> char list

(** Implode a list of characters into a string. *)
val string_implode : char list -> string

(** Read the file at the given path into a string. *)
val read_file_to_string : string -> string

(** Write the contents of a string to a file.

    The first argument is the path, the second the contents. *)
val write_string_to_file : string -> string -> unit

(** Remove all instances of a given character from a string. *)
val remove_char : string -> char -> string

(** Replace all instances of a given character with a string.

    The first argument is the string to search/replace on, the third
    argument is the string to replace the character with. *)
val replace_char : string -> char -> string -> string

type replacement = { text: string; search: string; replacement: string }

(** Replace a string with another in a text. *)
val search_replace : replacement -> string

(** Remove the first n characters of a string. *)
val remove_leading : string -> int -> string

(** Parse a hexadecimal string into an integer. *)
val parse_hex : string -> int

(** Parse a binary string into an integer. *)
val parse_bin : string -> int

(** Parse an octal string into an integer. *)
val parse_oct : string -> int

(** Parse an ASCII character into its integer code, handling escape sequences. *)
val parse_ascii_char : string -> int

(** Take the contents of a triple-quoted string, and trim the leading and ending
   empty lines as well as indentation. *)
val process_triple_string : string -> string

(** Check whether two lists of identifiers are the same set. *)
val ident_set_eq : identifier list -> identifier list -> bool

(** Represents the output of running an external command. *)
type command_output =
  CommandOutput of { command: string; code: int; stdout: string; stderr: string }

(** Run a shell command, and return its output. *)
val run_command : string -> command_output

(** Call the C++ compiler on the given file.

    The first argument is the source file path.

    The second argument is the output binary file path. *)
val compile_cpp_code : string -> string -> command_output

(** Like `mapAccumL` in Haskell. *)
val map_with_context : (('c * 'a) -> ('c * 'b)) -> 'c -> 'a list -> ('c * ('b list))

val iter_with_context : ('c -> 'a -> 'c) -> 'c -> 'a list -> 'c
