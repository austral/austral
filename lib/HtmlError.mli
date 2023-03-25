open Error

(* makes strings safe to put in html*)
val sanatize_string : string -> string

(* returns a string of the html representation of the error *)
val render_error_to_html : austral_error -> string

(* writes the html representation of the error to error.html *)
val html_error_dump : austral_error -> unit

