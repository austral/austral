open Span

(* Represents an error. *)
type error = Error of {
      (* The code span that triggered the error. *)
      span: span option;
      (* The error data. *)
      data: error_data;
    }

(* Represents the contents of an error. *)
and error_data =
  | GenericError of string
  | ParseError

(* Return the error description. *)
let error_text (error_data: error_data): string =
  match error_data with
  | GenericError msg ->
     msg
  | ParseError ->
     "Error during parse."

(* Return the error title. *)
let error_title (error_data: error_data): string =
  match error_data with
  | GenericError _ ->
     "Generic Error"
  | ParseError ->
     "Parse Error"

(* Render an error into a string for display in the terminal. *)
let render_error (error: error) (code: string): string =
  let (Error { span; data }) = error in
  let span_text =
    match span with
    | Some (Span { filename; startp; endp }) ->
       "  Location:\n"
       ^ "    Filename: '" ^ filename ^ "'\n"
       ^ "    From: " ^ (position_to_string startp) ^ "\n"
       ^ "    To: " ^ (position_to_string endp) ^ "\n"
    | None ->
       ""
  and code_text =
    match span with
    | Some span ->
       "  Code:\n"
       ^ (span_text code span)
    | None ->
       ""
  in
  "Error:\n"
  ^ "  Title: " ^ (error_title data) ^ "\n"
  ^ span_text
  ^ "  Description:\n"
  ^ (error_text data)
  ^ code_text
  ^ "\n"

(* Throw an error encapsulated as an exception. *)
exception Austral_error of error

(* Convenience functions for throwing errors. *)

(* Throw a generic error. *)
let err (message: string) =
  let e = Error {
              span = None;
              data = GenericError message
            }
  in
  raise (Austral_error e)

(* Throw a parse error. *)
let raise_parse_error (span: span) =
  let e = Error {
              span = Some span;
              data = ParseError
            }
  in
  raise (Austral_error e)
