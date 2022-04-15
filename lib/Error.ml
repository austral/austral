open Span

let indent_text (text: string) (indent: int): string =
  let lines = String.split_on_char '\n' text in
  let lines =
    List.map
      (fun line ->
        let pad = String.make indent ' '
        in
        pad ^ line)
      lines
  in
  String.concat "\n" lines

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
  | TypeMismatch of {
      expected: string;
      got: string;
    }

let error_filename (error: error): string option =
  let (Error { span; _ }) = error in
  match span with
  | Some (Span { filename; _ }) ->
     Some filename
  | None ->
     None

(* Return the error description. *)
let error_text (error_data: error_data): string =
  match error_data with
  | GenericError msg ->
     msg
  | ParseError ->
     "Error during parse."
  | TypeMismatch { expected; got; } ->
     ("Expected a value of type " ^ expected ^ " but got a value of type " ^ got)


(* Return the error title. *)
let error_title (error_data: error_data): string =
  match error_data with
  | GenericError _ ->
     "Generic Error"
  | ParseError ->
     "Parse Error"
  | TypeMismatch _ ->
     "Type Mismatch"

(* Render an error into a string for display in the terminal. *)
let render_error (error: error) (code: string option): string =
  let (Error { span; data }) = error in
  let span_text =
    match span with
    | Some (Span { filename; startp; endp }) ->
       "  Location:\n"
       ^ "    Filename: '" ^ filename ^ "'\n"
       ^ "    From: " ^ (position_to_string startp) ^ "\n"
       ^ "    To: " ^ (position_to_string endp) ^ "\n"
    | None ->
       "  Location:\n"
       ^ "    [no span available]\n"
  and code_text =
    match span with
    | Some span ->
       (match code with
        | Some code ->
           "  Code:\n"
           ^ (indent_text (span_text code span) 4)
        | None ->
           "  Code: [not available]\n")
    | None ->
       "  Code:\n"
       ^ "    [no span available]\n"
  in
  "Error:\n"
  ^ "  Title: " ^ (error_title data) ^ "\n"
  ^ span_text
  ^ "  Description:\n"
  ^ (indent_text (error_text data) 4) ^ "\n"
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

(* Throw a type mismatch error. *)
let raise_type_mismatch_error (expected: string) (got: string)  =
  let e = Error {
              span = None;
              data = TypeMismatch {
                         expected = expected;
                         got = got;
                       }
            }
  in
  raise (Austral_error e)

let adorn_error_with_span (spn: span) (f: unit -> 'a): 'a =
  try
    f ()
  with Austral_error error ->
    print_endline ("Backtrace:\n" ^ (Printexc.get_backtrace ()));
    let (Error { span; data }) = error in
    match span with
    | Some _ ->
       (* The error already has a span, do nothing. *)
       raise (Austral_error error)
    | None ->
       (* Add the span *)
       let new_err = Error { span = Some spn; data = data} in
       raise (Austral_error new_err)
