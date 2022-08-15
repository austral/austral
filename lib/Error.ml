open Identifier
open Span
open SourceContext
open ErrorText

(* Types *)

type error_kind =
  | GenericError
  | ParseError
  | TypeError
  | DeclarationError
  | EntrypointError
  | InternalError

type austral_error = AustralError of {
      module_name: module_name option;
      kind: error_kind;
      text: err_text;
      span: span option;
      source_ctx: source_ctx option;
    }

exception Austral_error of austral_error

(* Raising *)

let austral_raise (kind: error_kind) (text: err_text): 'a =
  let error: austral_error =
    AustralError {
        module_name = None;
        kind = kind;
        text = text;
        span = None;
        source_ctx = None;
      }
  in
  raise (Austral_error error)

(* Error context-augmenting functions *)

let add_source_ctx (error: austral_error) (ctx: source_ctx): austral_error =
  let (AustralError { module_name; kind; text; span; source_ctx }) = error in
  match source_ctx with
  | Some _ ->
     error
  | None ->
     AustralError { module_name; kind; text; span; source_ctx = Some ctx; }

let adorn_error_with_module_name (new_module_name: module_name) (f: unit -> 'a): 'a =
  try
    f ()
  with Austral_error error ->
    let (AustralError { module_name; kind; text; span; source_ctx }) = error in
    match module_name with
    | Some _ ->
       (* The error already has a module name, do nothing. *)
       raise (Austral_error error)
    | None ->
       (* Add the span *)
       let new_err = AustralError { module_name = Some new_module_name; kind; text; span; source_ctx } in
       raise (Austral_error new_err)

let adorn_error_with_span (new_span: span) (f: unit -> 'a): 'a =
  try
    f ()
  with Austral_error error ->
    let (AustralError { module_name; kind; text; span; source_ctx }) = error in
    match span with
    | Some _ ->
       (* The error already has a span, do nothing. *)
       raise (Austral_error error)
    | None ->
       (* Add the span *)
       let new_err = AustralError { module_name; kind; text; span = Some new_span; source_ctx } in
       raise (Austral_error new_err)

(* Error rendering to plain text *)

let error_title (kind: error_kind): string =
  match kind with
  | GenericError -> "Generic Error"
  | ParseError -> "Parse Error"
  | TypeError -> "Type Error"
  | DeclarationError -> "Declaration Error"
  | EntrypointError -> "Entrypoint Definition Error"
  | InternalError -> "Internal Error"

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

let render_error_to_plain (error: austral_error): string =
  let (AustralError { span; kind; text; source_ctx; module_name }) = error in
  let mn_text: string =
    match module_name with
    | Some mn ->
       "  Module:\n"
       ^ "    " ^ (mod_name_string mn) ^ "\n"
    | None ->
       "  Module:\n"
       ^ "    [unknown]\n"
  in
  let title: string = error_title kind in
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
    (match source_ctx with
     | Some ctx ->
        "  Code:\n"
        ^ (indent_text (source_ctx_to_plain_text ctx) 4)
     | None ->
        "  Code:\n"
        ^ "    [no span available]\n")
  in
  "Error:\n"
  ^ "  Title: " ^ title ^ "\n"
  ^ mn_text
  ^ span_text
  ^ "  Description:\n"
  ^ (indent_text (error_text_to_plain text) 4) ^ "\n"
  ^ code_text
  ^ "\n"

(* Utility functions. *)

let err (message: string) =
  let e: austral_error =
    AustralError {
        module_name = None;
        kind = GenericError;
        text = [Text message];
        span = None;
        source_ctx = None;
      }
  in
  raise (Austral_error e)

let internal_err (message: string) =
  let e: austral_error =
    AustralError {
        module_name = None;
        kind = InternalError;
        text = [
            Text "Internal compiler error:";
            Text message;
            Break;
            Text "This is a bug in the compiler, please open an issue here: ";
            Code "https://github.com/austral/austral/issues"
          ];
        span = None;
        source_ctx = None;
      }
  in
  raise (Austral_error e)
