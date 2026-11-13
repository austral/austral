(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Span
open SourceContext
open ErrorText

(* Types *)

type error_kind =
  | GenericError
  | ParseError
  | CliError
  | TypeError
  | LinearityError
  | DeclarationError
  | EntrypointError
  | InternalError

type error =
  | Parse of ParseErrors.t
  | Cli of CliErrors.t
  | Type of TypeErrors.t
  | Linearity of LinearityErrors.t
  | Declaration of DeclarationErrors.t
  | Entrypoint of EntrypointErrors.t
  | Generic of GenericErrors.t
  | Internal of string

type austral_error = AustralError of {
      module_name: module_name option;
      error: error;
      span: span option;
      source_ctx: source_ctx option;
    }

exception Austral_error of austral_error

let error_kind (error: error): error_kind =
  match error with
  | Parse _ -> ParseError
  | Cli _ -> CliError
  | Type _ -> TypeError
  | Linearity _ -> LinearityError
  | Declaration _ -> DeclarationError
  | Entrypoint _ -> EntrypointError
  | Generic _ -> GenericError
  | Internal _ -> InternalError

let error_text (error: error): err_text =
  match error with
  | Parse e -> ParseErrors.render e
  | Cli e -> CliErrors.render e
  | Type e -> TypeErrors.render e
  | Linearity e -> LinearityErrors.render e
  | Declaration e -> DeclarationErrors.render e
  | Entrypoint e -> EntrypointErrors.render e
  | Generic e -> GenericErrors.render e
  | Internal message ->
     [
       Text "Internal compiler error: ";
       Text message;
       Break;
       Text "This is a bug in the compiler, please open an issue here: ";
       Code "https://github.com/austral/austral/issues"
     ]

(* Raising *)

let raise_error (error: error): 'a =
  raise (Austral_error (AustralError { module_name = None; error; span = None; source_ctx = None }))

let parse_error (e: ParseErrors.t): 'a = raise_error (Parse e)

let parse_error_at (span: span) (e: ParseErrors.t): 'a =
  raise (Austral_error (AustralError { module_name = None; error = Parse e; span = Some span; source_ctx = None }))

let cli_error (e: CliErrors.t): 'a = raise_error (Cli e)

let type_error (e: TypeErrors.t): 'a = raise_error (Type e)

let linearity_error (e: LinearityErrors.t): 'a = raise_error (Linearity e)

let declaration_error (e: DeclarationErrors.t): 'a = raise_error (Declaration e)

let entrypoint_error (e: EntrypointErrors.t): 'a = raise_error (Entrypoint e)

let generic_error (e: GenericErrors.t): 'a = raise_error (Generic e)

let internal_err (message: string): 'a = raise_error (Internal message)

(* Error context-augmenting functions *)

let add_source_ctx (error: austral_error) (ctx: source_ctx): austral_error =
  let (AustralError { module_name; error = cause; span; source_ctx }) = error in
  match source_ctx with
  | Some _ ->
     error
  | None ->
     AustralError { module_name; error = cause; span; source_ctx = Some ctx; }

let adorn_error_with_module_name (new_module_name: module_name) (f: unit -> 'a): 'a =
  try
    f ()
  with Austral_error error ->
    let (AustralError { module_name; error = cause; span; source_ctx }) = error in
    match module_name with
    | Some _ ->
       (* The error already has a module name, do nothing. *)
       raise (Austral_error error)
    | None ->
       (* Add the span *)
       let new_err = AustralError { module_name = Some new_module_name; error = cause; span; source_ctx } in
       raise (Austral_error new_err)

let adorn_error_with_span (new_span: span) (f: unit -> 'a): 'a =
  try
    f ()
  with Austral_error error ->
    let (AustralError { module_name; error = cause; span; source_ctx }) = error in
    match span with
    | Some span' ->
       (* The error already has a span. *)
       if span' = empty_span then
         (* The error has the empty span, add ours. *)
         let new_err = AustralError { module_name; error = cause; span = Some new_span; source_ctx } in
         raise (Austral_error new_err)
       else
         (* The error has a non-empty span, reraise. *)
         raise (Austral_error error)
    | None ->
       (* Add the span *)
       let new_err = AustralError { module_name; error = cause; span = Some new_span; source_ctx } in
       raise (Austral_error new_err)

(* Error rendering to plain text *)

let error_title (kind: error_kind): string =
  match kind with
  | GenericError -> "Generic Error"
  | ParseError -> "Parse Error"
  | CliError -> "Command Line Arguments Error"
  | TypeError -> "Type Error"
  | LinearityError -> "Linearity Error"
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
  let (AustralError { span; error = cause; source_ctx; module_name }) = error in
  let mn_text: string =
    match module_name with
    | Some mn ->
       "  Module:\n"
       ^ "    " ^ (mod_name_string mn) ^ "\n"
    | None ->
       "  Module:\n"
       ^ "    [unknown]\n"
  in
  let title: string = error_title (error_kind cause) in
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
  ^ (indent_text (error_text_to_plain (error_text cause)) 4) ^ "\n"
  ^ code_text
  ^ "\n"

let pos_json (p: position): Yojson.Basic.t =
  let (Position { line; column; }) = p in
  `Assoc [
      ("line", `Int line);
      ("column", `Int column)
    ]

let span_json (span: span): Yojson.Basic.t =
  let (Span { filename; startp; endp; }) = span in
  `Assoc [
      ("filename", `String filename);
      ("startp", pos_json startp);
      ("end", pos_json endp)
    ]

let source_ctx_json (ctx: source_ctx): Yojson.Basic.t =
  let (SourceContext lines) = ctx in
  let line_json ((n, l): (int * string)): Yojson.Basic.t =
    `List [`Int n; `String l]
  in
  `List (List.map line_json lines)

let render_error_to_json (error: austral_error): Yojson.Basic.t =
  (* Break up the error. *)
  let (AustralError { span; error = cause; source_ctx; module_name }) = error in
  (* Render components to JSON. *)
  let module_name: Yojson.Basic.t =
    match module_name with
    | Some mn -> `String (mod_name_string mn)
    | None -> `Null
  in
  let span: Yojson.Basic.t =
    match span with
    | Some span -> span_json span
    | None -> `Null
  in
  let source_ctx: Yojson.Basic.t =
    match source_ctx with
    | Some source_ctx -> source_ctx_json source_ctx
    | None -> `Null
  in
  (* Put it together. *)
  `Assoc [
      ("module", module_name);
      ("kind", `String (error_title (error_kind cause)));
      ("text", error_text_to_json (error_text cause));
      ("span", span);
      ("context", source_ctx)
    ]
