(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *)

(** Error reporting infrastructure. *)
open Identifier
open Span
open SourceContext
open ErrorText

(** Represents a category of errors. *)
type error_kind =
  | GenericError
  (** A generic error. This should be gradually phased out. *)
  | ParseError
  (** A parse error. *)
  | CliError
  (** An error with the user's command line arguments. *)
  | TypeError
  (** A type error. *)
  | LinearityError
  (** Signalled when code breaks the linearity rules. *)
  | DeclarationError
  (** An error in the structure of a declaration. *)
  | EntrypointError
  (** An error with the definition of the program entrypoint. *)
  | InternalError
  (** An internal error in the compiler. *)

(** What went wrong. Each category has its own type, with one constructor per
    error; the wording is in the [render] function of the category's module. *)
type error =
  | Parse of ParseErrors.t
  | Cli of CliErrors.t
  | Type of TypeErrors.t
  | Linearity of LinearityErrors.t
  | Declaration of DeclarationErrors.t
  | Entrypoint of EntrypointErrors.t
  | Generic of GenericErrors.t
  | Internal of string
  (** A state the compiler believes cannot happen. The string says which. *)

(** An Austral compiler error. The module name, span, and source context rarely
    have to be passed in explicitly: they are added where the error is throw in
    the context of {!adorn_error_with_span}. *)
type austral_error = AustralError of {
      module_name: module_name option;
      (** The name of the module where the error occurred, if available. *)
      error: error;
      (** What went wrong. *)
      span: span option;
      (** The source span where the error happened, if available. *)
      source_ctx: source_ctx option;
      (** The source code where the error happened, if available. *)
    }

(** The exception that carries Austral errors. *)
exception Austral_error of austral_error

(** The category of an error. *)
val error_kind : error -> error_kind

(** The wording of an error. *)
val error_text : error -> err_text

(** Raise a parse error. *)
val parse_error : ParseErrors.t -> 'a

(** Raise a parse error at a place in the source text. The lexer and the
    parser know the place where they stop. *)
val parse_error_at : span -> ParseErrors.t -> 'a

(** Raise an error with the user's command line arguments. *)
val cli_error : CliErrors.t -> 'a

(** Raise a type error. *)
val type_error : TypeErrors.t -> 'a

(** Raise a linearity error. *)
val linearity_error : LinearityErrors.t -> 'a

(** Raise a declaration error. *)
val declaration_error : DeclarationErrors.t -> 'a

(** Raise an error with the definition of the program entrypoint. *)
val entrypoint_error : EntrypointErrors.t -> 'a

(** Raise a generic error. *)
val generic_error : GenericErrors.t -> 'a

(** Raise an internal error. *)
val internal_err : string -> 'a

(** Add a source context to an error if it doesn't have one. *)
val add_source_ctx : austral_error -> source_ctx -> austral_error

(** Run the callback, and if it throws an error that doesn't have a module name,
    put the given module name in the error and rethrow it. *)
val adorn_error_with_module_name : module_name -> (unit -> 'a) -> 'a

(** Run the callback, and if it throws an error that doesn't have a span, put
    the given span in the error and rethrow it. *)
val adorn_error_with_span : span -> (unit -> 'a) -> 'a

(** Render an error kind to a title. *)
val error_title : error_kind -> string

(** Render an error to plain text, for display to the terminal. *)
val render_error_to_plain : austral_error -> string

(** Render an error to JSON, for tools or the end to end tests. *)
val render_error_to_json : austral_error -> Yojson.Basic.t
