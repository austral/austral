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
  | TypeError
  (** A type error. *)
  | DeclarationError
  (** An error in the structure of a declaration. *)
  | InternalError
  (** An internal error in the compiler. *)

(** An Austral compiler error. The module name, span, and source context rarely
    have to be passed in explicitly: they are added where the error is throw in
    the context of {!adorn_error_with_span}. *)
type austral_error = AustralError of {
      module_name: module_name option;
      (** The name of the module where the error occurred, if available. *)
      kind: error_kind;
      (** The error kind. *)
      text: err_text;
      (** The error text. *)
      span: span option;
      (** The source span where the error happened, if available. *)
      source_ctx: source_ctx option;
      (** The source code where the error happened, if available. *)
    }

(** The exception that carries Austral errors. *)
exception Austral_error of austral_error

(** Raise an Austral error given the kind and text. *)
val austral_raise : error_kind -> err_text -> 'a

(** Add a source context to an error if it doesn't have one. *)
val add_source_ctx : austral_error -> source_ctx -> austral_error

(** Run the callback, and if it throws an error that doesn't have a span, put
    the given span in the error and rethrow it. *)
val adorn_error_with_span : span -> (unit -> 'a) -> 'a

(** Render an error kind to a title. *)
val error_title : error_kind -> string

(** Render an error to plain text. *)
val render_error_to_plain : austral_error -> string

(** Utility: raise a generic error. *)
val err : string -> 'a

(** Utility: raise an internal error. *)
val internal_err : string -> 'a
