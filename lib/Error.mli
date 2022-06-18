(** Error reporting infrastructure. *)
open Identifier
open Span
open SourceContext
open ErrorText

(** An Austral compiler error contains: the name of the module where the error
    occurred (if available), the error title, the error description, the span of code where the
    error occured (if available), the source context associated to that span (if
    available).

    The span and source context rarely have to be passed in explicitly: they are
    added where the error is throw in the context of
    {!adorn_error_with_span}. *)
type austral_error = AustralError of {
      module_name: module_name option;
      title: string;
      text: err_text;
      span: span option;
      source_ctx: source_ctx option;
    }

(** The exception that carries Austral errors. *)
exception Austral_error of austral_error

(** Raise an Austral error given the title and text. *)
val austral_raise : string -> err_text -> 'a

(** Add a source context to an error if it doesn't have one. *)
val add_source_ctx : austral_error -> source_ctx -> austral_error

(** Run the callback, and if it throws an error that doesn't have a span, put
    the given span in the error and rethrow it. *)
val adorn_error_with_span : span -> (unit -> 'a) -> 'a

(** Render an error to plain text. *)
val render_error_to_plain : austral_error -> string

(** Utility: raise a generic error. *)
val err : string -> 'a

(** Utility: raise an internal error. *)
val internal_err : string -> 'a
