(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Identifier
open Stages.Ast
open Stages.Combined
open Reporter
open Error
open Span

module Errors = struct
  let missing_return ~name ~declaration =
    austral_raise DeclarationError [
      Text "The ";
      Text declaration;
      Text " ";
      Code (ident_string name);
      Text " does not end in a return statement."
    ]
end

let is_abort (name: qident): bool =
  let mn: module_name = source_module_name name
  and orig: identifier = original_name name
  in
  (((mod_name_string mn) = "Austral.Pervasive")
   && ((ident_string orig) = "abort"))

let rec ends_in_return (stmt: astmt): bool =
  match stmt with
  | ASkip _ ->
     false
  | ALet (_, _, _, _, body) ->
     ends_in_return body
  | ADestructure (_, _, _, _, body) ->
     ends_in_return body
  | AAssign _ ->
     false
  | AIf (_, _, tb, fb) ->
     (ends_in_return tb) && (ends_in_return fb)
  | AWhen (_, _, b) ->
     ends_in_return b
  | ACase (_, _, whens) ->
     let l = List.map (fun (AbstractWhen (_, _, body)) -> ends_in_return body) whens in
     List.for_all (fun x -> x) l
  | AWhile (_, _, body) ->
     ends_in_return body
  | AFor { body; _ } ->
     ends_in_return body
  | ABorrow { body; _ } ->
     ends_in_return body
  | ABlock (_, _, body) ->
     ends_in_return body
  | ADiscarding (_, e) ->
     (match e with
      | FunctionCall (name, _) ->
         is_abort name
      | _ ->
         false)
  | AReturn _ ->
     true
  | AInitialAssign _ ->
     false

let check_ends_in_return (CombinedModule { decls; _ }): unit =
  with_frame "Return check"
    (fun _ ->
      let check_method_ends_in_return (span: span) (CMethodDef (name, _, _, _, _, body)): unit =
        adorn_error_with_span span
          (fun _ ->
            if ends_in_return body then
              ()
            else
              Errors.missing_return
                ~name:name
                ~declaration:"method")
      in
      let check_decl_ends_in_return (decl: combined_definition): unit =
        match decl with
        | CConstant _ ->
           ()
        | CRecord _ ->
           ()
        | CUnion _ ->
           ()
        | CFunction (span, _, name, _, _, _, body, _, pragmas) ->
           adorn_error_with_span span
             (fun _ ->
               match pragmas with
               | [] ->
                  (* No pragmas: regular function. *)
                  if ends_in_return body then
                    ()
                  else
                    Errors.missing_return
                      ~name
                      ~declaration:"function"
               | _ ->
                  (* Yes pragmas: foreign function, ignore the body. *)
                  ())
        | CTypeclass _ ->
           ()
        | CInstance (span, _, _, _, _, methods, _) ->
           let _ = List.map (check_method_ends_in_return span) methods in
           ()
      in
      let _ = List.map check_decl_ends_in_return decls
      in
      ())
