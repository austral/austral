(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Identifier
open TypeParameter
open Error
open ErrorText
open Sexplib
open Std

module Errors = struct
  let duplicate_type_parameter param =
    let text = match param with 
    | Some param -> [
        Text "Duplicate type parameter ";
        Code (typaram_name param |> ident_string)
      ]
    | None -> [
        Text "Multiple type parameters have the same name.";
      ]
    in
    austral_raise GenericError text
end

type typarams = TyParams of type_parameter list
[@@deriving (show, sexp)]

let empty_typarams: typarams = TyParams []

let typarams_size (typarams: typarams): int =
  let (TyParams lst) = typarams in
  List.length lst

let get_typaram (typarams: typarams) (name: identifier): type_parameter option =
  let (TyParams lst) = typarams in
  let pred (typaram: type_parameter): bool =
    equal_identifier name (typaram_name typaram)
  in
  List.find_opt pred lst

let add_typaram (typarams: typarams) (typaram: type_parameter): typarams =
  match get_typaram typarams (typaram_name typaram) with
  | Some _ ->
    Errors.duplicate_type_parameter (Some typaram)
  | None ->
    let (TyParams lst) = typarams in
    let lst = List.rev lst in
    let lst = typaram :: lst in
    let lst = List.rev lst in
    TyParams lst

let typarams_as_list (typarams: typarams): type_parameter list =
  let (TyParams lst) = typarams in
  lst

let typarams_from_list (lst: type_parameter list): typarams =
  List.fold_left (fun set typaram -> add_typaram set typaram)
    empty_typarams
    lst

let merge_typarams (a: typarams) (b: typarams): typarams =
  (* Convert both sets to lists *)
  let al: type_parameter list = typarams_as_list a
  and bl: type_parameter list = typarams_as_list b
  in
  (* If any element of b appears in a, error. *)
  let _ =
    List.map (fun tp ->
        if List.exists (fun tp' -> equal_identifier (typaram_name tp) (typaram_name tp')) al then
          Errors.duplicate_type_parameter None
        else
          ()) bl
  in
  TyParams (List.concat [al; bl])
