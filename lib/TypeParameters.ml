open Identifier
open Type
open Error
open Sexplib
open Std

type typarams = TyParams of type_parameter list
[@@deriving (show, sexp)]

let empty_typarams: typarams = TyParams []

let typarams_size (typarams: typarams): int =
  let (TyParams lst) = typarams in
  List.length lst

let typaram_name (typaram: type_parameter): identifier =
  let (TypeParameter (name, _, _, _)) = typaram in
  name

let get_typaram (typarams: typarams) (name: identifier): type_parameter option =
  let (TyParams lst) = typarams in
  let pred (typaram: type_parameter): bool =
    equal_identifier name (typaram_name typaram)
  in
  List.find_opt pred lst

let add_typaram (typarams: typarams) (typaram: type_parameter): typarams =
  match get_typaram typarams (typaram_name typaram) with
  | Some _ ->
     err "Duplicate type parameter."
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
