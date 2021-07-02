open Identifier
open Type
open Error

type lexenv = (identifier * ty) list

let empty_lexenv =
  []

let get_var l name =
  Option.map (fun (_, t) -> t) (List.find_opt (fun (n, _) -> n = name) l)

let push_var l name ty =
  match get_var l name with
  | (Some _) ->
     err "push_var: var with this name already exists"
  | None ->
     (name, ty) :: l

let pop_var = function
  | (_::rest) ->
     rest
  | [] ->
     err "pop_var called with empty lexenv"
