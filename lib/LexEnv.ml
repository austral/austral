open IdentifierMap
open Type
open Error

type scope = Scope of ty IdentifierMap.t

(* The first element is the innermost scope. *)
type lexenv = LexEnv of scope list

let empty_lexenv =
  LexEnv []

let empty_scope =
  Scope IdentifierMap.empty

let push_scope (LexEnv l) =
  LexEnv (List.cons empty_scope l)

let pop_scope (LexEnv l) =
  match l with
  | _::rest -> LexEnv rest
  | [] -> err "Empty lexical environment"

let rec get_var' l name =
  match l with
  | first::rest ->
     let (Scope m) = first in
     (match IdentifierMap.find_opt name m with
      | Some ty ->
         Some ty
      | None ->
         get_var' rest name)
  | [] -> None

let get_var (LexEnv l) name =
  get_var' l name

let add_var lenv name ty =
  match get_var lenv name with
  | Some _ ->
     err "Binding already exists in lexenv"
  | None ->
     let (LexEnv scopes) = lenv in
     match scopes with
     | (Scope s)::rest ->
        LexEnv (List.cons (Scope (IdentifierMap.add name ty s)) rest)
     | [] ->
        LexEnv [Scope (IdentifierMap.singleton name ty)]
