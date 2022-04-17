open Identifier
open Tast
open Error

let is_abort (name: qident): bool =
  let mn: module_name = source_module_name name
  and orig: identifier = original_name name
  in
  (((mod_name_string mn) = "Austral.Pervasive")
   && ((ident_string orig) = "Abort"))

let rec ends_in_return (stmt: tstmt): bool =
  match stmt with
  | TSkip _ ->
     false
  | TLet (_, _, _, _, body) ->
     ends_in_return body
  | TDestructure (_, _, _, body) ->
     ends_in_return body
  | TAssign _ ->
     false
  | TIf (_, _, tb, fb) ->
     (ends_in_return tb) && (ends_in_return fb)
  | TCase (_, _, whens) ->
     let l = List.map (fun (TypedWhen (_, _, body)) -> ends_in_return body) whens in
     List.for_all (fun x -> x) l
  | TWhile (_, _, body) ->
     ends_in_return body
  | TFor (_, _, _, _, body) ->
     ends_in_return body
  | TBorrow { body; _ } ->
     ends_in_return body
  | TBlock (_, _, body) ->
     ends_in_return body
  | TDiscarding (_, e) ->
     (match e with
      | TFuncall (_, name, _, _, _) ->
         is_abort name
      | _ ->
         false)
  | TReturn _ ->
     true

let check_ends_in_return (TypedModule (mn, decls)): unit =
  let check_method_ends_in_return (TypedMethodDef (_, name, _, _, body)): unit =
    if ends_in_return body then
      ()
    else
      err ("Method "
           ^ (ident_string name)
           ^ " in module "
           ^ (mod_name_string mn)
           ^ " doesn't end in a return statement.")
  in
  let check_decl_ends_in_return (decl: typed_decl): unit =
    match decl with
    | TConstant _ ->
       ()
    | TTypeAlias _ ->
       ()
    | TRecord _ ->
       ()
    | TUnion _ ->
       ()
    | TFunction (_, _, name, _, _, _, body, _) ->
       if ends_in_return body then
         ()
       else
         err ("Function "
              ^ (ident_string name)
              ^ " in module "
              ^ (mod_name_string mn)
              ^ " doesn't end in a return statement.")
    | TForeignFunction _ ->
       ()
    | TTypeClass _ ->
       ()
    | TInstance (_, _, _, _, _, methods, _) ->
       let _ = List.map check_method_ends_in_return methods in
       ()
  in
  let _ = List.map check_decl_ends_in_return decls
  in
  ()
