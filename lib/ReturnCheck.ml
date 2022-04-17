open Identifier
open Ast
open Combined
open Error

let is_abort (name: qident): bool =
  let mn: module_name = source_module_name name
  and orig: identifier = original_name name
  in
  (((mod_name_string mn) = "Austral.Pervasive")
   && ((ident_string orig) = "Abort"))

let rec ends_in_return (stmt: astmt): bool =
  match stmt with
  | ASkip _ ->
     false
  | ALet (_, _, _, _, body) ->
     ends_in_return body
  | ADestructure (_, _, _, body) ->
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

let check_ends_in_return (CombinedModule { name=mn; decls; _ }): unit =
  let check_method_ends_in_return (CMethodDef (name, _, _, _, body)): unit =
    if ends_in_return body then
      ()
    else
      err ("Method "
           ^ (ident_string name)
           ^ " in module "
           ^ (mod_name_string mn)
           ^ " doesn't end in a return statement.")
  in
  let check_decl_ends_in_return (decl: combined_definition): unit =
    match decl with
    | CConstant _ ->
       ()
    | CTypeAlias _ ->
       ()
    | CRecord _ ->
       ()
    | CUnion _ ->
       ()
    | CFunction (_, name, _, _, _, body, _, pragmas) ->
       (match pragmas with
        | [] ->
           (* No pragmas: regular function. *)
           if ends_in_return body then
             ()
           else
             err ("Function "
                  ^ (ident_string name)
                  ^ " in module "
                  ^ (mod_name_string mn)
                  ^ " doesn't end in a return statement.")
        | _ ->
           (* Yes pragmas: foreign function, ignore the body. *)
           ())
    | CTypeclass _ ->
       ()
    | CInstance (_, _, _, _, methods, _) ->
       let _ = List.map check_method_ends_in_return methods in
       ()
  in
  let _ = List.map check_decl_ends_in_return decls
  in
  ()
