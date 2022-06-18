open Env
open Identifier
open Type
open Id
open EnvTypes
open TypeBindings
open Reporter
open EnvExtras
open TypeMatch
open Error

let get_instance (env: env) (source_module_name: module_name) (dispatch_ty: ty) (typeclass: decl_id): decl * type_bindings =
  with_frame "Typeclass Resolution"
    (fun _ ->
      ps ("In module", (mod_name_string source_module_name));
      ps ("Typeclass", (get_decl_name_or_die env typeclass));
      pt ("Dispatch type", dispatch_ty);
      let mod_id: mod_id =
        let (ModRec { id; _ }) = Option.get (get_module_by_name env source_module_name) in
        id
      in
      let pred (decl: decl): (decl * type_bindings) option =
        match decl with
        | Instance { typeclass_id; argument; _ } ->
           if equal_decl_id typeclass_id typeclass then
             let _ = pt ("Trying instance with argument", argument) in
             try
               let bindings = match_type argument dispatch_ty in
               Some (decl, bindings)
             with
               Austral_error _ ->
               (* Does not match, just skip to the next instance, *)
               None
           else
             None
        | _ ->
           None
      in
      let filtered: (decl * type_bindings) list =
        with_frame "Filtering instances"
          (fun _ -> List.filter_map pred (visible_instances env mod_id))
      in
      match filtered with
      | [a] ->
         a
      | _::_ ->
         err "Multiple instances satisfy this call."
      | [] ->
         err (
             "Typeclass resolution failed. Typeclass: "
             ^ (get_decl_name_or_die env typeclass)
             ^ ". Dispatch type: "
             ^ (type_string dispatch_ty)))
