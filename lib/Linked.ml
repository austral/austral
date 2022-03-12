open Identifier
open Common
open Imports
open Ast
open Type
open Env
open Combined

(** The linked representation is essentially the same as the combined
    representation, but declarations are linked to their corresponding entry in
    the environment. *)

type linked_module = CombinedModule of {
      mod_id: mod_id;
      name: module_name;
      kind: module_kind;
      interface_docstring: docstring;
      interface_imports: import_map;
      body_docstring: docstring;
      body_imports: import_map;
      decls: linked_definition list;
    }

and linked_definition =
  | LConstant of decl_id * vis * identifier * qtypespec * aexpr * docstring
  | LTypeAlias of decl_id * type_vis * identifier * type_parameter list * universe * qtypespec * docstring
  | LRecord of decl_id * type_vis * identifier * type_parameter list * universe * qslot list * docstring
  | LUnion of decl_id * type_vis * identifier * type_parameter list * universe * qcase list * docstring
  | LFunction of decl_id * vis * identifier * type_parameter list * qparam list * qtypespec * astmt * docstring * pragma list
  | LTypeclass of decl_id * vis * identifier * type_parameter * linked_method_decl list * docstring
  | LInstance of decl_id * vis * qident * type_parameter list * qtypespec * linked_method_def list * docstring

and linked_method_decl = LMethodDecl of decl_id * identifier * qparam list * qtypespec * docstring

and linked_method_def = LMethodDef of ins_meth_id * identifier * qparam list * qtypespec * docstring * astmt
