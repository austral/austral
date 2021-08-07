open Identifier
open Common
open Imports
open Ast
open Type
open Semantic

type combined_module = CombinedModule of {
      name: module_name;
      kind: module_kind;
      interface_imports: import_map;
      body_imports: import_map;
      decls: combined_definition list;
    }

and combined_definition =
  | CConstant of vis * identifier * qtypespec * aexpr * docstring
  | CTypeAlias of type_vis * identifier * type_parameter list * universe * qtypespec * docstring
  | CRecord of type_vis * identifier * type_parameter list * universe * qslot list * docstring
  | CUnion of type_vis * identifier * type_parameter list * universe * qcase list * docstring
  | CFunction of vis * identifier * type_parameter list * qparam list * qtypespec * astmt * docstring * pragma list
  | CTypeclass of vis * identifier * type_parameter * combined_method_decl list * docstring
  | CInstance of vis * identifier * type_parameter list * qtypespec * combined_method_def list * docstring

and qslot = QualifiedSlot of identifier * qtypespec

and qcase = QualifiedCase of identifier * qslot list

and qparam = QualifiedParameter of identifier * qtypespec

and combined_method_decl = CMethodDecl of identifier * qparam list * qtypespec * docstring

and combined_method_def = CMethodDef of identifier * qparam list * qtypespec * docstring * astmt
