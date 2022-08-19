open Identifier
open Common
open Imports
open Ast
open Type
open TypeParameter

type combined_module = CombinedModule of {
      name: module_name;
      kind: module_kind;
      interface_docstring: docstring;
      interface_imports: import_map;
      body_docstring: docstring;
      body_imports: import_map;
      decls: combined_definition list;
    }

and unsourced_typarams = unsourced_typaram list

and combined_definition =
  | CConstant of vis * identifier * qtypespec * aexpr * docstring
  | CRecord of type_vis * identifier * unsourced_typarams * universe * qslot list * docstring
  | CUnion of type_vis * identifier * unsourced_typarams * universe * qcase list * docstring
  | CFunction of vis * identifier * unsourced_typarams * qparam list * qtypespec * astmt * docstring * pragma list
  | CTypeclass of vis * identifier * unsourced_typaram * combined_method_decl list * docstring
  | CInstance of vis * qident * unsourced_typarams * qtypespec * combined_method_def list * docstring

and qslot = QualifiedSlot of identifier * qtypespec

and qcase = QualifiedCase of identifier * qslot list

and qparam = QualifiedParameter of identifier * qtypespec

and combined_method_decl = CMethodDecl of identifier * unsourced_typarams * qparam list * qtypespec * docstring

and combined_method_def = CMethodDef of identifier * unsourced_typarams * qparam list * qtypespec * docstring * astmt
