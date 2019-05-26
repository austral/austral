(*
    Copyright 2018–2019 Fernando Borretti <fernando@borretti.me>

    This file is part of Austral.

    Austral is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Austral is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Austral.  If not, see <http://www.gnu.org/licenses/>.
*)

structure Syntax :> SYNTAX = struct
    (* Type aliases *)

    type name = Ident.ident

    datatype docstring = Docstring of string

    (* Expressions *)

    datatype type_specifier = NamedType of name
                            | Address of type_specifier
                            | Pointer of type_specifier
                            | TupleType of type_specifier list

    datatype expr = BoolConstant of bool
                  | IntConstant of string
                  | FloatConstant of string
                  | StringConstant of Escape.escaped_string
                  | Variable of name
                  | Let of binding list * expr
                  | If of expr * expr * expr
                  | ArithOp of Arith.kind * Arith.oper * expr * expr
                  | CompOp of Builtin.comp_op * expr * expr
                  | TupleCreate of expr list
                  | TupleProj of expr * int
                  | SizeOf of type_specifier
                  | Block of expr list
                  | Funcall of name * expr list
         and binding = VarBinding of name * expr
                     | TupBinding of name list * expr

    (* Declarations *)

    datatype module = Module of name * import list * declaration list

         and import = Import of name * imported_name list

         and imported_name = ImportedName of name
                           | ImportedNameAs of { original: name, rename: name }

         and declaration = TypeDeclaration of name * type_visibility * type_definition
                         | FunctionDefinition of func_visibility * name * params * type_specifier * expr

         and type_visibility = PublicType
                             | OpaqueType
                             | PrivateType

         and type_definition = TypeAlias of type_specifier
                             | RecordDefinition of name * slot_definition list
                             | UnionDefinition of name * case_definition list

         and slot_definition = SlotDefinition of name * type_specifier

         and case_definition = CaseDefinition of name * type_specifier option

         and params = ParamList of param list

         and param = Param of name * type_specifier
end
