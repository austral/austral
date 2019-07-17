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

structure Syntax = struct
    (* Type aliases *)

    type name = Name.ident

    datatype docstring = Docstring of string option

    (* Expressions *)

    (* Declarations *)

    datatype module = Module of docstring * Name.module_name * import list * declaration list

         and import = Import of Name.module_name * imported_name list

         and imported_name = ImportedName of name
                           | ImportedNameAs of { original: name, rename: name }

         and declaration = RecordDefinition of docstring * type_visibility * name * slot_definition list
                         | UnionDefinition of docstring * type_visibility * name * case_definition list
                         | FunctionDefinition of docstring * func_visibility * name * param list * type_specifier * expr

         and type_visibility = PublicType
                             | OpaqueType
                             | PrivateType

         and slot_definition = SlotDefinition of name * type_specifier * docstring

         and case_definition = CaseDefinition of name * type_specifier option * docstring

         and func_visibility = PublicFunction
                             | PrivateFunction

         and param = Param of name * type_specifier * docstring
end
