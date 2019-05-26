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

signature SYNTAX = sig
    (* Type aliases *)

    type name = Ident.ident

    (* Expressions *)

    datatype symbol = QualifiedSymbol of name * name
                    | UnqualifiedSymbol of name

    datatype type_specifier = NamedType of symbol
                            | Address of type_specifier
                            | Pointer of type_specifier
                            | TupleType of type_specifier list

    (* Declarations *)

    datatype module = Module of name * import list * declaration list

         and import = Import of name * name list

         and declaration = TypeDeclaration of name * type_visibility * type_definition

         and type_visibility = PublicType
                             | OpaqueType
                             | PrivateType

         and type_definition = TypeAlias of type_specifier
                             | RecordDefinition of name * slot_definition list
                             | UnionDefinition of name * case_definition list

         and slot_definition = SlotDefinition of name * type_specifier

         and case_definition = CaseDefinition of name * type_specifier option
end
