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

signature DAST = sig
    type name = Name.ident
    type module_name = Name.module_name
    type type_visibility = Syntax.type_visibility
    type func_visibility = Syntax.func_visibility
    type docstring = Syntax.docstring
    type ty = Type.ty
    type expr = Syntax.expr

    type imports = (Name.ident, module_name) Map.map

    datatype module = Module of module_name * docstring * imports * (Name.ident, declaration) Map.map
         and declaration = RecordDefinition of docstring * type_visibility * name * slot_definition list
                         | UnionDefinition of docstring * type_visibility * name * case_definition list
                         | FunctionDefinition of docstring * func_visibility * name * param list * ty * expr

         and slot_definition = SlotDefinition of name * ty * docstring

         and case_definition = CaseDefinition of name * ty option * docstring

         and param = Param of name * ty * docstring
end
