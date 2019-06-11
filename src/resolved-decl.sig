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

signature RESOLVED_DECL = sig
    type name = Name.ident
    type type_visibility = Syntax.type_visibility
    type func_visibility = Syntax.func_visibility
    type docstring = Syntax.docstring
    type type_specifier = Syntax.type_specifier
    type slot_definition = Syntax.slot_definition
    type case_definition = Syntax.case_definition
    type param = Syntax.param

    datatype module = Module of Name.module_name * docstring * Import.imports * (name, Syntax.declaration) Map.map
end
