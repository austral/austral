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

structure Module :> MODULE = struct
    type name = Name.module_name

    type type_visibility = Syntax.type_visibility

    type func_visibility = Syntax.type_visibility

    type docstring = Syntax.docstring

    type ty = Type.ty

    type imports = (Name.ident, Name.module_name) Map.map

    datatype module = Module of name * docstring * imports * (Name.ident, declaration) Map.map
         and declaration = RecordDefinition of docstring * type_visibility * name * slot_definition list
                         | UnionDefinition of docstring * type_visibility * name * case_definition list
                         | FunctionDefinition of docstring * func_visibility * name * param list * ty

         and slot_definition = SlotDefinition of name * ty * docstring

         and case_definition = CaseDefinition of name * ty option * docstring

         and param = Param of name * ty * docstring

    fun resolve (Syntax.Module (docstring, name, imports, declarations)) =
        Module (name, docstring, resolveImports imports, resolveDeclarations declarations)

    and resolveImports importList =
        raise Fail "derp"

    and resolveDeclarations declarationList =
        map resolveDeclaration declarationList

    and resolveDeclaration (Syntax.TypeAlias _) =
        raise Fail "Not implemented"
      | resolveDeclaration (Syntax.RecordDefinition (ds, tv, name, slots)) =
        RecordDefinition (ds, tv, name, map resolveSlotDefinition slots)
      | resolveDeclaration (Syntax.UnionDefinition (ds, tv, name, cases)) =
        UnionDefinition (ds, tv, name, map resolveCaseDefinition cases)
      | resolveDeclaration (Syntax.FunctionDefinition (ds, fv, name, params, rt, expr)) =
        let fun resolveParam (Syntax.Param (n, ts, ds)) =
                Param (n, resolveType ts, ds)
        in
            FunctionDefinition (ds, fv, name, map resolveParam params, resolveType rt, expr)
        end

    and resolveType (Syntax.NamedType name) =
        raise Fail "Not implemented"
      | resolveType (Syntax.Address ts) =
        Address (resolveType ts)
      | resolveType (Syntax.Pointer ts) =
        Pointer (resolveType ts)
      | resolveType (Syntax.TupleType tss) =
        TupleType (map resolveType tss)
end
