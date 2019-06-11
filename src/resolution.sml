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

structure Resolution :> RESOLUTION = struct
    type module_name = Module.module_name

    type module = DAst.module
    type declaration = DAst.declaration

    (* Here we implement the remainder of module resolution. This is mostly
       mapping syntax declarations to module declarations. The validation
       perform at this stage is basically ensuring declaration names don't
       collide (with each other, or with imports). Additionally, we resolve type
       specifiers to type objects.

    *)

    fun resolve menv (Syntax.Module (docstring, name, imports, declarations)) =
        Module.Module (name, docstring, resolveImports imports, resolveDeclarations declarations)

    and resolveImports menv importList =
        ImportResolution.resolve importList menv

    and resolveDeclarations declarationList =
        Error.notImplemented ()

    and resolveDeclaration (Syntax.RecordDefinition (ds, tv, name, slots)) =
        let fun resolveSlot (Syntax.SlotDefinition (n, ts, ds)) =
                DAst.SlotDefinition (n, resolveType ts, ds)
        in
            DAst.RecordDefinition (ds, tv, name, map resolveSlot slots)
        end
      | resolveDeclaration (Syntax.UnionDefinition (ds, tv, name, cases)) =
        let fun resolveCase (Syntax.CaseDefinition (n, ts, ds)) =
                DAst.CaseDefinition (n, Option.map resolveType ts, ds)
        in
            DAst.UnionDefinition (ds, tv, name, map resolveCase cases)
        end
      | resolveDeclaration (Syntax.FunctionDefinition (ds, fv, name, params, rt, expr)) =
        let fun resolveParam (Syntax.Param (n, ts, ds)) =
                DAst.Param (n, resolveType ts, ds)
        in
            DAst.FunctionDefinition (ds, fv, name, map resolveParam params, resolveType rt, expr)
        end

    and resolveType (Syntax.NamedType name) =
        Error.notImplemented ()
      | resolveType (Syntax.Address ts) =
        Type.Address (resolveType ts)
      | resolveType (Syntax.Pointer ts) =
        Type.Pointer (resolveType ts)
      | resolveType (Syntax.TupleType tss) =
        Type.TupleType (map resolveType tss)
end
