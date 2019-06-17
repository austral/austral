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

structure TypingPass :> TYPING_PASS = struct
    type ctx = Module.menv * ResolvedDecl.module

    (* Resolve named types *)

    fun resolveNamedType (menv, (ResolvedDecl.Module (moduleName, _, imports, decls))) name =
        case Import.getImport imports name of
            (SOME import) => resolveImportedNamedType menv import
          | NONE => resolveLocalNamedType moduleName decls name

    and resolveImportedNamedType menv import =
        let val moduleName = Import.importModuleName import
            and name = Import.importTrueName import
        in
            (* We force the result with Option.valOf since we know, from earlier
               validation passes, that all imports point to existing modules *)
            let val module = Option.valOf (Module.getModule menv moduleName)
            in
                case (Option.valOf (Module.getDeclaration module name)) of
                    (Module.RecordDefinition _) => Type.NamedType (moduleName, name)
                  | (Module.UnionDefinition _) => Type.NamedType (moduleName, name)
                  | _ => Error.semantic "Not a type definition"
            end
        end

    and resolveLocalNamedType moduleName decls name =
        case Map.get decls name of
            (SOME (Syntax.RecordDefinition _)) => Type.NamedType (moduleName, name)
          | (SOME (Syntax.UnionDefinition _)) => Type.NamedType (moduleName, name)
          | (SOME _) => Error.semantic "Not a type definition"
          | NONE => Error.semantic "No type with this name"

    (* Resolve all type specifiers *)

    fun resolveType ctx (Syntax.NamedType name) =
        resolveNamedType ctx name
      | resolveType ctx (Syntax.Address ty) =
        Type.Address (resolveType ctx ty)
      | resolveType ctx (Syntax.Pointer ty) =
        Type.Pointer (resolveType ctx ty)
      | resolveType ctx (Syntax.TupleType tys) =
        Type.TupleType (map (resolveType ctx) tys)

    (* Here, we have to resolve type specifiers. We go through all declarations,
       turning type specifiers into type objects, assigning named types
       appropriately based on whether the type is an imported name or a locally
       defined name (or, if it's undefined, throwing an error). *)

    fun resolve ctx (ResolvedDecl.Module (name, docstring, imports, decls)) =
        TypedDecl.Module (name, docstring, imports, resolveDecls ctx decls)

    and resolveDecls ctx decls =
        Map.fromList (map (fn (name, decl) =>
                              (name, resolveDecl ctx decl))
                          (Map.toList decls))

    and resolveDecl ctx (Syntax.RecordDefinition (docstring, vis, name, slots)) =
        TypedDecl.RecordDefinition (docstring, vis, name, map (resolveSlot ctx) slots)
      | resolveDecl ctx (Syntax.UnionDefinition (docstring, vis, name, cases)) =
        TypedDecl.UnionDefinition (docstring, vis, name, map (resolveCase ctx) cases)
      | resolveDecl ctx (Syntax.FunctionDefinition (docstring, vis, name, params, rt, body)) =
        TypedDecl.FunctionDefinition (docstring, vis, name, map (resolveParam ctx) params, resolveType ctx rt, body)

    and resolveSlot ctx (Syntax.SlotDefinition (name, ty, docstring)) =
        Module.SlotDefinition (name, resolveType ctx ty, docstring)

    and resolveCase ctx (Syntax.CaseDefinition (name, tyOpt, docstring)) =
        TypedDecl.CaseDefinition (name, Option.map (resolveType ctx) tyOpt, docstring)

    and resolveParam ctx (Syntax.Param (name, ty, docstring)) =
        TypedDecl.Param (name, resolveType ctx ty, docstring)
end
