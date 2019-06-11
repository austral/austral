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
    (* Resolve named types *)

    fun resolveNamedType menv (ResolvedDecl.Module (moduleName, _, imports, decls)) name =
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

    fun resolveType menv (Syntax.NamedType name) =
        resolveNamedType menv name
      | resolveType menv (Syntax.Address ty) =
        Type.Address (resolveType menv ty)
      | resolveType menv (Syntax.Pointer ty) =
        Type.Pointer (resolveType menv ty)
      | resolveType menv (Syntax.TupleType tys) =
        Type.TupleType (map (resolveType menv) tys)

    (* Here, we have to resolve type specifiers. We go through all declarations,
       turning type specifiers into type objects, assigning named types
       appropriately based on whether the type is an imported name or a locally
       defined name (or, if it's undefined, throwing an error). *)

    fun resolve (ResolvedDecl.Module (name, docstring, imports, decls)) =
        TypedDecl.Module (name, docstring, imports, resolveDecls decls)

    and resolveDecls decls =
        Map.fromList (map resolveDecl (Map.toList decls))

    and resolveDecl menv (SyntaxDecl.RecordDefinition (docstring, vis, name, slots)) =
        TypedDecl.RecordDefinition (docstring, vis, name, map (resolveSlot menv) slots)
      | resolveDecl (SyntaxDecl.UnionDefinition (docstring, vis, name, cases)) =
        TypedDecl.UnionDefinition (docstring, vis, name, map (resolveCase) cases)
      | resolveDecl (SyntaxDecl.FunctionDefinition (docstring, vis, name, params, rt, body)) =
        TypedDecl.FunctionDefinition (docstring, vis, name, map (resolveParam params), resolveType rt, body)

    and resolveSlot menv (Syntax.SlotDefinition (name, ty, docstring)) =
        TypedDecl.SlotDefinition (name, resolveType menv ty, docstring)

    and resolveCase menv (Syntax.CaseDefinition (name, tyOpt, docstring)) =
        TypedDecl.CaseDefinition (name, Option.map (resolveType menv) ty, docstring)

    and resolveParam menv (Syntax.Param (name, ty, docstring)) =
        TypedDecl.Param (name, resolveType menv ty, docstring)
end
