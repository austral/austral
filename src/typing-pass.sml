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
    (* Here, we have to resolve type specifiers. We go through all declarations,
       turning type specifiers into type objects, assigning named types
       appropriately based on whether the type is an imported name or a locally
       defined name (or, if it's undefined, throwing an error). *)

    fun resolveNamedType menv (ResolvedDecl.Module (moduleName, _, imports, decls)) name =
        case Import.getImport imports name of
            (SOME import) => resolveImportedNamedType import
          | NONE => resolveLocalNamedType moduleName decls name

    and resolveImportedNamedType import =
        let val moduleName = Import.importModuleName import
        in
            (* We force the result with Option.valOf since we know, from earlier
               validation passes, that all imports point to existing modules *)
            let val module = Option.valOf (Module.getModule menv module)
            in
                case Option.valOf (Module.getDeclaration module (Import.importTrueName import)) of
                    (Module.RecordDefinition _) => Type.NamedType (moduleName, name)
                  | (Module.UnionDefinition _) => Type.NamedType (moduleName, name)
                  | _ => Error.semantic "Not a type definition"
            end
        end

    and resolveLocalNamedType moduleName decls name =
        case Map.get decls name of
            (Syntax.RecordDefinition _) => Type.NamedType (moduleName, name)
          | (Syntax.UnionDefinition _) => Type.NamedType (moduleName, name)
          | _ => Error.semantic "Not a type definition"
end
