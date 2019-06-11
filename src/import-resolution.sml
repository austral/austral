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

structure ImportResolution :> IMPORT_RESOLUTION = struct
    type import = Syntax.import
    type name = Name.ident
    type module_name = Name.module_name

    (* Import resolution means taking a list of imports from the syntax module
       and resolving them to a map of imported names to the modules that import
       them.

       In the process, we need to validate a few things:

       1. Check that imports refer to modules that exist in the module
       environment.

       2. Check that imported names refer to declarations that exist in the
       referenced module.

       3. Check that those declarations are public (in the case of functions) or
       either public or opaque (in the case of types). Otherwise, we can't
       import them.

       4. Check that we don't repeat any names within an import list.

       5. Ensure that imports don't collide with each other: if we have `from A
       import a` and `from B import a`, this is clearly an error.

       Note that we don't check that imports don't collide with declarations in
       the module. This is because this pass just focuses on the imports as a
       standalone unit.

    *)

    fun validateImports imports menv =
        let val imports = flatten imports
        in
            map (validateImport menv) imports;
            Imports.fromList imports
        end

    and flatten imports =
        let fun flattenImport mn (Syntax.ImportedName name) =
                Import.mkImport { name=name,
                                  trueName=name,
                                  moduleName=mn }
              | flattenImport mn (Syntax.ImportedNameAs { rename, original }) =
                Import.mkImport { name=rename,
                                  trueName=original,
                                  moduleName=mn }
        in
            List.concat (map (fn (Syntax.Import (moduleName, names)) =>
                                 map (flattenImport moduleName) names)
                                 imports)
        end

    and validateImport menv import =
        let val module = referencedModuleExists menv import
        in
            let val decl = declarationExists module import
            in
                if declarationIsVisible decl then
                    ()
                else
                    Error.semantic ("Attempted to import a private name: '"
                                    ^
                                    (Name.identString name)
                                    ^
                                    "' in the module '"
                                    ^
                                    (Name.moduleNameString (Module.moduleName module))
                                    ^
                                    "' is private")
            end
        end

    and referencedModuleExists menv import =
        let val moduleName = Import.importModuleName import
        in
            case Module.getModule menv moduleName of
                (SOME module) => ()
              | NONE => Error.semantic ("No module with this name: " ^ (Name.moduleNameString moduleName))
        end

    and declarationExistse module import =
        case Module.getDeclaration module (Import.importTrueName import) of
            (SOME decl) => decl
          | NONE => Error.semantic ("Imported name '"
                                    ^
                                    (Name.identString (Import.importTrueName import))
                                    ^
                                    "' does not exist in module '"
                                    ^
                                    (Name.moduleNameString (Module.moduleName module))
                                    ^
                                    "'")

    (* Check if a declaration can be imported *)
    and declarationIsVisible (Module.RecordDefinition (_, vis, _, _)) =
        typeIsImportable vis
      | declarationIsVisible (Module.UnionDefinition (_, vis, _, _)) =
        typeIsImportable vis
      | declarationIsVisible (Module.FunctionDefinition (_, vis, _, _, _)) =
        functionIsImportable vis

    (* Given a type's visibility declaration, check if it can be imported *)
    and typeIsImportable Syntax.PublicType =
        true
      | typeIsImportable Syntax.OpaqueType =
        true
      | typeIsImportable Syntax.PrivateType =
        false

    (* Given a functions's visibility declaration, check if it can be imported *)
    and functionIsImportable Syntax.PublicFunction =
        true
      | functionIsImportable Syntax.PrivateFunction =
        false
end
