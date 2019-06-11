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
    type name = Name.name
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
        (* We are given a list of import statements. We call validateImport on
           each to get a set of imported names, and also to perform validation
           on that specific import list. We check that no import names are
           repeated across these sets, and finally, we construct an
           `Import.imports` object. *)
        let val importedNames : (module_name * Name.ident Set.set) list = map (validateImport menv) imports
        in
            (* To check that no names are repeated, we merge all sets into a
               single set, and compare sizes *)
            let val bigSet = Set.fromList (map (fn (_, s) => Set.toList s) importedNames)
                and totalNames = List.foldl (op +) 0 (map (fn (_, s) => Set.size s) importedNames)
            in
                if Set.size bigSet <> totalNames then
                    Error.semantic "Colliding import"
                else
                    let val imports = map (fn (mn, s) =>
                                              map (fn elem => Import.mkImport { name = elem, moduleName = mn))
                                                  (Set.toList s))
                                          importedNames
                    in
                        Import.fromList (List.concat imports)
                    end
            end
        end

    and validateImport menv (Syntax.Import (moduleName, names)) =
        let fun importNamesToSet names =
                let fun mapper (Syntax.ImportedName name) =
                        name
                      | mapper (Syntax.ImportedNameAs { rename, original }) =
                        rename
                in
                    let val names = map mapper names
                    in
                        Set.fromList names
                    end
                end
        in
            case Module.getModule menv moduleName of
                (SOME module) => let val importedNames = importNamesToSet names
                                 in
                                     map (validateImportedName module) names;
                                     if Set.size importedNames <> (List.length names) then
                                         Error.syntax "Repeated import"
                                     else
                                         (* All validation (except for point 5
                                            above) has been performed by this
                                            point, so construct and return set
                                            of ImportedName objects *)
                                         (moduleName, importedNames)
                                 end
              | NONE => Error.semantic ("No module with this name: " ^ (Name.moduleNameString moduleName))
        end

    and validateImportedName module name =
        let fun getName (Syntax.ImportedName name) =
                name
              | getName (Syntax.ImportedNameAs { original, rename }) =
                original
        in
            let val name = getName name
            in
                let val decl = validateDeclarationExists module name
                in
                    if validateDeclarationVisibility decl then
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
        end

    (* Check that a declaration exists *)
    and validateDeclarationExists module name =
        case Module.getDeclaration module name of
            (SOME decl) => decl
          | NONE => Error.semantic ("Imported name '"
                                    ^
                                    (Name.identString name)
                                    ^
                                    "' does not exist in module '"
                                    ^
                                    (Name.moduleNameString (Module.moduleName module))
                                    ^
                                    "'")

    (* Check if a declaration can be imported *)
    and validateDeclarationVisibility (Module.RecordDefinition (_, vis, _, _)) =
        validTypeVis vis
      | validateDeclarationVisibility (Module.UnionDefinition (_, vis, _, _)) =
        validTypeVis vis
      | validateDeclarationVisibility (Module.FunctionDefinition (_, vis, _, _, _)) =
        validFuncVis vis

    (* Given a type's visibility declaration, check if it can be imported *)
    and validTypeVis Syntax.PublicType =
        true
      | validTypeVis Syntax.OpaqueType =
        true
      | validTypeVis Syntax.PrivateType =
        false

    (* Given a functions's visibility declaration, check if it can be imported *)
    and validFuncVis Syntax.PublicFunction =
        true
      | validFuncVis Syntax.PrivateFunction =
        false
end
