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

    type menv = Module.menv

    (* Module Resolution

       The following functions resolve Syntax.module objects into module
       objects.

       First, we resolve imports. Import validation is fairly straightforward:
       we have a list of import statements, each of which refers to a single
       module and imports a set of names from that module, and have to turn it
       into a map of identifiers to the name of the module that identifier comes
       from.

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

    *)

    fun validateImports imports menv =
        (* We are given a list of import statements. We call validateImport on
           each to get a set of imported names, and also to perform validation
           on that specific import list. We check that no import names are
           repeated across these sets, and finally, return a map of imported
           names to the name of the module they're imported from. *)
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
                    let val pairs = map (fn (mn, s) =>
                                            map (fn elem => (elem, mn)) (Set.toList s))
                                        importedNames
                    in
                        Map.fromList (List.concat pairs)
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

    and validateDeclarationVisibility (Module.RecordDefinition (_, vis, _, _)) =
        validTypeVis vis
      | validateDeclarationVisibility (Module.UnionDefinition (_, vis, _, _)) =
        validTypeVis vis
      | validateDeclarationVisibility (Module.FunctionDefinition (_, vis, _, _, _)) =
        validFuncVis vis

    and validTypeVis Syntax.PublicType =
        true
      | validTypeVis Syntax.OpaqueType =
        true
      | validTypeVis Syntax.PrivateType =
        false

    and validFuncVis Syntax.PublicFunction =
        true
      | validFuncVis Syntax.PrivateFunction =
        false

    (* Here we implement the remainder of module resolution. This is mostly
       mapping syntax declarations to module declarations. The validation
       perform at this stage is basically ensuring declaration names don't
       collide (with each other, or with imports). Additionally, we resolve type
       specifiers to type objects.

    *)

    fun resolve (Syntax.Module (docstring, name, imports, declarations)) =
        Module (name, docstring, resolveImports imports, resolveDeclarations declarations)

    and resolveImports importList =
        Error.notImplemented ()

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
            DAst.FunctionDefinition (ds, fv, name, map resolveParam params, resolveType rt)
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
