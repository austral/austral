(*
    Copyright 2018 Fernando Borretti <fernando@borretti.me>

    This file is part of Boreal.

    Boreal is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Boreal is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Boreal.  If not, see <http://www.gnu.org/licenses/>.
*)

structure Module :> MODULE = struct
  open Symbol

  type nicknames = (module_name, module_name) Map.map

  datatype module = Module of module_name * nicknames * imports * exports
       and imports = Imports of (symbol_name, module_name) Map.map
       and exports = Exports of symbol_name Set.set

  datatype menv = MEnv of (module_name, module) Map.map

  fun moduleName (Module (n, _, _, _)) = n
  fun moduleNicknames (Module (_, ns, _, _)) = ns
  fun moduleExports (Module (_, _, _, Exports e)) = e
  fun moduleImports (Module (_, _, Imports i, _)) = i

  fun resolveNickname (m: module) (n: module_name) =
    case Map.get (moduleNicknames m) n of
        SOME n => n
      | NONE => n

  fun sourceModule m s =
    case (Map.get (moduleImports m) s) of
        SOME n => n
      | NONE => moduleName m

  fun doesModuleExport (m: module) (s: symbol_name) =
    Set.isIn (moduleExports m) s
end
