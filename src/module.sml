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
  type module_name = Ident.ident
  type symbol_name = Ident.ident

  datatype module = Module of module_name * imports * exports
       and imports = Imports of (symbol_name, module_name) Map.map
       and exports = Exports of symbol_name Set.set

  datatype menv = MEnv of (module_name, module) Map.map

  fun moduleName (Module (n, _, _)) = n

  fun moduleExports (Module (_, _, Exports e)) = e
  fun moduleImports (Module (_, Imports i, _)) = i
end
