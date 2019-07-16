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

signature RESOLUTION_PASS = sig
    val transform : Module.menv -> OrderedDecl.module -> ResolvedDecl.module
end

structure ResolutionPass :> RESOLUTION_PASS = struct
    fun transform menv (OrderedDecl.Module (docstring, name, imports, declarations)) =
        let val imports = ImportResolution.resolve menv imports
        in
            checkCollision imports declarations;
            ResolvedDecl.Module (docstring, name, imports, declarations)
        end

    and checkCollision imports declarations =
        map (fn (name, _) =>
                (case Import.getImport imports name of
                     (SOME _) => Error.semantic "Import collides with local declaration"
                   | NONE => ()))
            (Map.toList declarations)
end
