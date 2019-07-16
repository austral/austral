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

signature ORDERING_PASS = sig
    val transform : Syntax.module -> OrderedDecl.module
end

structure OrderingPass :> ORDERING_PASS = struct
    fun transform (Syntax.Module (docstring, name, imports, declarations)) =
      OrderedDecl.Module (name, docstring, imports, transformDeclarations declarations)

    and transformDeclarations decls =
      Map.fromList (map (fn decl => (declarationName decl, decl)) decls)

    and declarationName (Syntax.RecordDefinition (_, _, name, _)) = name
      | declarationName (Syntax.UnionDefinition (_, _, name, _)) = name
      | declarationName (Syntax.FunctionDefinition (_, _, name, _, _, _)) = name
end
