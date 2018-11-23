(*
    Copyright 2018 Fernando Borretti <fernando@borretti.me>

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

signature RCST = sig
    datatype rcst = IntConstant of string
                  | FloatConstant of string
                  | StringConstant of CST.escaped_string
                  | Symbol of Symbol.symbol
                  | Keyword of Symbol.symbol_name
                  | Splice of rcst
                  | List of rcst list

    (* Given the current module and a CST tree, resolve all nicknames in the CST
       to the true module names *)
    val resolveNicknames : Module.module -> CST.cst -> CST.cst

    val resolve : Module.menv -> Module.module -> CST.cst -> rcst Util.result
end
