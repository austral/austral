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

structure RCST :> RCST = struct
  datatype rcst = IntConstant of int
                | StringConstant of CST.escaped_string
                | Symbol of Symbol.symbol
                | Keyword of Symbol.symbol_name
                | List of rcst list

  fun resolve _ _ (CST.IntConstant i) = IntConstant i
    | resolve _ _ (CST.StringConstant es) = StringConstant es
    | resolve _ _ (CST.Keyword n) = Keyword n
    | resolve _ _ _ = raise Fail "NOT IMPLEMENTED YET"
end
