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

structure Type :> TYPE = struct
  datatype param = TypeParam of Symbol.symbol
                 | RegionParam of Symbol.symbol

  datatype gtypespec = Unit
                     | Bool
                     | Integer of signedness * width
                     | Name of Symbol.symbol
                     | TypeCons of Symbol.symbol * (gtypespec list)
       and signedness = Unsigned | Signed
       and width = Int8 | Int16 | Int32 | Int64

  datatype typedef = TypeAlias of param list * gtypespec
                   | Datatype of param list * gtypespec

  type tenv = (Symbol.symbol, typedef) Map.map
end
