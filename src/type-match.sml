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

structure TypeMatch = struct
  open Type

  datatype binding = Bind of Symbol.symbol * ty

  datatype bindings = Bindings of binding list
                    | Failure of string

  val emptyBindings = Bindings []

  fun mergeBindings (Bindings l) (Bindings l') =
      Bindings (l @ l')
    | mergeBindings (Bindings _) (Failure f) =
      Failure f
    | mergeBindings (Failure f) (Bindings _) =
      Failure f
    | mergeBindings (Failure f) (Failure f') =
      Failure f

  fun matchType Unit Unit = emptyBindings
    | matchType Unit Unit = emptyBindings
    | matchType (Integer (s, w)) (Integer (s', w')) =
      if (s = s') andalso (w = w') then
          emptyAssign
      else
          Failure "int subtypes dont match"
    | matchType (Float f) (Float f') =
      if f = f' then
          emptyBindings
      else
          Failure "float subtypes dont match"
    | matchType (Tuple tys) (Tuple tys') =
      List.foldl (fn (a, b) => mergeBindings a b)
                 emptyBindings
                 (ListPair.map (fn (a, b) => matchType a b) (tys, tys'))
    | matchType (Pointer t) (Pointer t') =
      (case matchType t t' of
           (Bindings l) => Bindings l
         | (Failure f) => Failure f)
    | matchType (ForeignPointer t) (ForeignPointer t') =
      (case matchType t t' of
           (Bindings l) => Bindings l
         | (Failure f) => Failure f)
    | matchType _ _ =
      false
end
