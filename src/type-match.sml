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

  datatype bindings = Bindings of (Symbol.symbol, ty) Map.map
                    | Failure of string

  val emptyBindings =
      Bindings Map.empty

  fun mergeBindings (Bindings bs) (Bindings bs') =
      let val bsKeys = Map.keys bs
          and bsKeys' = Map.keys bs'
      in
          (* Ensure the bindings don't have conflicting elements! *)
          let val common = Set.intersection bsKeys bsKeys'
          in
              if List.all (fn key =>
                              let val a = Option.valOf (Map.get bs key)
                                  and b = Option.valOf (Map.get bs' key)
                              in
                                  a = b
                              end)
                          (Set.toList common) then
                  Bindings (Map.mergeMaps bs bs')
              else
                  Failure "Conflicting type variables"
          end
      end
    | mergeBindings (Bindings _) (Failure f) =
      Failure f
    | mergeBindings (Failure f) (Bindings _) =
      Failure f
    | mergeBindings (Failure f) (Failure f') =
      Failure f

  fun matchType Unit Unit =
      emptyBindings
    | matchType Bool Bool =
      emptyBindings
    | matchType (Integer (s, w)) (Integer (s', w')) =
      if (s = s') andalso (w = w') then
          emptyBindings
      else
          Failure "int subtypes dont match"
    | matchType (Float f) (Float f') =
      if f = f' then
          emptyBindings
      else
          Failure "float subtypes dont match"
    | matchType (Tuple tys) (Tuple tys') =
      matchTypeLists tys tys'
    | matchType (Pointer t) (Pointer t') =
      (case matchType t t' of
           (Bindings l) => Bindings l
         | (Failure f) => Failure f)
    | matchType (ForeignPointer t) (ForeignPointer t') =
      (case matchType t t' of
           (Bindings l) => Bindings l
         | (Failure f) => Failure f)
    | matchType (StaticArray (t, len)) (StaticArray (t', len')) =
      (case matchType t t' of
           (Bindings l) => if len = len' then
                               Bindings l
                           else
                               Failure "Static array length doesn't match"
         | (Failure f) => Failure f)
    | matchType (Disjunction (n, args, _)) (Disjunction (n', args', _)) =
      if n = n' then
          matchTypeLists args args'
      else
          Failure "Disjunction names don't match"
    | matchType (TypeVariable n) t =
      Bindings (Map.fromList [(n, t)])
    | matchType _ _ =
      Failure "Type mismatch"

  and matchTypeLists tys tys' =
      List.foldl (fn (a, b) => mergeBindings a b)
                 emptyBindings
                 (ListPair.map (fn (a, b) => matchType a b) (tys, tys'))
end
