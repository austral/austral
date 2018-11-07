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

structure HirPass :> HIR_PASS = struct
    open HIR
    structure MT = MonoType

    val count = ref 0
    fun freshVar () =
        let
        in
            count := !count + 1;
            let val newid = !count
            in
                let val name = Symbol.mkSymbol (Ident.mkIdentEx "#", Ident.mkIdentEx ("g" ^ (Int.toString newid)))
                in
                    Symbol.Var (name, newid)
                end
            end
        end

    fun transformType MT.Unit =
        Unit
      | transformType MT.Bool =
        Bool
      | transformType (MT.Integer (s, w)) =
        Integer (s, w)
      | transformType (MT.Float f) =
        Float f
      | transformType (MT.Tuple tys) =
        Tuple (map transformType tys)
      | transformType (MT.Pointer ty) =
        Pointer (transformType ty)
      | transformType (MT.ForeignPointer ty) =
        Pointer (transformType ty)
      | transformType (MT.StaticArray ty) =
        StaticArray (transformType ty)
      | transformType (MT.Disjunction (name, id, _)) =
        Disjunction (name, id)

    structure M = MTAST

    fun transform M.UnitConstant =
        UnitConstant
      | transform (M.BoolConstant b) =
        BoolConstant b
      | transform (M.IntConstant (i, ty)) =
        IntConstant (i, transformType ty)
      | transform (M.FloatConstant (f, ty)) =
        FloatConstant (f, transformType ty)
      | transform (M.StringConstant s) =
        StringConstant s
      | transform (M.Variable (var, ty)) =
        Variable (var, transformType ty)
      | transform (M.Let (var, value, body)) =
        Let (var, transform value, transform body)
      | transform (M.Bind (vars, tup, body)) =
        let val tupvar = freshVar ()
        in
            Let (tupvar,
                 transform tup,
                 transformBind vars tupvar body)
        end
      | transform _ =
        raise Fail "Not done yet"

    and transformBind (vars: Symbol.variable list) (tupvar: Symbol.variable) (body: MTAST.ast) =
        let fun transformInner (head::tail) tupvar body i =
                Let (head,
                     TupleProj (Variable (tupvar, elemTy), i),
                     transformInner tail tupvar body (i + 1))
              | transformInner nil _ body _ =
                transform body
        in
            transformInner vars tupvar body 0
        end

    fun transformTop _ =
        raise Fail "Not done yet"
end
