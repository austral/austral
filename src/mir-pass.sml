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

structure MirPass :> MIR_PASS = struct
    open MIR

    (* Transform HIR types to MIR types *)

    val sizeType = Integer (Type.Unsigned, Type.Int64)

    fun transformType HIR.Unit =
        Bool
      | transformType HIR.Bool =
        Bool
      | transformType (HIR.Integer (s, w)) =
        Integer (s, w)
      | transformType (HIR.Float f) =
        Float f
      | transformType (HIR.Tuple tys) =
        Tuple (map transformType tys)
      | transformType (HIR.Pointer t) =
        Pointer (transformType t)
      | transformType (HIR.StaticArray t) =
        Tuple [sizeType, Pointer (transformType t)]
      | transformType (HIR.Disjunction (name, id)) =
        Disjunction (name, id)

    (* Transform HIR expressions to MIR basic blocks *)

    val regCount = ref 0
    fun freshRegister () =
        (regCount := !regCount + 1;
         RegisterOp (!regCount))

    fun transform HIR.UnitConstant =
        ([], BoolConstant false)
      | transform (HIR.IntConstant (i, ty)) =
        ([], IntConstant (i, transformType ty))
      | transform (HIR.FloatConstant (f, ty)) =
        ([], FloatConstant (f, transformType ty))
      | transform (HIR.StringConstant s) =
        raise Fail "String constants not implemented yet"
      | transform (HIR.Variable (name, ty)) =
        ([], VariableOp (name, transformType ty))
      | transform (HIR.Let (var, value, body)) =
        let val (valueBlock, value') = transform value
            and (bodyBlock, body') = transform body
            and ty = transformType (HIR.typeOf value)
        in
            let val decl = DeclareLocal (var, ty, value')
            in
                let val nodes = valueBlock @ [decl] @ bodyBlock
                in
                    (nodes, body')
                end
            end
        end
      | transform (Cond (t, c, a)) =
        let val (tBlock, t') = transform t
            and (cBlock, c') = transform c
            and (aBlock, a') = transform a
            and result = freshRegister ()
        in
            (tBlock,
             Cond { test = t',
                    consequent = cBlock,
                    alternate = aBlock,
                    result = result,
                    ty = transformType (HIR.typeOf c) })
        end
      | transform _ =
        raise Fail "Not implemented yet"
end
