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
         !regCount)

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
      | transform (HIR.Cond (t, c, a)) =
        let val (tBlock, t') = transform t
            and (cBlock, c') = transform c
            and (aBlock, a') = transform a
            and result = freshRegister ()
        in
            let val cond = Cond { test = t',
                                  consequent = cBlock,
                                  alternate = aBlock,
                                  result = result,
                                  ty = transformType (HIR.typeOf c) }
            in
                let val nodes = tBlock @ [cond]
                in
                    (nodes, RegisterOp result)
                end
            end
        end
      | transform (HIR.ArithOp (kind, oper, lhs, rhs)) =
        let val (lBlock, lhs') = transform lhs
            and (rBlock, rhs') = transform rhs
            and result = freshRegister ()
            and ty = transformType (HIR.typeOf (HIR.ArithOp (kind, oper, lhs, rhs)))
        in
            let val nodes = lBlock
                            @ rBlock
                            @ [Assignment (result, ArithOp (kind, oper, lhs', rhs'), ty)]
            in
                (nodes, RegisterOp result)
            end
        end
      | transform (HIR.TupleCreate exps) =
        let val exps' = map transform exps
            and result = freshRegister ()
            and ty = Tuple (map (transformType o HIR.typeOf) exps)
        in
            let val expBlocks = map (fn (is, _) => is) exps'
            in
                let val nodes = (List.concat expBlocks)
                                @ [Assignment (result, TupleCreate (map (fn (_, oper) => oper) exps'), ty)]
                in
                    (nodes, RegisterOp result)
                end
            end
        end
      | transform (HIR.TupleProj (tup, idx)) =
        let val (tupBlock, tup') = transform tup
            and result = freshRegister ()
            and ty = transformType (HIR.typeOf (HIR.TupleProj (tup, idx)))
        in
            (tupBlock @ [Assignment (result, TupleProj (tup', idx), ty)],
             RegisterOp result)
        end
      | transform (HIR.ArrayLength arr) =
        let val (arrBlock, arr') = transform arr
            and result = freshRegister ()
            and ty = sizeType
        in
            (arrBlock @ [Assignment (result, TupleProj (arr', 0), ty)],
             RegisterOp result)
        end
      | transform (HIR.ArrayPointer arr) =
        let val (arrBlock, arr') = transform arr
            and result = freshRegister ()
            and ty = transformType (HIR.typeOf (HIR.ArrayPointer arr))
        in
            (arrBlock @ [Assignment (result, TupleProj (arr', 1), ty)],
             RegisterOp result)
        end
      | transform (HIR.Allocate _) =
        raise Fail "Allocate not implemented"
      | transform (HIR.Load ptr) =
        let val (ptrBlock, ptr') = transform ptr
            and deref = freshRegister ()
            and result = freshRegister ()
            and ty = transformType (HIR.typeOf (HIR.Load ptr))
        in
            let val nodes = ptrBlock
                            @ [Assignment (deref, Load ptr', transformType (HIR.typeOf ptr)),
                               Assignment (result, TupleCreate [RegisterOp deref, ptr'], ty)]
            in
                (nodes, RegisterOp result)
            end
        end
      | transform (HIR.Store (ptr, value)) =
        let val (ptrBlock, ptr') = transform ptr
            and (valBlock, val') = transform value
        in
            let val nodes = ptrBlock
                            @ valBlock
                            @ [Store { ptr = ptr', value = val' }]
            in
                (nodes, ptr')
            end
        end
      | transform (HIR.Construct (ty, caseId, SOME value)) =
        let val (valBlock, value') = transform value
            and result = freshRegister ()
            and ty' = transformType ty
        in
            (valBlock @ [Assignment (result, Construct (transformType ty, caseId, SOME value'), ty')],
             RegisterOp result)
        end
      | transform (HIR.Construct (ty, caseId, NONE)) =
        let val result = freshRegister ()
            and ty' = transformType ty
        in
            ([Assignment (result, Construct (transformType ty, caseId, NONE), ty')],
             RegisterOp result)
        end
      | transform (HIR.Case (exp, cases, ty)) =
        raise Fail "Case not implemented yet"
      | transform (HIR.UnsafeExtractCase (exp, caseId, ty)) =
        let val (expBlock, exp') = transform exp
            and result = freshRegister ()
            and ty' = transformType ty
        in
            (expBlock
             @ [Assignment (result, TupleProj (exp', 1), ty')],
             RegisterOp result)
        end
      | transform (HIR.Seq (a, b)) =
        let val (aBlock, _) = transform a
            and (bBlock, b') = transform b
        in
            let val nodes = aBlock @ bBlock
            in
                (nodes, b')
            end
        end
      | transform _ =
        raise Fail "Not implemented yet"
end
