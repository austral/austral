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

    (* Transform types *)

    val sizeType = Integer (Type.Unsigned, Type.Int64)

    fun transformType HIR.Unit =
        Unit
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
        StaticArray (transformType t)
      | transformType (HIR.Disjunction (name, id)) =
        Disjunction (name, id)

    (* Transform HIR expressions to MIR basic blocks *)

    val regCount = ref 0
    fun freshRegister () =
        (regCount := !regCount + 1;
         !regCount)

    fun transform HIR.UnitConstant =
        ([], UnitConstant)
      | transform (HIR.BoolConstant b) =
        ([], BoolConstant b)
      | transform (HIR.IntConstant (i, ty)) =
        ([], IntConstant (i, transformType ty))
      | transform (HIR.FloatConstant (f, ty)) =
        ([], FloatConstant (f, transformType ty))
      | transform (HIR.StringConstant s) =
        ([], StringConstant s)
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
            (arrBlock @ [Assignment (result, ArrayLength arr', ty)],
             RegisterOp result)
        end
      | transform (HIR.ArrayPointer arr) =
        let val (arrBlock, arr') = transform arr
            and result = freshRegister ()
            and ty = transformType (HIR.typeOf (HIR.ArrayPointer arr))
        in
            (arrBlock @ [Assignment (result, ArrayPointer arr', ty)],
             RegisterOp result)
        end
      | transform (HIR.Load ptr) =
        let val (ptrBlock, ptr') = transform ptr
            and result = freshRegister ()
            and ty = transformType (HIR.typeOf (HIR.Load ptr))
        in
            let val nodes = ptrBlock
                            @ [Assignment (result, Load ptr', ty)]
            in
                (nodes, RegisterOp result)
            end
        end
      | transform (HIR.Store (ptr, value)) =
        let val (ptrBlock, ptr') = transform ptr
            and (valBlock, val') = transform value
            and result = freshRegister ()
            and ty = transformType (HIR.typeOf ptr)
        in
            let val nodes = ptrBlock
                            @ valBlock
                            @ [Assignment (result, Store { ptr = ptr', value = val' }, ty)]
            in
                (nodes, RegisterOp result)
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
        transformCases exp cases ty
      | transform (HIR.UnsafeExtractCase (exp, caseId, ty)) =
        let val (expBlock, exp') = transform exp
            and result = freshRegister ()
            and ty' = transformType ty
        in
            (expBlock
             @ [Assignment (result, UnsafeExtractCase (exp', caseId), ty')],
             RegisterOp result)
        end
      | transform (HIR.ForeignFuncall (name, args, rt)) =
        (case rt of
             (* The function returns void, so we have to handle it especially. *)
             HIR.Unit => transformVoidForeignFuncall name args
           | _ => transformForeignFuncall name args rt)
      | transform (HIR.ForeignNull ty) =
        let val result = freshRegister ()
            and ty' = transformType (HIR.typeOf (HIR.ForeignNull ty))
        in
            let val nodes = [Assignment (result, ForeignNull (transformType ty), ty')]
            in
                (nodes, RegisterOp result)
            end
        end
      | transform (HIR.SizeOf ty) =
        let val result = freshRegister ()
            and ty' = transformType (HIR.typeOf (HIR.SizeOf ty))
        in
            let val nodes = [Assignment (result, SizeOf (transformType ty), ty')]
            in
                (nodes, RegisterOp result)
            end
        end
      | transform (HIR.AddressOf (var, ty)) =
        let val result = freshRegister ()
            and ty' = transformType ty
        in
            let val nodes = [Assignment (result, AddressOf var, ty')]
            in
                (nodes, RegisterOp result)
            end
        end
      | transform (HIR.Cast (ty, value)) =
        let val (valBlock, val') = transform value
            and result = freshRegister ()
            and ty' = transformType (HIR.typeOf (HIR.Cast (ty, value)))
        in
            let val nodes = valBlock
                            @ [Assignment (result, Cast (transformType ty, val'), ty')]
            in
                (nodes, RegisterOp result)
            end
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
      | transform (HIR.ConcreteFuncall (name, args, rt)) =
        let val (argBlocks, argOps) = transformArgs args
            and result = freshRegister ()
            and ty = transformType rt
        in
            let val nodes = (List.concat argBlocks)
                            @ [Assignment (result, ConcreteFuncall (name, argOps), ty)]
            in
                (nodes, RegisterOp result)
            end
        end
      | transform (HIR.GenericFuncall (name, id, args, rt)) =
        let val (argBlocks, argOps) = transformArgs args
            and result = freshRegister ()
            and ty = transformType rt
        in
            let val nodes = (List.concat argBlocks)
                            @ [Assignment (result, GenericFuncall (name, id, argOps), ty)]
            in
                (nodes, RegisterOp result)
            end
        end

    and transformCases exp cases ty =
        let val (expBlock, exp') = transform exp
        in
            let val cases' = map transformCase cases
            in
                raise Fail "Case not implemented yet"
            end
        end

    and transformCase (HIR.VariantCase (name, body)) =
        raise Fail "Not implemented yet"

    and transformVoidForeignFuncall name args =
        let val (argBlocks, argOps) = transformArgs args
        in
            let val nodes = (List.concat argBlocks)
                            @ [VoidForeignFuncall (name, argOps)]
            in
                (nodes, UnitConstant)
            end
        end

    and transformForeignFuncall name args rt =
        let val (argBlocks, argOps) = transformArgs args
            and result = freshRegister ()
            and ty = transformType rt
        in
            let val nodes = (List.concat argBlocks)
                            @ [Assignment (result, ForeignFuncall (name, argOps), ty)]
            in
                (nodes, RegisterOp result)
            end
        end

    and transformArgs args =
        let val args' = map transform args
        in
            let val argBlocks = map (fn (is, _) => is) args'
                and argOps = map (fn (_, oper) => oper) args'
            in
                (argBlocks, argOps)
            end
        end
end
