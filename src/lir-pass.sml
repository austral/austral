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

structure LirPass :> LIR_PASS = struct
    type ty = LIR.ty
    structure L = LIR

    (* Tuples *)

    type tuple_types = (ty list, ty) Map.map

    val emptyTupleTypes =
        Map.empty

    fun getTuple tt tys =
        Map.get tt tys

    val count = ref 0
    fun freshId () =
        (count := !count + 1;
         !count)

    fun addTuple tt tys =
        case getTuple tt tys of
            (SOME ty) => (ty, tt)
          | NONE => let val id = freshId ()
                    in
                        let val ty = L.Tuple id
                        in
                            (ty, Map.iadd tt (tys, ty))
                        end
                    end

    fun newTuples old new =
        let val oldKeys = Map.keys old
            and newKeys = Map.keys new
        in
            let val newKeys' = Set.minus newKeys oldKeys
            in
                map (fn tys => let val ty = Option.valOf (Map.get new tys)
                               in
                                   case ty of
                                       (LIR.Tuple id) => (id, tys)
                                     | _ => raise Fail "Internal compiler error: not a tuple"
                               end)
                    (Set.toList newKeys')
            end
        end

    (* Transform types *)

    val sizeType = L.Integer (Type.Unsigned, Type.Int64)

    fun transformType tt MIR.Bool =
        (L.Bool, tt)
      | transformType tt (MIR.Integer (s, w)) =
        (L.Integer (s, w), tt)
      | transformType tt (MIR.Float f) =
        (L.Float f, tt)
      | transformType tt (MIR.Tuple tys) =
        let val (tys', tt) = transformTypes tt tys
        in
            addTuple tt tys'
        end
      | transformType tt (MIR.Pointer t) =
        let val (t', tt) = transformType tt t
        in
            (L.Pointer t', tt)
        end
      | transformType tt (MIR.StaticArray t) =
        let val (t', tt) = transformType tt t
        in
            addTuple tt [sizeType, t']
        end
      | transformType tt (MIR.Disjunction (name, id)) =
        (L.Disjunction (name, id), tt)

    and transformTypes tt tys =
        Util.foldThread (fn (ty, tt) =>
                            transformType tt ty)
                        tys
                        tt

    (* Transform code *)

    fun transformOperand tt (MIR.BoolConstant b) =
        (L.BoolConstant b, tt)
      | transformOperand tt (MIR.IntConstant (i, ty)) =
        let val (ty, tt) = transformType tt ty
        in
            (L.IntConstant (i, ty), tt)
        end
      | transformOperand tt (MIR.FloatConstant (f, ty)) =
        let val (ty, tt) = transformType tt ty
        in
            (L.FloatConstant (f, ty), tt)
        end
      | transformOperand tt (MIR.StringConstant s) =
        (L.StringConstant s, tt)
      | transformOperand tt (MIR.RegisterOp r) =
        (L.RegisterOp r, tt)
      | transformOperand tt (MIR.VariableOp (var, ty)) =
        let val (ty, tt) = transformType tt ty
        in
            (L.VariableOp (var, ty), tt)
        end

    fun transformOperation tt (MIR.ArithOp (kind, oper, lhs, rhs)) =
        let val (lhs, tt) = transformOperand tt lhs
        in
            let val (rhs, tt) = transformOperand tt rhs
            in
                (L.ArithOp (kind, oper, lhs, rhs), tt)
            end
        end
      | transformOperation tt (MIR.CompOp (oper, lhs, rhs)) =
        let val (lhs, tt) = transformOperand tt lhs
        in
            let val (rhs, tt) = transformOperand tt rhs
            in
                (L.CompOp (oper, lhs, rhs), tt)
            end
        end
      | transformOperation tt (MIR.TupleCreate opers) =
        let val (opers, tt) = transformOperands tt opers
        in
            (L.TupleCreate opers, tt)
        end
      | transformOperation tt (MIR.TupleProj (tup, idx)) =
        let val (tup, tt) = transformOperand tt tup
        in
            (L.TupleProj (tup, idx), tt)
        end
      | transformOperation tt (MIR.ArrayLength arr) =
        let val (arr, tt) = transformOperand tt arr
        in
            (L.ArrayLength arr, tt)
        end
      | transformOperation tt (MIR.ArrayPointer arr) =
        let val (arr, tt) = transformOperand tt arr
        in
            (L.ArrayPointer arr, tt)
        end
      | transformOperation tt (MIR.Load ptr) =
        let val (ptr, tt) = transformOperand tt ptr
        in
            (L.Load ptr, tt)
        end
      | transformOperation tt (MIR.Construct (ty, id, SOME oper)) =
        let val (ty, tt) = transformType tt ty
        in
            let val (oper, tt) = transformOperand tt oper
            in
                (L.Construct (ty, id, SOME oper), tt)
            end
        end
      | transformOperation tt (MIR.Construct (ty, id, NONE)) =
        let val (ty, tt) = transformType tt ty
        in
            (L.Construct (ty, id, NONE), tt)
        end
      | transformOperation tt (MIR.UnsafeExtractCase (oper, id)) =
        let val (oper, tt) = transformOperand tt oper
        in
            (L.UnsafeExtractCase (oper, id), tt)
        end
      | transformOperation tt (MIR.ForeignFuncall (name, args)) =
        let val (args, tt) = transformOperands tt args
        in
            (L.ForeignFuncall (name, args), tt)
        end
      | transformOperation tt (MIR.ForeignNull ty) =
        let val (ty, tt) = transformType tt ty
        in
            (L.ForeignNull ty, tt)
        end
      | transformOperation tt (MIR.SizeOf ty) =
        let val (ty, tt) = transformType tt ty
        in
            (L.SizeOf ty, tt)
        end
      | transformOperation tt (MIR.AddressOf var) =
        (L.AddressOf var, tt)
      | transformOperation tt (MIR.Cast (ty, oper)) =
        let val (ty, tt) = transformType tt ty
        in
            let val (oper, tt) = transformOperand tt oper
            in
                (L.Cast (ty, oper), tt)
            end
        end
      | transformOperation tt (MIR.ConcreteFuncall (name, args)) =
        let val (args, tt) = transformOperands tt args
        in
            (L.ConcreteFuncall (name, args), tt)
        end
      | transformOperation tt (MIR.GenericFuncall (name, id, args)) =
        let val (args, tt) = transformOperands tt args
        in
            (L.GenericFuncall (name, id, args), tt)
        end

    and transformOperands tt opers =
        Util.foldThread (fn (oper, tt) =>
                            transformOperand tt oper)
                        opers
                        tt

    fun transformInstruction tt (MIR.Assignment (r, oper, ty)) =
        let val (oper, tt) = transformOperation tt oper
        in
            let val (ty, tt) = transformType tt ty
            in
                (L.Assignment (r, oper, ty), tt)
            end
        end
      | transformInstruction tt (MIR.DeclareLocal (var, ty, oper)) =
        let val (ty, tt) = transformType tt ty
        in
            let val (oper, tt) = transformOperand tt oper
            in
                (L.DeclareLocal (var, ty, oper), tt)
            end
        end
      | transformInstruction tt (MIR.Cond { test, consequent, consequent_res, alternate, alternate_res, result, ty }) =
        let val (test, tt) = transformOperand tt test
        in
            let val (consequent, tt) = transformInstructions tt consequent
            in
                let val (consequent_res, tt) = transformOperand tt consequent_res
                in
                    let val (alternate, tt) = transformInstructions tt alternate
                    in
                        let val (alternate_res, tt) = transformOperand tt alternate_res
                        in
                            let val (ty, tt) = transformType tt ty
                            in
                                (L.Cond { test = test,
                                          consequent = consequent,
                                          consequent_res = consequent_res,
                                          alternate = alternate,
                                          alternate_res = alternate_res,
                                          result = result,
                                          ty = ty},
                                 tt)
                            end
                        end
                    end
                end
            end
        end
      | transformInstruction tt (MIR.Store { ptr, value, result, ty }) =
        let val (ptr, tt) = transformOperand tt ptr
        in
            let val (value, tt) = transformOperand tt value
            in
                let val (ty, tt) = transformType tt ty
                in
                    (L.Store { ptr = ptr,
                               value = value,
                               result = result,
                               ty = ty },
                     tt)
                end
            end
        end
      | transformInstruction tt (MIR.Case (oper, variants, register, ty)) =
        let val (oper, tt) = transformOperand tt oper
        in
            let val (variants, tt) = transformVariants tt variants
            in
                let val (ty, tt) = transformType tt ty
                in
                    (L.Case (oper, variants, register, ty), tt)
                end
            end
        end
      | transformInstruction tt (MIR.VoidForeignFuncall (name, args)) =
        let val (args, tt) = transformOperands tt args
        in
            (L.VoidForeignFuncall (name, args), tt)
        end

    and transformInstructions tt opers =
        Util.foldThread (fn (oper, tt) =>
                            transformInstruction tt oper)
                        opers
                        tt

    and transformVariants tt variants =
        Util.foldThread (fn (MIR.VariantCase (id, insts, oper, ty), tt) =>
                            let val (insts, tt) = transformInstructions tt insts
                            in
                                let val (oper, tt) = transformOperand tt oper
                                in
                                    let val (ty, tt) = transformType tt ty
                                    in
                                        (L.VariantCase (id, insts, oper, ty), tt)
                                    end
                                end
                            end)
                        variants
                        tt

    fun transformTop tt node =
        let val (node, tt') = transformTop' tt node
        in
            (* After transforming a top-level node, extract new tuple types and
               define them *)
            let val new = newTuples tt tt'
            in
                let val tupNodes = map (fn (id, tys) =>
                                           (LIR.Deftuple (id, tys)))
                                       new
                in
                    (LIR.ToplevelProgn (tupNodes @ [node]), tt')
                end
            end
        end

    and transformTop' tt (MIR.Defun (name, params, ty, insts, oper)) =
        let val (params, tt) = transformParams tt params
        in
            let val (ty, tt) = transformType tt ty
            in
                let val (insts, tt) = transformInstructions tt insts
                in
                    let val (oper, tt) = transformOperand tt oper
                    in
                        (LIR.Defun (name, params, ty, insts, oper), tt)
                    end
                end
            end
        end
      | transformTop' tt (MIR.DefunMonomorph (name, params, ty, insts, oper, id)) =
        let val (params, tt) = transformParams tt params
        in
            let val (ty, tt) = transformType tt ty
            in
                let val (insts, tt) = transformInstructions tt insts
                in
                    let val (oper, tt) = transformOperand tt oper
                    in
                        (LIR.DefunMonomorph (name, params, ty, insts, oper, id), tt)
                    end
                end
            end
        end
      | transformTop' tt (MIR.DefdatatypeMono (name, id, tys)) =
        let val (tys, tt) = transformTypes tt tys
        in
            (LIR.DefdatatypeMono (name, id, tys), tt)
        end
      | transformTop' tt (MIR.ToplevelProgn nodes) =
        let val (nodes, tt) = Util.foldThread (fn (node, tt) =>
                                                  transformTop' tt node)
                                              nodes
                                              tt
        in
            (LIR.ToplevelProgn nodes, tt)
        end

    and transformParams tt params =
        Util.foldThread (fn (MIR.Param (var, ty), tt) =>
                            let val (ty, tt) = transformType tt ty
                            in
                                (LIR.Param (var, ty), tt)
                            end)
                        params
                        tt
end
