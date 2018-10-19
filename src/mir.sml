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

structure MIR :> MIR = struct
    type name = string

    datatype ty = Bool
                | UInt8
                | SInt8
                | UInt16
                | SInt16
                | UInt32
                | SInt32
                | UInt64
                | SInt64
                | SingleFloat
                | DoubleFloat
                | NamedType of name
                | Pointer of ty
                | Array of ty * int
                | Tuple of ty list
                | Struct of slot list
                | Union of slot list
                | TypeCons of name * ty list
                | TypeVariable of name
         and slot = Slot of name * ty

    datatype exp_ast = BoolConstant of bool
                     | IntConstant of string
                     | FloatConstant of string
                     | StringConstant of CST.escaped_string
                     | Negation of exp_ast
                     | NullConstant
                     | Variable of string
                     | IntArithOp of Arith.oper * exp_ast * exp_ast
                     | FloatArithOp of Arith.oper * exp_ast * exp_ast
                     | ComparisonOp of Builtin.comp_op * exp_ast * exp_ast
                     | Cast of ty * exp_ast
                     | Load of exp_ast
                     | AddressOf of exp_ast
                     | SizeOf of ty
                     | TupleCreate of exp_ast list
                     | TupleProj of exp_ast * int
                     | Funcall of string * ty list * exp_ast list

    datatype block_ast = Progn of block_ast list
                       | Declare of string * ty
                       | Assign of exp_ast * exp_ast
                       | Store of exp_ast * exp_ast
                       | Cond of exp_ast * block_ast * block_ast
                       | StandaloneExp of exp_ast

    type typaram = name

    datatype top_ast = Defun of name * typaram list * param list * ty * block_ast * exp_ast
                     | Deftype of name * typaram list * ty
                     | ToplevelProgn of top_ast list
         and param = Param of name * ty

    (* Utilities for dealing with unions *)

    (* Given the position of a variant in a disjunction, return the
       corresponding union's slot name *)
    fun unionSlotName i =
        "_" ^ (Int.toString i)

    (* Fresh variables *)

    val count = ref 0

    fun freshVar () =
        let
        in
            count := !count + 1;
            "auto_" ^ (Int.toString (!count))
        end

    (* Transformations *)

    local
        open Type
    in
        fun transformIntType Unsigned Int8 = UInt8
          | transformIntType Signed   Int8 = SInt8
          | transformIntType Unsigned Int16 = UInt16
          | transformIntType Signed   Int16 = SInt16
          | transformIntType Unsigned Int32 = UInt32
          | transformIntType Signed   Int32 = SInt32
          | transformIntType Unsigned Int64 = UInt64
          | transformIntType Signed   Int64 = SInt64
    end

    fun transformType Type.Unit =
        Bool
      | transformType Type.Bool =
        Bool
      | transformType (Type.Integer (s, w)) =
        transformIntType s w
      | transformType (Type.Float Type.Single) =
        SingleFloat
      | transformType (Type.Float Type.Double) =
        DoubleFloat
      | transformType (Type.Tuple tys) =
        Tuple (map transformType tys)
      | transformType (Type.Pointer ty) =
        Pointer (transformType ty)
      | transformType (Type.ForeignPointer ty) =
        Pointer (transformType ty)
      | transformType (Type.StaticArray (ty, idx)) =
        Array (transformType ty, idx)
      | transformType (Type.Disjunction (name, args, _)) =
        TypeCons (HIR.escapeSymbol name, map transformType args)
      | transformType (Type.TypeVariable name) =
        TypeVariable (HIR.escapeSymbol name)

    fun transformExp (HIR.BoolConstant b) =
        (Progn [], BoolConstant b)
      | transformExp (HIR.IntConstant i) =
        (Progn [], IntConstant i)
      | transformExp (HIR.FloatConstant f) =
        (Progn [], FloatConstant f)
      | transformExp (HIR.StringConstant s) =
        (Progn [], StringConstant s)
      | transformExp (HIR.Negation v) =
        let val (valueBlock, valueExp) = transformExp v
        in
            (valueBlock, Negation valueExp)
        end
      | transformExp (HIR.Variable name) =
        (Progn [], Variable name)
      | transformExp (HIR.Let (name, ty, value, body)) =
        let val (valueBlock, valueExp) = transformExp value
            and (bodyBlock, bodyExp) = transformExp body
        in
            (Progn [valueBlock,
                    Declare (name, transformType ty),
                    Assign (Variable name, valueExp),
                    bodyBlock],
             bodyExp)
        end
      | transformExp (HIR.Cond (test, cons, alt, ty)) =
        let val ty' = transformType ty
            and result = freshVar ()
        in
            let val (testBlock, testExp) = transformExp test
                and (consBlock, consExp) = transformExp cons
                and (altBlock, altExp) = transformExp alt
            in
                (Progn [testBlock,
                        Declare (result, ty'),
                        Cond (testExp,
                              Progn [
                                  consBlock,
                                  Assign (Variable result, consExp)
                              ],
                              Progn [
                                  altBlock,
                                  Assign (Variable result, altExp)
                             ])
                       ],
                 Variable result)
            end
        end
      | transformExp (HIR.IntArithOp (oper, lhs, rhs)) =
        let val (lhsBlock, lhsExp) = transformExp lhs
            and (rhsBlock, rhsExp) = transformExp rhs
        in
            (Progn [lhsBlock, rhsBlock],
             IntArithOp (oper, lhsExp, rhsExp))
        end
      | transformExp (HIR.FloatArithOp (oper, lhs, rhs)) =
        let val (lhsBlock, lhsExp) = transformExp lhs
            and (rhsBlock, rhsExp) = transformExp rhs
        in
            (Progn [lhsBlock, rhsBlock],
             FloatArithOp (oper, lhsExp, rhsExp))
        end
      | transformExp (HIR.ComparisonOp (oper, lhs, rhs)) =
        let val (lhsBlock, lhsExp) = transformExp lhs
            and (rhsBlock, rhsExp) = transformExp rhs
        in
            (Progn [lhsBlock, rhsBlock],
             ComparisonOp (oper, lhsExp, rhsExp))
        end
      | transformExp (HIR.TupleCreate exps) =
        let val exps' = map transformExp exps
        in
            let fun pairBlocks (b, _) = b
                and pairExp (_, e) = e
            in
                (Progn (map pairBlocks exps'),
                 TupleCreate (map pairExp exps'))
            end
        end
      | transformExp (HIR.TupleProj (tup, idx)) =
        let val (tupBlock, tupExp) = transformExp tup
        in
            (tupBlock, TupleProj (tupExp, idx))
        end
      | transformExp (HIR.Allocate (exp, ty)) =
        let val pointer = freshVar ()
            and ty' = transformType ty
            and (expBlock, exp') = transformExp exp
        in
            (Progn [expBlock,
                    Declare (pointer, Pointer ty'),
                    Assign (Variable pointer, Funcall ("malloc", [], [SizeOf ty'])),
                    Store (Variable pointer, exp')],
             Variable pointer)
        end
      | transformExp (HIR.Load ptr) =
        let val (ptrBlock, ptrExp) = transformExp ptr
        in
            (ptrBlock, Load ptrExp)
        end
      | transformExp (HIR.Store (ptr, value)) =
        let val (ptrBlock, ptrExp) = transformExp ptr
            and (valBlock, valExp) = transformExp value
        in
            (Progn [ptrBlock, valBlock, Store (ptrExp, valExp)],
             ptrExp)
        end
      | transformExp (HIR.Cast (ty, exp)) =
        let val (expBlock, exp') = transformExp exp
        in
            (expBlock, Cast (transformType ty, exp'))
        end
      | transformExp (HIR.SizeOf ty) =
        (Progn [], SizeOf (transformType ty))
      | transformExp (HIR.Progn exps) =
        if List.length exps > 0 then
            let val exps' = map transformExp exps
            in
                let fun pairBlocks (b, _) = b
                    and pairExp (_, e) = e
                in
                    (Progn (map pairBlocks exps'),
                     pairExp (List.last exps'))
                end
            end
        else
            (Progn [], BoolConstant false)
      | transformExp (HIR.Funcall (name, args)) =
        let val args' = map transformExp args
        in
            let fun pairExp (_, e) = e
            in
                (Progn (map (fn (b, e) => Progn [b, StandaloneExp e]) args'),
                 Funcall (name, [], map pairExp args'))
            end
        end

    fun transformTop (HIR.Defun (name, params, ty, body)) =
        let val (bodyBlock, bodyExp) = transformExp body
        in
            Defun (name,
                   [],
                   mapParams params,
                   transformType ty,
                   bodyBlock,
                   bodyExp)
        end
      | transformTop (HIR.Defgeneric (name, typarams, params, ty, body)) =
        let val (bodyBlock, bodyExp) = transformExp body
        in
            Defun (name,
                   typarams,
                   mapParams params,
                   transformType ty,
                   bodyBlock,
                   bodyExp)
        end
      | transformTop (HIR.Deftype (name, params, ty)) =
        Deftype (name,
                 params,
                 transformType ty)
      | transformTop (HIR.Defdisjunction (name, params, variants)) =
        Deftype (name,
                 params,
                 Struct [
                     Slot ("_tag", UInt8),
                     Slot ("_data", Union (Util.mapidx transformVariant variants))
                 ])
      | transformTop (HIR.ToplevelProgn nodes) =
        ToplevelProgn (map transformTop nodes)

    and mapParams params =
        map (fn (HIR.Param (name, ty)) => Param (name, transformType ty))
            params

    and transformVariant (Type.Variant (name, SOME ty), idx) =
        Slot (unionSlotName idx, transformType ty)
      | transformVariant (Type.Variant (name, NONE), idx) =
        Slot (unionSlotName idx, Bool)
end
