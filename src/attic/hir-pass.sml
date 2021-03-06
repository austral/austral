(*
    Copyright 2018 Fernando Borretti <fernando@borretti.me>

    This file is part of Austral.

    Austral is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Austral is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Austral.  If not, see <http://www.gnu.org/licenses/>.
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
                let val name = Symbol.mkSymbol (Ident.mkIdentEx "#",
                                                Ident.mkIdentEx ("g" ^ (Int.toString newid)))
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
      | transformType (MT.Address ty) =
        Pointer (transformType ty)
      | transformType (MT.PositiveAddress ty) =
        Pointer (transformType ty)
      | transformType (MT.StaticArray ty) =
        StaticArray (transformType ty)
      | transformType (MT.Disjunction (name, id)) =
        Disjunction (name, id)
      | transformType (MT.Record (name, id)) =
        Record (name, id)

    fun caseNameIdx tenv ty name =
        let val variants = Type.getDisjunctionVariants tenv (MonoType.disjName ty)
        in
            case Type.posInVariants variants name of
                SOME idx => idx
              | NONE => raise Fail "Internal error: no such name"
        end

    structure M = MTAST

    fun transform _ M.UnitConstant =
        UnitConstant
      | transform _ (M.BoolConstant b) =
        BoolConstant b
      | transform _(M.IntConstant (i, ty)) =
        IntConstant (i, transformType ty)
      | transform _ (M.FloatConstant (f, ty)) =
        FloatConstant (f, transformType ty)
      | transform _ (M.StringConstant s) =
        StringConstant s
      | transform _ (M.Variable (var, ty)) =
        Variable (var, transformType ty)
      | transform e (M.Let (var, value, body)) =
        Let (var, transform e value, transform e body)
      | transform e (M.Bind (vars, tup, body)) =
        (* Since we drop linearity in HIR, we can turn bind expressions into a
           Let that simply projects each tuple element. *)
        let val tupvar = freshVar ()
        in
            Let (tupvar,
                 transform e tup,
                 transformBind e (transformType (M.typeOf tup)) vars tupvar body)
        end
      | transform e (M.Cond (t, c, a)) =
        Cond (transform e t, transform e c, transform e a)
      | transform e (M.ArithOp (kind, oper, lhs, rhs)) =
        ArithOp (kind, oper, transform e lhs, transform e rhs)
      | transform e (M.CompOp (oper, lhs, rhs)) =
        CompOp (oper, transform e lhs, transform e rhs)
      | transform e (M.TupleCreate elems) =
        TupleCreate (map (transform e) elems)
      | transform e (M.TupleProj (tup, idx)) =
        TupleProj (transform e tup, idx)
      | transform e (M.ArrayLength arr) =
        ArrayLength (transform e arr)
      | transform e (M.ArrayPointer arr) =
        ArrayPointer (transform e arr)
      | transform e (M.Malloc (ty, len)) =
        let val ty' = transformType ty
            and len' = transform e len
        in
            Cast (Pointer ty',
                  ForeignFuncall ("malloc",
                                  [ArithOp (Arith.Modular,
                                            Arith.Mul,
                                            len',
                                            SizeOf ty')],
                                  Pointer ty'))
        end
      | transform e (M.Free ptr) =
        ForeignFuncall ("free", [transform e ptr], Unit)
      | transform e (M.Load exp) =
        Load (transform e exp)
      | transform e (M.Store (ptr, value)) =
        Store (transform e ptr, transform e value)
      | transform e (M.CoerceAddress addr) =
        let val ty = transformType (M.typeOf (M.CoerceAddress addr))
        in
            Cast (ty, transform e addr)
        end
      | transform e (M.AddressOffset (addr, offset)) =
        AddressOffset (transform e addr, transform e offset)
      | transform e (M.The (ty, exp)) =
        Cast (transformType ty, transform e exp)
      | transform e (M.Construct (ty, name, value)) =
        Construct (transformType ty,
                   caseNameIdx e ty name,
                   Option.map (transform e) value)
      | transform e (M.MakeRecord (ty, slots)) =
        MakeRecord (transformType ty,
                    map (fn (name, exp) =>
                            (name, transform e exp))
                        slots)
      | transform e (M.ReadSlot (r, name, ty)) =
        ReadSlot (transform e r,
                  name,
                  transformType ty)
      | transform e (M.Case (exp, cases, ty)) =
        let val expTy = MTAST.typeOf exp
        in
            let val expvar = freshVar ()
            in
                (* TODO: better name for this *)
                let val expvarVar = Variable (expvar, transformType ty)
                in
                    let fun transformCase (M.VariantCase (M.NameOnly name, body)) =
                            VariantCase (caseNameIdx e expTy name, transform e body)
                          | transformCase (M.VariantCase (M.NameBinding { casename, var, ty }, body)) =
                            let val id = caseNameIdx e expTy casename
                            in
                                VariantCase (id,
                                             Let (var,
                                                  UnsafeExtractCase (expvarVar,
                                                                     id,
                                                                     transformType ty),
                                                  transform e body))
                            end
                    in
                        Let (expvar,
                             transform e exp,
                             Case (expvarVar,
                                   map transformCase cases,
                                   transformType ty))
                    end
                end
            end
        end
      | transform e (M.ForeignFuncall (name, args, ty)) =
        ForeignFuncall (name, map (transform e) args, transformType ty)
      | transform _ (M.NullPointer ty) =
        NullPointer (transformType ty)
      | transform _ (M.SizeOf ty) =
        SizeOf (transformType ty)
      | transform _ (M.AddressOf (var, ty)) =
        AddressOf (var, transformType ty)
      | transform e (M.Cast (ty, exp)) =
        Cast (transformType ty, transform e exp)
      | transform e (M.Seq (a, b)) =
        Seq (transform e a, transform e b)
      | transform e (M.While (test, body)) =
        While (transform e test, transform e body)
      | transform e (M.ConcreteFuncall (name, args, ty)) =
        ConcreteFuncall (name, map (transform e) args, transformType ty)
      | transform e (M.GenericFuncall (name, id, args, ty)) =
        GenericFuncall (name,
                        id,
                        map (transform e) args,
                        transformType ty)

    and transformBind tenv tupty (vars: Symbol.variable list) (tupvar: Symbol.variable) (body: MTAST.ast) =
        let fun transformInner (head::tail) tupvar body i =
                Let (head,
                     TupleProj (Variable (tupvar, tupty), i),
                     transformInner tail tupvar body (i + 1))
              | transformInner nil _ body _ =
                transform tenv body
        in
            transformInner vars tupvar body 0
        end

    fun transformTop tenv (M.Defun (name, params, ty, body)) =
        Defun (name,
               mapParams params,
               transformType ty,
               transform tenv body)
      | transformTop tenv (M.DefunMonomorph (name, params, ty, body, id)) =
        DefunMonomorph (name,
                        mapParams params,
                        transformType ty,
                        transform tenv body,
                        id)
      | transformTop _ (M.DefdatatypeMono (name, id, tys)) =
        DefdatatypeMono (name,
                         id,
                         map transformType tys)
      | transformTop _ (M.DefrecordMono (name, id, slots)) =
        let fun mapSlot (name, ty) =
                (name, transformType ty)
        in
            DefrecordMono (name,
                           id,
                           Map.fromList (map mapSlot (Map.toList slots)))
        end
      | transformTop _ (M.Defcfun (_, rawname, params, arity, rt)) =
        Defcfun (rawname,
                 map (fn (M.Param (_, t)) => transformType t) params,
                 arity,
                 transformType rt)
      | transformTop e (M.ToplevelProgn l) =
        ToplevelProgn (map (transformTop e) l)

    and mapParams l =
        map mapParam l

    and mapParam (MTAST.Param (var, ty)) =
        Param (var, transformType ty)
end
