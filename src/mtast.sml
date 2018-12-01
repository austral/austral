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

structure MTAST :> MTAST = struct
    type name = Symbol.symbol
    type ty = MonoType.ty

    (* Expression AST *)

    datatype ast = UnitConstant
                 | BoolConstant of bool
                 | IntConstant of string * ty
                 | FloatConstant of string * ty
                 | StringConstant of CST.escaped_string
                 | Variable of Symbol.variable * ty
                 | Let of Symbol.variable * ast * ast
                 | Bind of Symbol.variable list * ast * ast
                 | Cond of ast * ast * ast
                 | ArithOp of Arith.kind * Arith.oper * ast * ast
                 | CompOp of Builtin.comp_op * ast * ast
                 | TupleCreate of ast list
                 | TupleProj of ast * int
                 | ArrayLength of ast
                 | ArrayPointer of ast
                 | Malloc of ty * ast
                 | Free of ast
                 | Load of ast
                 | Store of ast * ast
                 | CoerceAddress of ast
                 | AddressOffset of ast * ast
                 | The of ty * ast
                 | Construct of ty * name * ast option
                 | Case of ast * variant_case list * ty
                 | ForeignFuncall of string * ast list * ty
                 | NullPointer of ty
                 | SizeOf of ty
                 | AddressOf of Symbol.variable * ty
                 | Cast of ty * ast
                 | Seq of ast * ast
                 | ConcreteFuncall of name * ast list * ty
                 | GenericFuncall of name * int * ast list * ty
         and variant_case = VariantCase of case_name * ast
         and case_name = NameOnly of name
                       | NameBinding of { casename: name, var: Symbol.variable, ty: ty }

    local
        open MonoType
    in
        val sizeType = Integer (Type.Unsigned, Type.IntSize)

        fun typeOf UnitConstant =
            Unit
          | typeOf (BoolConstant _) =
            Bool
          | typeOf (IntConstant (_, t)) =
            t
          | typeOf (FloatConstant (_, t)) =
            t
          | typeOf (StringConstant s) =
            StaticArray (Integer (Type.Unsigned, Type.Int8))
          | typeOf (Variable (_, t)) =
            t
          | typeOf (Let (_, _, b)) =
            typeOf b
          | typeOf (Bind (_, _, b)) =
            typeOf b
          | typeOf (Cond (_, tb, _)) =
            typeOf tb
          | typeOf (ArithOp (kind, _, lhs, _)) =
            (case kind of
                 Arith.Checked => Tuple [typeOf lhs, Bool]
               | _ => typeOf lhs)
          | typeOf (CompOp _) =
            Bool
          | typeOf (TupleCreate exps) =
            Tuple (map typeOf exps)
          | typeOf (TupleProj (tup, idx)) =
            (case typeOf tup of
                 Tuple tys => List.nth (tys, idx)
               | _ => raise Fail "Not a tuple")
          | typeOf (ArrayLength _) =
            sizeType
          | typeOf (ArrayPointer arr) =
            (case (typeOf arr) of
                 (StaticArray ty) => PositiveAddress ty
               | _ => raise Fail "Invalid type for ArrayPointer")
          | typeOf (Malloc (t, _)) =
            Address t
          | typeOf (Free _) =
            Unit
          | typeOf (Load p) =
            (case typeOf p of
                 (Pointer t) => t
               | _ => raise Fail "Not a pointer")
          | typeOf (Store (_, v)) =
            typeOf v
          | typeOf (CoerceAddress addr) =
            (case typeOf addr of
                 (Address t) => PositiveAddress t
               | _ => raise Fail "Not an address")
          | typeOf (AddressOffset (addr, _)) =
            typeOf addr
          | typeOf (The (t, _)) =
            t
          | typeOf (Construct (t, _, _)) =
            t
          | typeOf (Case (_, _, t)) =
            t
          | typeOf (SizeOf _) =
            sizeType
          | typeOf (AddressOf (_, ty)) =
            ty
          | typeOf (Cast (ty, _)) =
            ty
          | typeOf (ForeignFuncall (_, _, rt)) =
            rt
          | typeOf (NullPointer ty) =
            Address ty
          | typeOf (Seq (_, v)) =
            typeOf v
          | typeOf (ConcreteFuncall (_, _, ty)) =
            ty
          | typeOf (GenericFuncall (_, _, _, ty)) =
            ty
    end

    (* Block AST *)

    datatype top_ast = Defun of name * param list * ty * ast
                     | DefunMonomorph of name * param list * ty * ast * int
                     | DefdatatypeMono of name * int * ty list
                     | Defcfun of name * string * param list * Function.foreign_arity * ty
                     | ToplevelProgn of top_ast list
         and param = Param of Symbol.variable * ty

    (* Fresh monomorph ids *)

    val id = ref 0

    fun freshId () =
        let
        in
            id := !id + 1;
            !id
        end

    (* Monomorphization *)

    type type_monomorphs = MonoType.type_monomorphs

    datatype fn_monomorphs = FuncMonos of ((name * ty list), int) Map.map

    datatype context = Context of type_monomorphs * fn_monomorphs

    val emptyContext =
        Context (MonoType.emptyMonomorphs,
                 FuncMonos Map.empty)

    fun getMonomorph (Context (_, FuncMonos fm)) name tyargs =
        Map.get fm (name, tyargs)

    fun addMonomorph (Context (tm, FuncMonos fm)) name tyargs id =
        Context (tm, FuncMonos (Map.iadd fm ((name, tyargs), id)))

    (* Diffing contexts *)

    fun newFuncMonomorphs (Context (_, FuncMonos old)) (Context (_, FuncMonos new)) =
        let val oldKeys = Map.keys old
            and newKeys = Map.keys new
        in
            let val newKeys' = Set.minus newKeys oldKeys
            in
                map (fn k =>
                        let val id = Option.valOf (Map.get new k)
                            and (name, args) = k
                        in
                            (name, args, id)
                        end)
                    (Set.toList newKeys')
            end
        end

    fun newTypeMonomorphs (Context (old, _)) (Context (new, _)) =
        MonoType.newMonomorphs old new

    (* Monomorphization utilities *)

    fun monoType ctx rs ty =
        let val (Context (tm, fm)) = ctx
        in
            let val (ty', tm') = MonoType.monomorphize tm rs ty
            in
                (ty', Context (tm', fm))
            end
        end

    fun monoTypes ctx rs tys =
        Util.foldThread (fn (ty, ctx) => monoType ctx rs ty)
                        tys
                        ctx

    (* Monomorphize a type with an empty replacements map, that is, in a
       concrete context. *)

    type replacements = MonoType.replacements

    fun monomorphize ctx _ TAST.UnitConstant =
        (UnitConstant, ctx)
      | monomorphize ctx _ (TAST.BoolConstant b) =
        (BoolConstant b, ctx)
      | monomorphize ctx rs (TAST.IntConstant (i, ty)) =
        let val (ty', ctx) = monoType ctx rs ty
        in
            case ty' of
                (MonoType.Integer _) => (IntConstant (i, ty'), ctx)
              | _ => raise Fail "Internal error: not a valid type for an integer constant"
        end
      | monomorphize ctx rs (TAST.FloatConstant (f, ty)) =
        let val (ty', ctx) = monoType ctx rs ty
        in
            case ty' of
                (MonoType.Float _) => (FloatConstant (f, ty'), ctx)
              | _ => raise Fail "Internal error: not a valid type for a float constant"
        end
      | monomorphize ctx _ (TAST.StringConstant s) =
        (StringConstant s, ctx)
      | monomorphize ctx rs (TAST.Variable (var, ty)) =
        let val (ty', ctx) = monoType ctx rs ty
        in
            (Variable (var, ty'), ctx)
        end
      | monomorphize ctx rs (TAST.Let (name, value, body)) =
        let val (value', ctx) = monomorphize ctx rs value
        in
            let val (body', ctx) = monomorphize ctx rs body
            in
                (Let (name, value', body'), ctx)
            end
        end
      | monomorphize ctx rs (TAST.Bind (names, tup, body)) =
        let val (tup', ctx) = monomorphize ctx rs tup
        in
            let val (body', ctx) = monomorphize ctx rs body
            in
                (Bind (names, tup', body'), ctx)
            end
        end
      | monomorphize ctx rs (TAST.Cond (t, c, a)) =
        let val (t', ctx) = monomorphize ctx rs t
        in
            let val (c', ctx) = monomorphize ctx rs c
            in
                let val (a', ctx) = monomorphize ctx rs a
                in
                    (Cond (t', c', a'), ctx)
                end
            end
        end
      | monomorphize ctx rs (TAST.ArithOp (kind, oper, lhs, rhs)) =
        let val (lhs', ctx) = monomorphize ctx rs lhs
        in
            let val (rhs', ctx) = monomorphize ctx rs rhs
            in
                (ArithOp (kind, oper, lhs', rhs'), ctx)
            end
        end
      | monomorphize ctx rs (TAST.CompOp (oper, lhs, rhs)) =
        let val (lhs', ctx) = monomorphize ctx rs lhs
        in
            let val (rhs', ctx) = monomorphize ctx rs rhs
            in
                (CompOp (oper, lhs', rhs'), ctx)
            end
        end
      | monomorphize ctx rs (TAST.TupleCreate exps) =
        let val (exps', ctx) = monomorphizeList ctx rs exps
        in
            (TupleCreate exps', ctx)
        end
      | monomorphize ctx rs (TAST.TupleProj (tup, idx)) =
        let val (tup', ctx) = monomorphize ctx rs tup
        in
            (TupleProj (tup', idx), ctx)
        end
      | monomorphize ctx rs (TAST.ArrayLength arr) =
        let val (arr', ctx) = monomorphize ctx rs arr
        in
            (ArrayLength arr', ctx)
        end
      | monomorphize ctx rs (TAST.ArrayPointer arr) =
        let val (arr', ctx) = monomorphize ctx rs arr
        in
            (ArrayPointer arr', ctx)
        end
      | monomorphize ctx rs (TAST.Malloc (ty, len)) =
        let val (ty', ctx) = monoType ctx rs ty
        in
            let val (len', ctx) = monomorphize ctx rs len
            in
                (Malloc (ty', len'), ctx)
            end
        end
      | monomorphize ctx rs (TAST.Free ptr) =
        let val (ptr', ctx) = monomorphize ctx rs ptr
        in
            (Free ptr', ctx)
        end
      | monomorphize ctx rs (TAST.Load exp) =
        let val (exp', ctx) = monomorphize ctx rs exp
        in
            (Load exp', ctx)
        end
      | monomorphize ctx rs (TAST.Store (ptr, value)) =
        let val (ptr', ctx) = monomorphize ctx rs ptr
        in
            let val (value', ctx) = monomorphize ctx rs value
            in
                (Store (ptr', value'), ctx)
            end
        end
      | monomorphize ctx rs (TAST.CoerceAddress addr) =
        let val (addr', ctx) = monomorphize ctx rs addr
        in
            (CoerceAddress addr', ctx)
        end
      | monomorphize ctx rs (TAST.AddressOffset (addr, offset)) =
        let val (addr', ctx) = monomorphize ctx rs addr
        in
            let val (offset', ctx) = monomorphize ctx rs offset
            in
                (AddressOffset (addr', offset'), ctx)
            end
        end
      | monomorphize ctx rs (TAST.The (ty, exp)) =
        let val (ty', ctx) = monoType ctx rs ty
        in
            let val (exp', ctx) = monomorphize ctx rs exp
            in
                (The (ty', exp'), ctx)
            end
        end
      | monomorphize ctx rs (TAST.Construct (ty, name, expOpt)) =
        let val (ty', ctx) = monoType ctx rs ty
        in
            case expOpt of
                (SOME exp) => let val (exp', ctx) = monomorphize ctx rs exp
                              in
                                  (Construct (ty', name, SOME exp'), ctx)
                              end
              | NONE => (Construct (ty', name, NONE), ctx)
        end
      | monomorphize ctx rs (TAST.Case (exp, cases, ty)) =
        let val (exp', ctx) = monomorphize ctx rs exp
        in
            let fun monomorphizeCases ctx cases =
                    Util.foldThread (fn (c, ctx) =>
                                      monomorphizeCase ctx c)
                                    cases
                                    ctx

                and monomorphizeCase ctx (TAST.VariantCase (name, body)) =
                    let val (body', ctx) = monomorphize ctx rs body
                    in
                        let val (name', ctx) = mapName ctx name
                        in
                            (VariantCase (name', body'), ctx)
                        end
                    end

                and mapName ctx (TAST.NameOnly name) =
                    (NameOnly name, ctx)
                  | mapName ctx (TAST.NameBinding { casename, var, ty}) =
                    let val (ty', ctx) = monoType ctx rs ty
                    in
                        (NameBinding { casename = casename, var = var, ty = ty' }, ctx)
                    end

            in
                let val (cases', ctx) = monomorphizeCases ctx cases
                in
                    let val (ty', ctx) = monoType ctx rs ty
                    in
                        (Case (exp', cases', ty'), ctx)
                    end
                end
            end
        end
      | monomorphize ctx rs (TAST.ForeignFuncall (name, args, ty)) =
        let val (args', exp) = monomorphizeList ctx rs args
        in
            let val (ty', exp) = monoType ctx rs ty
            in
                (ForeignFuncall (name, args', ty'), ctx)
            end
        end
      | monomorphize ctx rs (TAST.NullPointer ty) =
        let val (ty', ctx) = monoType ctx rs ty
        in
            (NullPointer ty', ctx)
        end
      | monomorphize ctx rs (TAST.SizeOf ty) =
        let val (ty', ctx) = monoType ctx rs ty
        in
            (SizeOf ty', ctx)
        end
      | monomorphize ctx rs (TAST.AddressOf (var, ty)) =
        let val (ty', ctx) = monoType ctx rs ty
        in
            (AddressOf (var, ty'), ctx)
        end
      | monomorphize ctx rs (TAST.Cast (ty, exp)) =
        let val (ty', ctx) = monoType ctx rs ty
        in
            let val (exp', ctx) = monomorphize ctx rs exp
            in
                (Cast (ty', exp'), ctx)
            end
        end
      | monomorphize ctx rs (TAST.Seq (a, b)) =
        let val (a', ctx) = monomorphize ctx rs a
        in
            let val (b', ctx) = monomorphize ctx rs b
            in
                (Seq (a', b'), ctx)
            end
        end
      | monomorphize ctx rs (TAST.ConcreteFuncall (name, args, ty)) =
        let val (args', ctx) = monomorphizeList ctx rs args
        in
            let val (ty', ctx) = monoType ctx rs ty
            in
                (ConcreteFuncall (name, args', ty'), ctx)
            end
        end
      | monomorphize ctx rs (TAST.GenericFuncall (name, typarams, tyargs, args, ty)) =
        let val (rs', ctx) = monoReplacements ctx rs tyargs
        in
            let val (tyargs', ctx) = monoTypes ctx rs (Function.typeArgs typarams tyargs)
            in
                let val (args', ctx) = monomorphizeList ctx rs args
                in
                    let val (ty', ctx) = monoType ctx rs' ty
                    in
                        (* Check the table of function monomorphs. If this
                           name+type arg list combination doesn't exist yet, add
                           it *)
                        case getMonomorph ctx name tyargs' of
                            SOME id => let val gfcall = GenericFuncall (name, id, args', ty')
                                       in
                                           (gfcall, ctx)
                                       end
                          | NONE => let val id = freshId ()
                                    in
                                        let val ctx = addMonomorph ctx name tyargs' id
                                        in
                                            let val gfcall = GenericFuncall (name, id, args', ty')
                                            in
                                                (gfcall, ctx)
                                            end
                                        end
                                    end
                    end
                end
            end
        end
      | monomorphize ctx rs (TAST.MethodFuncall (name, tyargs, args, ty)) =
        raise Fail "monomorphize: method funcall not implemented yet"

    and monomorphizeList ctx rs exps =
        Util.foldThread (fn (exp, ctx) => monomorphize ctx rs exp)
                        exps
                        ctx

    and monoReplacements ctx rs tyargs =
        let val pairs = Map.toList tyargs
        in
            let val names = map (fn (n, _) => n) pairs
                and tys = map (fn (_, t) => t) pairs
            in
                let val (tys', ctx) = monoTypes ctx rs tys
                in
                    let val pairs' = ListPair.zip (names, tys')
                    in
                        (Map.fromList pairs', ctx)
                    end
                end
            end
        end

    fun monomorphizeTop' ctx (TAST.Defun (name, params, rt, _, body)) =
        monomorphizeDefun ctx name params rt body
      | monomorphizeTop' ctx (TAST.Defgeneric (name, typarams, params, rt, docstring, body)) =
        (ToplevelProgn [], ctx)
      | monomorphizeTop' ctx (TAST.Defclass _) =
        (ToplevelProgn [], ctx)
      | monomorphizeTop' _ (TAST.Definstance (name, arg, docstring, methods)) =
        raise Fail "definstance: not implemented yet"
      | monomorphizeTop' ctx (TAST.Deftype (name, params, _, ty)) =
        (* Type aliases don't need to be compiled to anything *)
        (ToplevelProgn [], ctx)
      | monomorphizeTop' ctx (TAST.Defdatatype (name, params, _, variants)) =
        (ToplevelProgn [], ctx)
      | monomorphizeTop' ctx (TAST.Deftemplate _) =
        (ToplevelProgn [], ctx)
      | monomorphizeTop' ctx (TAST.DefineSymbolMacro _) =
        (ToplevelProgn [], ctx)
      | monomorphizeTop' ctx (TAST.Defmodule _) =
        (ToplevelProgn [], ctx)
      | monomorphizeTop' ctx (TAST.InModule _) =
        (ToplevelProgn [], ctx)
      | monomorphizeTop' ctx (TAST.Defcfun (name, rawname, params, arity, rt, _)) =
        monomorphizeDefcfun ctx name rawname params arity rt

    and monomorphizeDefun ctx name params rt body =
        let val (params', ctx) = mapParams ctx params
        in
            let val (rt', ctx) = monoType ctx Map.empty rt
            in
                let val (body', ctx) = monomorphize ctx Map.empty body
                in
                    let val node = Defun (name,
                                          params',
                                          rt',
                                          body')
                    in
                        (node, ctx)
                    end
                end
            end
        end

    and monomorphizeDefcfun ctx name rawname params arity rt =
        let val (params', ctx) = mapParams ctx params
        in
            let val (rt', ctx) = monoType ctx Map.empty rt
            in
                (Defcfun (name, rawname, params', arity, rt'), ctx)
            end
        end

    and mapParams ctx params =
        Util.foldThread (fn (TAST.Param (var, ty), ctx) =>
                                    let val (ty', ctx) = monoType ctx Map.empty ty
                                    in
                                        (Param (var, ty'), ctx)
                                    end)
                                params
                                ctx

    and monomorphizeTop tenv fenv fdefenv ctx node =
        let val (node, ctx') = monomorphizeTop' ctx node
        in
            (* When we monomorphize a toplevel node, we have two contents: the
               starting context `ctx` and the resulting context `ctx'`. To
               create monomorph placeholders, we take a diff of both contexts,
               and create placeholders for concrete function definitions and
               type definitions that are implied by the monomorphs in the ctx'
               but are not present in the ctx. *)
            let val newFuncs = newFuncMonomorphs ctx ctx'
                and newTypes = newTypeMonomorphs ctx ctx'
            in
                let val defuns = map (fn (name, args, id) =>
                                         expandDefgeneric ctx' fenv fdefenv name args id)
                                     newFuncs
                    and deftypes = map (fn (name, _, ty, id) =>
                                           expandDefdisjunction ctx' tenv name id ty)
                                       newTypes
                in
                    (ToplevelProgn (defuns @ deftypes @ [node]),
                     ctx')
                end
            end
        end

    and expandDefgeneric ctx fenv fdefenv name args id =
        (case Function.envGet fenv name of
             (SOME (Function.CallableGFunc gf)) => expandGf ctx gf fdefenv name args id
           | _ => raise Fail "Internal compiler error: alleged generic function is not a gf")

    and expandDefdisjunction ctx tenv name id ty =
        let val variants = case ty of
                               (MonoType.Disjunction (name, _)) =>
                               (* Monomorphize the variants *)
                               let val variants = Type.getDisjunctionVariants tenv name
                                   and rs = Map.empty
                               in
                                   let fun mapVariant ctx (Type.Variant (_, SOME ty)) =
                                           monoType ctx rs ty
                                         | mapVariant ctx (Type.Variant (_, NONE)) =
                                           (MonoType.Unit, ctx)
                                   in
                                       let val (variants', ctx) = Util.foldThread (fn (var, ctx) =>
                                                                                      mapVariant ctx var)
                                                                                  variants
                                                                                  ctx
                                       in
                                           variants'
                                       end
                                   end
                               end
                             | _ => raise Fail "expandDefdisjunction: not a disjunction"
        in
            DefdatatypeMono (name,
                             id,
                             map (fn (MonoType.Variant (_, to)) =>
                                     case to of
                                         SOME t => t
                                       | NONE => MonoType.Unit)
                                 variants)
        end

    and expandGf ctx gf fdefenv name tyargs id =
        let val (params, body) = Option.valOf (FDefs.getDefinition fdefenv name)
            and (Function.GenericFunction (name, typarams, _, ty, _)) = gf
        in
            let val rs = makeReplacements typarams tyargs
            in
                let val (ty', ctx) = monoType ctx rs ty
                in
                    let val (params', ctx) = Util.foldThread (fn (TAST.Param (name, ty), ctx) =>
                                                                 let val (ty, ctx) = monoType ctx rs ty
                                                                 in
                                                                     (Param (name, ty), ctx)
                                                                 end)
                                                             params
                                                             ctx
                    in
                        let val (body', ctx) = monomorphize ctx rs body
                        in
                            let val node = DefunMonomorph (name, params', ty', body', id)
                            in
                                node
                            end
                        end
                    end
                end
            end
        end

    and makeReplacements (params: Type.typarams) (args: ty list) =
        Map.fromList
            (Util.mapidx (fn (Type.TypeParam name, idx) =>
                             (name, List.nth (args, idx)))
                         (OrderedSet.toList params))
end
