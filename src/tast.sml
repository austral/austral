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

structure TAst :> TAST = struct
    type ty = Type.ty

    datatype ast = UnitConstant
                 | BoolConstant of bool
                 | IntConstant of string * ty
                 | FloatConstant of string * ty
                 | StringConstant of CST.escaped_string
                 | Variable of Symbol.variable * ty
                 | Let of Symbol.variable * ast * ast
                 | Cond of ast * ast * ast
                 | ArithOp of Arith.kind * Arith.oper * ast * ast
                 | TupleCreate of ast list
                 | TupleProj of ast * int
                 | Allocate of ast
                 | Load of ast
                 | Store of ast * ast
                 | The of ty * ast
                 | ForeignFuncall of string * ty * ast list
                 | ForeignNull of ty
                 | SizeOf of ty
                 | Seq of ast * ast
                 | Funcall of Symbol.symbol * ty list * ast list * ty

    type name = Symbol.symbol
    type param_name = name
    type docstring = string option
    type symbol = Symbol.symbol
    type variable = Symbol.variable

    datatype top_ast = Defun of name * param list * ty * docstring * ast
                     | Defgeneric of name * Type.typarams * param list * ty * docstring * ast
                     | Defclass of name * param_name * docstring * method_decl list
                     | Definstance of name * instance_arg * docstring * method_def list
                     | Deftype of name * Type.typarams * docstring * ty
                     | Defdisjunction of name * Type.typarams * docstring * Type.variant list
                     | Deftemplate of Macro.template
                     | DefineSymbolMacro of name * RCST.rcst * docstring
                     | Defmodule of Symbol.module_name * Module.defmodule_clause list
                     | InModule of Symbol.symbol_name
         and param = Param of variable * ty
         and method_decl = MethodDecl of name * param list * ty * docstring
         and method_def = MethodDef of name * param list * ty * docstring * ast
         and instance_arg = InstanceArg of name * Type.typarams

    local
        open Type
    in
        fun typeOf UnitConstant = Unit
          | typeOf (BoolConstant _) = Bool
          | typeOf (IntConstant (_, t)) = t
          | typeOf (FloatConstant (_, t)) = t
          | typeOf (StringConstant s) =
            StaticArray (Integer (Unsigned, Int8),
                         String.size (CST.escapedToString s))
          | typeOf (Variable (_, t)) = t
          | typeOf (Let (_, _, b)) = typeOf b
          | typeOf (Cond (_, tb, _)) = typeOf tb
          | typeOf (ArithOp (kind, _, lhs, _)) =
            (* The type of most arithmetic operations is the type of the
               arguments, except for checked operations, where the result is a
               tuple of the resulting value and a boolean indicating overflow *)
            (case kind of
                 Arith.Checked => Tuple [typeOf lhs, Bool]
               | _ => typeOf lhs)
          | typeOf (TupleCreate exps) = Tuple (map typeOf exps)
          | typeOf (TupleProj (tup, idx)) =
            (case typeOf tup of
                 Tuple tys => List.nth (tys, idx)
               | _ => raise Fail "Not a tuple")
          | typeOf (Allocate v) = Pointer (typeOf v)
          | typeOf (Load p) =
            (case typeOf p of
                 (Pointer t) => t
               | _ => raise Fail "Not a pointer")
          | typeOf (Store (p, _)) = typeOf p
          | typeOf (The (t, _)) = t
          | typeOf (SizeOf _) =
            Integer (Unsigned, Int64)
          | typeOf (ForeignFuncall (_, rt, _)) = rt
          | typeOf (ForeignNull ty) =
            ForeignPointer ty
          | typeOf (Seq (_, v)) = typeOf v
          | typeOf (Funcall (_, _, _, t)) = t
    end

    (* Bindings *)

    datatype binding = Binding of ty * mutability
         and mutability = Immutable
                        | Mutable

    fun bindType (Binding (t, _)) = t

    type bindings = (Symbol.variable, binding) Map.map

    (* Context data for the augment function *)

    datatype context = Context of bindings * Type.tenv * Type.param Set.set * Function.fenv

    fun mkContext b t p f = Context (b, t, p, f)

    fun ctxBindings (Context (b, _, _, _)) = b
    fun ctxTenv (Context (_, t, _, _)) = t
    fun ctxTyParams (Context (_, _, ps, _)) = ps
    fun ctxFenv (Context (_, _, _, f)) = f

    (* Augment AST with type information *)

    local
        open Type
    in
        val defaultIntType = Integer (Signed, Int32)
        val sizeType = Integer (Unsigned, Int64)
        val defaultFloatType = Float Double

        fun augment AST.UnitConstant _ = UnitConstant
          | augment (AST.BoolConstant b) _ = BoolConstant b
          | augment (AST.IntConstant i) _ = IntConstant (i, defaultIntType)
          | augment (AST.FloatConstant f) _ = FloatConstant (f, defaultFloatType)
          | augment (AST.StringConstant s) _ = StringConstant s
          | augment (AST.Variable name) c =
            (case (Map.get (ctxBindings c) name) of
                 SOME bind => Variable (name, bindType bind)
               | NONE => raise Fail ("No such variable: " ^ (Symbol.varToString name)))
          | augment (AST.Let (name, v, body)) c =
            let val v' = augment v c
            in
                let val s' = Map.iadd (ctxBindings c)
                                      (name, (Binding (typeOf v', Mutable)))
                in
                    Let (name,
                         v',
                         augment body (mkContext s' (ctxTenv c) (ctxTyParams c) (ctxFenv c)))
                end
            end
          | augment (AST.Cond (test, cons, alt)) c =
            let val test' = augment test c
                and cons' = augment cons c
                and alt'  = augment alt c
            in
                if (typeOf test') <> Bool then
                    raise Fail "The test in an `if` must be of boolean type"
                else
                    if (typeOf cons') <> (typeOf alt') then
                        raise Fail "The consequent and the alternate must have the same type"
                    else
                        Cond (test', cons', alt')
            end
          | augment (AST.ArithOp (kind, oper, lhs, rhs)) c =
            let val lhs' = augment lhs c
                and rhs' = augment rhs c
            in
                let val lhsTy = typeOf lhs'
                    and rhsTy = typeOf rhs'
                in
                    if rhsTy = lhsTy then
                        case kind of
                            Arith.Float => if Type.isFloat lhsTy then
                                               ArithOp (kind, oper, lhs', rhs')
                                           else
                                               raise Fail "Argument must be a float"
                          | _ => if Type.isInteger lhsTy then
                                     ArithOp (kind, oper, lhs', rhs')
                                 else
                                     raise Fail "Argument must be an integer"
                    else
                        raise Fail "Both arguments to an arithmetic operator must be of the same type"
                end
            end
          | augment (AST.TupleCreate exps) c =
            TupleCreate (map (fn e => augment e c) exps)
          | augment (AST.TupleProj (exp, i)) c =
            TupleProj (augment exp c, i)
          | augment (AST.StaticArrayLength arr) c =
            let val arr' = augment arr c
            in
                case typeOf arr' of
                    (StaticArray (_, len)) => IntConstant (Int.toString len, sizeType)
                  | _ => raise Fail "Argument to static-array-length not a static array"

            end
          | augment (AST.Allocate v) c =
            Allocate (augment v c)
          | augment (AST.Load e) c =
            let val e' = augment e c
            in
                case (typeOf e') of
                    Pointer t => Load e'
                  | _ => raise Fail "load: not a pointer"
            end
          | augment (AST.Store (p, v)) c =
            let val p' = augment p c
                and v' = augment v c
            in
                case (typeOf p') of
                    Pointer t => let val ty = typeOf v'
                                 in
                                     if ty = t then
                                         Store (p', v')
                                     else
                                         raise Fail "store: type mismatch"
                                 end
                  | _ => raise Fail "store: first argument must be a pointer"
            end
          | augment (AST.The (typespec, exp)) c =
            let val tenv = ctxTenv c
            in
                let val ty = resolve tenv (ctxTyParams c) typespec
                in
                    case exp of
                        (AST.IntConstant i) => if isInteger ty then
                                                   The (ty, augment exp c)
                                               else
                                                   raise Fail "Bad types for `the`"
                      | (AST.Funcall (name, args)) => augmentFuncall name args c (SOME ty)
                      | e => let val exp' = augment exp c
                             in
                                 if ty = typeOf exp' then
                                     The (ty, exp')
                                 else
                                     raise Fail "Bad types for `the`"
                             end
                end
            end
          | augment (AST.SizeOf typespec) c =
            SizeOf (resolve (ctxTenv c) (ctxTyParams c) typespec)
          | augment (AST.ForeignNull ty) c =
            ForeignNull (resolve (ctxTenv c) (ctxTyParams c) typespec)
          | augment (AST.ForeignFuncall (name, typespec, args)) c =
            let val tenv = ctxTenv c
            in
                ForeignFuncall (name,
                                resolve tenv (ctxTyParams c) typespec,
                                map (fn a => augment a c) args)
            end
          | augment (AST.Seq (a, b)) c =
            Seq (augment a c,
                 augment b c)
          | augment (AST.Funcall (name, args)) c =
            augmentFuncall name args c NONE

        and augmentFuncall name args c the_context =
            let val fenv = ctxFenv c
                and auKer = Symbol.auKer
            in
                if Builtin.isBuiltin name then
                    Funcall (name,
                             [],
                             map (fn e => augment e c) args,
                             Bool)
                else
                    case Function.envGet fenv name of
                        SOME f => augmentCallable f args c the_context
                      | NONE => raise Fail ("No function with this name: " ^ (Symbol.toString name))
            end

        and augmentCallable (Function.CallableFunc f) args c _ =
            augmentConcreteFuncall f args c
          | augmentCallable (Function.CallableGFunc gf) args c the_context =
            if Function.isRTP gf then
                case the_context of
                    SOME ty => let val (Function.GenericFunction (name, typarams, params, rt, _)) = gf
                               in
                                   if (List.length params) = (List.length args) then
                                       let val args' = map (fn a => augment a c) args
                                       in
                                           let val argTypes = map typeOf args'
                                           in
                                               let val binds = Function.matchFunc params rt argTypes ty
                                               in
                                                   Funcall (name,
                                                            Function.typeArgs typarams binds,
                                                            args',
                                                            ty)
                                               end
                                           end
                                       end
                                   else
                                       raise Fail "Funcall arity error"
                               end
                  | NONE => raise Fail ("Error in call to function "
                                        ^ (Symbol.toString (Function.gFunctionName gf))
                                        ^ ": generic functions that are return-type polymorphic must be called in the context of a `the` form.")
            else
                augmentGenericFuncall gf args c
          | augmentCallable Function.CallableMethod args c _ =
            augmentMethodCall args c

        and augmentConcreteFuncall (Function.Function (name, params, rt, _)) args c =
            if (List.length params) = (List.length args) then
                Funcall (name,
                         [],
                         map (fn a => augment a c) args,
                         rt)
            else
                raise Fail "Funcall arity error"

        and augmentGenericFuncall (Function.GenericFunction (name, typarams, params, rt, _)) args c =
            if (List.length params) = (List.length args) then
                Funcall (name,
                         [], (* We don't need to supply type parameters for non-RTP functions *)
                         map (fn a => augment a c) args,
                         rt)
            else
                raise Fail "Funcall arity error"

        and augmentMethodCall args c =
            raise Fail "method calls not implemented yet"
    end

    fun funcContext params typarams tenv fenv =
        let val bindings = Map.fromList (map (fn (DAST.Param (var, ty)) => (var, Binding (ty, Immutable)))
                                             params)
        in
            Context (bindings, tenv, typarams, fenv)
        end

    fun mapParam (DAST.Param (n, ty)) =
        Param (n, ty)

    fun mapParams ps =
        map mapParam ps

    fun augmentTop (DAST.Defun (name, params, ty, docstring, ast)) tenv fenv =
        Defun (name,
               mapParams params,
               ty,
               docstring,
               augment ast (funcContext params Set.empty tenv fenv))
      | augmentTop (DAST.Defgeneric (name, typarams, params, ty, docstring, ast)) tenv fenv =
        Defgeneric (name,
                    typarams,
                    mapParams params,
                    ty,
                    docstring,
                    augment ast (funcContext params (OrderedSet.toUnordered typarams) tenv fenv))
      | augmentTop (DAST.Defclass (name, paramName, docstring, methods)) tenv fenv =
        let fun augmentMethod (DAST.MethodDecl (name, params, ty, docstring)) =
                MethodDecl (name,
                            mapParams params,
                            ty,
                            docstring)
        in
            Defclass (name, paramName, docstring, map augmentMethod methods)
        end
      | augmentTop (DAST.Definstance (name, DAST.InstanceArg (arg, typarams), docstring, defs)) tenv fenv =
        let fun mapDef (DAST.MethodDef (name, params, ty, docstring, ast)) =
                MethodDef (name,
                           mapParams params,
                           ty,
                           docstring,
                           augment ast (funcContext params (OrderedSet.toUnordered typarams) tenv fenv))
        in
            Definstance (name, InstanceArg (arg, typarams), docstring, map mapDef defs)
        end
      | augmentTop (DAST.Deftype (name, params, docstring, ty)) tenv _ =
            Deftype (name,
                     params,
                     docstring,
                     ty)
      | augmentTop (DAST.Defdisjunction (name, params, docstring, variants)) tenv _ =
        Defdisjunction (name,
                        params,
                        docstring,
                        variants)
      | augmentTop (DAST.Deftemplate tmpl) _ _ =
        Deftemplate tmpl
      | augmentTop (DAST.DefineSymbolMacro (name, exp, docstring)) _ _ =
        DefineSymbolMacro (name, exp, docstring)
      | augmentTop (DAST.Defmodule clauses) _ _ =
        Defmodule clauses
      | augmentTop (DAST.InModule name) _ _ =
        InModule name
end
