(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Stages
open Identifier
open Common
open Span

(* Lift anonymous borrows from expressions. *)

type acc_rec = { original: qident; rename: qident; mode: borrow_stmt_kind }
type acc = acc_rec list

let tmp_counter: int ref = ref 0

let fresh_tmp _: int =
  let fresh_id = !tmp_counter in
  tmp_counter := !tmp_counter + 1;
  fresh_id

let make_rename (original: qident): qident =
  let a = source_module_name original
  and b = original_name original
  and c = local_name original in
  let id = string_of_int (fresh_tmp ()) in
  let b = make_ident ((ident_string b) ^ "_tmp_ref" ^ id) in
  let c = make_ident ((ident_string c) ^ "_tmp_ref" ^ id) in
  make_qident (a, b, c)

let make_mode (mode: borrowing_mode): borrow_stmt_kind =
  match mode with
  | ReadBorrow -> Read
  | WriteBorrow -> Write

let rec lift_borrows (expr: AstDP.aexpr) (acc: acc): AstDB.aexpr * acc =
  match expr with
  | AstDP.NilConstant -> (AstDB.NilConstant, acc)
  | AstDP.BoolConstant b -> (AstDB.BoolConstant b, acc)
  | AstDP.IntConstant i -> (AstDB.IntConstant i, acc)
  | AstDP.FloatConstant f -> (AstDB.FloatConstant f, acc)
  | AstDP.StringConstant s -> (AstDB.StringConstant s, acc)
  | AstDP.Variable id -> (AstDB.Variable id, acc)
  | AstDP.Temporary id -> (AstDB.Temporary id, acc)
  | AstDP.BorrowExpr (mode, original) ->
    let rename = make_rename original in
    let acc_rec = { original; rename; mode=make_mode mode } in
    let renamed_expr = AstDB.Variable rename in
    (renamed_expr, acc_rec :: acc)
  | AstDP.Reborrow original ->
    let rename = make_rename original in
    let acc_rec = { original; rename; mode = Reborrow } in
    let renamed_expr = AstDB.Variable rename in
    (renamed_expr, acc_rec :: acc)
  | AstDP.FunctionCall (id, args) ->
    let (args', acc) = lift_arglist args acc in
    let expr' = AstDB.FunctionCall (id, args') in
    (expr', acc)
  | AstDP.ArithmeticExpression (op, left, right) ->
    let (left', acc) = lift_borrows left acc in
    let (right', acc) = lift_borrows right acc in
    (AstDB.ArithmeticExpression (op, left', right'), acc)
  | AstDP.Comparison (op, left, right) ->
    let (left', acc) = lift_borrows left acc in
    let (right', acc) = lift_borrows right acc in
    (AstDB.Comparison (op, left', right'), acc)
  | AstDP.Conjunction (left, right) ->
    let (left', acc) = lift_borrows left acc in
    let (right', acc) = lift_borrows right acc in
    (AstDB.Conjunction (left', right'), acc)
  | AstDP.Disjunction (left, right) ->
    let (left', acc) = lift_borrows left acc in
    let (right', acc) = lift_borrows right acc in
    (AstDB.Disjunction (left', right'), acc)
  | AstDP.Negation expr' ->
    let (expr'', acc) = lift_borrows expr' acc in
    (AstDB.Negation expr'', acc)
  | AstDP.IfExpression (cond, then', else') ->
    let (cond', acc) = lift_borrows cond acc in
    let (then'', acc) = lift_borrows then' acc in
    let (else'', acc) = lift_borrows else' acc in
    (AstDB.IfExpression (cond', then'', else''), acc)
  | AstDP.Embed (ty, str, exprs) ->
    let (exprs', acc) = lift_borrows_list exprs acc in
    (AstDB.Embed (ty, str, exprs'), acc)
  | AstDP.Deref expr' ->
    let (expr'', acc) = lift_borrows expr' acc in
    (AstDB.Deref expr'', acc)
  | AstDP.Typecast (expr', ty) ->
    let (expr'', acc) = lift_borrows expr' acc in
    (AstDB.Typecast (expr'', ty), acc)
  | AstDP.SizeOf ty -> (AstDB.SizeOf ty, acc)
  | AstDP.SlotAccessor (subpath, name) ->
     let (subpath, acc) = lift_borrows subpath acc in
     (AstDB.SlotAccessor (subpath, name), acc)
  | AstDP.PointerSlotAccessor (subpath, name) ->
     let (subpath, acc) = lift_borrows subpath acc in
     (AstDB.PointerSlotAccessor (subpath, name), acc)
  | AstDP.ArrayIndex (subpath, expr) ->
     let (subpath, acc) = lift_borrows subpath acc in
     let (expr, acc) = lift_borrows expr acc in
     (AstDB.ArrayIndex (subpath, expr), acc)

and lift_arglist (args: AstDP.abstract_arglist) (acc: acc): (AstDB.abstract_arglist * acc) =
  match args with
  | AstDP.Positional exprs ->
    let (exprs', acc') = lift_borrows_list exprs acc in
    (AstDB.Positional exprs', acc')
  | AstDP.Named pairs ->
    let (pairs', acc') = lift_named_arg_pairs pairs acc in
    (AstDB.Named pairs', acc')

and lift_named_arg_pairs (pairs: (identifier * AstDP.aexpr) list) (acc: acc): ((identifier * AstDB.aexpr) list * acc) =
  let (acc, l) = Util.map_with_context (fun (acc, pair) ->
                     let (name, expr) = pair in
                     let (expr, acc) = lift_borrows expr acc in
                     let pair = (name, expr) in
                     (acc, pair))
                   acc
                   pairs
  in
  (l, acc)

and lift_borrows_list (exprs: AstDP.aexpr list) (acc: acc): (AstDB.aexpr list * acc) =
  let (acc, l) = Util.map_with_context (fun (acc, expr) ->
                     let (expr, acc) = lift_borrows expr acc in
                     (acc, expr))
                   acc
                   exprs
  in
  (l, acc)

(* Wrap statements in borrow statements *)

let counter: int ref = ref 0

let anonymous_region _: identifier =
  let fresh_id = "_tmp_region_" ^ string_of_int !counter in
  counter := !counter + 1;
  make_ident fresh_id

let wrap_stmt_with_borrows (stmt: AstDB.astmt) (acc: acc): AstDB.astmt =
  let wrap_with_borrow (stmt: AstDB.astmt) (acc_rec: acc_rec): AstDB.astmt =
    AstDB.ABorrow {
        span = empty_span;
        original = original_name acc_rec.original;
        rename = original_name acc_rec.rename;
        region = anonymous_region ();
        body = stmt;
        mode = acc_rec.mode;
      }
  in
  List.fold_left wrap_with_borrow stmt acc

(* Transform statements. *)

let rec transform_stmt (stmt: AstDP.astmt): AstDB.astmt =
  match stmt with
  | AstDP.ASkip span ->
    AstDB.ASkip span
  | AstDP.ALet (span, mutability, name, ty, body) ->
    let body = transform_stmt body in
    let stmt = AstDB.ALet (span, mutability, name, ty, body) in
    stmt
  | AstDP.ADestructure (span, mutability, bindings, value, body) ->
    let (value, acc) = lift_borrows value [] in
    let body = transform_stmt body in
    let stmt = AstDB.ADestructure (span, mutability, bindings, value, body) in
    wrap_stmt_with_borrows stmt acc
  | AstDP.AAssign (span, path, value) ->
     let (path, acc) = lift_borrows path [] in
     let (value, acc) = lift_borrows value acc in
     let stmt = AstDB.AAssign (span, path, value) in
     wrap_stmt_with_borrows stmt acc
  | AstDP.AAssignVar (span, name, value) ->
     let (value, acc) = lift_borrows value [] in
     let stmt = AstDB.AAssignVar (span, name, value) in
     wrap_stmt_with_borrows stmt acc
  | AstDP.AInitialAssign (name, ty, value) ->
     let (value, acc) = lift_borrows value [] in
     let stmt = AstDB.AInitialAssign (name, ty, value) in
     wrap_stmt_with_borrows stmt acc
  | AstDP.AIf (span, cond, then', else') ->
    let (cond, acc) = lift_borrows cond [] in
    let then' = transform_stmt then' in
    let else' = transform_stmt else' in
    let stmt = AstDB.AIf (span, cond, then', else') in
    wrap_stmt_with_borrows stmt acc
  | AstDP.AWhen (span, cond, body) ->
    let (cond, acc) = lift_borrows cond [] in
    let body = transform_stmt body in
    let stmt = AstDB.AWhen (span, cond, body) in
    wrap_stmt_with_borrows stmt acc
  | AstDP.ACase (span, expr, whens) ->
    let (expr, acc) = lift_borrows expr [] in
    let whens = transform_whens whens in
    let stmt = AstDB.ACase (span, expr, whens) in
    wrap_stmt_with_borrows stmt acc
  | AstDP.AWhile (span, cond, body) ->
    let (cond, acc) = lift_borrows cond [] in
    let body = transform_stmt body in
    let stmt = AstDB.AWhile (span, cond, body) in
    wrap_stmt_with_borrows stmt acc
  | AstDP.AFor { span; name; initial; final; body } ->
    let (initial, acc) = lift_borrows initial [] in
    let (final, acc) = lift_borrows final acc in
    let body = transform_stmt body in
    let stmt = AstDB.AFor { span; name; initial; final; body } in
    wrap_stmt_with_borrows stmt acc
  | AstDP.ABlock (span, stmt1, stmt2) ->
    let stmt1 = transform_stmt stmt1 in
    let stmt2 = transform_stmt stmt2 in
    AstDB.ABlock (span, stmt1, stmt2)
  | AstDP.ADiscarding (span, expr) ->
    let (expr, acc) = lift_borrows expr [] in
    let stmt = AstDB.ADiscarding (span, expr) in
    wrap_stmt_with_borrows stmt acc
  | AstDP.AReturn (span, expr) ->
    let (expr, acc) = lift_borrows expr [] in
    let stmt = AstDB.AReturn (span, expr) in
    wrap_stmt_with_borrows stmt acc
  | AstDP.ABorrow { span; original; rename; region; body; mode } ->
    let body = transform_stmt body in
    AstDB.ABorrow { span; original; rename; region; body; mode }

and transform_whens (whens: AstDP.abstract_when list): AstDB.abstract_when list =
  List.map (fun (AstDP.AbstractWhen (id, bindings, body)) ->
    let body = transform_stmt body in
    AstDB.AbstractWhen (id, bindings, body)
  ) whens

let transform_expr (expr: AstDP.aexpr): AstDB.aexpr =
  let (expr, acc) = lift_borrows expr [] in
  if (List.length acc) > 0 then
    Error.err "Borrow expressions not allowed in this context."
  else
    expr
