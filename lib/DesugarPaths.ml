(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *)
open Identifier
open Common
open Stages
open Error

let rec desugar_elem (elem: AstLC.path_elem) (subpath: AstDP.aexpr) (is_ref_transform: bool): AstDP.aexpr =
  match elem with
  | AstLC.SlotAccessor name ->
     AstDP.SlotAccessor (subpath, name)
  | AstLC.PointerSlotAccessor name ->
     if is_ref_transform then
       AstDP.SlotAccessor (subpath, name)
     else
       AstDP.PointerSlotAccessor (subpath, name)
  | AstLC.ArrayIndex expr ->
     let expr = transform_expr expr in
     AstDP.ArrayIndex (subpath, expr)

and desugar_path (head: qident) (exprs: AstLC.path_elem list) (mode: borrowing_mode) (is_ref_transform: bool): AstDP.aexpr =
  desugar_path_inner head (List.rev exprs) mode is_ref_transform

and desugar_path_inner (head: qident) (exprs: AstLC.path_elem list) (mode: borrowing_mode) (is_ref_transform: bool): AstDP.aexpr =
  match exprs with
  | [] ->
     if is_ref_transform then
       (* Base case: do nothing. *)
       AstDP.Variable head
     else
       (* Base case: borrow the head. *)
       AstDP.BorrowExpr (mode, head)
  | first::rest ->
     let rest: AstDP.aexpr = desugar_path_inner head rest mode is_ref_transform in
     let first: AstDP.aexpr = desugar_elem first rest is_ref_transform in
     first

and transform_expr (expr: AstLC.aexpr): AstDP.aexpr =
  match expr with
  (* Transform paths *)
  | AstLC.Path (head, path_exprs) ->
     let path = desugar_path head path_exprs ReadBorrow false in
     AstDP.Deref path
  | AstLC.RefPath (head, path_exprs) ->
     desugar_path head path_exprs ReadBorrow true
  (* Everything else is the identity transform. *)
  | AstLC.NilConstant ->
     AstDP.NilConstant
  | AstLC.BoolConstant b ->
     AstDP.BoolConstant b
  | AstLC.IntConstant s ->
     AstDP.IntConstant s
  | AstLC.FloatConstant s ->
     AstDP.FloatConstant s
  | AstLC.StringConstant es ->
     AstDP.StringConstant es
  | AstLC.Variable q ->
     AstDP.Variable q
  | AstLC.Temporary n ->
     AstDP.Temporary n
  | AstLC.FunctionCall (q, al) ->
     AstDP.FunctionCall (q, transform_arglist al)
  | AstLC.ArithmeticExpression (ao, e1, e2) ->
     AstDP.ArithmeticExpression (ao, transform_expr e1, transform_expr e2)
  | AstLC.Comparison (co, e1, e2) ->
     AstDP.Comparison (co, transform_expr e1, transform_expr e2)
  | AstLC.Conjunction (e1, e2) ->
     AstDP.Conjunction (transform_expr e1, transform_expr e2)
  | AstLC.Disjunction (e1, e2) ->
     AstDP.Disjunction (transform_expr e1, transform_expr e2)
  | AstLC.Negation e ->
     AstDP.Negation (transform_expr e)
  | AstLC.IfExpression (e1, e2, e3) ->
     AstDP.IfExpression (transform_expr e1, transform_expr e2, transform_expr e3)
  | AstLC.Embed (qt, s, el) ->
     AstDP.Embed (qt, s, List.map transform_expr el)
  | AstLC.Deref e ->
     AstDP.Deref (transform_expr e)
  | AstLC.Typecast (e, qt) ->
     AstDP.Typecast (transform_expr e, qt)
  | AstLC.SizeOf qt ->
     AstDP.SizeOf qt
  | AstLC.BorrowExpr (bm, q) ->
     AstDP.BorrowExpr (bm, q)
  | AstLC.Reborrow q ->
     AstDP.Reborrow q

and transform_lvalue (expr: AstLC.aexpr): AstDP.aexpr =
  match expr with
  (* Transform paths *)
  | AstLC.Path (head, path_exprs) ->
     desugar_path head path_exprs WriteBorrow false
  (* Everything else is not a valid lvalue. *)
  | _ ->
     err "Invalid lvalue."

and transform_stmt (stmt: AstLC.astmt): AstDP.astmt =
  match stmt with
  | AstLC.ASkip span ->
     AstDP.ASkip span
  | AstLC.ALet (span, m, id, qt, s) ->
     AstDP.ALet (span, m, id, qt, transform_stmt s)
  | AstLC.ADestructure (span, m, qbl, e, s) ->
     AstDP.ADestructure (span, m, qbl, transform_expr e, transform_stmt s)
  | AstLC.AAssign (span, lv, e) -> begin
      match lv with
      | AstLC.Path (name, []) ->
         AstDP.AAssignVar (span, name, transform_expr e)
      | _ ->
         AstDP.AAssign (span, transform_lvalue lv, transform_expr e)
     end
  | AstLC.AInitialAssign (name, ty, e) ->
     AstDP.AInitialAssign (name, ty, transform_expr e)
  | AstLC.LetTmp (name, expr, body) ->
     let expr = transform_expr expr
     and body = transform_stmt body in
     AstDP.LetTmp (name, expr, body)
  | AstLC.AssignTmp (name, expr) ->
     let expr = transform_expr expr in
     AstDP.AssignTmp (name, expr)
  | AstLC.AIf (span, e, s1, s2) ->
     AstDP.AIf (span, transform_expr e, transform_stmt s1, transform_stmt s2)
  | AstLC.AWhen (span, e, s) ->
     AstDP.AWhen (span, transform_expr e, transform_stmt s)
  | AstLC.ACase (span, e, awl) ->
     AstDP.ACase (span, transform_expr e, List.map transform_abstract_when awl)
  | AstLC.AWhile (span, e, s) ->
     AstDP.AWhile (span, transform_expr e, transform_stmt s)
  | AstLC.AFor { span; name; initial; final; body } ->
     AstDP.AFor { span; name; initial=transform_expr initial; final=transform_expr final; body=transform_stmt body}
  | AstLC.ABorrow {span; original; rename; region; body; mode} ->
     AstDP.ABorrow {span; original; rename; region; body=transform_stmt body; mode}
  | AstLC.ABlock (span, s1, s2) ->
     AstDP.ABlock (span, transform_stmt s1, transform_stmt s2)
  | AstLC.ADiscarding (span, e) ->
     AstDP.ADiscarding (span, transform_expr e)
  | AstLC.AReturn (span, e) ->
     AstDP.AReturn (span, transform_expr e)

and transform_abstract_when aw =
  match aw with
  | AstLC.AbstractWhen (id, qbl, s) ->
     AstDP.AbstractWhen (id, qbl, transform_stmt s)

and transform_arglist al =
  match al with
  | AstLC.Positional el ->
     AstDP.Positional (List.map transform_expr el)
  | AstLC.Named idel ->
     AstDP.Named (List.map (fun (id, e) -> (id, transform_expr e)) idel)
