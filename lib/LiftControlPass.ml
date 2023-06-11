(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Stages
open Temporary
open Span

let rec lift (stmt: Ast.astmt): AstLC.astmt =
  match stmt with
  | Ast.ASkip span ->
    AstLC.ASkip span
  | Ast.ALet (span, m, id, qt, s) ->
    AstLC.ALet (span, m, id, qt, lift s)
  | Ast.ADestructure (span, m, qbl, e, s) ->
    AstLC.ADestructure (span, m, qbl, transform e, lift s)
  | Ast.AAssign (span, lv, e, b) ->
    AstLC.AAssign (span, lv, transform e, b)
  | Ast.AIf (span, e, s1, s2) ->
    let tmp: tmp = fresh_tmp () in
    let e = transform e in
    AstLC.ABlock (
      empty_span,
      AstLC.LetTmp (tmp, e),
      AstLC.AIf (
        span,
        Temporary tmp,
        lift s1,
        lift s2
      )
    )
  | Ast.AWhen (span, e, s) ->
    AstLC.AWhen (span, transform e, lift s)
  | Ast.ACase (span, e, awl) ->
    let tmp: tmp = fresh_tmp () in
    let e = transform e in
    AstLC.ABlock (
      empty_span,
      AstLC.LetTmp (tmp, e),
      AstLC.ACase (
        span,
        Temporary tmp,
        List.map transform_abstract_when awl
      )
    )
  | Ast.AWhile (span, e, s) ->
    let tmp: tmp = fresh_tmp () in
    let e = transform e in
    AstLC.ABlock (
      empty_span,
      AstLC.LetTmp (tmp, e),
      AstLC.AWhile (
        span,
        Temporary tmp,
        AstLC.ABlock (
          empty_span,
          lift s,
          AssignTmp (tmp, e)
        )
      )
    )
  | Ast.AFor {span; name; initial; final; body} ->
    AstLC.AFor {span; name; initial=transform initial; final=transform final; body=lift body}
  | Ast.ABorrow {span; original; rename; region; body; mode} ->
    AstLC.ABorrow {span; original; rename; region; body=lift body; mode}
  | Ast.ABlock (span, s1, s2) ->
    AstLC.ABlock (span, lift s1, lift s2)
  | Ast.ADiscarding (span, e) ->
    AstLC.ADiscarding (span, transform e)
  | Ast.AReturn (span, e) ->
    AstLC.AReturn (span, transform e)

and transform (expr: Ast.aexpr): AstLC.aexpr =
  match expr with
  | Ast.NilConstant ->
    AstLC.NilConstant
  | Ast.BoolConstant b ->
    AstLC.BoolConstant b
  | Ast.IntConstant s ->
    AstLC.IntConstant s
  | Ast.FloatConstant s ->
    AstLC.FloatConstant s
  | Ast.StringConstant es ->
    AstLC.StringConstant es
  | Ast.Variable q ->
    AstLC.Variable q
  | Ast.FunctionCall (q, al) ->
    AstLC.FunctionCall (q, transform_arglist al)
  | Ast.ArithmeticExpression (ao, e1, e2) ->
    AstLC.ArithmeticExpression (ao, transform e1, transform e2)
  | Ast.Comparison (co, e1, e2) ->
    AstLC.Comparison (co, transform e1, transform e2)
  | Ast.Conjunction (e1, e2) ->
    AstLC.Conjunction (transform e1, transform e2)
  | Ast.Disjunction (e1, e2) ->
    AstLC.Disjunction (transform e1, transform e2)
  | Ast.Negation e ->
    AstLC.Negation (transform e)
  | Ast.IfExpression (e1, e2, e3) ->
    AstLC.IfExpression (transform e1, transform e2, transform e3)
  | Ast.Path (e, pel) ->
    AstLC.Path (transform e, pel)
  | Ast.RefPath (e, rpel) ->
    AstLC.RefPath (transform e, rpel)
  | Ast.Embed (qt, s, el) ->
    AstLC.Embed (qt, s, List.map transform el)
  | Ast.Deref e ->
    AstLC.Deref (transform e)
  | Ast.Typecast (e, qt) ->
    AstLC.Typecast (transform e, qt)
  | Ast.SizeOf qt ->
    AstLC.SizeOf qt
  | Ast.BorrowExpr (bm, q) ->
    AstLC.BorrowExpr (bm, q)
  | Ast.Reborrow q ->
    AstLC.Reborrow q

and transform_abstract_when aw =
  match aw with
  | Ast.AbstractWhen (id, qbl, s) -> AstLC.AbstractWhen (id, qbl, lift s)

and transform_arglist al =
  match al with
  | Ast.Positional el -> AstLC.Positional (List.map transform el)
  | Ast.Named idel -> AstLC.Named (List.map (fun (id, e) -> (id, transform e)) idel)
