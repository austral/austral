(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Stages

module C = Combined
module SC = SmallCombined

let te (expr: Ast.aexpr): AstDB.aexpr =
  DesugarBorrows.transform_expr expr

let ts (stmt: Ast.astmt): AstDB.astmt =
  DesugarBorrows.transform_stmt stmt

let copy_combined_method_def (C.CMethodDef (id, tp, qp, qt, ds, ast)) =
  SC.CMethodDef (id, tp, qp, qt, ds, ts ast)

let desugar_decl (decl: C.combined_definition): SC.combined_definition =
  match decl with
  | C.CConstant (s, v, id, qt, ae, ds) -> SC.CConstant (s, v, id, qt, te ae, ds)
  | C.CRecord (s, tv, id, tp, u, ql, ds) -> SC.CRecord (s, tv, id, tp, u, ql, ds)
  | C.CUnion (s, tv, id, tp, u, qc, ds) -> SC.CUnion (s, tv, id, tp, u, qc, ds)
  | C.CFunction (s, v, id, tp, qp, qt, ast, ds, pl) -> SC.CFunction (s, v, id, tp, qp, qt, ts ast, ds, pl)
  | C.CTypeclass (s, v, id, tp, methods, ds) -> SC.CTypeclass (s, v, id, tp, methods, ds)
  | C.CInstance (s, v, qi, tp, qt, cmd, ds) -> SC.CInstance (s, v, qi, tp, qt, List.map copy_combined_method_def cmd, ds)

let desugar (md: C.combined_module): SC.combined_module =
  let (C.CombinedModule { name; kind; interface_docstring; interface_imports; body_docstring; body_imports; decls }) = md in
  SC.CombinedModule { name; kind; interface_docstring; interface_imports; body_docstring; body_imports; decls=List.map desugar_decl decls }
