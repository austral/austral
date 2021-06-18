open Imports
open Cst
open Ast
open Qualifier

let rec abs_stmt im stmt =
  match stmt with
  | CSkip ->
     ASkip
  | CLet (name, ty, value) ->
     ALet (name, qualify_typespec im ty, abs_expr im value)
  | CAssign (name, value) ->
     AAssign (name, abs_expr im value)
  | CIf (condition_branches, else_branch) ->
     abs_if im condition_branches else_branch
  | CCase (e, cases) ->
     ACase (abs_expr im e, List.map (abs_when im) cases)
  | CWhile (c, b) ->
     AWhile (abs_expr im c, abs_stmt im b)
  | CFor (n, i, f, b) ->
     AFor {
         name = n;
         initial = abs_expr im i;
         final = abs_expr im f;
         body = abs_stmt im b
       }
  | CBlock l ->
     abs_block im l
  | CDiscarding e ->
     ADiscarding (abs_expr im e)
  | CReturn e ->
     AReturn (abs_expr im e)

and abs_expr im expr =
  match expr with
  | CNilConstant -> NilConstant
  | CBoolConstant b -> BoolConstant b
  | CIntConstant i -> IntConstant i
  | CFloatConstant f -> FloatConstant f
  | CStringConstant s -> StringConstant s
  | CVariable i -> Variable i
  | CFuncall (name, args) ->
     FunctionCall (qualify_identifier im name, abs_arglist im args)
  | CArith (op, lhs, rhs) ->
     ArithmeticExpression (op, abs_expr im lhs, abs_expr im rhs)
  | CComparison (op, lhs, rhs) ->
     Comparison (op, abs_expr im lhs, abs_expr im rhs)
  | CConjunction (lhs, rhs) ->
     Conjunction (abs_expr im lhs, abs_expr im rhs)
  | CDisjunction (lhs, rhs) ->
     Disjunction (abs_expr im lhs, abs_expr im rhs)
  | CNegation e ->
     Negation (abs_expr im e)
  | CIfExpression (c, t, f) ->
     IfExpression (abs_expr im c, abs_expr im t, abs_expr im f)

and abs_if (im: import_map) (cbranches: condition_branch list) (else_branch: cstmt): astmt =
  match cbranches with
  | (ConditionBranch (c, b))::rest ->
     AIf (abs_expr im c, abs_stmt im b, abs_if im rest else_branch)
  | [] ->
     ASkip

and abs_when im (ConcreteWhen (name, params, body)) =
    AbstractWhen (name,
                  List.map (fun (ConcreteParam (n, t)) -> (n, qualify_typespec im t)) params,
                  abs_stmt im body)

and abs_block (im: import_map) (l: cstmt list): astmt =
  match l with
  | (first::rest) ->
     ABlock (abs_stmt im first, abs_block im rest)
  | [] -> ASkip

and abs_arglist im args =
  match args with
  | ConcretePositionalArgs l ->
     Positional (List.map (abs_expr im) l)
  | ConcreteNamedArgs l ->
     Named (List.map (fun (n, v) -> (n, abs_expr im v)) l)
