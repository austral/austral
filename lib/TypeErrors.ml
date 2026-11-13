(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** The type errors, and their wording. *)
open Identifier
open Type
open ErrorText

(** Where a condition is. *)
type condition =
  | IfExpressionCondition
  | IfStatementCondition
  | WhileStatementCondition

(** The kinds of type whose values are built with a constructor. *)
type aggregate =
  | RecordAggregate
  | UnionAggregate

let aggregate_word (aggregate: aggregate): string =
  match aggregate with
  | RecordAggregate -> "record"
  | UnionAggregate -> "union"

type logical_operator =
  | ConjunctionOperator
  | DisjunctionOperator
  | NegationOperator

let logical_operator_word (operator: logical_operator): string =
  match operator with
  | ConjunctionOperator -> "conjunction"
  | DisjunctionOperator -> "disjunction"
  | NegationOperator -> "negation"

(** The two ends of the range of a [for] loop. *)
type loop_bound =
  | InitialBound
  | FinalBound

let loop_bound_word (bound: loop_bound): string =
  match bound with
  | InitialBound -> "initial"
  | FinalBound -> "final"

type t =
  | ArithmeticIncompatibleTypes of { lhs: ty; rhs: ty }
  | ArithmeticNotNumeric of ty
  | CannotBorrowImmutableVarMutably of identifier
  | CannotBorrowParamMutably of { name: identifier; ty: ty }
  | CallNonCallable of ty
  | CallWrongArity of { name: identifier; expected: int; actual: int }
  | CaseNonExhaustive
  | CaseNonPublic
  | CaseNonUnion of ty
  | CaseWrongSlots of { binding_names: identifier list; slot_names: identifier list }
  | CastDifferentReferences of { different_types: bool; different_regions: bool }
  | CastInvalid of { target: ty; source: ty }
  | CastWriteRefToNonRef
  | ConditionNotBoolean of { condition: condition; ty: ty }
  | ConstructorNotNamed of aggregate
  | ConstructorWrongArgs of aggregate
  | DereferenceNonReference of ty
  | DestructureNonRecord of ty
  | DestructureNotPublic of ty
  | DestructureWrongSlots of ty
  | DiscardLinear of universe
  | ExpressionNotConstant
  | ForBoundsNonIntegral of { bound: loop_bound; ty: ty }
  | FunPointerNamedArgs
  | IfInequal of { lhs: ty; rhs: ty }
  | LogicalOperandsNotBoolean of { operator: logical_operator; types: ty list }
  | NoSuchSlot of { type_name: identifier; slot_name: identifier }
  | NoSuitableInstance of { typeclass: identifier; ty: ty }
  | PathNotPublic of { type_name: identifier; slot_name: identifier }
  | PathNotRecord of ty
  | SlotWrongType of { name: identifier; expected: ty; actual: ty }
  | UnconstrainedGenericFunction of identifier
  | UnconstrainedTypeParameter of { typeclass: identifier; typaram: identifier }
  | CantAccessSlotNotRecord of { slot_name: identifier; ty: ty }
  | CantIndexNotSpan of ty

let render (error: t): err_text =
  match error with
  | ArithmeticIncompatibleTypes { lhs; rhs } ->
     [
       Text "Both operands to an arithmetic expression must be compatible types. The LHS has type ";
       Type lhs;
       Text " but the RHS has type ";
       Type rhs;
       Text "."
     ]
  | ArithmeticNotNumeric ty ->
     [
       Text "Both operands of an arithmetic operator must be numeric types, but ";
       Type ty;
       Text " is not."
     ]
  | CannotBorrowImmutableVarMutably name ->
     [
       Text "Cannot borrow the variable ";
       Code (ident_string name);
       Text " mutably because the variable is immutable."
     ]
  | CannotBorrowParamMutably { name; ty } ->
     [
       Text "Cannot borrow the variable ";
       Code (ident_string name);
       Text " mutably because it is a function parameter, and therefore immutable.";
       Text "\nYou can move it to a mutable local variable: ";
       Code ("var mut_" ^ (ident_string name) ^ ": " ^ (type_string ty) ^ " := " ^ (ident_string name) ^ ";");
     ]
  | CallNonCallable ty ->
     [
       Text "The type ";
       Type ty;
       Text " is neither a function, method or function pointer, and cannot be called."
     ]
  | CallWrongArity { name; expected; actual } ->
     [
       Text "The callable ";
       Code (ident_string name);
       Text " expects ";
       Text (string_of_int expected);
       Text " argument";
       Text (if expected = 1 then " " else "s ");
       Text "but ";
       Text (string_of_int actual);
       Text (if actual = 1 then " was " else " were ");
       Text "given."
     ]
  | CaseNonExhaustive ->
     [
       Text "Non-exhaustive case statement."
     ]
  | CaseNonPublic ->
     [
       Text "Union must be public or from the same module to be used in a case statement."
     ]
  | CaseNonUnion ty ->
     [
       Text "The case expression has type ";
       Type ty;
       Text " which is not a union type."
     ]
  | CaseWrongSlots { binding_names; slot_names } ->
     [
       Text "The set of slots (";
       Code (String.concat ", " (List.map ident_string binding_names));
       Text ") in the case statement doesn't match the set of slots in the union definition (";
       Code (String.concat ", " (List.map ident_string slot_names));
       Text ")."
     ]
  | CastDifferentReferences { different_types; different_regions } ->
     (match different_types, different_regions with
      | true, false -> [
          Text "Cannot cast a reference to one with a different underlying type."
        ]
      | false, true -> [
          Text "Cannot cast a reference to one with a different underlying region."
        ]
      | _ -> [
          Text "Cannot cast to a different region"
        ])
  | CastInvalid { target; source } ->
     [
       Text "Cannot cast to ";
       Type target;
       Text " from ";
       Type source
     ]
  | CastWriteRefToNonRef ->
     [
       Text "Write references can only be casted to a read reference."
     ]
  | ConditionNotBoolean { condition; ty } ->
     let (kind, form) = match condition with
       | IfExpressionCondition -> ("if", "expression")
       | IfStatementCondition -> ("if", "statement")
       | WhileStatementCondition -> ("while", "statement")
     in
     [
       Text "The condition in ";
       Code kind;
       Text "-";
       Text form;
       Text "s should be a boolean type, but is ";
       Type ty
     ]
  | ConstructorNotNamed aggregate ->
     [
       Text "Arguments to ";
       Text (aggregate_word aggregate);
       Text " constructor must be named."
     ]
  | ConstructorWrongArgs aggregate ->
     [
       Text "Arguments to ";
       Text (aggregate_word aggregate);
       Text " constructor have the wrong names."
     ]
  | DereferenceNonReference ty ->
     [
       Text "Only references can be dereferenced, but ";
       Type ty;
       Text " is not."
     ]
  | DestructureNonRecord ty ->
     [
       Text "The type ";
       Type ty;
       Text " cannot be destructured because it is not a record."
     ]
  | DestructureNotPublic ty ->
     [
       Text "The type ";
       Type ty;
       Text " is not a public record and so cannot be destructured."
     ]
  | DestructureWrongSlots ty ->
     [
       Text "The set of slots mentioned differs from the set of slots in ";
       Type ty;
     ]
  | DiscardLinear universe ->
     [
       Text "Cannot discard a value in the ";
       Code (universe_string universe);
       Text " universe."
     ]
  | ExpressionNotConstant ->
     [
       Text "The expression is not a valid constant expression."
     ]
  | ForBoundsNonIntegral { bound; ty } ->
     [
       Text "The type of the ";
       Text (loop_bound_word bound);
       Text " value in the loop range is ";
       Type ty;
       Text " which is not ";
       Code "Index";
       Text "."
     ]
  | FunPointerNamedArgs ->
     [
       Text "You can't call a function pointer with a named argument list, because function pointers don't preserve parameter names, so we wouldn't know how to assign arguments to parameters."
     ]
  | IfInequal { lhs; rhs } ->
     [
       Text "The branches of the ";
       Code "if";
       Text "-expression have different types: ";
       Type lhs;
       Text " and ";
       Type rhs
     ]
  | LogicalOperandsNotBoolean { operator; types } ->
     (match types with
      | [ty] ->
         [
           Text "The operand of a ";
           Text (logical_operator_word operator);
           Text " operator must be a boolean expression, but it has the type ";
           Type ty
         ]
      | [first; second] ->
         [
           Text "Both operands of a ";
           Text (logical_operator_word operator);
           Text " operator must be boolean expressions, but they have types ";
           Type first;
           Text " and ";
           Type second
         ]
      | _ ->
         [
           Text "All operands of a logical operator must be boolean expressions."
         ])
  | NoSuchSlot { type_name; slot_name } ->
     [
       Text "The type ";
       Code (ident_string type_name);
       Text " has no slot with name ";
       Code (ident_string slot_name)
     ]
  | NoSuitableInstance { typeclass; ty } ->
     [
       Text "The typeclass ";
       Code (ident_string typeclass);
       Text " has no instance which matches for ";
       Type ty
     ]
  | PathNotPublic { type_name; slot_name } ->
     [
       Text "The slot ";
       Code (ident_string slot_name);
       Text " is not publically visible in the type ";
       Code (ident_string type_name);
       Text " and so cannot be read."
     ]
  | PathNotRecord ty ->
     [
       Text "Cannot take a path of the type ";
       Type ty;
       Text " because it is not a record."
     ]
  | SlotWrongType { name; expected; actual } ->
     [
       Text "The slot ";
       Code (ident_string name);
       Text " should have type ";
       Type expected;
       Text " but is declared to have type ";
       Type actual
     ]
  | UnconstrainedGenericFunction name ->
     [
       Code (ident_string name);
       Text " is generic but unconstrained, so its type is ambiguous.";
       Break;
       Text "Consider clarifying its type using a cast."
     ]
  | UnconstrainedTypeParameter { typeclass; typaram } ->
     [
       Text "The type parameter ";
       Code (ident_string typaram);
       Text " does not implement the type class ";
       Code (ident_string typeclass);
       Break;
       Text "Consider adding a constraint to the type parameter."
     ]
  | CantAccessSlotNotRecord { slot_name; ty } ->
     [
       Text "Can't access the slot ";
       Code (ident_string slot_name);
       Text " because the type ";
       Type ty;
       Text " is not a record."
     ]
  | CantIndexNotSpan ty ->
     [
       Text "I can't use the array indexing operator here, because the type ";
       Type ty;
       Text " is not a span."
     ]
