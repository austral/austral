(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** The errors that have no category yet, and their wording. *)
open Identifier
open Type
open MonoType
open TypeParameter
open ErrorText

(** What a name that was not found was expected to name. *)
type name_kind =
  | CallableName
  | VariableName

let name_kind_word (kind: name_kind): string =
  match kind with
  | CallableName -> "callable"
  | VariableName -> "variable"

type t =
  | UnknownName of { kind: name_kind; name: identifier }
  | CannotAssignToConstant
  | CannotAssignToParameter
  | CantAssignToImmutableVar of identifier
  | RefTransformNeedsRef of ty
  | VarCollidesWithDecl of identifier
  | WrongNumberOfArguments of { expected: int; actual: int }
  | BrokenOrphanRule
  | OverlappingInstances
  | DuplicateTypeParameter of type_parameter option
  | TypeWrongArity of { typename: identifier; expected: int; actual: int option }
  | LetOutsideStatementList
  | DestructureOutsideStatementList
  | ForeignFunctionTypeNotAllowed
  | InvalidDeclInContext
  | BorrowExpressionNotAllowed
  | InvalidLvalue
  | NoUnionWithId
  | DeclHasNoName
  | BadWhitespaceEscape
  | SymbolImportedTwice
  | VariableAlreadyExists of identifier
  | TypeclassResolutionFailed of ty
  | ParameterNotFound
  | ParameterArgumentLengthMismatch of { parameters: type_parameter list; arguments: mono_ty list }
  | TypeOfStringConstant
  | RegionAlreadyExists
  | NoRecordWithName of identifier
  | BindingUniverseMismatch of { expected: universe; actual: universe }
  | NoBindingForParameter of identifier
  | ConstructorUniverseMismatch
  | InstanceArgumentShapeNotAllowed
  | InstanceOverlapTypeNotAllowed
  | MatchTypeOnMonoTy
  | FreeTypeWithNonFreeArgument of qident
  | TypeVariableNotReplaced of identifier
  | IsNumericOnMonoTy
  | AssignToNonMutableReference of ty
  | ReborrowOfNonMutableReference of ty
  | MinOfEmptyList
  | SplitReturnedEmpty
  | LeftoverArguments

let render (error: t): err_text =
  match error with
  | UnknownName { kind; name } ->
     [
       Text "I can't find a ";
       Text (name_kind_word kind);
       Text " named ";
       Code (ident_string name);
       Text "."
     ]
  | CannotAssignToConstant ->
     [
       Text "Cannot assign a value to a constant."
     ]
  | CannotAssignToParameter ->
     [
       Text "Cannot assign a value to a function parameter."
     ]
  | CantAssignToImmutableVar name ->
     [
       Text "Cannot assign a value to ";
       Code (ident_string name);
       Text " because it is immutable."
     ]
  | RefTransformNeedsRef ty ->
     [
       Text "The head of a reference transform expression must be of a reference type, but I got ";
       Type ty
     ]
  | VarCollidesWithDecl name ->
     [
       Text "The variable ";
       Code (ident_string name);
       Text " collides with the name of a declaration."
     ]
  | WrongNumberOfArguments { expected; actual } ->
     [
       Text "Wrong number of arguments. Expected ";
       Code (string_of_int expected);
       Text " and got ";
       Code (string_of_int actual);
       Text "."
     ]
  | BrokenOrphanRule ->
     [
       Text "Orphan rule broken: typeclass and type are both foreign."
     ]
  | OverlappingInstances ->
     [
       Text "Instance overlaps."
     ]
  | DuplicateTypeParameter param ->
     (match param with
      | Some param -> [
          Text "Duplicate type parameter ";
          Code (typaram_name param |> ident_string)
        ]
      | None -> [
          Text "Multiple type parameters have the same name.";
        ])
  | TypeWrongArity { typename; expected; actual } ->
     (let text = [
        Text "The type ";
        Code (ident_string typename);
        Text " expects ";
        Code (string_of_int expected);
        Text " type arguments";
      ]
      in
      text @ match actual with
      | Some actual -> [
          Text ", but I only found ";
          Code (string_of_int actual);
          Text " arguments."
        ]
      | None -> [Text "."])
  | LetOutsideStatementList ->
     [
       Text "Let statement not in a list context"
     ]
  | DestructureOutsideStatementList ->
     [
       Text "Destructure statement not in a list context"
     ]
  | ForeignFunctionTypeNotAllowed ->
     [
       Text "Not allowed"
     ]
  | InvalidDeclInContext ->
     [
       Text "Invalid decl in this context"
     ]
  | BorrowExpressionNotAllowed ->
     [
       Text "Borrow expressions not allowed in this context."
     ]
  | InvalidLvalue ->
     [
       Text "Invalid lvalue."
     ]
  | NoUnionWithId ->
     [
       Text "No union with ID"
     ]
  | DeclHasNoName ->
     [
       Text "decl has no name"
     ]
  | BadWhitespaceEscape ->
     [
       Text "Bad whitespace escape sequence"
     ]
  | SymbolImportedTwice ->
     [
       Text "Cannot add symbol to the import map twice."
     ]
  | VariableAlreadyExists name ->
     [
       Text ("push_var: var with the name " ^ (ident_string name) ^ " already exists")
     ]
  | TypeclassResolutionFailed dispatch_ty ->
     [
       Text ("Typeclass resolution failed. Dispatch type: "
             ^ (type_string dispatch_ty))
     ]
  | ParameterNotFound ->
     [
       Text "Parameter not found."
     ]
  | ParameterArgumentLengthMismatch { parameters; arguments } ->
     [
       Text ("Parameter list and argument list don't have the same length:\n\nparameters:\n"
             ^ (String.concat ", " (List.map show_type_parameter parameters))
             ^ "\narguments:\n"
             ^ (String.concat ", " (List.map show_mono_ty arguments)))
     ]
  | TypeOfStringConstant ->
     [
       Text "Type of the string constant"
     ]
  | RegionAlreadyExists ->
     [
       Text "Region map already has a region with this name"
     ]
  | NoRecordWithName name ->
     [
       Text ("No record with this name: " ^ (ident_string name))
     ]
  | BindingUniverseMismatch { expected; actual } ->
     [
       Text ("Mismatched universes: expected "
             ^ (show_universe expected)
             ^ " and got "
             ^ (show_universe actual))
     ]
  | NoBindingForParameter name ->
     [
       Text ("No binding for this parameter: " ^ (ident_string name))
     ]
  | ConstructorUniverseMismatch ->
     [
       Text "Universe mismatch"
     ]
  | InstanceArgumentShapeNotAllowed ->
     [
       Text "Not allowed"
     ]
  | InstanceOverlapTypeNotAllowed ->
     [
       Text "Not allowed"
     ]
  | MatchTypeOnMonoTy ->
     [
       Text "match_type called with MonoTy argument"
     ]
  | FreeTypeWithNonFreeArgument name ->
     [
       Text ("Free type called with non-free argument: " ^ (qident_debug_name name))
     ]
  | TypeVariableNotReplaced name ->
     [
       Text ("strip_type': Variable not replaced: '" ^ (ident_string name) ^ "'")
     ]
  | IsNumericOnMonoTy ->
     [
       Text "is_numeric called with MonoTy argument"
     ]
  | AssignToNonMutableReference ty ->
     [
       Text ("Can't assign: lvalue is not a mutable reference: " ^ (type_string ty))
     ]
  | ReborrowOfNonMutableReference ty ->
     [
       Text ("Cannot reborrow something that is not a mutable reference: " ^ (type_string ty))
     ]
  | MinOfEmptyList ->
     [
       Text "empty list passed to min_list"
     ]
  | SplitReturnedEmpty ->
     [
       Text "split_on_char returned empty"
     ]
  | LeftoverArguments ->
     [
       Text "There are leftover arguments."
     ]
