open Error
open ErrorText
open Identifier
open Type

let arithmetic_incompatible_types ~lhs ~rhs =
  austral_raise TypeError [
    Text "Both operands to an arithmetic expression must be compatible types. The LHS has type ";
    Type lhs;
    Text " but the RHS has type ";
    Type rhs;
    Text "."
  ]

let arithmetic_not_numeric ty =
  austral_raise TypeError [
    Text "Both operands of an arithmetic operator must be numeric types, but ";
    Type ty;
    Text " is not."
  ]

let array_indexing_disallowed ty =
  austral_raise TypeError [
    Text "The array indexing operator is not allowed for the type ";
    Type ty
  ]

let borrow_constant () =
  austral_raise TypeError [
    Text "Constants cannot be borrowed."
  ]

let reborrow_constant () =
  austral_raise TypeError [
    Text "Constants cannot be borrowed."
  ]

let reborrow_wrong_type (ty: ty) =
  austral_raise TypeError [
      Text "Reborrow expressions take a variable of mutable reference type, but the expression has type ";
      Type ty
  ]

let borrow_non_linear ty =
  austral_raise TypeError [
      Text "Cannot borrow ";
      Type ty;
      Text " because it is not a linear type."
    ]

let cannot_borrow_immutable_var_mutably (name: identifier) =
  austral_raise TypeError [
      Text "Cannot borrow the variable ";
      Code (ident_string name);
      Text " mutably because the variable is immutable."
    ]

let cannot_borrow_param_mutably (name: identifier) =
  austral_raise TypeError [
      Text "Cannot borrow the variable ";
      Code (ident_string name);
      Text " mutably because the it is a function parameter, and therefore immutable."
    ]

let call_non_callable ty =
  austral_raise TypeError [
    Text "The type ";
    Type ty;
    Text " is neither a function, method or function pointer, and cannot be called."
  ]

let call_wrong_arity ~name ~expected ~actual =
  austral_raise TypeError [
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

let case_non_exhaustive () =
  austral_raise TypeError [
    Text "Non-exhaustive case statement."
  ]

let case_non_public () =
  austral_raise TypeError [
    Text "Union must be public or from the same module to be used in a case statement."
  ]

let case_non_union ty =
  austral_raise TypeError [
    Text "The case expression has type ";
    Type ty;
    Text " which is not a union type."
  ]

let case_wrong_slots () =
  austral_raise TypeError [
    Text "The set of slots in the case statement doesn't match the set of slots in the union definition."
  ]

let cast_different_references ~different_types ~different_regions =
  let text = match different_types, different_regions with
  | true, false -> [
      Text "Cannot cast a reference to one with a different underlying type."
    ]
  | false, true -> [
      Text "Cannot cast a reference to one with a different underlying region."
    ]
  | _ -> [
      Text "Cannot cast to a different region"
    ]
  in
  austral_raise TypeError text

let cast_invalid ~target ~source =
  austral_raise TypeError [
    Text "Cannot cast to ";
    Type target;
    Text " from ";
    Type source
  ]

let cast_write_ref_to_non_ref () =
  austral_raise TypeError [
    Text "Write references can only be casted to a read reference."
  ]

let condition_not_boolean ~kind ~form ~ty =
  austral_raise TypeError [
    Text "The condition in ";
    Code kind;
    Text "-";
    Text form;
    Text "s should be a boolean type, but is ";
    Type ty
  ]

let constructor_not_named declaration =
  austral_raise TypeError [
    Text "Arguments to ";
    Text declaration;
    Text " constructor must be named."
  ]

let constructor_wrong_args declaration =
  austral_raise TypeError [
    Text "Arguments to ";
    Text declaration;
    Text " constructor have the wrong names."
  ]

let dereference_non_reference ty =
  austral_raise TypeError [
    Text "Only references can be dereferenced, but ";
    Type ty;
    Text " is not."
  ]

let dereference_non_free ty =
  austral_raise LinearityError [
    Text "Cannot dereference non-free type ";
    Type ty
  ]

let destructure_non_record ty =
  austral_raise TypeError [
    Text "The type ";
    Type ty;
    Text " cannot be destructured because it is not a record."
  ]

let destructure_not_public ty =
  austral_raise TypeError [
    Text "The type ";
    Type ty;
    Text " is not a public record and so cannot be destructured."
  ]

let destructure_wrong_slots ty =
  austral_raise TypeError [
    Text "The set of slots mentioned differs from the set of slots in ";
    Type ty;
  ]

let discard_linear universe =
  austral_raise TypeError [
    Text "Cannot discard a value in the ";
    Code (universe_string universe);
    Text " universe."
  ]

let expression_not_constant () =
  austral_raise TypeError [
    Text "The expression is not a valid constant expression."
  ]

let for_bounds_non_integral ~bound ~ty =
  austral_raise TypeError [
    Text "The type of the ";
    Text bound;
    Text " value in the loop range is ";
    Type ty;
    Text " which is not an integer type."
  ]

let foreign_in_safe_module () =
  austral_raise DeclarationError [
    Text "Can't declare a foreign function in a safe module."
  ]

let foreign_type_parameters () =
  austral_raise DeclarationError [
    Text "Foreign functions can't have type parameters."
  ]

let fun_pointer_named_args () =
  austral_raise TypeError [
    Text "You can't call a function pointer with a named argument list, because function pointers don't preserve parameter names, so we wouldn't know how to assign arguments to parameters."
  ]

let fun_invalid_pragmas () =
  austral_raise DeclarationError [
    Text "Only import and export pragmas are allowed inside functions."
  ]

let if_inequal ~lhs ~rhs =
  austral_raise TypeError [
    Text "The branches of the ";
    Code "if";
    Text "-expression have different types: ";
    Type lhs;
    Text " and ";
    Type rhs
  ]

let logical_operands_not_boolean ~operator ~types = match types with
| [ty] ->
    austral_raise TypeError [
      Text "The operand of a ";
      Text operator;
      Text " operator must be a boolean expression, but it has the type ";
      Type ty
    ]
| [first; second] ->
    austral_raise TypeError [
      Text "Both operands of a ";
      Text operator;
      Text " operator must be boolean expressions, but they have types ";
      Type first;
      Text " and ";
      Type second
    ]
  | _ ->
    austral_raise TypeError [
      Text "All operands of a logical operator must be boolean expressions."
    ]

let lvalue_index () =
  austral_raise TypeError [
    Text "The array index operator doesn't work in lvalues."
  ]

let no_such_slot ~type_name ~slot_name =
  austral_raise TypeError [
    Text "The type ";
    Code (ident_string type_name);
    Text " has no slot with name ";
    Code (ident_string slot_name)
  ]

let no_suitable_instance ~typeclass ~ty =
  austral_raise TypeError [
    Text "The typeclass ";
    Code (ident_string typeclass);
    Text " has no instance which matches for ";
    Type ty
  ]

let path_not_free ty =
  austral_raise TypeError [
    Text "The path ends with the type ";
    Type ty;
    Text " which is not in the ";
    Code "Free";
    Text " universe."
  ]

let path_not_public ~type_name ~slot_name =
  austral_raise TypeError [
    Text "The slot ";
    Code (ident_string slot_name);
    Text " is not publically visible in the type ";
    Code (ident_string type_name);
    Text " and so cannot be read."
  ]

let path_not_record ty =
  austral_raise TypeError [
    Text "Cannot take a path of the type ";
    Type ty;
    Text " because it is not a record."
  ]

let slot_wrong_type ~name ~expected ~actual =
  austral_raise TypeError [
    Text "The slot ";
    Code (ident_string name);
    Text " should have type ";
    Type expected;
    Text " but is declared to have type ";
    Type actual
  ]

let unconstrained_generic_function name =
  austral_raise TypeError [
    Code (ident_string name);
    Text " is generic but unconstrained, so its type is ambiguous.";
    Break;
    Text "Consider clarifying its type using a cast."
  ]

let unconstrained_type_parameter ~typeclass ~typaram =
  austral_raise TypeError [
    Text "The type parameter ";
    Code (ident_string typaram);
    Text " does not implement the type class ";
    Code (ident_string typeclass);
    Break;
    Text "Consider adding a constraint to the type parameter."
  ]

let unknown_name ~kind ~name =
  austral_raise GenericError [
    Text "I can't find a ";
    Text kind;
    Text " named ";
    Code (ident_string name);
    Text "."
  ]

let cannot_assign_to_constant _ =
  austral_raise GenericError [
      Text "Cannot assign a value to a constant."
    ]

let cannot_assign_to_parameter _ =
  austral_raise GenericError [
      Text "Cannot assign a value to a function parameter."
    ]

let cant_assign_to_immutable_var (name: identifier) =
  austral_raise GenericError [
      Text "Cannot assign a value to ";
      Code (ident_string name);
      Text " because it is immutable."
    ]

let ref_transform_needs_ref ty =
  austral_raise GenericError [
      Text "The head of a reference transform expression must be of a reference type, but I got ";
      Type ty
    ]

let ref_transform_not_record ty =
  austral_raise GenericError [
      Text "Tried to transform a reference to a record into a reference into a slot, but the type ";
      Type ty;
      Text " is not a record"
    ]
