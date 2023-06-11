(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(** Compiler stages. *)

(** The AST is the CST, but imports are qualified and `let` statements are
    reshaped. *)
module Ast = struct
  open Identifier
  open Common
  open Span
  open Escape

  type qtypespec =
    | QTypeSpecifier of qident * qtypespec list
    | QReadRef of qtypespec * qtypespec
    | QWriteRef of qtypespec * qtypespec
  [@@deriving show]

  type qbinding =
    QBinding of {
        name: identifier;
        ty: qtypespec;
        rename: identifier;
      }

  type astmt =
    | ASkip of span
    | ALet of span * mutability * identifier * qtypespec * astmt
    | ADestructure of span * mutability * qbinding list * aexpr * astmt
    | AAssign of span * aexpr * aexpr
    | AInitialAssign of qident * qtypespec * aexpr
    | AIf of span * aexpr * astmt * astmt
    | AWhen of span * aexpr * astmt
    | ACase of span * aexpr * abstract_when list
    | AWhile of span * aexpr * astmt
    | AFor of {
        span: span;
        name: identifier;
        initial: aexpr;
        final: aexpr;
        body: astmt
      }
    | ABorrow of {
        span: span;
        original: identifier;
        rename: identifier;
        region: identifier;
        body: astmt;
        mode: borrow_stmt_kind
      }
    | ABlock of span * astmt * astmt
    | ADiscarding of span * aexpr
    | AReturn of span * aexpr

  and aexpr =
    | NilConstant
    | BoolConstant of bool
    | IntConstant of string
    | FloatConstant of string
    | StringConstant of escaped_string
    | Variable of qident
    | FunctionCall of qident * abstract_arglist
    | ArithmeticExpression of arithmetic_operator * aexpr * aexpr
    | Comparison of comparison_operator * aexpr * aexpr
    | Conjunction of aexpr * aexpr
    | Disjunction of aexpr * aexpr
    | Negation of aexpr
    | IfExpression of aexpr * aexpr * aexpr
    | Path of qident * path_elem list
    | RefPath of qident * path_elem list
    | Embed of qtypespec * string * aexpr list
    | Deref of aexpr
    | Typecast of aexpr * qtypespec
    | SizeOf of qtypespec
    | BorrowExpr of borrowing_mode * qident
    | Reborrow of qident

  and abstract_when =
    | AbstractWhen of identifier * qbinding list * astmt

  and abstract_arglist =
    | Positional of aexpr list
    | Named of (identifier * aexpr) list

  and path_elem =
    | SlotAccessor of identifier
    | PointerSlotAccessor of identifier
    | ArrayIndex of aexpr

  and ref_path_elem =
    | RefSlotAccessor of identifier

  and lvalue =
    LValue of identifier * path_elem list
end

(** The combined representation merges the interface and body. *)
module Combined = struct
  open Identifier
  open Common
  open Imports
  open Ast
  open Type
  open TypeParameter
  open TypeParameters
  open Span

  type combined_module = CombinedModule of {
        name: module_name;
        kind: module_kind;
        interface_docstring: docstring;
        interface_imports: import_map;
        body_docstring: docstring;
        body_imports: import_map;
        decls: combined_definition list;
      }

  and combined_definition =
    | CConstant of span * vis * identifier * qtypespec * aexpr * docstring
    | CRecord of span * type_vis * identifier * typarams * universe * qslot list * docstring
    | CUnion of span * type_vis * identifier * typarams * universe * qcase list * docstring
    | CFunction of span * vis * identifier * typarams * qparam list * qtypespec * astmt * docstring * pragma list
    | CTypeclass of span * vis * identifier * type_parameter * combined_method_decl list * docstring
    | CInstance of span * vis * qident * typarams * qtypespec * combined_method_def list * docstring

  and qslot = QualifiedSlot of identifier * qtypespec

  and qcase = QualifiedCase of identifier * qslot list

  and qparam = QualifiedParameter of identifier * qtypespec

  and combined_method_decl = CMethodDecl of identifier * typarams * qparam list * qtypespec * docstring

  and combined_method_def = CMethodDef of identifier * typarams * qparam list * qtypespec * docstring * astmt
end

(* The AST, but expressions in control structures are lifted into temporaries. *)
module AstLC = struct
  open Identifier
  open Common
  open Span
  open Escape

  type qtypespec = Ast.qtypespec

  type qbinding = Ast.qbinding

  type ref_path_elem = Ast.ref_path_elem

  type astmt =
    | ASkip of span
    | ALet of span * mutability * identifier * qtypespec * astmt
    | ADestructure of span * mutability * qbinding list * aexpr * astmt
    | AAssign of span * lvalue * aexpr * bool
    | AIf of span * aexpr * astmt * astmt
    | AWhen of span * aexpr * astmt
    | ACase of span * aexpr * abstract_when list
    | AWhile of span * aexpr * astmt
    | AFor of {
        span: span;
        name: identifier;
        initial: aexpr;
        final: aexpr;
        body: astmt
      }
    | ABorrow of {
        span: span;
        original: identifier;
        rename: identifier;
        region: identifier;
        body: astmt;
        mode: borrow_stmt_kind
      }
    | ABlock of span * astmt * astmt
    | ADiscarding of span * aexpr
    | AReturn of span * aexpr
    | LetTmp of identifier * aexpr * astmt
    | AssignTmp of identifier * aexpr

  and aexpr =
    | NilConstant
    | BoolConstant of bool
    | IntConstant of string
    | FloatConstant of string
    | StringConstant of escaped_string
    | Variable of qident
    | Temporary of identifier
    | FunctionCall of qident * abstract_arglist
    | ArithmeticExpression of arithmetic_operator * aexpr * aexpr
    | Comparison of comparison_operator * aexpr * aexpr
    | Conjunction of aexpr * aexpr
    | Disjunction of aexpr * aexpr
    | Negation of aexpr
    | IfExpression of aexpr * aexpr * aexpr
    | Path of aexpr * path_elem list
    | RefPath of aexpr * ref_path_elem list
    | Embed of qtypespec * string * aexpr list
    | Deref of aexpr
    | Typecast of aexpr * qtypespec
    | SizeOf of qtypespec
    | BorrowExpr of borrowing_mode * qident
    | Reborrow of qident

  and abstract_when =
    | AbstractWhen of identifier * qbinding list * astmt

  and abstract_arglist =
    | Positional of aexpr list
    | Named of (identifier * aexpr) list

  and path_elem =
    | SlotAccessor of identifier
    | PointerSlotAccessor of identifier
    | ArrayIndex of aexpr

  and lvalue =
    LValue of identifier * path_elem list
end

(** The AstLC, but paths are desugared. *)
module AstDP = struct
  open Identifier
  open Common
  open Span
  open Escape

  type qtypespec = Ast.qtypespec

  type qbinding = Ast.qbinding

  type astmt =
    | ASkip of span
    | ALet of span * mutability * identifier * qtypespec * astmt
    | ADestructure of span * mutability * qbinding list * aexpr * astmt
    | AAssign of span * aexpr * aexpr
    | AAssignVar of span * qident * aexpr
    | AInitialAssign of qident * qtypespec * aexpr
    | AIf of span * aexpr * astmt * astmt
    | AWhen of span * aexpr * astmt
    | ACase of span * aexpr * abstract_when list
    | AWhile of span * aexpr * astmt
    | AFor of {
        span: span;
        name: identifier;
        initial: aexpr;
        final: aexpr;
        body: astmt
      }
    | ABorrow of {
        span: span;
        original: identifier;
        rename: identifier;
        region: identifier;
        body: astmt;
        mode: borrow_stmt_kind
      }
    | ABlock of span * astmt * astmt
    | ADiscarding of span * aexpr
    | AReturn of span * aexpr
    | LetTmp of identifier * aexpr * astmt
    | AssignTmp of identifier * aexpr

  and aexpr =
    | NilConstant
    | BoolConstant of bool
    | IntConstant of string
    | FloatConstant of string
    | StringConstant of escaped_string
    | Variable of qident
    | Temporary of identifier
    | FunctionCall of qident * abstract_arglist
    | ArithmeticExpression of arithmetic_operator * aexpr * aexpr
    | Comparison of comparison_operator * aexpr * aexpr
    | Conjunction of aexpr * aexpr
    | Disjunction of aexpr * aexpr
    | Negation of aexpr
    | IfExpression of aexpr * aexpr * aexpr
    | Embed of qtypespec * string * aexpr list
    | Deref of aexpr
    | Typecast of aexpr * qtypespec
    | SizeOf of qtypespec
    | BorrowExpr of borrowing_mode * qident
    | Reborrow of qident
    (* Path expressions *)
    | SlotAccessor of aexpr * identifier
    | PointerSlotAccessor of aexpr * identifier
    | ArrayIndex of aexpr * aexpr

  and abstract_when =
    | AbstractWhen of identifier * qbinding list * astmt

  and abstract_arglist =
    | Positional of aexpr list
    | Named of (identifier * aexpr) list
end

(** The AstDP, but anonymous borrows are desugared. *)
module AstDB = struct
  open Identifier
  open Common
  open Span
  open Escape

  type qtypespec = Ast.qtypespec

  type qbinding = Ast.qbinding

  type astmt =
    | ASkip of span
    | ALet of span * mutability * identifier * qtypespec * astmt
    | ADestructure of span * mutability * qbinding list * aexpr * astmt
    | AAssign of span * lvalue * aexpr * bool
    | AIf of span * aexpr * astmt * astmt
    | AWhen of span * aexpr * astmt
    | ACase of span * aexpr * abstract_when list
    | AWhile of span * aexpr * astmt
    | AFor of {
        span: span;
        name: identifier;
        initial: aexpr;
        final: aexpr;
        body: astmt
      }
    | ABorrow of {
        span: span;
        original: identifier;
        rename: identifier;
        region: identifier;
        body: astmt;
        mode: borrow_stmt_kind
      }
    | ABlock of span * astmt * astmt
    | ADiscarding of span * aexpr
    | AReturn of span * aexpr
    | LetTmp of identifier * aexpr * astmt
    | AssignTmp of identifier * aexpr

  and aexpr =
    | NilConstant
    | BoolConstant of bool
    | IntConstant of string
    | FloatConstant of string
    | StringConstant of escaped_string
    | Variable of qident
    | Temporary of identifier
    | FunctionCall of qident * abstract_arglist
    | ArithmeticExpression of arithmetic_operator * aexpr * aexpr
    | Comparison of comparison_operator * aexpr * aexpr
    | Conjunction of aexpr * aexpr
    | Disjunction of aexpr * aexpr
    | Negation of aexpr
    | IfExpression of aexpr * aexpr * aexpr
    | Path of aexpr * path_elem list
    | RefPath of aexpr * ref_path_elem list
    | Embed of qtypespec * string * aexpr list
    | Deref of aexpr
    | Typecast of aexpr * qtypespec
    | SizeOf of qtypespec

  and abstract_when =
    | AbstractWhen of identifier * qbinding list * astmt

  and abstract_arglist =
    | Positional of aexpr list
    | Named of (identifier * aexpr) list

  and path_elem =
    | SlotAccessor of identifier
    | PointerSlotAccessor of identifier
    | ArrayIndex of aexpr

  and ref_path_elem =
    | RefSlotAccessor of identifier

  and lvalue =
    LValue of identifier * path_elem list
end

(** The combined representation, but bodies are thoroughly desugared. *)
module SmallCombined = struct
  open Identifier
  open Common
  open Imports
  open AstDB
  open Type
  open TypeParameter
  open TypeParameters
  open Span

  type qslot = Combined.qslot
  type qcase = Combined.qcase
  type qparam = Combined.qparam

  type combined_module = CombinedModule of {
        name: module_name;
        kind: module_kind;
        interface_docstring: docstring;
        interface_imports: import_map;
        body_docstring: docstring;
        body_imports: import_map;
        decls: combined_definition list;
      }

  and combined_definition =
    | CConstant of span * vis * identifier * qtypespec * aexpr * docstring
    | CRecord of span * type_vis * identifier * typarams * universe * qslot
 list * docstring
    | CUnion of span * type_vis * identifier * typarams * universe * qcase list * docstring
    | CFunction of span * vis * identifier * typarams * qparam list * qtypespec * astmt * docstring * pragma list
    | CTypeclass of span * vis * identifier * type_parameter * Combined.combined_method_decl list * docstring
    | CInstance of span * vis * qident * typarams * qtypespec * combined_method_def list * docstring

  and combined_method_def = CMethodDef of identifier * typarams * qparam list * qtypespec * docstring * astmt
end

(** The linked representation is essentially the same as the combined
    representation, but declarations are linked to their corresponding entry in
    the environment. *)
module Linked = struct
  open Identifier
  open Common
  open Imports
  open AstDB
  open Type
  open TypeParameter
  open TypeParameters
  open Id

  type linked_module = LinkedModule of {
        mod_id: mod_id;
        name: module_name;
        kind: module_kind;
        interface_docstring: docstring;
        interface_imports: import_map;
        body_docstring: docstring;
        body_imports: import_map;
        decls: linked_definition list;
      }

  and linked_definition =
    | LConstant of decl_id * vis * identifier * ty * aexpr * docstring
    | LRecord of decl_id * type_vis * identifier * typarams * universe * typed_slot list * docstring
    | LUnion of decl_id * type_vis * identifier * typarams * universe * linked_case list * docstring
    | LFunction of decl_id * vis * identifier * typarams * value_parameter list * ty * astmt * docstring * pragma list
    | LTypeclass of decl_id * vis * identifier * type_parameter * linked_method_decl list * docstring
    | LInstance of decl_id * vis * qident * typarams * ty * linked_method_def list * docstring

  and linked_case = LCase of decl_id * identifier * typed_slot list

  and linked_method_decl = LMethodDecl of decl_id * identifier * value_parameter list * ty * docstring

  and linked_method_def = LMethodDef of ins_meth_id * identifier * value_parameter list * ty * docstring * astmt
end

(** The typed AST. *)
module Tast = struct
  open Identifier
  open Common
  open Escape
  open Type
  open TypeParameter
  open TypeParameters
  open TypeBindings
  open Span
  open Linked
  open Id

  type typed_binding =
    TypedBinding of {
        name: identifier;
        ty: ty;
        rename: identifier;
      }
  [@@deriving (show, sexp)]

  type case_ref =
    | CasePlain
    | CaseRef
  [@@deriving (show, sexp)]

  type tstmt =
    | TSkip of span
    | TLet of span * mutability * identifier * ty * tstmt
    | TDestructure of span * mutability * typed_binding list * texpr * tstmt
    | TAssign of span * typed_lvalue * texpr * bool
    | TIf of span * texpr * tstmt * tstmt
    | TCase of span * texpr * typed_when list * case_ref
    | TWhile of span * texpr * tstmt
    | TFor of span * identifier * texpr * texpr * tstmt
    | TBorrow of {
        span: span;
        original: identifier;
        rename: identifier;
        region: identifier;
        orig_type: ty;
        ref_type: ty;
        body: tstmt;
        mode: borrow_stmt_kind
      }
    | TBlock of span * tstmt * tstmt
    | TDiscarding of span * texpr
    | TReturn of span * texpr
    | TLetTmp of identifier * ty * texpr * tstmt
    | TAssignTmp of identifier * texpr
  [@@deriving show]

  and texpr =
    | TNilConstant
    | TBoolConstant of bool
    | TIntConstant of string
    | TFloatConstant of string
    | TStringConstant of escaped_string
    | TConstVar of qident * ty
    | TParamVar of identifier * ty
    | TLocalVar of identifier * ty
    | TFunVar of decl_id * ty * type_bindings
    | TTemporary of identifier * ty
    (** Represents accessing a function as a value. *)
    | TFuncall of decl_id * qident * texpr list * ty * type_bindings
    | TMethodCall of ins_meth_id * qident * typarams * texpr list * ty * type_bindings
    | TVarMethodCall of {
        source_module_name: module_name;
        typeclass_id: decl_id;
        params: value_parameter list;
        method_name: qident;
        args: texpr list;
        dispatch_ty: ty;
        rt: ty;
        bindings: type_bindings;
      }
    | TFptrCall of identifier * texpr list * ty
    | TCast of texpr * ty
    | TComparison of comparison_operator * texpr * texpr
    | TConjunction of texpr * texpr
    | TDisjunction of texpr * texpr
    | TNegation of texpr
    | TIfExpression of texpr * texpr * texpr
    | TRecordConstructor of ty * (identifier * texpr) list
    | TUnionConstructor of ty * identifier * (identifier * texpr) list
    | TPath of {
        head: texpr;
        elems: typed_path_elem list;
        ty: ty
      }
    | TRefPath of texpr * typed_ref_path_elem list * ty
    | TEmbed of ty * string * texpr list
    | TDeref of texpr
    | TSizeOf of ty
  [@@deriving show]

  and typed_when =
    TypedWhen of identifier * typed_binding list * tstmt
  [@@deriving show]

  and typed_path_elem =
    | TSlotAccessor of identifier * ty
    | TPointerSlotAccessor of identifier * ty
    | TArrayIndex of texpr * ty
  [@@deriving show]

  and typed_ref_path_elem =
    | TRefSlotAccessor of identifier * ty
  [@@deriving show]

  and typed_lvalue =
    TypedLValue of identifier * typed_path_elem list
  [@@deriving show]

  and typed_arglist =
    | TPositionalArglist of texpr list
    | TNamedArglist of (identifier * texpr) list
  [@@deriving show]

  type typed_method_decl =
    TypedMethodDecl of decl_id * identifier * value_parameter list * ty

  type typed_method_def =
    TypedMethodDef of ins_meth_id * identifier * value_parameter list * ty * tstmt

  type typed_decl =
    | TConstant of decl_id * vis * identifier * ty * texpr * docstring
    | TRecord of decl_id * type_vis * identifier * typarams * universe * typed_slot list * docstring
    | TUnion of decl_id * type_vis * identifier * typarams * universe * linked_case list * docstring
    | TFunction of decl_id * vis * identifier * typarams * value_parameter list * ty * tstmt * docstring
    | TForeignFunction of decl_id * vis * identifier * value_parameter list * ty * string * docstring
    | TTypeClass of decl_id * vis * identifier * type_parameter * typed_method_decl list * docstring
    | TInstance of decl_id * vis * qident * typarams * ty * typed_method_def list * docstring

  type typed_module = TypedModule of module_name * typed_decl list
end

(** Monomorphic typed AST. *)
module Mtast = struct
  open Id
  open Identifier
  open Common
  open Escape
  open MonoType

  type mono_binding =
    MonoBinding of {
        name: identifier;
        ty: mono_ty;
        rename: identifier;
      }

  type mono_module = MonoModule of module_name * mdecl list

  and mdecl =
    | MConstant of decl_id * identifier * mono_ty * mexpr
    | MRecord of decl_id * identifier * mono_slot list
    | MRecordMonomorph of mono_id * mono_slot list
    | MUnion of decl_id * identifier * mono_case list
    | MUnionMonomorph of mono_id * mono_case list
    | MFunction of decl_id * identifier * mvalue_parameter list * mono_ty * mstmt
    | MFunctionMonomorph of mono_id * mvalue_parameter list * mono_ty * mstmt
    | MForeignFunction of decl_id * identifier * mvalue_parameter list * mono_ty * string
    | MConcreteInstance of decl_id * qident * mono_ty  * concrete_method list
    | MMethodMonomorph of mono_id * mvalue_parameter list * mono_ty * mstmt

  and concrete_method =
    MConcreteMethod of ins_meth_id * identifier * mvalue_parameter list * mono_ty * mstmt

  and mstmt =
    | MSkip
    | MLet of identifier * mono_ty * mstmt
    | MDestructure of mono_binding list * mexpr * mstmt
    | MAssign of mtyped_lvalue * mexpr
    | MIf of mexpr * mstmt * mstmt
    | MCase of mexpr * mtyped_when list * Tast.case_ref
    | MWhile of mexpr * mstmt
    | MFor of identifier * mexpr * mexpr * mstmt
    | MBorrow of {
        original: identifier;
        rename: identifier;
        region: identifier;
        orig_type: mono_ty;
        ref_type: mono_ty;
        body: mstmt;
        mode: borrow_stmt_kind
      }
    | MBlock of mstmt * mstmt
    | MDiscarding of mexpr
    | MReturn of mexpr
    | MLetTmp of identifier * mono_ty * mexpr * mstmt
    | MAssignTmp of identifier * mexpr

  and mexpr =
    | MNilConstant
    | MBoolConstant of bool
    | MIntConstant of string
    | MFloatConstant of string
    | MStringConstant of escaped_string
    | MConstVar of qident * mono_ty
    | MParamVar of identifier * mono_ty
    | MLocalVar of identifier * mono_ty
    | MTemporary of identifier * mono_ty
    | MGenericFunVar of mono_id * mono_ty
    | MConcreteFunVar of decl_id * mono_ty
    | MConcreteFuncall of decl_id * qident * mexpr list * mono_ty
    (** Represents a call to a concrete function. *)
    | MGenericFuncall of mono_id * mexpr list * mono_ty
    (** Represents a call to a generic function. *)
    | MConcreteMethodCall of ins_meth_id * qident * mexpr list * mono_ty
    (** Represents a call to an instance method of a concrete instance. *)
    | MGenericMethodCall of ins_meth_id * mono_id * mexpr list * mono_ty
    (** Represents a call to an instance method of a generic instance. *)
    | MFptrCall of identifier * mexpr list * mono_ty
    | MCast of mexpr * mono_ty
    | MComparison of comparison_operator * mexpr * mexpr
    | MConjunction of mexpr * mexpr
    | MDisjunction of mexpr * mexpr
    | MNegation of mexpr
    | MIfExpression of mexpr * mexpr * mexpr
    | MRecordConstructor of mono_ty * (identifier * mexpr) list
    | MUnionConstructor of mono_ty * identifier * (identifier * mexpr) list
    | MPath of {
        head: mexpr;
        elems: mtyped_path_elem list;
        ty: mono_ty
      }
    | MRefPath of mexpr * mtyped_ref_path_elem list * mono_ty
    | MEmbed of mono_ty * string * mexpr list
    | MDeref of mexpr
    | MTypecast of mexpr * mono_ty
    | MSizeOf of mono_ty

  and mtyped_when =
    MTypedWhen of identifier * mono_binding list * mstmt

  and mtyped_path_elem =
    | MSlotAccessor of identifier * mono_ty
    | MPointerSlotAccessor of identifier * mono_ty
    | MArrayIndex of mexpr * mono_ty

  and mtyped_ref_path_elem =
    | MRefSlotAccessor of identifier * mono_ty

  and mtyped_lvalue =
    MTypedLValue of identifier * mtyped_path_elem list

  and mvalue_parameter =
    MValueParameter of identifier * mono_ty

  type mono_method =
    MonoMethod of {
        method_id: ins_meth_id;
        params: mvalue_parameter list;
        rt: mono_ty;
        body: mstmt;
      }
end
