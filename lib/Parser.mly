(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

%{
open Identifier
open Common
open Cst
open CstUtil
open Type
open Util
open Span
%}

/* Brackets */
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LCURLY
%token RCURLY
/* Arithmetic operators */
%token PLUS
%token MINUS
%token MUL
%token DIV
/* Comparison operators */
%token EQ
%token NEQ
%token LT
%token LTE
%token GT
%token GTE
/* Logical operators */
%token AND
%token OR
%token NOT
/* Borrowing */
%token BORROW_WRITE
%token BORROW_READ
/* Keywords */
%token MODULE
%token IS
%token BODY
%token IMPORT
%token AS
%token END
%token CONSTANT
%token TYPE
%token FUNCTION
%token GENERIC
%token RECORD
%token UNION
%token CASE
%token OF
%token WHEN
%token TYPECLASS
%token INSTANCE
%token METHOD
%token IF
%token THEN
%token ELSE
%token ELSE_IF
%token LET
%token WHILE
%token FOR
%token DO
%token FROM
%token TO
%token BORROW
%token MUTABLE_BORROW
%token IN
%token RETURN
%token SKIP
%token UNIVERSE_FREE
%token UNIVERSE_LINEAR
%token UNIVERSE_TYPE
%token UNIVERSE_REGION
%token PRAGMA
%token SIZEOF
/* Symbols */
%token SEMI
%token COMMA
%token PERIOD
%token COLON
%token HYPHEN_RIGHT
%token RIGHT_ARROW
%token ASSIGN
%token DEREF
/* Strings and docstrings */
%token <string> STRING_CONSTANT
%token <string> TRIPLE_STRING_CONSTANT
/* Identifiers and constants */
%token NIL
%token TRUE
%token FALSE
%token <string> DEC_CONSTANT
%token <string> HEX_CONSTANT
%token <string> BIN_CONSTANT
%token <string> OCT_CONSTANT
%token <string> CHAR_CONSTANT
%token <string> FLOAT_CONSTANT
%token <string> IDENTIFIER
/* Specials */
%token EMBED
/* etc. */
%token EOF

/* Types */

%type <Cst.concrete_module_interface> module_int
%type <Cst.concrete_module_body> module_body
%type <Cst.concrete_decl> standalone_interface_decl
%type <Cst.cstmt> standalone_statement
%type <Cst.cexpr> standalone_expression
%type <Identifier.identifier> standalone_identifier

%start module_int
%start module_body
%start standalone_interface_decl
%start standalone_statement
%start standalone_expression
%start standalone_identifier

%%

/* Entrypoints */

standalone_interface_decl:
  | interface_decl EOF { $1 }
  ;

standalone_statement:
  | statement EOF { $1 }
  ;

standalone_expression:
  | expression EOF { $1 }
  ;

standalone_identifier:
  | identifier EOF { $1 }
  ;

/* Module interfaces and bodies */

module_int:
  | doc=docstringopt imports=import_stmt* MODULE
    name=module_name IS decls=interface_decl*
    END MODULE PERIOD EOF
    { ConcreteModuleInterface (name, doc, imports, decls) }
  ;

module_body:
  | doc=docstringopt imports=import_stmt* MODULE BODY
    name=module_name IS pragmas=pragma* decls=body_decl*
    END MODULE BODY PERIOD EOF
    { make_module_body name imports pragmas decls doc }
  ;

/* Imports */

import_stmt:
  | IMPORT name=module_name LPAREN
    symbols=separated_list(COMMA, imported_symbol)
    RPAREN SEMI
    { ConcreteImportList (name, symbols) }
  ;

imported_symbol:
  | name=identifier { ConcreteImport (name, None) }
  | name=identifier AS nickname=identifier { ConcreteImport (name, Some nickname) }
  ;

/* Declarations */

interface_decl:
  | constant_decl { $1 }
  | type_decl { $1 }
  | record { ConcreteRecordDecl $1 }
  | union { ConcreteUnionDecl $1 }
  | function_decl { $1 }
  | typeclass { ConcreteTypeClassDecl $1 }
  | instance_decl { $1 }
  ;

constant_decl:
  | docstringopt CONSTANT identifier COLON typespec SEMI { ConcreteConstantDecl ($3, $5, $1) }
  ;

type_decl:
  | doc=docstringopt TYPE name=identifier
    typarams=type_parameter_list COLON universe=universe
    SEMI { ConcreteOpaqueTypeDecl (name, typarams, universe, doc) }
  ;

record:
  | doc=docstringopt RECORD name=identifier
    typarams=type_parameter_list COLON universe=universe
    IS slots=slot*
    END SEMI { ConcreteRecord (name, typarams, universe, slots, doc) }
  ;

slot:
  | docstringopt name=identifier COLON ty=typespec SEMI { ConcreteSlot (name, ty) }
  ;

union:
  | doc=docstringopt UNION name=identifier
    typarams=type_parameter_list COLON universe=universe
    IS cases=case*
    END SEMI { ConcreteUnion (name, typarams, universe, cases, doc) }
  ;

case:
  | docstringopt CASE name=identifier SEMI { ConcreteCase (name, []) }
  | docstringopt CASE name=identifier IS slots=slot* { ConcreteCase (name, slots) }
  ;

function_decl:
  | doc=docstringopt typarams=generic_segment
    FUNCTION name=identifier LPAREN params=parameter_list RPAREN
    COLON rt=typespec SEMI
    { ConcreteFunctionDecl (name, typarams, params, rt, doc) }
  ;

generic_segment:
  | option(generic_segment_inner) { Option.value $1 ~default:[] }
  ;

generic_segment_inner:
  | GENERIC type_parameter_list { $2 }
  ;

typeclass:
  | doc=docstringopt TYPECLASS name=identifier LPAREN typaram=type_parameter RPAREN
    IS methods=method_decl* END SEMI
    { ConcreteTypeClass (name, typaram, methods, doc) }
  ;

method_decl:
  | doc=docstringopt typarams=generic_segment
    METHOD name=identifier
    LPAREN params=parameter_list RPAREN COLON rt=typespec SEMI
   { ConcreteMethodDecl (name, typarams, params, rt, doc) }
  ;

instance_decl:
  | doc=docstringopt typarams=generic_segment
    INSTANCE name=identifier LPAREN arg=typespec RPAREN SEMI
    { ConcreteInstanceDecl (name, typarams, arg, doc) }
  ;

body_decl:
  | constant_def { $1 }
  | record { ConcreteRecordDef $1 }
  | union { ConcreteUnionDef $1 }
  | function_def { $1 }
  | typeclass { ConcreteTypeClassDef $1 }
  | instance_def { $1 }
  ;

constant_def:
  | doc=docstringopt CONSTANT name=identifier COLON ty=typespec
    ASSIGN v=expression SEMI { ConcreteConstantDef (name, ty, v, doc) }
  ;

function_def:
  | doc=docstringopt typarams=generic_segment
    FUNCTION name=identifier LPAREN params=parameter_list RPAREN
    COLON rt=typespec IS pragmas=pragma* body=block? END SEMI
    { ConcreteFunctionDef (name, typarams, params, rt, Option.value body ~default:(CBlock (from_loc $loc, [])), doc, pragmas) }
  ;

pragma:
  | PRAGMA name=identifier SEMI
    { make_pragma name (ConcretePositionalArgs []) }
  | PRAGMA name=identifier args=argument_list SEMI
    { make_pragma name args }
  ;

instance_def:
  | doc=docstringopt typarams=generic_segment
    INSTANCE name=identifier LPAREN arg=typespec RPAREN IS
    methods=method_def*
    END SEMI
    { ConcreteInstanceDef (ConcreteInstance (name, typarams, arg, methods, doc)) }
  ;

method_def:
  | doc=docstringopt typarams=generic_segment
    METHOD name=identifier
    LPAREN params=parameter_list RPAREN COLON rt=typespec
    IS body=block END SEMI
   { ConcreteMethodDef (name, typarams, params, rt, body, doc) }
  ;

/* Statements */

statement:
  | if_statement { $1 }
  | let_stmt { $1 }
  | lvalue ASSIGN expression SEMI { CAssign (from_loc $loc, $1, $3) }
  | expression SEMI { CDiscarding (from_loc $loc, $1) }
  | CASE expression OF when_stmt* END CASE SEMI { CCase (from_loc $loc, $2, $4) }
  | WHILE expression DO block END WHILE SEMI { CWhile (from_loc $loc, $2, $4) }
  | FOR identifier FROM expression TO expression DO block END FOR SEMI { CFor (from_loc $loc, $2, $4, $6, $8) }
  | borrow_stmt { $1 }
  | RETURN expression SEMI { CReturn (from_loc $loc, $2) }
  | SKIP SEMI { CSkip (from_loc $loc) }
  ;

if_statement:
  | IF expression THEN block else_clause { CIf (from_loc $loc, $2, $4, $5) }
  ;

else_clause:
  | ELSE_IF expression THEN block else_clause { CIf (from_loc $loc, $2, $4, $5) }
  | ELSE block END IF SEMI { $2 }
  | END IF SEMI { CSkip (from_loc $loc) }
  ;

let_stmt:
  | let_destructure { $1 }
  | let_simple { $1 }
  ;

let_simple:
  | LET identifier COLON typespec ASSIGN expression SEMI { CLet (from_loc $loc, $2, $4, $6) }
  ;

let_destructure:
  | LET LCURLY b=binding_list RCURLY ASSIGN e=expression SEMI { CDestructure (from_loc $loc, b, e) }
  ;

binding_list:
  | separated_list(COMMA, binding) { $1 }
  ;

binding:
  | name=identifier COLON ty=typespec { ConcreteBinding { name = name; ty = ty; rename = name } }
  | name=identifier AS rename=identifier COLON ty=typespec { ConcreteBinding { name = name; ty = ty; rename = rename } }
  ;

lvalue:
  | n=identifier elems=path_rest* { ConcreteLValue (n, elems) }

when_stmt:
  | WHEN identifier LPAREN binding_list RPAREN DO block { ConcreteWhen ($2, $4, $7) }
  | WHEN identifier DO block { ConcreteWhen ($2, [], $4) }
  ;

borrow_stmt:
  | read_borrow_stmt { $1 }
  | mutable_borrow_stmt { $1 }
  ;

read_borrow_stmt:
  | BORROW o=identifier AS n=identifier IN r=identifier DO b=block END SEMI { CBorrow { span=from_loc $loc; original=o; rename=n; region=r; body=b; mode=ReadBorrow } }
  ;

mutable_borrow_stmt:
  | MUTABLE_BORROW o=identifier AS n=identifier IN r=identifier DO b=block END SEMI { CBorrow { span=from_loc $loc; original=o; rename=n; region=r; body=b; mode=WriteBorrow } }
  ;

block:
  | blocklist { CBlock (from_loc $loc, $1) }

blocklist:
  | statement blocklist { $1 :: $2 }
  | statement { [$1] }
  ;

/* Expressions */

expression:
  | atomic_expression { $1 }
  | compound_expression { $1 }
  ;

atomic_expression:
  | NIL { CNilConstant (from_loc $loc) }
  | TRUE { CBoolConstant (from_loc $loc, true) }
  | FALSE { CBoolConstant (from_loc $loc, false) }
  | int_constant { $1 }
  | float_constant { $1 }
  | string_constant { $1 }
  | path { $1 }
  | variable { $1 }
  | funcall { $1 }
  | parenthesized_expr { $1 }
  | intrinsic { $1 }
  | SIZEOF LPAREN typespec RPAREN { CSizeOf (from_loc $loc, $3) }
  | BORROW_READ identifier { CBorrowExpr (from_loc $loc, ReadBorrow, $2) }
  | BORROW_WRITE identifier { CBorrowExpr (from_loc $loc, WriteBorrow, $2) }
  | DEREF atomic_expression { CDeref (from_loc $loc, $2) }
  ;

int_constant:
  | DEC_CONSTANT { CIntConstant (from_loc $loc, remove_char $1 '\'') }
  | HEX_CONSTANT { CIntConstant (from_loc $loc, string_of_int (parse_hex (remove_leading (remove_char $1 '\'') 2))) }
  | BIN_CONSTANT { CIntConstant (from_loc $loc, string_of_int (parse_bin (remove_leading (remove_char $1 '\'') 2))) }
  | OCT_CONSTANT { CIntConstant (from_loc $loc, string_of_int (parse_oct (remove_leading (remove_char $1 '\'') 2))) }
  | CHAR_CONSTANT { CIntConstant (from_loc $loc, string_of_int (parse_ascii_char (remove_char $1 '\''))) }
  ;

float_constant:
  | FLOAT_CONSTANT { CFloatConstant (from_loc $loc, remove_char $1 '\'') }
  ;

string_constant:
  | STRING_CONSTANT { CStringConstant (from_loc $loc, $1) }
  | TRIPLE_STRING_CONSTANT { CStringConstant (from_loc $loc, process_triple_string $1) }
  ;

variable:
  | identifier { CVariable (from_loc $loc, $1) }
  ;

funcall:
  | identifier argument_list { CFuncall (from_loc $loc, $1, $2) }
  ;

parenthesized_expr:
  | LPAREN expression RPAREN { $2 }
  ;

argument_list:
  | LPAREN positional_arglist RPAREN { ConcretePositionalArgs $2 }
  | LPAREN named_arglist RPAREN { ConcreteNamedArgs $2 }
  | LPAREN RPAREN { ConcretePositionalArgs [] }
  ;

positional_arglist:
  | separated_list(COMMA, expression) { $1 }
  ;

named_arglist:
  | separated_list(COMMA, named_arg) { $1 }
  ;

named_arg:
  | identifier RIGHT_ARROW expression { ($1, $3) }

intrinsic:
  | EMBED LPAREN ty=typespec COMMA exp=STRING_CONSTANT COMMA args=separated_list(COMMA, expression) RPAREN
    { CEmbed (from_loc $loc, ty, exp, args) }
  | EMBED LPAREN ty=typespec COMMA exp=STRING_CONSTANT RPAREN
    { CEmbed (from_loc $loc, ty, exp, []) }
  ;

compound_expression:
  | atomic_expression comp_op atomic_expression { CComparison (from_loc $loc, $2, $1, $3) }
  | atomic_expression AND atomic_expression { CConjunction (from_loc $loc, $1, $3) }
  | atomic_expression OR atomic_expression { CDisjunction (from_loc $loc, $1, $3) }
  | NOT atomic_expression { CNegation (from_loc $loc, $2) }
  | arith_expr { $1 }
  | IF expression THEN expression ELSE expression { CIfExpression (from_loc $loc, $2, $4, $6) }
  | atomic_expression COLON typespec { CTypecast (from_loc $loc, $1, $3) }
  ;

path:
  | initial=atomic_expression elems=path_rest+ { CPath (from_loc $loc, initial, elems) }
  ;

path_rest:
  | slot_accessor { $1 }
  | pointer_slot_accessor { $1 }
  | array_index { $1 }
  ;

slot_accessor:
  | PERIOD identifier { CSlotAccessor $2 }
  ;

pointer_slot_accessor:
  | HYPHEN_RIGHT identifier { CPointerSlotAccessor $2 }
  ;

array_index:
  | LBRACKET expression RBRACKET { CArrayIndex $2 }
  ;

comp_op:
  | EQ { Equal }
  | NEQ { NotEqual }
  | LT { LessThan }
  | LTE { LessThanOrEqual }
  | GT { GreaterThan }
  | GTE { GreaterThanOrEqual }
  ;

arith_expr:
  | atomic_expression PLUS atomic_expression  { CArith (from_loc $loc, Add, $1, $3) }
  | atomic_expression MINUS atomic_expression { CArith (from_loc $loc, Subtract, $1, $3) }
  | atomic_expression MUL atomic_expression   { CArith (from_loc $loc, Multiply, $1, $3) }
  | atomic_expression DIV atomic_expression   { CArith (from_loc $loc, Divide, $1, $3) }
  | MINUS atomic_expression                   { CArith (from_loc $loc, Subtract, CIntConstant (from_loc $loc, "0"), $2) }
  ;

/* Common */

identifier:
  | IDENTIFIER { make_ident $1 }
  ;

module_name:
  | module_name_inner { make_mod_name (String.concat "." $1) }
  ;

module_name_inner:
  | IDENTIFIER PERIOD module_name_inner { $1 :: $3 }
  | IDENTIFIER { [$1] }
  ;

typespec:
  | identifier LBRACKET separated_list(COMMA, typespec) RBRACKET { TypeSpecifier ($1, $3) }
  | identifier { TypeSpecifier ($1, []) }
  | BORROW_READ LBRACKET typespec COMMA typespec RBRACKET { ConcreteReadRef ($3, $5) }
  | BORROW_WRITE LBRACKET typespec COMMA typespec RBRACKET { ConcreteWriteRef ($3, $5) }
  ;

universe:
  | UNIVERSE_FREE { FreeUniverse }
  | UNIVERSE_LINEAR { LinearUniverse }
  | UNIVERSE_TYPE { TypeUniverse }
  | UNIVERSE_REGION { RegionUniverse }
  ;

parameter_list:
  | separated_list(COMMA, parameter) { $1 }
  ;

parameter:
  | identifier COLON typespec { ConcreteParam ($1, $3) }
  ;

type_parameter_list:
  | option(type_parameter_list_inner) { Option.value $1 ~default:[] }
  ;

type_parameter_list_inner:
  | LBRACKET separated_list(COMMA, type_parameter) RBRACKET { $2 }
  ;

type_parameter:
  | identifier COLON universe { ConcreteTypeParam ($1, $3, []) }
  | identifier COLON universe LPAREN separated_list(COMMA, identifier) RPAREN { ConcreteTypeParam ($1, $3, $5) }
  ;

docstring:
  | TRIPLE_STRING_CONSTANT { Docstring (process_triple_string $1) }
  ;

docstringopt:
  | option(docstring) { Option.value $1 ~default:(Docstring "") }
  ;
