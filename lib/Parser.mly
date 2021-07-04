%{
open Identifier
open Common
open Cst
open Type
%}

/* Brackets */
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
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
/* Keywords */
%token MODULE
%token IS
%token BODY
%token IMPORT
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
%token LET
%token WHILE
%token FOR
%token DO
%token FROM
%token TO
%token RETURN
%token SKIP
%token UNIVERSE_FREE
%token UNIVERSE_LINEAR
%token UNIVERSE_TYPE
%token UNIVERSE_REGION
/* Symbols */
%token SEMI
%token COMMA
%token COLON
%token RIGHT_ARROW
%token ASSIGN
/* Strings and docstrings */
%token DOCSTRING_MARKER
/* Identifiers and constants */
%token NIL
%token TRUE
%token FALSE
%token <string> INT_CONSTANT
%token <string> FLOAT_CONSTANT
%token <string> IDENTIFIER
/* etc. */
%token EOF

/* Types */

%type <Cst.concrete_decl> standalone_declaration
%type <Cst.cstmt> standalone_statement
%type <Cst.cexpr> standalone_expression
%type <Identifier.identifier> standalone_identifier

%start standalone_declaration
%start standalone_statement
%start standalone_expression
%start standalone_identifier

%%

/* Entrypoints */

standalone_declaration:
  | declaration EOF { $1 }
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

/* Declarations */

declaration:
  | constant_decl { $1 }
  | type_decl { $1 }
  | type_definition { $1 }
  | record_definition { $1 }
  | union_definition { $1 }
  | function_decl { $1 }
  | typeclass_def { $1 }
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

type_definition:
  | doc=docstringopt TYPE name=identifier
    typarams=type_parameter_list COLON universe=universe
    IS def=typespec
    SEMI { ConcreteTypeAliasDecl (ConcreteTypeAlias (name, typarams, universe, def, doc)) }
  ;

record_definition:
  | doc=docstringopt RECORD name=identifier
    typarams=type_parameter_list COLON universe=universe
    IS slots=slot*
    END RECORD SEMI { ConcreteRecordDecl (ConcreteRecord (name, typarams, universe, slots, doc)) }
  ;

slot:
  | name=identifier COLON ty=typespec SEMI { ConcreteSlot (name, ty) }
  ;

union_definition:
  | doc=docstringopt UNION name=identifier
    typarams=type_parameter_list COLON universe=universe
    IS cases=case*
    END RECORD SEMI { ConcreteUnionDecl (ConcreteUnion (name, typarams, universe, cases, doc)) }
  ;

case:
  | CASE name=identifier SEMI { ConcreteCase (name, []) }
  | CASE name=identifier IS slots=slot* { ConcreteCase (name, slots) }
  ;

function_decl:
  | doc=docstringopt FUNCTION name=identifier LPAREN params=parameter_list RPAREN
    COLON rt=typespec SEMI
    { ConcreteFunctionDecl (name, [], params, rt, doc) }
  | doc=docstringopt GENERIC typarams=type_parameter_list
    FUNCTION name=identifier LPAREN params=parameter_list RPAREN
    COLON rt=typespec SEMI
    { ConcreteFunctionDecl (name, typarams, params, rt, doc) }
  ;

typeclass_def:
  | doc=docstringopt TYPECLASS name=identifier LPAREN typaram=type_parameter
    IS methods=method_decl* END TYPECLASS SEMI
    { ConcreteTypeClassDecl (ConcreteTypeClass (name, typaram, methods, doc)) }
  ;

method_decl:
  | doc=docstringopt METHOD name=identifier
    LPAREN params=parameter_list RPAREN COLON rt=typespec SEMI
   { ConcreteMethodDecl (name, params, rt, doc) }
  ;

instance_decl:
  | doc=docstringopt GENERIC typarams=type_parameter_list
    INSTANCE name=identifier LPAREN arg=typespec RPAREN SEMI
    { ConcreteInstanceDecl (name, typarams, arg, doc) }
  | doc=docstringopt INSTANCE name=identifier LPAREN arg=typespec RPAREN SEMI
    { ConcreteInstanceDecl (name, [], arg, doc) }
  ;

/* Statements */

statement:
  | if_statement { $1 }
  | LET identifier COLON typespec ASSIGN expression SEMI { CLet ($2, $4, $6) }
  | identifier ASSIGN expression SEMI { CAssign ($1, $3) }
  | expression SEMI { CDiscarding $1 }
  | CASE expression OF when_stmt* END CASE SEMI { CCase ($2, $4) }
  | WHILE expression DO block END WHILE SEMI { CWhile ($2, $4) }
  | FOR identifier FROM expression TO expression DO block END FOR SEMI { CFor ($2, $4, $6, $8) }
  | RETURN expression SEMI { CReturn $2 }
  | SKIP SEMI { CSkip }
  ;

if_statement:
  | IF expression THEN block else_clause { CIf ($2, $4, $5) }
  ;

else_clause:
  | ELSE IF expression THEN block else_clause { CIf ($3, $5, $6) }
  | ELSE block END IF SEMI { $2 }
  | END IF SEMI { CSkip }

when_stmt:
  | WHEN identifier LPAREN parameter_list RPAREN DO block SEMI { ConcreteWhen ($2, $4, $7) }
  | WHEN identifier DO block SEMI { ConcreteWhen ($2, [], $4) }
  ;

block:
  | blocklist { CBlock $1 }

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
  | NIL { CNilConstant }
  | TRUE { CBoolConstant true }
  | FALSE { CBoolConstant false }
  | int_constant { $1 }
  | float_constant { $1 }
  | variable { $1 }
  | funcall { $1 }
  | parenthesized_expr { $1 }
  ;

int_constant:
  | INT_CONSTANT { CIntConstant $1 }
  ;

float_constant:
  | FLOAT_CONSTANT { CFloatConstant $1 }
  ;

variable:
  | identifier { CVariable $1 }
  ;

funcall:
  | identifier argument_list { CFuncall ($1, $2) }
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

compound_expression:
  | atomic_expression comp_op atomic_expression { CComparison ($2, $1, $3) }
  | atomic_expression AND atomic_expression { CConjunction ($1, $3) }
  | atomic_expression OR atomic_expression { CDisjunction ($1, $3) }
  | NOT atomic_expression { CNegation $2 }
  | arith_expr { $1 }
  | IF expression THEN expression ELSE expression { CIfExpression ($2, $4, $6) }
  ;

comp_op:
  | EQ { Equal }
  | NEQ { NotEqual }
  | LT { LessThan }
  | LTE { LessThanOrEqual }
  | GT { GreaterThanOrEqual }
  | GTE { GreaterThanOrEqual }

arith_expr:
  | term PLUS arith_expr { CArith (Add, $1, $3) }
  | term MINUS arith_expr { CArith (Subtract, $1, $3) }
  | term { $1 }
  ;

term:
  | factor MUL term { CArith (Multiply, $1, $3) }
  | factor DIV term { CArith (Divide, $1, $3) }
  | factor  { $1 }
  | MINUS factor  { CArith (Subtract, CIntConstant "0", $2) }
  ;

factor:
  | int_constant { $1 }
  | float_constant { $1 }
  | variable { $1 }
  | funcall { $1 }
  | parenthesized_expr { $1 }
  ;


/* Common */

identifier:
  | IDENTIFIER { make_ident $1 }
  ;

typespec:
  | identifier LBRACKET separated_list(COMMA, typespec) RBRACKET { TypeSpecifier ($1, $3) }
  | identifier { TypeSpecifier ($1, []) }
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
  | identifier COLON universe { TypeParameter ($1, $3) }

docstring:
  | DOCSTRING_MARKER { Docstring "" }
  ;

docstringopt:
  | option(docstring) { Option.value $1 ~default:(Docstring "") }
