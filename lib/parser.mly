%token PLUS
%token MINUS
%token MUL
%token DIV
%token
%token MODULE
%token IS
%token END
%token BODY
%token IMPORT
%token LPAREN
%token RPAREN
%token SEMI
%token COMMA
%token AS
%token CONSTANT
%token COLON
%token TYPE
%token LBRACKET
%token RBRACKET
%token FUNCTION
%token GENERIC
%token ASSIGN
%token RECORD
%token UNION
%token CASE
%token OF
%token WHEN
%token INTERFACE
%token IMPLEMENTATION
%token METHOD
%token DOCSTRING_MARKER
%token IF
%token THEN
%token ELSE
%token LET
%token WHILE
%token DO
%token FOR
%token FROM
%token TO
%token RETURN
%token BEGIN
%token BORROW
%token IN
%token SKIP_TOKEN
%token NIL
%token TRUE
%token FALSE
%token PERIOD
%token DQUOTE
%token AND
%token OR
%token NOT
%token RIGHT_ARROW
%token UNDERSCORE
%token PRAGMA

%token UNIVERSE_FREE
%token UNIVERSE_LINEAR
%token UNIVERSE_TYPE
%token UNIVERSE_REGION

%token EQUAL
%token NOT_EQUAL
%token LESS_THAN
%token LESS_THAN_OR_EQUAL
%token GREATER_THAN
%token GREATER_THAN_OR_EQUAL

%token EOF

%%

program:
  | module_interface
  | module_body
