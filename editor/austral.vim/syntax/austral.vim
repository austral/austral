" Language:    austral

" quit when a syntax file was already loaded
if exists("b:current_syntax")
	finish
endif
let s:keepcpo = &cpo
set cpo&vim


let b:current_syntax = "austral"


" Section: Operatoren {{{1
"
syntax keyword australOperator not
syntax match   australOperator "\<and\>"
syntax match   australOperator "\<and\s\+then\>"
syntax match   australOperator "\<or\>"
syntax match   australOperator "\<or\s\+else\>"
syntax match   australOperator "[-+*/<>&]"
syntax keyword australOperator **
syntax match   australOperator "[/<>]="
syntax match   australOperator "="



" Section: Keywords {{{1
"
syntax keyword australKeyword return let nil borrow skip generic instance method

" Section: Specials {{{1
"
syntax match australSpecial	":="
syntax match australSpecial "=>"
syntax match        australSpecial       "[;().,]"  


" Section: Function 
" 
" function = "function" identifier "(" [ identifier { "," identifier } ] ")" [ ":" type ] [ "is" ] { statement } "end"
"
"syntax region australFunction start="\<function\>\s*\z(\k*\)" end="end\s*;"
"			\ keepend extend transparent fold contains=ALL



" Section: Numbers, including floating point. {{{1
"
" digit = [0-9]
" digits = digit, { digit | "_" };
" integer constant = ["+", "-"], digits;
" float constant = digits, ".", digits, ["e", ["+", "-"], integer constant];
"
syntax match australNumber		"\v[+-]?[0-9_]+" " integer constant
syntax match australFloat		"\v[+-]?[0-9_]+\.[0-9_]+([eE][+-]?[0-9_]+)?" " float constant


" Section: Boolean Constants {{{1
" 
syntax keyword australBoolean	true false

" Section: Preprocessor {{{1
"
" https://austral-lang.org/spec/spec.html#modules 
"
syntax keyword  australPreproc		 pragma
syntax keyword  australPreproc		 import

" Section: Structures {{{1
"
" https://austral-lang.org/spec/spec.html#record-definition
"
"         \ start="\<record\>"
"         \ end="\<end\>"
"         \ keepend extend transparent fold contains=ALL
syntax match australStructure "\<[A-Z][a-zA-Z0-9_]*\>"


" Section: Typedef
"
" https://austral-lang.org/spec/spec.html#union-definition
" https://austral-lang.org/spec/spec.html#typeclass-definition
" 
"syntax match australTypedef   "\<typeclass\>"
"syntax match australTypedef   "\<union\>"


" Section: Labels {{{1
"
" case = "case" expression "of" { "when" expression "do" statement } "end case"
"
" https://austral-lang.org/spec/spec.html#stmt-case
"
syntax match australLabel		"\<end\s\+case\>"
syntax keyword australLabel		case when do of

" Section: String and character constants. {{{1
"
" string constant = '"', { any character - '"' | '\"' }, '"'; 
"
syntax region australString start=+"+ skip=+\\"+ end=+"+ contains=@Spell


" Section: Todo (only highlighted in comments) {{{1
"
syntax keyword australTodo contained TODO FIXME XXX NOTE


" Section: Comments. {{{1
"
" Austral oneline comments are start with "--" and end with a newline.
" Multiline comments are enclosed in "```\n", "\n```".
"
" comment = "-- ", {any character}, "\n";
" docstring = "```\n", { any character - "```" } ,"\n```";
"
" Single line comments
syntax region  australComment 
			\ oneline 
			\ contains=australTodo,@Spell
			\ start="--" 
			\ end="$" 
" Multiline comments
syntax region  australComment start=+```+ end=+```+ contains=australTodo,@Spell keepend



" Section: Austral Regions {{{1
"
syntax match australRegion "\<module\>"
syntax match australRegion "\<module\s\+body\>"
syntax match australRegion	"\<function\>"
syntax match australRegion  /\<end\>/
syntax match australRegion   "\<typeclass\>"
syntax match australRegion   "\<is\>"
syntax match australRegion   "\<record\>"
syntax match australRegion   "\<union\>"


" Section: Conditionals {{{1
"
" https://austral-lang.org/spec/spec.html#stmt-if
"
syntax match australConditional		"\<if\>"
syntax match australConditional		"\<then\>"
syntax match australConditional		"\<else\>"
syntax match    australConditional	"\<else\s\+if>"
syntax match    australConditional	"\<end\s\+if\>"
syntax match    australConditional	"\<end\s\+case\>"


" Section: Repeat {{{1
" 
" while = "while", expression, "do", { statement }, "end while";
" for = "for", identifier, "from", expression, "to" expression, "do", { statement }, "end for";
"
" https://austral-lang.org/spec/spec.html#stmt-while
" https://austral-lang.org/spec/spec.html#stmt-for
"
syntax match australRepeat "\<while\>"
syntax match australRepeat "\<end\s\+while\>"
syntax match australRepeat "\<for\>"
syntax match australRepeat "\<end\s\+for\>"


" Section: The default methods for highlighting. Can be overridden later. {{{1
"
highlight def link australComment	    Comment
highlight def link australString	    String
highlight def link australNumber	    Number
highlight def link australFloat		    Float
highlight def link australBoolean	    Boolean
highlight def link australRegion	    Function
highlight def link australConditional       Conditional
highlight def link australRepeat            Repeat
highlight def link australLabel             Label
highlight def link australOperator          Operator
highlight def link australKeyword	    Keyword
highlight def link australPreproc           PreProc
highlight def link australStructure	    Structure
highlight def link australTodo		    Todo
highlight def link australTypedef	    Typedef
highlight def link australSpecial	    Special


let &cpo = s:keepcpo
unlet s:keepcpo

finish
