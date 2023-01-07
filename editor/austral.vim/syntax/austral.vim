" Vim syntax file
" Language:    austral

" quit when a syntax file was already loaded
if exists("b:current_syntax")
    finish
endif
let s:keepcpo = &cpo
set cpo&vim


let b:current_syntax = "austral"

syntax	case ignore

" Section: Keyword {{{1
"
syntax keyword australKeyword and or not module is body import as end constant type function generic record union case of when typeclass instance method if then else let while for do from to borrow borrow! in return skip Free Linear Type Region pragma nil true false

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
syntax keyword australOperator =>
syntax match   australOperator "\.\."
syntax match   australOperator "="

" Section: := {{{1
"
syntax match australAssignment		":="

" Section: Numbers, including floating point. {{{1
"
syntax match   australNumber		"\<\d[0-9_]*\(\.\d[0-9_]*\)\=\([Ee][+-]\=\d[0-9_]*\)\=\>"
syntax match   australNumber		"\<\d\d\=#\x[0-9A-Fa-f_]*\(\.\x[0-9A-Fa-f_]*\)\=#\([Ee][+-]\=\d[0-9_]*\)\="

" Section: Boolean Constants {{{1
" Boolean Constants.
syntax keyword australBoolean	true false


" Section: end {{{1
" Unless special ("end loop", "end if", etc.), "end" marks the end of a
" begin, package, task etc. Assigning it to australEnd.
syntax match    australEnd	/\<end\>/

syntax keyword  australPreproc		 pragma

syntax keyword  australRepeat	 exit for while
syntax match    australRepeat		   "\<end\s\>"

" Section: Handle Austral's record keywords. {{{1
"
syntax match australStructure   "\<record\>"	contains=australRecord
syntax match australStructure   "\<end\s\+record\>"	contains=australRecord
syntax match australKeyword	"\<record;"me=e-1

" Section: Conditionals {{{1
"
syntax match    australConditional  "\<then\>"
syntax match    australConditional	"\<else\>"
syntax match    australConditional	"\<end\s\+if\>"
syntax match    australConditional	"\<end\s\+case\>"
syntax keyword  australConditional	if case


" Section: begin keywords {{{1
"
syntax match    australBegin	"\<function\>" contains=australFunction
syntax match    australBegin	"\<module\>" contains=australModule


" Section: String and character constants. {{{1
"
syntax region  australString	contains=@Spell start=+"+ skip=+""+ end=+"+ 
syntax match   australCharacter "'.'"

" Section: Todo (only highlighted in comments) {{{1
"
syntax keyword australTodo contained TODO FIXME XXX NOTE

" Section: Comments. {{{1
"
syntax region  australComment 
    \ oneline 
    \ contains=australTodo,@Spell
    \ start="--" 
    \ end="$"

" Section: syntax folding {{{1
"
"	Syntax folding is very tricky - for now I still suggest to use
"	indent folding
"
if exists("g:austral_folding") && g:austral_folding[0] == 's'
   if stridx (g:austral_folding, 'p') >= 0
      syntax region australModule
         \ start="\<module\>\s*\z(\k*\)"
         \ end="end\s\+\z1\s*;"
         \ keepend extend transparent fold contains=ALL
   endif
   if stridx (g:austral_folding, 'f') >= 0
      syntax region australFunction
         \ start="\<function\>\s*\z(\k*\)"
         \ end="end\s\+\z1\s*;"
         \ keepend extend transparent fold contains=ALL
   endif
   if stridx (g:austral_folding, 'f') >= 0
      syntax region australRecord
         \ start="\<is\s\+record\>"
         \ end="\<end\s\+record\>"
         \ keepend extend transparent fold contains=ALL
   endif
endif


" Section: The default methods for highlighting. Can be overridden later. {{{1
"
highlight def link australComment	    Comment
highlight def link australConditional       Conditional
highlight def link australPreproc           PreProc
highlight def link australKeyword	    Keyword
highlight def link australNumber	    Number
highlight def link australOperator	    Operator
highlight def link australString	    String
highlight def link australStructure	    Structure
highlight def link australTodo		    Todo
highlight def link australTypedef	    Typedef
highlight def link australBoolean	    Boolean
highlight def link australAssignment        Special

" Subsection: Begin, End {{{2
"
if exists ("austral_begin_preproc")
   " This is the old default display:
   highlight def link australBegin   PreProc
   highlight def link australEnd     PreProc
else
   " This is the new default display:
   highlight def link australBegin   Keyword
   highlight def link australEnd     Keyword
endif


let &cpo = s:keepcpo
unlet s:keepcpo

finish
