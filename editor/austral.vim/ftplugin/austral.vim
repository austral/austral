" Only do this when not done yet for this buffer
if exists("b:did_ftplugin")
  finish
endif

let b:did_ftplugin = 1

let s:cpoptions = &cpoptions
set cpoptions-=C


setlocal expandtab
setlocal tabstop=4
setlocal softtabstop=4
setlocal shiftwidth=4


" Section: Comments  {{{1
"
setlocal comments=O:--,:--\ \ 
setlocal commentstring=--\ \ %s


" 1}}}
" Reset cpoptions
let &cpoptions = s:cpoptions
unlet s:cpoptions

finish " 1}}}
