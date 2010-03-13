" vim:foldmethod=marker:fen:
scriptencoding utf-8

" Saving 'cpoptions' {{{
let s:save_cpo = &cpo
set cpo&vim
" }}}

func! s:run() "{{{
    call simpletap#pass()
endfunc "}}}


call s:run()
Done


" Restore 'cpoptions' {{{
let &cpo = s:save_cpo
" }}}

