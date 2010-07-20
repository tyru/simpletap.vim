" vim:foldmethod=marker:fen:
scriptencoding utf-8

" Saving 'cpoptions' {{{
let s:save_cpo = &cpo
set cpo&vim
" }}}

func! s:run() "{{{
    Skip

    call simpletap#stdout_like(
    \   "call simpletap#ok(0)",
    \   'NOT ok'
    \)
    call simpletap#stdout_unlike(
    \   "call simpletap#ok(1)",
    \   'NOT ok'
    \)
endfunc "}}}


call s:run()
Done


" Restore 'cpoptions' {{{
let &cpo = s:save_cpo
" }}}
