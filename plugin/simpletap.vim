" vim:foldmethod=marker:fen:
scriptencoding utf-8

" Load Once {{{
if exists('g:loaded_simpletap') && g:loaded_simpletap
    finish
endif
let g:loaded_simpletap = 1
" }}}
" Saving 'cpoptions' {{{
let s:save_cpo = &cpo
set cpo&vim
" }}}


if globpath(&rtp, 'autoload/openbuf.vim') == ''
\   || globpath(&rtp, 'autoload/vice.vim') == ''
    echohl ErrorMsg
    echomsg 'openbuf.vim and/or vice.vim are not installed! (:help simpletap-requirements)'
    echohl None
    finish
endif


command!
\   -nargs=1 -complete=file
\   SimpleTapRun
\   call simpletap#run(<q-args>)

command!
\   -nargs=? -complete=dir
\   SimpleTapRunDir
\   call simpletap#run_dir(<q-args> == '' ? getcwd() : <q-args>)

command!
\   -nargs=1 -complete=file
\   SimpleTapRunFile
\   call simpletap#run_file(<q-args>)

command!
\   SimpleTapSingleTest
\   call simpletap#run_single_test()


" Restore 'cpoptions' {{{
let &cpo = s:save_cpo
" }}}
