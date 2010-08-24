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

if !exists('g:simpletap_run_command_complete')
    let g:simpletap_run_command_complete = ['dir']
endif
if g:simpletap_run_command != ''
    execute
    \   'command!'
    \   '-nargs=? -complete='.join(g:simpletap_run_command_complete, ',')
    \   'SimpleTapRun'
    \   'call simpletap#run(<f-args>)'
endif

" Restore 'cpoptions' {{{
let &cpo = s:save_cpo
" }}}
