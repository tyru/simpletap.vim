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

" Global Variables {{{
if !exists('g:simpletap_diag_echohl')
    let g:simpletap_diag_echohl = 'Comment'
endif
if !exists('g:simpletap_error_echohl')
    let g:simpletap_error_echohl = 'WarningMsg'
endif
if !exists('g:simpletap_done_echohl')
    let g:simpletap_done_echohl = 'Underlined'
endif
if !exists('g:simpletap_begin_echohl')
    let g:simpletap_begin_echohl = 'None'
endif
if !exists('g:simpletap_failed_echohl')
    let g:simpletap_failed_echohl = 'WarningMsg'
endif
" }}}


" Restore 'cpoptions' {{{
let &cpo = s:save_cpo
" }}}
