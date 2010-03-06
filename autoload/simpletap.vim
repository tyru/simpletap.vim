" vim:foldmethod=marker:fen:
scriptencoding utf-8

" NEW BSD LICENSE {{{
"   Copyright (c) 2009, tyru
"   All rights reserved.
"
"   Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
"
"       * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
"       * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
"       * Neither the name of the tyru nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
"
"   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
" }}}

" Saving 'cpoptions' {{{
let s:save_cpo = &cpo
set cpo&vim
" }}}

" TODO
" - OO interface
" - Capture output of Ex command.
" - Add command macros.


" Variables {{{

let simpletap#ok_ok_str = 'ok'
let simpletap#ok_not_ok_str = 'NOT ok'
let simpletap#is_ok_str = 'ok'
let simpletap#is_not_ok_str = 'got: %s, expected: %s'
let simpletap#test_dir = '.'

let s:current_test_num = 1

" }}}

" Functions {{{
func! s:warn(...) "{{{
    echohl WarningMsg
    echomsg join(a:000, ' ')
    echohl None
endfunc "}}}

func! s:glob(expr) "{{{
    return split(glob(a:expr), "\n")
endfunc "}}}

func! s:equal(l, r) "{{{
    return type(a:l) == type(a:r)
    \   && a:l ==# a:r
endfunc "}}}


func! simpletap#run() "{{{
    let tested = 0
    for t in s:glob(printf('%s/*.vim', g:simpletap#test_dir))
        echomsg 'begin test:' t
        execute 'source' t
        let tested = 1
    endfor

    if !tested
        call s:warn('no tests to run.')
    endif
endfunc "}}}

func! simpletap#ok(cond, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if a:cond
        echomsg printf(
        \   '%d. %s ... %s',
        \   s:current_test_num,
        \   testname,
        \   g:simpletap#ok_ok_str
        \)
    else
        call s:warn(
        \   printf(
        \      '%d. %s ... %s',
        \      s:current_test_num,
        \      testname,
        \      g:simpletap#ok_not_ok_str
        \   )
        \)
    endif

    let s:current_test_num += 1
endfunc "}}}

func! simpletap#is(got, expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if s:equal(a:got, a:expected)
        echomsg printf(
        \   '%d. %s ... %s',
        \   s:current_test_num,
        \   testname,
        \   g:simpletap#is_ok_str
        \)
    else
        call s:warn(
        \   printf(
        \      '%d. %s ... %s',
        \      s:current_test_num,
        \      testname,
        \      printf(
        \          g:simpletap#is_not_ok_str,
        \          string(a:got),
        \          string(a:expected))
        \   )
        \)
    endif

    let s:current_test_num += 1
endfunc "}}}

func! simpletap#raise_ok(command, regex, ...) "{{{
    try
        execute a:command
        call call('simpletap#ok', [0] + a:000)
    catch
        if v:exception =~# a:regex
            call call('simpletap#ok', [1] + a:000)
        else
            throw substitute(v:exception, '^Vim'.'\C', '', '')
        endif
    endtry
endfunc "}}}

func! simpletap#exists_func(Fn, ...) "{{{
    let args = a:0 != 0 ? a:1 : []

    try
        call call(a:Fn, args)
        return 1
    catch /E116:/
        return 0
    catch /E117:/    " Unknown function: ...
        return 0
    catch /E118:/
        return 0
    catch /E119:/    " Not enough arguments for function: ...
        return 1
    catch /E120:/
        return 0
    endtry
endfunc "}}}

func! simpletap#diag(msg) "{{{
    echohl Comment
    echomsg '#' a:msg
    echohl None
endfunc "}}}


func! s:cmd_begin_test() "{{{
    let s:current_test_num = 1
endfunc "}}}

func! s:cmd_end_test() "{{{
    let s:current_test_num = 1
    echomsg 'Done.'
endfunc "}}}
" }}}

" Commands {{{
command! TestBegin
\   call s:cmd_begin_test()

command! TestEnd
\   call s:cmd_end_test()
" }}}


" Restore 'cpoptions' {{{
let &cpo = s:save_cpo
" }}}
