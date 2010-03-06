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
" - Add command macros.
" - Put format string to dictionary.
" - s:get_func_varnames()
"   - Add function to define autoload functions.
" - Add global vars to define echohl
" - functions
"   - cmp_ok()
"   - is_deeply()
"   - done_testing()
"   - plan()
"   - pass()
"   - fail()
"   - todo_skip()
"   - subtest
"   - eq_array()
"   - eq_hash()
"   - eq_set()
"   - stderr_*()
"     - I don't know how to implement
"
" Needless:
" - can_ok()
"   - Use has_key() and ok()
" - isa_ok()
" - not_throws_ok()
"   - Use ok() in :try, not in :catch.


" Variables {{{
let s:current_test_num = 1
let s:dont_change_current_num = 0
" }}}

" Functions {{{

func! simpletap#load() "{{{
    runtime! plugin/simpletap.vim
endfunc "}}}

func! s:initialize_once() "{{{
    call simpletap#load()

    func! s:varname(v) "{{{
        return 'g:simpletap#' . a:v
    endfunc "}}}
    func! s:def(varname, default) "{{{
        let v = s:varname(a:varname)
        if !exists(v)
            let {v} = a:default
        endif
    endfunc "}}}
    func! s:def_hash(varname, default) "{{{
        let v = s:varname(a:varname)
        if !exists(v)
            let {v} = {}
        endif
        call extend({v}, a:default, 'keep')
    endfunc "}}}

    call s:def_hash(
    \   'pass_fmt',
    \   {
    \       'ok': 'ok',
    \       'is': 'ok',
    \       'isnt': 'ok',
    \       'like': 'ok',
    \       'unlike': 'ok',
    \       'stdout_is': 'ok',
    \       'stdout_like': 'ok',
    \       'stdout_unlike': 'ok',
    \   },
    \)
    call s:def_hash(
    \   'fail_fmt',
    \   {
    \       'ok': 'NOT ok',
    \       'is': 'got: %s, expected: %s',
    \       'isnt': 'got: %s, expected not: %s',
    \       'like': 'got: %s, expected like: %s',
    \       'unlike': 'got: %s, expected like not: %s',
    \       'stdout_is': 'got: %s, expected: %s',
    \       'stdout_like': 'got: %s, expected like: %s',
    \       'stdout_unlike': 'got: %s, expected like not: %s',
    \   },
    \)
    call s:def('silent', 1)
    call s:def('test_dir', '.')

    delfunc s:varname
    delfunc s:def
    delfunc s:def_hash
endfunc "}}}


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

func! s:like(got, regex) "{{{
    return type(a:got) == type("")
    \   && type(a:regex) == type("")
    \   && a:got =~# a:regex
endfunc "}}}

func! s:passed(testname, funcname) "{{{
    echomsg printf(
    \   '%d. %s ... %s',
    \   s:current_test_num,
    \   a:testname,
    \   g:simpletap#pass_fmt[a:funcname]
    \)
endfunc "}}}

func! s:failed(testname, funcname, ...) "{{{
    if a:0 == 0
        call s:warn(
        \   printf(
        \      '%d. %s ... %s',
        \      s:current_test_num,
        \      a:testname,
        \      g:simpletap#fail_fmt[a:funcname]
        \   )
        \)
    else
        let got = a:1
        let expected = a:2
        call s:warn(
        \   printf(
        \      '%d. %s ... %s',
        \      s:current_test_num,
        \      a:testname,
        \      printf(
        \          g:simpletap#fail_fmt[a:funcname],
        \          string(got),
        \          string(expected))
        \   )
        \)
    endif
endfunc "}}}

func! s:error(msg) "{{{
    return "simpletap:" . a:msg
endfunc "}}}

func! s:get_output(Code) "{{{
    let s:dont_change_current_num = 1
    redir => output

    try
        if type(a:Code) == type(function('tr'))
            let ex = 'call a:Code()'
        elseif type(a:Code) == type("")
            let ex = 'execute a:Code'
        else
            throw s:error("type error")
        endif
        if g:simpletap#silent
            silent execute ex
        else
            execute ex
        endif
        redir END
        return substitute(output, '^\n', '', '')
    finally
        redir END
        let s:dont_change_current_num = 0
    endtry
endfunc "}}}

func! s:step_num() "{{{
    if !s:dont_change_current_num
        let s:current_test_num += 1
    endif
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
        call s:passed(testname, 'ok')
    else
        call s:failed(testname, 'ok')
    endif

    call s:step_num()
endfunc "}}}

func! simpletap#is(got, expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if s:equal(a:got, a:expected)
        call s:passed(testname, 'is')
    else
        call s:failed(testname, 'is', a:got, a:expected)
    endif

    call s:step_num()
endfunc "}}}

func! simpletap#isnt(got, expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if !s:equal(a:got, a:expected)
        call s:passed(testname, 'is')
    else
        call s:failed(testname, 'is', a:got, a:expected)
    endif

    call s:step_num()
endfunc "}}}

func! simpletap#like(got, regex, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if s:like(a:got, a:regex)
        call s:passed(testname, 'like')
    else
        call s:failed(testname, 'like', a:got, a:regex)
    endif

    call s:step_num()
endfunc "}}}

func! simpletap#unlike(got, regex, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if !s:like(a:got, a:regex)
        call s:passed(testname, 'unlike')
    else
        call s:failed(testname, 'unlike', a:got, a:regex)
    endif

    call s:step_num()
endfunc "}}}

func! simpletap#throws_ok(command, regex, ...) "{{{
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

func! simpletap#stdout_is(Code, expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    let output = s:get_output(a:Code)
    if s:equal(output, a:expected)
        call s:passed(testname, 'stdout_is')
    else
        call s:failed(testname, 'stdout_is', output, a:expected)
    endif

    call s:step_num()
endfunc "}}}

func! simpletap#stdout_like(Code, regex, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    let output = s:get_output(a:Code)
    if s:like(output, a:regex)
        call s:passed(testname, 'stdout_like')
    else
        call s:failed(testname, 'stdout_like', output, a:regex)
    endif

    call s:step_num()
endfunc "}}}

func! simpletap#stdout_unlike(Code, regex, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    let output = s:get_output(a:Code)
    if !s:like(output, a:regex)
        call s:passed(testname, 'stdout_unlike')
    else
        call s:failed(testname, 'stdout_unlike', output, a:regex)
    endif

    call s:step_num()
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

call s:initialize_once()

" Restore 'cpoptions' {{{
let &cpo = s:save_cpo
" }}}
