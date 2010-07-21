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
" - Add command macros.
" - functions
"   - plan()
"   - todo_skip()
"   - subtest
"   - eq_array()
"   - eq_hash()
"   - eq_set()
"   - stderr_*()
"     - I don't know how to implement
" - More tests for simpletap.
" - Define global variable or environment variable.
" - :Is as reference equality test. :Same as current :Is.
" - Output to buffer.
"
" Needless:
" - can_ok()
"   - Use has_key() and ok()
" - isa_ok()
" - not_throws_ok()
"   - Use ok() in :try, not in :catch.


" Variables {{{
" s:test_stat {{{
let s:test_stat = {
\   'vars': {
\       'current_test_num': 1,
\       'done_testing': 0,
\       'test_result': [],
\       'output_info': [],
\   },
\
\   'is_locked': 0,
\}

function! s:test_vars_new() "{{{
    return deepcopy(s:test_stat)
endfunction "}}}

function! s:test_stat.initialize() dict "{{{
    for [key, val] in items(s:test_stat.vars)
        call self.set(key, val)
        unlet val
    endfor
endfunction "}}}

function! s:test_stat.get(varname) dict "{{{
    call s:assert(has_key(self.vars, a:varname))

    return copy(self.vars[a:varname])
endfunction "}}}

function! s:test_stat.set(varname, value) dict "{{{
    call s:assert(has_key(self.vars, a:varname))
    if self.is_locked | return | endif

    let self.vars[a:varname] = a:value
endfunction "}}}

function! s:test_stat.increment(varname) dict "{{{
    if self.is_locked | return | endif

    let val = self.get(a:varname)
    call s:assert(type(val) == type(0))
    call self.set(a:varname, val + 1)
endfunction "}}}

function! s:test_stat.add(varname, value) dict "{{{
    if self.is_locked | return | endif

    let list = self.get(a:varname)
    call s:assert(type(list) == type([]))
    call self.set(a:varname, add(list, a:value))
endfunction "}}}

function! s:test_stat.lock() dict "{{{
    let self.is_locked = 1
endfunction "}}}

function! s:test_stat.unlock() dict "{{{
    let self.is_locked = 0
endfunction "}}}

lockvar s:test_stat
" }}}

let s:stat = s:test_vars_new()

let s:PASS = 1
lockvar s:PASS
let s:FAIL = 2
lockvar s:FAIL
" }}}

" Functions {{{

function! simpletap#load() "{{{
    runtime! plugin/simpletap.vim
endfunction "}}}

function! simpletap#define_macro() "{{{
    call s:begin_test_once()
endfunction "}}}

function! s:initialize_once() "{{{
    call simpletap#load()

    function! s:varname(v) "{{{
        return 'g:simpletap#' . a:v
    endfunction "}}}
    function! s:def(varname, default) "{{{
        let v = s:varname(a:varname)
        if !exists(v)
            let {v} = a:default
        endif

        let s:simpletap[a:varname] = deepcopy({v})
    endfunction "}}}
    function! s:def_hash(varname, default) "{{{
        let v = s:varname(a:varname)
        if !exists(v)
            let {v} = {}
        endif
        call extend({v}, a:default, 'keep')

        let s:simpletap[a:varname] = deepcopy({v})
    endfunction "}}}

    call s:def_hash(
    \   'pass_fmt',
    \   {
    \       'ok': 'ok',
    \       'cmp_ok': 'ok',
    \       'is': 'ok',
    \       'isnt': 'ok',
    \       'is_deeply': 'ok',
    \       'like': 'ok',
    \       'unlike': 'ok',
    \       'stdout_is': 'ok',
    \       'stdout_isnt': 'ok',
    \       'stdout_like': 'ok',
    \       'stdout_unlike': 'ok',
    \       'throws_ok': 'ok',
    \   },
    \)
    call s:def_hash(
    \   'fail_fmt',
    \   {
    \       'ok': 'NOT ok',
    \       'cmp_ok': 'got: %s, expected: %s',
    \       'is': 'got: %s, expected: %s',
    \       'isnt': 'got: %s, expected not: %s',
    \       'is_deeply': 'got: %s, expected: %s',
    \       'like': 'got: %s, expected like: %s',
    \       'unlike': 'got: %s, expected like not: %s',
    \       'stdout_is': 'got: %s, expected: %s',
    \       'stdout_isnt': 'got: %s, expected: %s',
    \       'stdout_like': 'got: %s, expected like: %s',
    \       'stdout_unlike': 'got: %s, expected like not: %s',
    \       'throws_ok': 'got exception: %s, expected like: %s',
    \   },
    \)
    call s:def('silent', 1)
    call s:def('test_dir', '%:p:h')
    call s:def('echohl_diag', 'Comment')
    call s:def('echohl_error', 'WarningMsg')
    call s:def('echohl_begin', 'None')
    call s:def('echohl_done', 'Underlined')
    call s:def('echohl_skip', 'Underlined')
    call s:def('echohl_output', 'None')
    call s:def('recursive', 1)
    call s:def('show_only_failed', 1)
    call s:def('show_exception', 1)
    call s:def('report', 1)
    call s:def('output_to', 'buffer')

    delfunc s:varname
    delfunc s:def
    delfunc s:def_hash

    lockvar s:simpletap
endfunction "}}}


" Utilities {{{

function! s:warn(msg) "{{{
    call s:echomsg(g:simpletap#echohl_error, a:msg)
endfunction "}}}

function! s:warnf(...) "{{{
    call call('s:warn', [call('printf', a:000)])
endfunction "}}}

function! s:echomsg(hl, msg) "{{{
    redraw
    execute 'echohl' a:hl
    echomsg a:msg
    echohl None
endfunction "}}}

function! s:glob(expr) "{{{
    return split(glob(a:expr), "\n")
endfunction "}}}

function! s:error(msg) "{{{
    return "simpletap:" . a:msg
endfunction "}}}

function! s:assert(cond) "{{{
    if !a:cond
        throw s:error("assertion failure")
    endif
endfunction "}}}

function! s:get_output(Code) "{{{
    call s:stat.lock()

    redir END
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
        call s:stat.unlock()
    endtry
endfunction "}}}


function! s:equal(l, r) "{{{
    return type(a:l) == type(a:r)
    \   && type(a:l) != type({})
    \   && type(a:l) != type([])
    \   && type(a:r) != type({})
    \   && type(a:r) != type([])
    \   && a:l ==# a:r
endfunction "}}}

function! s:equal_deeply(l, r) "{{{
    return type(a:l) == type(a:r)
    \   && a:l ==# a:r
endfunction "}}}

function! s:like(got, regex) "{{{
    return type(a:got) == type("")
    \   && type(a:regex) == type("")
    \   && a:got =~# a:regex
endfunction "}}}

function! s:cmp(l, op, r) "{{{
    return eval(printf('a:l %s a:r', a:op))
endfunction "}}}


function! s:passed(testname, funcname) "{{{
    call s:assert(s:stat.get('current_test_num') == len(s:stat.get('output_info')) + 1)
    call s:stat.add(
    \   'output_info',
    \   [
    \       g:simpletap#echohl_output,
    \       printf(
    \          '%d. %s ... %s',
    \          s:stat.get('current_test_num'),
    \          a:testname,
    \          g:simpletap#pass_fmt[a:funcname]
    \       )
    \   ]
    \)

    call s:assert(s:stat.get('current_test_num') == len(s:stat.get('test_result')) + 1)
    call s:stat.add('test_result', s:PASS)

    call s:step_num()

    return 1
endfunction "}}}

function! s:failed(testname, funcname, ...) "{{{
    call s:assert(s:stat.get('current_test_num') == len(s:stat.get('output_info')) + 1)
    if a:0 == 0
        call s:stat.add(
        \   'output_info',
        \   [
        \       g:simpletap#echohl_output,
        \       printf(
        \          '%d. %s ... %s',
        \          s:stat.get('current_test_num'),
        \          a:testname,
        \          g:simpletap#fail_fmt[a:funcname]
        \       )
        \   ]
        \)
    else
        let got = a:1
        let expected = a:2
        call s:stat.add(
        \   'output_info',
        \   [
        \       g:simpletap#echohl_output,
        \       printf(
        \          '%d. %s ... %s',
        \          s:stat.get('current_test_num'),
        \          a:testname,
        \          printf(
        \              g:simpletap#fail_fmt[a:funcname],
        \              string(got),
        \              string(expected))
        \       )
        \   ]
        \)
    endif

    call s:assert(s:stat.get('current_test_num') == len(s:stat.get('test_result')) + 1)
    call s:stat.add('test_result', s:FAIL)

    call s:step_num()

    return 0
endfunction "}}}

function! s:step_num() "{{{
    call s:stat.increment('current_test_num')
endfunction "}}}


function! s:begin_test_once() "{{{
    " TODO Don't do it yourself
    command!
    \   -nargs=*
    \   OK
    \   call simpletap#ok(<args>)
    command!
    \   -nargs=*
    \   Ok
    \   call simpletap#ok(<args>)
    command!
    \   -nargs=*
    \   Is
    \   call simpletap#is(<args>)
    command!
    \   -nargs=*
    \   Isnt
    \   call simpletap#isnt(<args>)
    command!
    \   -nargs=*
    \   IsDeeply
    \   call simpletap#is_deeply(<args>)
    command!
    \   -nargs=*
    \   Like
    \   call simpletap#like(<args>)
    command!
    \   -nargs=*
    \   Unlike
    \   call simpletap#unlike(<args>)
    command!
    \   -nargs=*
    \   ThrowsOK
    \   call simpletap#throws_ok(<args>)
    command!
    \   -nargs=*
    \   ThrowsOk
    \   call simpletap#throws_ok(<args>)
    command!
    \   -nargs=*
    \   StdoutIs
    \   call simpletap#stdout_is(<args>)
    command!
    \   -nargs=*
    \   StdoutIsnt
    \   call simpletap#stdout_isnt(<args>)
    command!
    \   -nargs=*
    \   StdoutLike
    \   call simpletap#stdout_like(<args>)
    command!
    \   -nargs=*
    \   StdoutUnlike
    \   call simpletap#stdout_unlike(<args>)
    command!
    \   -nargs=*
    \   Diag
    \   call simpletap#diag(<args>)
    command!
    \   -nargs=*
    \   Pass
    \   call simpletap#pass(<args>)
    command!
    \   -nargs=*
    \   Fail
    \   call simpletap#fail(<args>)
    command!
    \   -nargs=*
    \   Skip
    \   call simpletap#skip(<args>)

    command!
    \   -nargs=* -bar
    \   Done
    \   call s:stat.set('done_testing', 1)

    command!
    \   -bar
    \   StatLock
    \   call s:stat.lock()
    command!
    \   -bar
    \   StatUnlock
    \   call s:stat.unlock()
endfunction "}}}

function! s:begin_test(file) "{{{
    call s:stat.initialize()

    execute 'echohl' g:simpletap#echohl_begin
    echomsg 'Begin' '...' a:file
    echohl None
endfunction "}}}

function! s:end_test_once() "{{{
    delcommand OK
    delcommand Ok
    delcommand Is
    delcommand Isnt
    delcommand IsDeeply
    delcommand Like
    delcommand Unlike
    delcommand ThrowsOK
    delcommand ThrowsOk
    delcommand StdoutIs
    delcommand StdoutIsnt
    delcommand StdoutLike
    delcommand StdoutUnlike
    delcommand Diag
    delcommand Pass
    delcommand Fail
    delcommand Skip
    delcommand Done
    delcommand StatLock
    delcommand StatUnlock
endfunction "}}}

function! s:end_test(file, skipped) "{{{
    let test_result = s:stat.get('test_result')
    let failed_result_num = len(filter(copy(test_result), 'v:val ==# s:FAIL'))
    let passed_result_num = len(test_result) - failed_result_num

    if a:skipped
        call s:stat.add('output_info', [g:simpletap#echohl_skip, 'Skip.'])
    elseif !s:stat.get('done_testing')
        call s:warnf("test '%s' has not done properly.", a:file)
    elseif empty(test_result)
        call s:warnf("test '%s' has done but no tests performed.", a:file)
    else
        let hl = (failed_result_num ? g:simpletap#echohl_error : g:simpletap#echohl_done)
        let msg = printf('Done %d test(s). (PASS:%d, FAIL:%d)', passed_result_num + failed_result_num, passed_result_num, failed_result_num)
        call s:stat.add('output_info', [hl, msg])
    endif
endfunction "}}}

function! s:source(file) "{{{
    call s:begin_test(a:file)
    try
        source `=a:file`
        call s:end_test(a:file, 0)
        let results = s:stat.get('test_result')
        let failed = !empty(filter(copy(results), 'v:val ==# s:FAIL'))
        return failed ? 0 : 1
    catch /^simpletap - SKIP$/
        call s:end_test(a:file, 1)
        return 1
    catch
        if g:simpletap#show_exception
            call s:warn('# Exception throwed.')
            call s:warnf('# v:exception = %s', string(v:exception))
            call s:warnf('# v:throwpoint = %s', string(v:throwpoint))
        endif
        return 0
    endtry
endfunction "}}}

function! s:output_summary(bufnr) "{{{
    let results = s:stat.get('test_result')
    let failed = !empty(filter(copy(results), 'v:val ==# s:FAIL'))
    let output_info = s:stat.get('output_info')
    if !g:simpletap#show_only_failed || g:simpletap#show_only_failed && failed
        if a:bufnr ==# -1
            for [hl, msg] in output_info
                call s:echomsg(hl, msg)
            endfor
        else
            call setline('$', map(copy(output_info), 'v:val[1]'))
        endif
    endif
endfunction "}}}

function! s:create_buffer() "{{{
    new
    setlocal bufhidden=hide buftype=nofile noswapfile nobuflisted
    return bufnr('%')
endfunction "}}}

" }}}


" OO interface {{{
let s:simpletap = {}    " See defintions at s:initialize_once().

function! simpletap#new(...) "{{{
    let obj = deepcopy(s:simpletap)

    let obj.save_prop = a:0 != 0 ? a:1 : {}
    lockvar obj.save_prop

    let obj.stat = deepcopy(s:stat)

    return obj
endfunction "}}}

function! s:add_method(name) "{{{
    if has_key(s:simpletap, a:name)
        return
    endif

    let template = [
    \   'function! s:simpletap.%s(...) dict',
    \       'let saved = empty(self.save_prop) ? {} : s:get_current_global_vars()',
    \       'call s:set_global_vars(self.save_prop)',
    \       'let saved_stat = s:stat',
    \       'let s:stat = self.stat',
    \       'try',
    \           'return call("simpletap#%s", a:000)',
    \       'finally',
    \           'call s:set_global_vars(saved)',
    \           'let s:stat = saved_stat',
    \       'endtry',
    \   'endfunction',
    \]
    execute printf(join(template, "\n"), a:name, a:name)
endfunction "}}}

function! s:get_global_varnames() "{{{
    return keys(filter(copy(s:simpletap), 'type(v:val) != type(function("tr"))'))
endfunction "}}}

function! s:get_current_global_vars() "{{{
    let ret = {}
    for var in s:get_global_varnames()
        let ret[var] = deepcopy(g:simpletap#{var})
    endfor
    return ret
endfunction "}}}

function! s:set_global_vars(prop) "{{{
    for var in keys(a:prop)
        let g:simpletap#{var} = deepcopy(a:prop[var])
    endfor
endfunction "}}}

" }}}

" Autoload {{{

function! simpletap#run(...) "{{{
    let dir = a:0 != 0 ? a:1 : g:simpletap#test_dir
    let dir = expand(dir)
    if !isdirectory(dir)
        call s:warnf("'%s' is not directory.", dir)
        return
    endif
    let pat = dir . '/' . (g:simpletap#recursive ? '**/*.vim' : '*.vim')

    " Create buffer if needed.
    let output_bufnr = -1
    if g:simpletap#output_to ==# 'buffer'
        let output_bufnr = s:create_buffer()
    endif

    call s:begin_test_once()

    let pass_all = 1
    for t in s:glob(pat)
        if !s:source(t)
            let pass_all = 0
        endif
        call s:output_summary(output_bufnr)
        echon "\n"
    endfor
    call s:end_test_once()

    " Small summary of all.
    if pass_all
        call s:echomsg(g:simpletap#echohl_done, 'All test(s) passed.')
    elseif empty(s:stat.get('test_result'))
        call s:warn('no tests to run.')
    endif

    if g:simpletap#report && output_bufnr ==# -1
        messages
    endif
endfunction "}}}
call s:add_method('run')


function! simpletap#ok(cond, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if a:cond
        return s:passed(testname, 'ok')
    else
        return s:failed(testname, 'ok')
    endif
endfunction "}}}
call s:add_method('ok')


function! simpletap#cmp_ok(got, op, expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if s:cmp(a:got, a:op, a:expected)
        return s:passed(testname, 'cmp_ok')
    else
        " TODO Output a:op.
        return s:failed(testname, 'cmp_ok', a:got, a:expected)
    endif
endfunction "}}}
call s:add_method('cmp_ok')


function! simpletap#is(got, expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if s:equal(a:got, a:expected)
        return s:passed(testname, 'is')
    else
        return s:failed(testname, 'is', a:got, a:expected)
    endif
endfunction "}}}
call s:add_method('is')

function! simpletap#isnt(got, expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if !s:equal(a:got, a:expected)
        return s:passed(testname, 'isnt')
    else
        return s:failed(testname, 'isnt', a:got, a:expected)
    endif
endfunction "}}}
call s:add_method('isnt')


function! simpletap#is_deeply(got, expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if s:equal_deeply(a:got, a:expected)
        return s:passed(testname, 'is_deeply')
    else
        return s:failed(testname, 'is_deeply', a:got, a:expected)
    endif
endfunction "}}}
call s:add_method('is_deeply')


function! simpletap#like(got, regex, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if s:like(a:got, a:regex)
        return s:passed(testname, 'like')
    else
        return s:failed(testname, 'like', a:got, a:regex)
    endif
endfunction "}}}
call s:add_method('like')

function! simpletap#unlike(got, regex, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if !s:like(a:got, a:regex)
        return s:passed(testname, 'unlike')
    else
        return s:failed(testname, 'unlike', a:got, a:regex)
    endif
endfunction "}}}
call s:add_method('unlike')


function! simpletap#throws_ok(excmd, regex, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    try
        execute a:excmd
    catch
        let ex = v:exception
    endtry

    if exists('ex') && s:like(ex, a:regex)
        return s:passed(testname, 'throws_ok')
    else
        return s:failed(testname, 'throws_ok', ex, a:regex)
    endif
endfunction "}}}
call s:add_method('throws_ok')


function! simpletap#stdout_is(Code, expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    let output = s:get_output(a:Code)
    if s:equal(output, a:expected)
        return s:passed(testname, 'stdout_is')
    else
        return s:failed(testname, 'stdout_is', output, a:expected)
    endif
endfunction "}}}
call s:add_method('stdout_is')

function! simpletap#stdout_isnt(Code, expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    let output = s:get_output(a:Code)
    if !s:equal(output, a:expected)
        return s:passed(testname, 'stdout_isnt')
    else
        return s:failed(testname, 'stdout_isnt', output, a:expected)
    endif
endfunction "}}}
call s:add_method('stdout_isnt')

function! simpletap#stdout_like(Code, regex, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    let output = s:get_output(a:Code)
    if s:like(output, a:regex)
        return s:passed(testname, 'stdout_like')
    else
        return s:failed(testname, 'stdout_like', output, a:regex)
    endif
endfunction "}}}
call s:add_method('stdout_like')

function! simpletap#stdout_unlike(Code, regex, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    let output = s:get_output(a:Code)
    if !s:like(output, a:regex)
        return s:passed(testname, 'stdout_unlike')
    else
        return s:failed(testname, 'stdout_unlike', output, a:regex)
    endif
endfunction "}}}
call s:add_method('stdout_unlike')


function! simpletap#diag(...) "{{{
    call s:echomsg(g:simpletap#echohl_diag, '# ' . join(a:000, ' '))
endfunction "}}}
call s:add_method('diag')


function! simpletap#pass() "{{{
    return simpletap#ok(1)
endfunction "}}}
call s:add_method('pass')

function! simpletap#fail() "{{{
    return simpletap#ok(0)
endfunction "}}}
call s:add_method('fail')


function! simpletap#skip(...) "{{{
    if a:0 != 0
        Diag a:1
    endif
    throw 'simpletap - SKIP'
endfunction "}}}

" }}}

" }}}

" Commands {{{
" }}}

call s:initialize_once()

" Restore 'cpoptions' {{{
let &cpo = s:save_cpo
" }}}
