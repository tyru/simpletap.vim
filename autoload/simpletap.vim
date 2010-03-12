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
" - test
" - Show more info when test(s) failed.
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
\   },
\
\   'is_locked': 0,
\}

func! s:test_vars_new() "{{{
    return deepcopy(s:test_stat)
endfunc "}}}

func! s:test_stat.initialize() dict "{{{
    for [key, val] in items(s:test_stat.vars)
        call self.set(key, val)
        unlet val
    endfor
endfunc "}}}

func! s:test_stat.get(varname) dict "{{{
    call s:assert(has_key(self.vars, a:varname))

    return copy(self.vars[a:varname])
endfunc "}}}

func! s:test_stat.set(varname, value) dict "{{{
    call s:assert(has_key(self.vars, a:varname))
    if self.is_locked | return | endif

    let self.vars[a:varname] = a:value
endfunc "}}}

func! s:test_stat.increment(varname) dict "{{{
    if self.is_locked | return | endif

    let val = self.get(a:varname)
    call s:assert(type(val) == type(0))
    call self.set(a:varname, val + 1)
endfunc "}}}

func! s:test_stat.add(varname, value) dict "{{{
    if self.is_locked | return | endif

    let list = self.get(a:varname)
    call s:assert(type(list) == type([]))
    call self.set(a:varname, add(list, a:value))
endfunc "}}}

func! s:test_stat.lock() dict "{{{
    let self.is_locked = 1
endfunc "}}}

func! s:test_stat.unlock() dict "{{{
    let self.is_locked = 0
endfunc "}}}

lockvar s:test_stat
" }}}

let s:stat = s:test_vars_new()

let s:PASS = 1
lockvar s:PASS
let s:FAIL = 2
lockvar s:FAIL
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

        let s:simpletap[a:varname] = deepcopy({v})
    endfunc "}}}
    func! s:def_hash(varname, default) "{{{
        let v = s:varname(a:varname)
        if !exists(v)
            let {v} = {}
        endif
        call extend({v}, a:default, 'keep')

        let s:simpletap[a:varname] = deepcopy({v})
    endfunc "}}}

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
    \   },
    \)
    call s:def_hash(
    \   'fail_fmt',
    \   {
    \       'ok': 'NOT ok',
    \       'cmp_ok': 'got: %s, expected: %s',
    \       'is': 'got: %s, expected: %s',
    \       'isnt': 'got: %s, expected not: %s',
    \       'is_deeply': 'got: %s, expected not: %s',
    \       'like': 'got: %s, expected like: %s',
    \       'unlike': 'got: %s, expected like not: %s',
    \       'stdout_is': 'got: %s, expected: %s',
    \       'stdout_isnt': 'got: %s, expected: %s',
    \       'stdout_like': 'got: %s, expected like: %s',
    \       'stdout_unlike': 'got: %s, expected like not: %s',
    \   },
    \)
    call s:def('silent', 1)
    call s:def('test_dir', '.')
    call s:def('echohl_diag', 'Comment')
    call s:def('echohl_error', 'WarningMsg')
    call s:def('echohl_begin', 'None')
    call s:def('echohl_done', 'Underlined')
    call s:def('recursive', 0)
    call s:def('show_only_failed', 0)

    delfunc s:varname
    delfunc s:def
    delfunc s:def_hash

    lockvar s:simpletap
endfunc "}}}


" Utilities {{{

func! s:warn(...) "{{{
    execute 'echohl' g:simpletap#echohl_error
    echomsg join(a:000, ' ')
    echohl None
endfunc "}}}

func! s:warnf(...) "{{{
    call call('s:warn', [call('printf', a:000)])
endfunc "}}}

func! s:glob(expr) "{{{
    return split(glob(a:expr), "\n")
endfunc "}}}

func! s:equal(l, r) "{{{
    return type(a:l) == type(a:r)
    \   && type(a:l) != type({})
    \   && type(a:l) != type([])
    \   && type(a:r) != type({})
    \   && type(a:r) != type([])
    \   && a:l ==# a:r
endfunc "}}}

func! s:equal_deeply(l, r) "{{{
    return type(a:l) == type(a:r)
    \   && a:l ==# a:r
endfunc "}}}

func! s:like(got, regex) "{{{
    return type(a:got) == type("")
    \   && type(a:regex) == type("")
    \   && a:got =~# a:regex
endfunc "}}}

func! s:cmp(l, op, r) "{{{
    return eval(printf('a:l %s a:r', a:op))
endfunc "}}}

func! s:assert(cond) "{{{
    if !a:cond
        throw s:error("assertion failure")
    endif
endfunc "}}}

func! s:passed(testname, funcname) "{{{
    echomsg printf(
    \   '%d. %s ... %s',
    \   s:stat.get('current_test_num'),
    \   a:testname,
    \   g:simpletap#pass_fmt[a:funcname]
    \)

    call s:assert(s:stat.get('current_test_num') == len(s:stat.get('test_result')) + 1)
    call s:stat.add('test_result', s:PASS)

    call s:step_num()
endfunc "}}}

func! s:failed(testname, funcname, ...) "{{{
    if a:0 == 0
        call s:warn(
        \   printf(
        \      '%d. %s ... %s',
        \      s:stat.get('current_test_num'),
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
        \      s:stat.get('current_test_num'),
        \      a:testname,
        \      printf(
        \          g:simpletap#fail_fmt[a:funcname],
        \          string(got),
        \          string(expected))
        \   )
        \)
    endif

    call s:assert(s:stat.get('current_test_num') == len(s:stat.get('test_result')) + 1)
    call s:stat.add('test_result', s:FAIL)

    call s:step_num()
endfunc "}}}

func! s:error(msg) "{{{
    return "simpletap:" . a:msg
endfunc "}}}

func! s:get_output(Code) "{{{
    call s:stat.lock()
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
endfunc "}}}

func! s:step_num() "{{{
    call s:stat.increment('current_test_num')
endfunc "}}}

func! s:begin_test_once() "{{{
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
    \   ExistsFunc
    \   call simpletap#exists_func(<args>)
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
    \   -nargs=* -bar
    \   Done
    \   call s:stat.set('done_testing', 1)
endfunc "}}}

func! s:begin_test(file) "{{{
    call s:stat.initialize()

    execute 'echohl' g:simpletap#echohl_begin
    echomsg 'Begin' '...' a:file
    echohl None
endfunc "}}}

func! s:end_test_once() "{{{
    delcommand Done
endfunc "}}}

func! s:end_test(file) "{{{
    let test_result = s:stat.get('test_result')
    let failed_result = filter(copy(test_result), 'v:val ==# s:FAIL')

    if !s:stat.get('done_testing')
        call s:warnf("!!! test '%s' has not done properly !!!", a:file)
    elseif empty(test_result)
        call s:warnf("test '%s' has done but no tests performed.", a:file)
    elseif !empty(failed_result)
        call s:warnf('failed %d test(s).', len(failed_result))
    else
        execute 'echohl' g:simpletap#echohl_done
        echomsg 'Done.'
        echohl None
    endif
endfunc "}}}

func! s:source(file, silent) "{{{
    if a:silent
        silent call s:begin_test(a:file)
        silent execute 'source' a:file
        silent call s:end_test(a:file)
    else
        call s:begin_test(a:file)
        execute 'source' a:file
        call s:end_test(a:file)
    endif
endfunc "}}}

" }}}


" OO interface {{{
let s:simpletap = {}    " See defintions at s:initialize_once().

func! simpletap#new(...) "{{{
    let obj = deepcopy(s:simpletap)
    let obj.save_prop = a:0 != 0 ? a:1 : {}
    lockvar obj.save_prop
    return obj
endfunc "}}}

func! s:add_method(name) "{{{
    if has_key(s:simpletap, a:name)
        return
    endif

    let template = [
    \   'func! s:simpletap.%s(...) dict',
    \       'let saved = empty(self.save_prop) ? {} : s:get_current_global_vars()',
    \       'call s:set_global_vars(self.save_prop)',
    \       'try',
    \           'return call("simpletap#%s", a:000)',
    \       'finally',
    \           'call s:set_global_vars(saved)',
    \       'endtry',
    \   'endfunc',
    \]
    execute printf(join(template, "\n"), a:name, a:name)
endfunc "}}}

func! s:get_global_varnames() "{{{
    return keys(filter(copy(s:simpletap), 'type(v:val) != type(function("tr"))'))
endfunc "}}}

func! s:get_current_global_vars() "{{{
    let ret = {}
    for var in s:get_global_varnames()
        let ret[var] = deepcopy(g:simpletap#{var})
    endfor
    return ret
endfunc "}}}

func! s:set_global_vars(prop) "{{{
    for var in keys(a:prop)
        let g:simpletap#{var} = deepcopy(a:prop[var])
    endfor
endfunc "}}}

" }}}

" Autoload {{{

func! simpletap#run(...) "{{{
    let dir = a:0 != 0 ? a:1 : g:simpletap#test_dir
    let dir = expand(dir)
    if !isdirectory(dir)
        call s:warnf("'%s' is not directory.", dir)
        return
    endif
    if g:simpletap#recursive
        let pat = printf('%s/**/*.vim', dir)
    else
        let pat = printf('%s/*.vim', dir)
    endif

    let tested = 0
    let lastline = 0    " lastline is number but becomes string when test failed.
    call s:begin_test_once()

    for t in s:glob(pat)
        if g:simpletap#show_only_failed
            redir => output
                call s:source(t, 1)
            redir END

            " Show messages only when test(s) failed.
            let failed = !empty(filter(copy(s:stat.get('test_result')), 'v:val ==# s:FAIL'))
            if failed
                let lines = split(output, '\n')
                let lastline = remove(lines, -1)
                for l in lines
                    echomsg l
                endfor
            endif
        else
            call s:source(t, 0)
        endif

        let tested = 1
    endfor
    call s:end_test_once()

    if g:simpletap#show_only_failed
        redraw
        if type(lastline) == type("")
            echomsg lastline
        else
            echomsg 'All test(s) passed.'
        endif
    endif
    if !tested
        call s:warn('no tests to run.')
    endif
endfunc "}}}
call s:add_method('run')


func! simpletap#ok(cond, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if a:cond
        call s:passed(testname, 'ok')
    else
        call s:failed(testname, 'ok')
    endif
endfunc "}}}
call s:add_method('ok')


func! simpletap#cmp_ok(got, op, expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if s:cmp(a:got, a:op, a:expected)
        call s:passed(testname, 'cmp_ok')
    else
        call s:failed(testname, 'cmp_ok', a:got, a:expected)
    endif
endfunc "}}}
call s:add_method('cmp_ok')


func! simpletap#is(got, expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if s:equal(a:got, a:expected)
        call s:passed(testname, 'is')
    else
        call s:failed(testname, 'is', a:got, a:expected)
    endif
endfunc "}}}
call s:add_method('is')

func! simpletap#isnt(got, expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if !s:equal(a:got, a:expected)
        call s:passed(testname, 'is')
    else
        call s:failed(testname, 'is', a:got, a:expected)
    endif
endfunc "}}}
call s:add_method('isnt')


func! simpletap#is_deeply(got, expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if s:equal_deeply(a:got, a:expected)
        call s:passed(testname, 'is_deeply')
    else
        call s:failed(testname, 'is_deeply', a:got, a:expected)
    endif
endfunc "}}}
call s:add_method('is_deeply')


func! simpletap#like(got, regex, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if s:like(a:got, a:regex)
        call s:passed(testname, 'like')
    else
        call s:failed(testname, 'like', a:got, a:regex)
    endif
endfunc "}}}
call s:add_method('like')

func! simpletap#unlike(got, regex, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if !s:like(a:got, a:regex)
        call s:passed(testname, 'unlike')
    else
        call s:failed(testname, 'unlike', a:got, a:regex)
    endif
endfunc "}}}
call s:add_method('unlike')


func! simpletap#throws_ok(Code, regex, ...) "{{{
    try
        if type(a:Code) == type(function("tr"))
            call a:Code()
        elseif type(a:Code) == type("")
            execute a:Code
        else
            throw s:error("type error")
        endif
        call call('simpletap#ok', [0] + a:000)
    catch
        if v:exception =~# a:regex
            call call('simpletap#ok', [1] + a:000)
        else
            throw substitute(v:exception, '^Vim'.'\C', '', '')
        endif
    endtry
endfunc "}}}
call s:add_method('throws_ok')


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
call s:add_method('exists_func')


func! simpletap#stdout_is(Code, expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    let output = s:get_output(a:Code)
    if s:equal(output, a:expected)
        call s:passed(testname, 'stdout_is')
    else
        call s:failed(testname, 'stdout_is', output, a:expected)
    endif
endfunc "}}}
call s:add_method('stdout_is')

func! simpletap#stdout_isnt(Code, expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    let output = s:get_output(a:Code)
    if !s:equal(output, a:expected)
        call s:passed(testname, 'stdout_isnt')
    else
        call s:failed(testname, 'stdout_isnt', output, a:expected)
    endif
endfunc "}}}
call s:add_method('stdout_isnt')

func! simpletap#stdout_like(Code, regex, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    let output = s:get_output(a:Code)
    if s:like(output, a:regex)
        call s:passed(testname, 'stdout_like')
    else
        call s:failed(testname, 'stdout_like', output, a:regex)
    endif
endfunc "}}}
call s:add_method('stdout_like')

func! simpletap#stdout_unlike(Code, regex, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    let output = s:get_output(a:Code)
    if !s:like(output, a:regex)
        call s:passed(testname, 'stdout_unlike')
    else
        call s:failed(testname, 'stdout_unlike', output, a:regex)
    endif
endfunc "}}}
call s:add_method('stdout_unlike')


func! simpletap#diag(msg) "{{{
    execute 'echohl' g:simpletap#echohl_diag
    echomsg '#' a:msg
    echohl None
endfunc "}}}
call s:add_method('diag')


func! simpletap#pass() "{{{
    return simpletap#ok(1)
endfunc "}}}
call s:add_method('pass')

func! simpletap#fail() "{{{
    return simpletap#ok(0)
endfunc "}}}
call s:add_method('fail')

" }}}

" }}}

" Commands {{{
" }}}

call s:initialize_once()

" Restore 'cpoptions' {{{
let &cpo = s:save_cpo
" }}}
