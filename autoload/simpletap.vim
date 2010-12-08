" vim:foldmethod=marker:fen:sw=4:sts=4
scriptencoding utf-8

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
" - Show more detailed messages at s:output_all_summary().
"   - Need to create s:stat per one script file.
" - syntax/simpletap-summary.vim
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
\       'finalizer': {},
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
    return self.vars[a:varname]
endfunction "}}}

function! s:test_stat.set(varname, value) dict "{{{
    if self.is_locked | return | endif

    let self.vars[a:varname] = a:value
endfunction "}}}

function! s:test_stat.increment(varname) dict "{{{
    if self.is_locked | return | endif

    let val = self.get(a:varname)
    call s:assert(type(val) == type(0), 'type(val) is Number')
    call self.set(a:varname, val + 1)
endfunction "}}}

function! s:test_stat.add(varname, value) dict "{{{
    if self.is_locked | return | endif

    let list = self.get(a:varname)
    call s:assert(type(list) == type([]), 'type(list) is List')
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
    call s:stat.initialize()
    call s:begin_test_once()
    call s:stat.initialize()
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

function! s:assert(cond, msg) "{{{
    if !a:cond
        throw s:error("assertion failure: " . a:msg)
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

function! s:format_to_string(val) "{{{
    if exists('g:loaded_prettyprint')
        return PrettyPrint(a:val)
    else
        return string(a:val)
    endif
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

function! s:like(Got, regex) "{{{
    return type(a:Got) == type("")
    \   && type(a:regex) == type("")
    \   && a:Got =~# a:regex
endfunction "}}}

function! s:cmp(l, op, r) "{{{
    return eval(printf('a:l %s a:r', a:op))
endfunction "}}}


function! s:passed(testname, funcname) "{{{
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

    call s:stat.add('test_result', s:PASS)

    call s:step_num()

    return 1
endfunction "}}}

function! s:failed(testname, funcname, ...) "{{{
    if a:0 == 0
        call s:stat.add(
        \   'output_info',
        \   [
        \       g:simpletap#echohl_output,
        \       printf(
        \          '!!!%d. %s ... %s',
        \          s:stat.get('current_test_num'),
        \          a:testname,
        \          g:simpletap#fail_fmt[a:funcname]
        \       )
        \   ]
        \)
    else
        let Got = a:1
        let Expected = a:2
        let msg = printf(
        \   '%d. %s ... %s',
        \   s:stat.get('current_test_num'),
        \   a:testname,
        \   printf(
        \       g:simpletap#fail_fmt[a:funcname],
        \       s:format_to_string(Got),
        \       s:format_to_string(Expected)
        \   )
        \)
        if stridx(msg, "\n")
            for l in split(msg, '\n')
                call s:stat.add('output_info', [g:simpletap#echohl_output, '!!!' . l])
            endfor
        else
            call s:stat.add('output_info', [g:simpletap#echohl_output, '!!!' . msg])
        endif
    endif

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

    let [hl, msg] = [g:simpletap#echohl_begin, 'Testing ... ' . a:file]
    if g:simpletap#output_to ==# 'buffer'
        call s:echomsg(hl, msg)
    endif
    call s:stat.add('output_info', [hl, msg])
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
        for msg in [
        \   '!!!# Exception throwed.',
        \   '!!!# v:exception = ' . string(v:exception),
        \   '!!!# v:throwpoint = ' . string(v:throwpoint),
        \]
            if g:simpletap#show_exception
                call s:warn(msg)
            endif
            call s:stat.add('output_info', [g:simpletap#echohl_error, msg])
        endfor
        call s:stat.add('test_result', s:FAIL)    " dummy
        return 0
    finally
        let f = s:stat.get('finalizer')
        for name in empty(f) ? [] : sort(keys(f))
            let call_args = [f[name].fn, f[name].args, f[name]]
            if has_key(f[name], 'dict')
                call add(call_args, f[name].dict)
            endif
            call call('call', call_args)
        endfor
    endtry
endfunction "}}}

function! s:output(bufnr, lines) "{{{
    if a:bufnr ==# -1
        for [hl, msg] in a:lines
            call s:echomsg(hl, msg)
        endfor
    else
        call s:assert(a:bufnr ==# bufnr('%'), 's:output(): a:bufnr is current buffer')
        call setline(line('$'), map(copy(a:lines), 'v:val[1]'))
    endif
endfunction "}}}

function! s:output_summary(bufnr) "{{{
    let results = copy(s:stat.get('test_result'))
    let failed = !empty(filter(results, 'v:val ==# s:FAIL'))
    let output_info = s:stat.get('output_info')
    if !g:simpletap#show_only_failed || g:simpletap#show_only_failed && failed
        call s:output(a:bufnr, output_info)
    endif
endfunction "}}}

function! s:output_all_summary(bufnr, pass_all) "{{{
    let lines = []

    if a:pass_all
        call add(lines, ['None', ''])
        call add(lines, [g:simpletap#echohl_done, 'All test(s) passed.'])
    elseif empty(s:stat.get('test_result'))
        call add(lines, ['None', ''])
        call add(lines, [g:simpletap#echohl_error, 'no tests to run.'])
    else
        return
    endif

    call s:output(a:bufnr, lines)
endfunction "}}}

function! s:create_buffer() "{{{
    new
    
    " Clean up the screen.
    % delete _
    
    setlocal bufhidden=hide buftype=nofile noswapfile nobuflisted
    setlocal filetype=simpletap-summary
    
    if has('conceal')
        setlocal conceallevel=3
        setlocal concealcursor=n

        syntax match   simpletapErrorHidden            '^!!!' contained conceal
    else
        syntax match   simpletapErrorHidden            '^!!!' contained
    endif
    syntax match simpletapError   '^!!!.*' contains=simpletapErrorHidden
    syntax match simpletapMessage   '^#.*'
    
    highlight def link simpletapError Error
    highlight def link simpletapErrorHidden Ignore
    highlight def link simpletapMessage Comment
    
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

function! simpletap#finalizer() "{{{
    return s:stat.get('finalizer')
endfunction "}}}


function! simpletap#run(path) "{{{
    if getftype(a:path) == ''
        return
    endif
    if isdirectory(a:path)
        call simpletap#run_dir(a:path)
    else
        call simpletap#run_file(a:path)
    endif
endfunction "}}}

function! simpletap#run_file(file) "{{{
    let file = expand(a:file)
    if !filereadable(file)
        call s:warnf("'%s' is not file.", file)
        return
    endif

    " Create buffer if needed.
    let output_bufnr = -1
    if g:simpletap#output_to ==# 'buffer'
        let output_bufnr = s:create_buffer()
    endif

    call s:begin_test_once()
    let passed = s:source(file)
    call s:output_summary(output_bufnr)
    call s:end_test_once()
    call s:output_all_summary(output_bufnr, passed)

    if g:simpletap#report && output_bufnr ==# -1
        messages
    endif
endfunction "}}}
call s:add_method('run_file')

function! simpletap#run_dir(dir) "{{{
    let dir = expand(a:dir)
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
    endfor
    call s:end_test_once()
    call s:output_all_summary(output_bufnr, pass_all)

    if g:simpletap#report && output_bufnr ==# -1
        messages
    endif
endfunction "}}}
call s:add_method('run_dir')


function! simpletap#ok(cond, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if a:cond
        return s:passed(testname, 'ok')
    else
        return s:failed(testname, 'ok')
    endif
endfunction "}}}
call s:add_method('ok')


function! simpletap#cmp_ok(Got, op, Expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if s:cmp(a:Got, a:op, a:Expected)
        return s:passed(testname, 'cmp_ok')
    else
        " TODO Output a:op.
        return s:failed(testname, 'cmp_ok', a:Got, a:Expected)
    endif
endfunction "}}}
call s:add_method('cmp_ok')


function! simpletap#is(Got, Expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if s:equal(a:Got, a:Expected)
        return s:passed(testname, 'is')
    else
        return s:failed(testname, 'is', a:Got, a:Expected)
    endif
endfunction "}}}
call s:add_method('is')

function! simpletap#isnt(Got, Expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if !s:equal(a:Got, a:Expected)
        return s:passed(testname, 'isnt')
    else
        return s:failed(testname, 'isnt', a:Got, a:Expected)
    endif
endfunction "}}}
call s:add_method('isnt')


function! simpletap#is_deeply(Got, Expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if s:equal_deeply(a:Got, a:Expected)
        return s:passed(testname, 'is_deeply')
    else
        return s:failed(testname, 'is_deeply', a:Got, a:Expected)
    endif
endfunction "}}}
call s:add_method('is_deeply')


function! simpletap#like(Got, regex, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if s:like(a:Got, a:regex)
        return s:passed(testname, 'like')
    else
        return s:failed(testname, 'like', a:Got, a:regex)
    endif
endfunction "}}}
call s:add_method('like')

function! simpletap#unlike(Got, regex, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if !s:like(a:Got, a:regex)
        return s:passed(testname, 'unlike')
    else
        return s:failed(testname, 'unlike', a:Got, a:regex)
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


function! simpletap#stdout_is(Code, Expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    let output = s:get_output(a:Code)
    if s:equal(output, a:Expected)
        return s:passed(testname, 'stdout_is')
    else
        return s:failed(testname, 'stdout_is', output, a:Expected)
    endif
endfunction "}}}
call s:add_method('stdout_is')

function! simpletap#stdout_isnt(Code, Expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    let output = s:get_output(a:Code)
    if !s:equal(output, a:Expected)
        return s:passed(testname, 'stdout_isnt')
    else
        return s:failed(testname, 'stdout_isnt', output, a:Expected)
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
    call s:stat.add('output_info', [g:simpletap#echohl_diag, '# ' . join(a:000)])
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


function! s:SID() "{{{
    return matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze_SID$')
endfunction "}}}
let s:SID_PREFIX = s:SID()
delfunc s:SID

" s:Simpletap {{{
let s:Simpletap = vice#class(
\   'Simpletap',
\   s:SID_PREFIX,
\   {'generate_stub': 1}
\)

function! {s:Simpletap.method('ok')}(this, cond, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if a:cond
        return s:passed(testname, 'ok')
    else
        return s:failed(testname, 'ok')
    endif
endfunction "}}}

function! {s:Simpletap.method('cmp_ok')}(this, Got, op, Expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if s:cmp(a:Got, a:op, a:Expected)
        return s:passed(testname, 'cmp_ok')
    else
        " TODO Output a:op.
        return s:failed(testname, 'cmp_ok', a:Got, a:Expected)
    endif
endfunction "}}}

function! {s:Simpletap.method('is')}(this, Got, Expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if s:equal(a:Got, a:Expected)
        return s:passed(testname, 'is')
    else
        return s:failed(testname, 'is', a:Got, a:Expected)
    endif
endfunction "}}}

function! {s:Simpletap.method('isnt')}(this, Got, Expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if !s:equal(a:Got, a:Expected)
        return s:passed(testname, 'isnt')
    else
        return s:failed(testname, 'isnt', a:Got, a:Expected)
    endif
endfunction "}}}

" }}}

call s:initialize_once()

" Restore 'cpoptions' {{{
let &cpo = s:save_cpo
" }}}
