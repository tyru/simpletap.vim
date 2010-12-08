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


" Variables
let s:runner = {}
let s:tap = {}
let s:stat = {}

let s:PASS = 1
let s:FAIL = 2



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
    endfunction "}}}
    function! s:def_hash(varname, default) "{{{
        let v = s:varname(a:varname)
        if !exists(v)
            let {v} = {}
        endif
        call extend({v}, a:default, 'keep')
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

    call s:set_up_variables()
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

    call s:stat.step_num()

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

    call s:stat.step_num()

    return 0
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

    call s:set_up_variables()
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


function! s:set_up_variables() "{{{
    let s:runner = s:Runner.new()
    let s:tap = s:Simpletap.new()
    let s:stat = s:tap._stat
endfunction "}}}

" }}}


function! simpletap#new(...) "{{{
    return s:Simpletap.new()
endfunction "}}}


" Autoload {{{

function! simpletap#run(...) "{{{
    return call(s:runner.run, a:000, s:runner)
endfunction "}}}

function! simpletap#run_file(...) "{{{
    return call(s:runner.run_file, a:000, s:runner)
endfunction "}}}

function! simpletap#run_dir(...) "{{{
    return call(s:runner.run_dir, a:000, s:runner)
endfunction "}}}


function! simpletap#ok(...) "{{{
    return call(s:tap.ok, a:000, s:tap)
endfunction "}}}


function! simpletap#cmp_ok(...) "{{{
    return call(s:tap.cmp_ok, a:000, s:tap)
endfunction "}}}


function! simpletap#is(...) "{{{
    return call(s:tap.is, a:000, s:tap)
endfunction "}}}

function! simpletap#isnt(...) "{{{
    return call(s:tap.isnt, a:000, s:tap)
endfunction "}}}


function! simpletap#is_deeply(...) "{{{
    return call(s:tap.is_deeply, a:000, s:tap)
endfunction "}}}


function! simpletap#like(...) "{{{
    return call(s:tap.like, a:000, s:tap)
endfunction "}}}

function! simpletap#unlike(...) "{{{
    return call(s:tap.unlike, a:000, s:tap)
endfunction "}}}


function! simpletap#throws_ok(...) "{{{
    return call(s:tap.throws_ok, a:000, s:tap)
endfunction "}}}


function! simpletap#stdout_is(...) "{{{
    return call(s:tap.stdout_is, a:000, s:tap)
endfunction "}}}

function! simpletap#stdout_isnt(...) "{{{
    return call(s:tap.stdout_isnt, a:000, s:tap)
endfunction "}}}

function! simpletap#stdout_like(...) "{{{
    return call(s:tap.stdout_like, a:000, s:tap)
endfunction "}}}

function! simpletap#stdout_unlike(...) "{{{
    return call(s:tap.stdout_unlike, a:000, s:tap)
endfunction "}}}


function! simpletap#diag(...) "{{{
    return call(s:tap.diag, a:000, s:tap)
endfunction "}}}


function! simpletap#pass(...) "{{{
    return call(s:tap.pass, a:000, s:tap)
endfunction "}}}

function! simpletap#fail(...) "{{{
    return call(s:tap.fail, a:000, s:tap)
endfunction "}}}


function! simpletap#skip(...) "{{{
    return call(s:tap.skip, a:000, s:tap)
endfunction "}}}

" }}}



function! s:SID() "{{{
    return matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze_SID$')
endfunction "}}}
let s:SID_PREFIX = s:SID()
delfunc s:SID

" s:Runner {{{
let s:Runner = vice#class(
\   'Runner',
\   s:SID_PREFIX,
\   {'generate_stub': 1}
\)

function! {s:Runner.method('run')}(this, path) "{{{
    if getftype(a:path) == ''
        return
    endif
    if isdirectory(a:path)
        call simpletap#run_dir(a:path)
    else
        call simpletap#run_file(a:path)
    endif
endfunction "}}}

function! {s:Runner.method('run_file')}(this, file) "{{{
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

function! {s:Runner.method('run_dir')}(this, dir) "{{{
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

" }}}
" s:Simpletap {{{
let s:Simpletap = vice#class(
\   'Simpletap',
\   s:SID_PREFIX,
\   {'generate_stub': 1}
\)

function! {s:Simpletap.constructor()}(this) "{{{
    let a:this._stat = s:Stat.new()
endfunction "}}}



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

function! {s:Simpletap.method('is_deeply')}(this, Got, Expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if s:equal_deeply(a:Got, a:Expected)
        return s:passed(testname, 'is_deeply')
    else
        return s:failed(testname, 'is_deeply', a:Got, a:Expected)
    endif
endfunction "}}}

function! {s:Simpletap.method('like')}(this, Got, regex, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if s:like(a:Got, a:regex)
        return s:passed(testname, 'like')
    else
        return s:failed(testname, 'like', a:Got, a:regex)
    endif
endfunction "}}}

function! {s:Simpletap.method('unlike')}(this, Got, regex, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if !s:like(a:Got, a:regex)
        return s:passed(testname, 'unlike')
    else
        return s:failed(testname, 'unlike', a:Got, a:regex)
    endif
endfunction "}}}

function! {s:Simpletap.method('throws_ok')}(this, excmd, regex, ...) "{{{
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

function! {s:Simpletap.method('stdout_is')}(this, Code, Expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    let output = s:get_output(a:Code)
    if s:equal(output, a:Expected)
        return s:passed(testname, 'stdout_is')
    else
        return s:failed(testname, 'stdout_is', output, a:Expected)
    endif
endfunction "}}}

function! {s:Simpletap.method('stdout_isnt')}(this, Code, Expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    let output = s:get_output(a:Code)
    if !s:equal(output, a:Expected)
        return s:passed(testname, 'stdout_isnt')
    else
        return s:failed(testname, 'stdout_isnt', output, a:Expected)
    endif
endfunction "}}}

function! {s:Simpletap.method('stdout_like')}(this, Code, regex, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    let output = s:get_output(a:Code)
    if s:like(output, a:regex)
        return s:passed(testname, 'stdout_like')
    else
        return s:failed(testname, 'stdout_like', output, a:regex)
    endif
endfunction "}}}

function! {s:Simpletap.method('stdout_unlike')}(this, Code, regex, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    let output = s:get_output(a:Code)
    if !s:like(output, a:regex)
        return s:passed(testname, 'stdout_unlike')
    else
        return s:failed(testname, 'stdout_unlike', output, a:regex)
    endif
endfunction "}}}

function! {s:Simpletap.method('diag')}(this, ...) "{{{
    call a:this._stat.add('output_info', [g:simpletap#echohl_diag, '# ' . join(a:000)])
endfunction "}}}

function! {s:Simpletap.method('pass')}(this) "{{{
    return a:this.ok(1)
endfunction "}}}

function! {s:Simpletap.method('fail')}(this) "{{{
    return a:this.ok(0)
endfunction "}}}

function! {s:Simpletap.method('skip')}(this, ...) "{{{
    if a:0 != 0
        Diag a:1
    endif
    throw 'simpletap - SKIP'
endfunction "}}}

" }}}
" s:Stat {{{
let s:Stat = vice#class(
\   'Stat',
\   s:SID_PREFIX,
\   {'generate_stub': 1}
\)

function! {s:Stat.constructor()}(this) "{{{
    let a:this.vars = {
    \   'current_test_num': 1,
    \   'done_testing': 0,
    \   'test_result': [],
    \   'output_info': [],
    \}
    let a:this.is_locked = 0
endfunction "}}}

function! {s:Stat.method('initialize')}(this) "{{{
    for [key, val] in items(s:Stat.new().vars)
        call a:this.set(key, val)
        unlet val
    endfor
endfunction "}}}

function! {s:Stat.method('get')}(this, varname) "{{{
    return a:this.vars[a:varname]
endfunction "}}}

function! {s:Stat.method('set')}(this, varname, value) "{{{
    if a:this.is_locked | return | endif

    let a:this.vars[a:varname] = a:value
endfunction "}}}

function! {s:Stat.method('increment')}(this, varname) "{{{
    if a:this.is_locked | return | endif

    let val = a:this.get(a:varname)
    call s:assert(type(val) == type(0), 'type(val) is Number')
    call a:this.set(a:varname, val + 1)
endfunction "}}}

function! {s:Stat.method('add')}(this, varname, value) "{{{
    if a:this.is_locked | return | endif

    let list = a:this.get(a:varname)
    call s:assert(type(list) == type([]), 'type(list) is List')
    call a:this.set(a:varname, add(list, a:value))
endfunction "}}}

function! {s:Stat.method('lock')}(this) "{{{
    let a:this.is_locked = 1
endfunction "}}}

function! {s:Stat.method('unlock')}(this) "{{{
    let a:this.is_locked = 0
endfunction "}}}


function! {s:Stat.method('get_output')}(this, Code) "{{{
    call a:this.lock()

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
        call a:this.unlock()
    endtry
endfunction "}}}

function! {s:Stat.method('passed')}(this, testname, funcname) "{{{
    call a:this.add(
    \   'output_info',
    \   [
    \       g:simpletap#echohl_output,
    \       printf(
    \          '%d. %s ... %s',
    \          a:this.get('current_test_num'),
    \          a:testname,
    \          g:simpletap#pass_fmt[a:funcname]
    \       )
    \   ]
    \)

    call a:this.add('test_result', s:PASS)

    call a:this.step_num()

    return 1
endfunction "}}}

function! {s:Stat.method('failed')}(this, testname, funcname, ...) "{{{
    if a:0 == 0
        call a:this.add(
        \   'output_info',
        \   [
        \       g:simpletap#echohl_output,
        \       printf(
        \          '!!!%d. %s ... %s',
        \          a:this.get('current_test_num'),
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
        \   a:this.get('current_test_num'),
        \   a:testname,
        \   printf(
        \       g:simpletap#fail_fmt[a:funcname],
        \       s:format_to_string(Got),
        \       s:format_to_string(Expected)
        \   )
        \)
        if stridx(msg, "\n")
            for l in split(msg, '\n')
                call a:this.add('output_info', [g:simpletap#echohl_output, '!!!' . l])
            endfor
        else
            call a:this.add('output_info', [g:simpletap#echohl_output, '!!!' . msg])
        endif
    endif

    call a:this.add('test_result', s:FAIL)

    call a:this.step_num()

    return 0
endfunction "}}}

function! {s:Stat.method('step_num')}(this) "{{{
    call a:this.increment('current_test_num')
endfunction "}}}


function! {s:Stat.method('begin_test')}(this, file) "{{{
    call a:this.initialize()

    let [hl, msg] = [g:simpletap#echohl_begin, 'Testing ... ' . a:file]
    if g:simpletap#output_to ==# 'buffer'
        call s:echomsg(hl, msg)
    endif
    call a:this.add('output_info', [hl, msg])
endfunction "}}}

function! {s:Stat.method('end_test')}(this, file, skipped) "{{{
    let test_result = a:this.get('test_result')
    let failed_result_num = len(filter(copy(test_result), 'v:val ==# s:FAIL'))
    let passed_result_num = len(test_result) - failed_result_num

    if a:skipped
        call a:this.add('output_info', [g:simpletap#echohl_skip, 'Skip.'])
    elseif !a:this.get('done_testing')
        call s:warnf("test '%s' has not done properly.", a:file)
    elseif empty(test_result)
        call s:warnf("test '%s' has done but no tests performed.", a:file)
    else
        let hl = (failed_result_num ? g:simpletap#echohl_error : g:simpletap#echohl_done)
        let msg = printf('Done %d test(s). (PASS:%d, FAIL:%d)', passed_result_num + failed_result_num, passed_result_num, failed_result_num)
        call a:this.add('output_info', [hl, msg])
    endif
endfunction "}}}

function! {s:Stat.method('source')}(this, file) "{{{
    call s:begin_test(a:file)
    try
        source `=a:file`
        call s:end_test(a:file, 0)
        let results = a:this.get('test_result')
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
            call a:this.add('output_info', [g:simpletap#echohl_error, msg])
        endfor
        call a:this.add('test_result', s:FAIL)    " dummy
        return 0
    endtry
endfunction "}}}

function! {s:Stat.method('output_summary')}(this, bufnr) "{{{
    let results = copy(a:this.get('test_result'))
    let failed = !empty(filter(results, 'v:val ==# s:FAIL'))
    let output_info = a:this.get('output_info')
    if !g:simpletap#show_only_failed || g:simpletap#show_only_failed && failed
        call s:output(a:bufnr, output_info)
    endif
endfunction "}}}

function! {s:Stat.method('output_all_summary')}(this, bufnr, pass_all) "{{{
    let lines = []

    if a:pass_all
        call add(lines, ['None', ''])
        call add(lines, [g:simpletap#echohl_done, 'All test(s) passed.'])
    elseif empty(a:this.get('test_result'))
        call add(lines, ['None', ''])
        call add(lines, [g:simpletap#echohl_error, 'no tests to run.'])
    else
        return
    endif

    call s:output(a:bufnr, lines)
endfunction "}}}

" }}}



call s:initialize_once()

" Restore 'cpoptions' {{{
let &cpo = s:save_cpo
" }}}
