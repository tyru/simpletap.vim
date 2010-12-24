" vim:foldmethod=marker:fen:sw=4:sts=4
scriptencoding utf-8

" Saving 'cpoptions' {{{
let s:save_cpo = &cpo
set cpo&vim
" }}}


" Current instances of each class.
let s:runner = {}
let s:tap = {}
let s:stat = {}

" Test status
let [s:PASS, s:FAIL] = range(2)



" Interface {{{

function! simpletap#load() "{{{
    runtime! plugin/simpletap.vim
endfunction "}}}


function! simpletap#new(...) "{{{
    return s:Simpletap.new()
endfunction "}}}



function! simpletap#run(...) "{{{
    return call(s:runner.run, a:000, s:runner)
endfunction "}}}

function! simpletap#run_file(...) "{{{
    return call(s:runner.run_file, a:000, s:runner)
endfunction "}}}

function! simpletap#run_dir(...) "{{{
    return call(s:runner.run_dir, a:000, s:runner)
endfunction "}}}

function! simpletap#run_single_test(...) "{{{
    return call(s:runner.run_single_test, a:000, s:runner)
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

function! simpletap#done(...) "{{{
    return call(s:tap.done, a:000, s:tap)
endfunction "}}}



function! simpletap#_stat_lock() "{{{
    call s:stat.lock()
endfunction "}}}

function! simpletap#_stat_unlock() "{{{
    call s:stat.unlock()
endfunction "}}}

" }}}


" Implementation {{{



" Utilities

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

function! s:format_to_string(val) "{{{
    " If installed prettyprint.vim:
    "   https://github.com/thinca/vim-prettyprint

    if exists('*PrettyPrint')
        return PrettyPrint(a:val)
    else
        return string(a:val)
    endif
endfunction "}}}



" Equality check functions

function! s:equal(L, R) "{{{
    return type(a:L) == type(a:R)
    \   && type(a:L) != type({})
    \   && type(a:L) != type([])
    \   && type(a:R) != type({})
    \   && type(a:R) != type([])
    \   && a:L ==# a:R
endfunction "}}}

function! s:equal_deeply(L, R) "{{{
    return type(a:L) == type(a:R)
    \   && a:L ==# a:R
endfunction "}}}

function! s:like(Got, regex) "{{{
    return type(a:Got) == type("")
    \   && type(a:regex) == type("")
    \   && a:Got =~# a:regex
endfunction "}}}

function! s:cmp(L, op, R) "{{{
    return eval(printf('a:L %s a:R', a:op))
endfunction "}}}


" NOTE: This changes s: scope variables.
function! s:set_up_variables() "{{{
    let s:runner = s:Runner.new()
    let s:tap = s:Simpletap.new()
    let s:stat = s:tap._stat
endfunction "}}}



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

    " Create buffer.
    if a:this.create_buffer() < 0
        return
    endif

    call a:this.define_commands()
    call s:stat.begin_test(file)
    let passed = s:stat.source(file)
    call s:stat.end_test(file)
    call s:stat.output_summary()
    call a:this.delete_commands()
    call s:stat.output_all_summary(passed)
endfunction "}}}

function! {s:Runner.method('run_dir')}(this, dir) "{{{
    let dir = expand(a:dir)
    if !isdirectory(dir)
        call s:warnf("'%s' is not directory.", dir)
        return
    endif
    let pat = dir . '/' . (g:simpletap#recursive ? '**/*.vim' : '*.vim')

    " Create buffer.
    if a:this.create_buffer() < 0
        return
    endif

    call a:this.define_commands()
    let pass_all = 1
    for t in s:glob(pat)
        call s:stat.begin_test(t)
        if !s:stat.source(t)
            let pass_all = 0
        endif
        call s:stat.end_test(t)
        call s:stat.output_summary()
    endfor
    call a:this.delete_commands()
    call s:stat.output_all_summary(pass_all)
endfunction "}}}

function! {s:Runner.method('run_single_test')}(this) "{{{
    if s:runner.create_buffer() < 0
        return
    endif
    call s:runner.define_commands()
    call s:stat.set('running_single_test', 1)
endfunction "}}}


function! {s:Runner.method('create_buffer')}(this) "{{{
    try
        execute g:simpletap#open_command
    catch
        return -1
    endtry

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

function! {s:Runner.method('define_commands')}(this) "{{{
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
    \   call simpletap#done()


    call s:set_up_variables()
endfunction "}}}

function! {s:Runner.method('delete_commands')}(this) "{{{
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



function! {s:Simpletap.method('ok')}(this, Cond, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if a:Cond
        return a:this._stat.passed(testname, 'ok')
    else
        return a:this._stat.failed(testname, 'ok')
    endif
endfunction "}}}

function! {s:Simpletap.method('cmp_ok')}(this, Got, op, Expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if s:cmp(a:Got, a:op, a:Expected)
        return a:this._stat.passed(testname, 'cmp_ok')
    else
        " TODO Output a:op.
        return a:this._stat.failed(testname, 'cmp_ok', a:Got, a:Expected)
    endif
endfunction "}}}

function! {s:Simpletap.method('is')}(this, Got, Expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if s:equal(a:Got, a:Expected)
        return a:this._stat.passed(testname, 'is')
    else
        return a:this._stat.failed(testname, 'is', a:Got, a:Expected)
    endif
endfunction "}}}

function! {s:Simpletap.method('isnt')}(this, Got, Expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if !s:equal(a:Got, a:Expected)
        return a:this._stat.passed(testname, 'isnt')
    else
        return a:this._stat.failed(testname, 'isnt', a:Got, a:Expected)
    endif
endfunction "}}}

function! {s:Simpletap.method('is_deeply')}(this, Got, Expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if s:equal_deeply(a:Got, a:Expected)
        return a:this._stat.passed(testname, 'is_deeply')
    else
        return a:this._stat.failed(testname, 'is_deeply', a:Got, a:Expected)
    endif
endfunction "}}}

function! {s:Simpletap.method('like')}(this, Got, regex, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if s:like(a:Got, a:regex)
        return a:this._stat.passed(testname, 'like')
    else
        return a:this._stat.failed(testname, 'like', a:Got, a:regex)
    endif
endfunction "}}}

function! {s:Simpletap.method('unlike')}(this, Got, regex, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    if !s:like(a:Got, a:regex)
        return a:this._stat.passed(testname, 'unlike')
    else
        return a:this._stat.failed(testname, 'unlike', a:Got, a:regex)
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
        return a:this._stat.passed(testname, 'throws_ok')
    else
        return a:this._stat.failed(testname, 'throws_ok', ex, a:regex)
    endif
endfunction "}}}

function! {s:Simpletap.method('stdout_is')}(this, Code, Expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    let output = a:this._stat.get_output(a:Code)
    if s:equal(output, a:Expected)
        return a:this._stat.passed(testname, 'stdout_is')
    else
        return a:this._stat.failed(testname, 'stdout_is', output, a:Expected)
    endif
endfunction "}}}

function! {s:Simpletap.method('stdout_isnt')}(this, Code, Expected, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    let output = a:this._stat.get_output(a:Code)
    if !s:equal(output, a:Expected)
        return a:this._stat.passed(testname, 'stdout_isnt')
    else
        return a:this._stat.failed(testname, 'stdout_isnt', output, a:Expected)
    endif
endfunction "}}}

function! {s:Simpletap.method('stdout_like')}(this, Code, regex, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    let output = a:this._stat.get_output(a:Code)
    if s:like(output, a:regex)
        return a:this._stat.passed(testname, 'stdout_like')
    else
        return a:this._stat.failed(testname, 'stdout_like', output, a:regex)
    endif
endfunction "}}}

function! {s:Simpletap.method('stdout_unlike')}(this, Code, regex, ...) "{{{
    let testname = a:0 != 0 ? a:1 : ''

    let output = a:this._stat.get_output(a:Code)
    if !s:like(output, a:regex)
        return a:this._stat.passed(testname, 'stdout_unlike')
    else
        return a:this._stat.failed(testname, 'stdout_unlike', output, a:regex)
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

function! {s:Simpletap.method('done')}(this) "{{{
    call s:stat.set('done_testing', 1)

    if s:stat.get('running_single_test')
        call s:stat.set('running_single_test', 0)

        call s:stat.output_summary()
        call s:runner.delete_commands()

        call s:stat.output_all_summary(s:stat.passed_all())
    endif
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
    \   'test_results': [],
    \   'output_info': [],
    \   'skipped': 0,
    \   'running_single_test': 0,
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
    return copy(a:this.vars[a:varname])
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

        silent execute ex
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

    call a:this.add('test_results', s:PASS)

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

    call a:this.add('test_results', s:FAIL)

    call a:this.step_num()

    return 0
endfunction "}}}

function! {s:Stat.method('step_num')}(this) "{{{
    call a:this.increment('current_test_num')
endfunction "}}}


function! {s:Stat.method('begin_test')}(this, file) "{{{
    call a:this.initialize()

    let [hl, msg] = [g:simpletap#echohl_begin, 'Testing ... ' . a:file]
    call s:echomsg(hl, msg)
    call a:this.add('output_info', [hl, msg])
endfunction "}}}

function! {s:Stat.method('end_test')}(this, file) "{{{
    let test_results = a:this.get('test_results')
    let failed_result_num = len(filter(copy(test_results), 'v:val ==# s:FAIL'))
    let passed_result_num = len(test_results) - failed_result_num

    if s:stat.get('skipped')
        call a:this.add('output_info', [g:simpletap#echohl_skip, 'Skip.'])
    elseif !a:this.get('done_testing')
        call s:warnf("test '%s' has not done properly.", a:file)
    elseif empty(test_results)
        call s:warnf("test '%s' has done but no tests performed.", a:file)
    else
        let hl = (failed_result_num ? g:simpletap#echohl_error : g:simpletap#echohl_done)
        let msg = printf('Done %d test(s). (PASS:%d, FAIL:%d)', passed_result_num + failed_result_num, passed_result_num, failed_result_num)
        call a:this.add('output_info', [hl, msg])
    endif
endfunction "}}}

function! {s:Stat.method('source')}(this, file) "{{{
    try
        source `=a:file`
        let results = a:this.get('test_results')
        let failed = !empty(filter(copy(results), 'v:val ==# s:FAIL'))
        return failed ? 0 : 1
    catch /^simpletap - SKIP$/
        call s:stat.set('skipped', 1)
        return 1
    catch
        for msg in [
        \   '!!!# Exception throwed.',
        \   '!!!# v:exception = ' . string(v:exception),
        \   '!!!# v:throwpoint = ' . string(v:throwpoint),
        \]
            call a:this.add('output_info', [g:simpletap#echohl_error, msg])
        endfor
        call a:this.add('test_results', s:FAIL)    " dummy
        return 0
    endtry
endfunction "}}}

function! {s:Stat.method('output')}(this, lines) "{{{
    " Assumption: current buffer is output buffer.
    call setline(line('$'), map(copy(a:lines), 'v:val[1]'))
endfunction "}}}

function! {s:Stat.method('output_summary')}(this) "{{{
    let results = copy(a:this.get('test_results'))
    let failed = !empty(filter(results, 'v:val ==# s:FAIL'))
    let output_info = a:this.get('output_info')
    if !g:simpletap#show_only_failed || g:simpletap#show_only_failed && failed
        call a:this.output(output_info)
    endif
endfunction "}}}

function! {s:Stat.method('output_all_summary')}(this, pass_all) "{{{
    let lines = []

    if a:pass_all
        call add(lines, ['None', ''])
        call add(lines, [g:simpletap#echohl_done, 'All test(s) passed.'])
    elseif empty(a:this.get('test_results'))
        call add(lines, ['None', ''])
        call add(lines, [g:simpletap#echohl_error, 'no tests to run.'])
    else
        return
    endif

    call a:this.output(lines)
endfunction "}}}

function! {s:Stat.method('passed_all')}(this) "{{{
    for r in s:stat.get('test_results')
        if r ==# s:FAIL
            return 0
        endif
    endfor
    return 1
endfunction "}}}

" }}}

" }}}


function! s:initialize() "{{{
    call simpletap#load()

    function! s:def(varname, default) "{{{
        if !exists(a:varname)
            let {a:varname} = a:default
        endif
    endfunction "}}}
    function! s:def_hash(varname, default) "{{{
        if !exists(a:varname)
            let {a:varname} = {}
        endif
        call extend({a:varname}, a:default, 'keep')
    endfunction "}}}

    call s:def_hash(
    \   'g:simpletap#pass_fmt',
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
    \   'g:simpletap#fail_fmt',
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
    call s:def('g:simpletap#echohl_diag', 'Comment')
    call s:def('g:simpletap#echohl_error', 'WarningMsg')
    call s:def('g:simpletap#echohl_begin', 'None')
    call s:def('g:simpletap#echohl_done', 'Underlined')
    call s:def('g:simpletap#echohl_skip', 'Underlined')
    call s:def('g:simpletap#echohl_output', 'None')
    call s:def('g:simpletap#recursive', 1)
    call s:def('g:simpletap#show_only_failed', 1)
    call s:def('g:simpletap#open_command', 'new')

    delfunc s:def
    delfunc s:def_hash

    call s:set_up_variables()
endfunction "}}}
call s:initialize()


" Restore 'cpoptions' {{{
let &cpo = s:save_cpo
" }}}
