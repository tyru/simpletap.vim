" vim:foldmethod=marker:fen:
scriptencoding utf-8

" Saving 'cpoptions' {{{
let s:save_cpo = &cpo
set cpo&vim
" }}}


function! s:locked_call(Fn, args, ...) "{{{
    if a:0 == 0
        call simpletap#_stat_lock()
        try
            return call(a:Fn, a:args)
        finally
            call simpletap#_stat_unlock()
        endtry
    else
        let obj = a:1
        call obj._stat.lock()
        try
            return call(a:Fn, a:args, obj)
        finally
            call obj._stat.unlock()
        endtry
    endif
endfunction "}}}

function! s:run() "{{{
    let o = simpletap#new()

    for method in sort(keys(filter(copy(o), 'type(v:val) == type(function("tr"))')))
        " Skip some methods.
        if method ==# 'run'
        \   || method ==# 'run_file'
        \   || method ==# 'run_dir'
        \   || method ==# 'skip'
            Diag 'skip:', method
            continue
        endif

        " Get the methods' arguments for the test.
        let not_found = []
        let args = get({
        \   'ok': [1],
        \   'cmp_ok': [1, '==', 1],
        \   'is': [1, 1],
        \   'isnt': [1, 1],
        \   'is_deeply': [1, 1],
        \   'like': [1, 1],
        \   'unlike': [1, 1],
        \   'throws_ok': ['throw "error test"', '^error test$'],
        \   'stdout_is': ["echo 1", '1'],
        \   'stdout_isnt': ["echo 1", '2'],
        \   'stdout_like': ["echo 1", '1'],
        \   'stdout_unlike': ["echo 1", '2'],
        \   'pass': [],
        \   'fail': [],
        \   'diag': ['diag test'],
        \   'done': [],
        \}, method, not_found)
        if args is not_found
            Diag "unknown method: " . method
            call simpletap#fail()
            continue
        endif

        Diag method

        " Methods using :redir cannot be tested.
        " Because these tests use :redir .
        " TODO Stackable :redir
        if method ==# 'stdout_is'
        \ || method ==# 'stdout_isnt'
        \ || method ==# 'stdout_like'
        \ || method ==# 'stdout_unlike'
            silent let got = s:locked_call(o[method], args, o)
            silent let expected = s:locked_call('simpletap#' . method, args)

            Is got, expected, method
        else
            redir => got_output
                silent let got = s:locked_call(o[method], args, o)
            redir END
            redir => expected_output
                silent let expected = s:locked_call('simpletap#' . method, args)
            redir END

            Is got, expected, method
            Is got_output, expected_output, method
        endif
    endfor
endfunction "}}}


call s:run()
Done


" Restore 'cpoptions' {{{
let &cpo = s:save_cpo
" }}}
