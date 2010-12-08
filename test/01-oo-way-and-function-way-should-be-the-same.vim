" vim:foldmethod=marker:fen:
scriptencoding utf-8

" Saving 'cpoptions' {{{
let s:save_cpo = &cpo
set cpo&vim
" }}}


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
        \}, method, not_found)
        if args is not_found
            Diag "unknown method: " . method
            call simpletap#fail()
            continue
        endif

        " Methods using :redir cannot be tested.
        " Because these tests use :redir .
        " TODO Stackable :redir
        if method ==# 'stdout_is'
        \ || method ==# 'stdout_isnt'
        \ || method ==# 'stdout_like'
        \ || method ==# 'stdout_unlike'
            let got = simpletap#util#locked_call_silent(o[method], args, o)
            let expected = simpletap#util#locked_call_silent('simpletap#' . method, args)

            Is got, expected, method
        else
            redir => got_output
                let got = simpletap#util#locked_call_silent(o[method], args, o)
            redir END
            redir => expected_output
                let expected = simpletap#util#locked_call_silent('simpletap#' . method, args)
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
