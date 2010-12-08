" vim:foldmethod=marker:fen:
scriptencoding utf-8

" Saving 'cpoptions' {{{
let s:save_cpo = &cpo
set cpo&vim
" }}}

function! s:get_args(method) "{{{
    let args = {
    \   'ok': [1],
    \   'cmp_ok': [1, '==', 1],
    \   'is': [1, 1],
    \   'isnt': [1, 1],
    \   'is_deeply': [1, 1],
    \   'like': [1, 1],
    \   'unlike': [1, 1],
    \   'throws_ok': ['throw "error test"', '^error test$'],
    \   'stdout_is': ["echo 1", 1],
    \   'stdout_isnt': ["echo 1", 2],
    \   'stdout_like': ["echo 1", 1],
    \   'stdout_unlike': ["echo 1", 2],
    \   'pass': [],
    \   'fail': [],
    \   'diag': ['diag test'],
    \}

    if has_key(args, a:method)
        return args[a:method]
    elseif a:method ==# 'run'
        throw 'skip'
    else
        throw 'no args'
    endif
endfunction "}}}

function! s:run() "{{{
    let o = simpletap#new()

    Diag 'OO way and function way result in same result.'

    for method in sort(keys(filter(copy(o), 'type(v:val) == type(function("tr"))')))
        try
            let args = s:get_args(method)
        catch /^no args$/
            Diag 'no args:', method
            call simpletap#fail()
            continue
        catch /^skip$/
            Diag 'skip:', method
            continue
        endtry

        " methods using :redir cannot be tested
        " because these tests use :redir
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
