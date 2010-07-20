" vim:foldmethod=marker:fen:
scriptencoding utf-8

" Saving 'cpoptions' {{{
let s:save_cpo = &cpo
set cpo&vim
" }}}

func! s:get_methods(obj) "{{{
    return sort(keys(filter(copy(a:obj), 'type(v:val) == type(function("tr"))')))
endfunc "}}}

func! s:get_args(method) "{{{
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
    let skip = ['run']

    if has_key(args, a:method)
        return args[a:method]
    elseif s:list_has(skip, a:method)
        throw 'skip'
    else
        throw 'no args'
    endif
endfunc "}}}

func! s:list_has(list, elem) "{{{
    return !empty(filter(copy(a:list), 'v:val ==# a:elem'))
endfunc "}}}

func! s:run() "{{{
    let o = simpletap#new()

    Diag 'OO way and function way result in same result.'

    for method in s:get_methods(o)
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

        " TODO Stackable :redir
        if s:list_has(['stdout_is', 'stdout_isnt', 'stdout_like', 'stdout_unlike'], method)
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
endfunc "}}}


call s:run()
Done


" Restore 'cpoptions' {{{
let &cpo = s:save_cpo
" }}}