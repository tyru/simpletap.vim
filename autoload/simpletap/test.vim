" vim:foldmethod=marker:fen:
scriptencoding utf-8

" Saving 'cpoptions' {{{
let s:save_cpo = &cpo
set cpo&vim
" }}}



function! simpletap#test#really_exists_func(Fn, ...) "{{{
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
    catch            " Other error
        return 1
    endtry
endfunction "}}}

function! simpletap#test#get_local_func(pat, funcname) "{{{
    redir => out
    silent scriptnames
    redir END

    for line in split(out, '\n')
        let sid = matchstr(line, '^\s*\zs\d\+\ze:'.'\C')
        if line =~# a:pat && sid != ''
            return printf('<SNR>%d_%s', sid, a:funcname)
        endif
    endfor

    return ''
endfunction "}}}



" for the test of simpletap itself.

function! simpletap#test#locked_call(...) "{{{
    return call('s:locked_call', a:000)
endfunction "}}}

function! simpletap#test#locked_call_silent(...) "{{{
    silent return call('s:locked_call', a:000)
endfunction "}}}

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



" Restore 'cpoptions' {{{
let &cpo = s:save_cpo
" }}}
