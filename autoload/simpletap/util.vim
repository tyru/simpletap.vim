" vim:foldmethod=marker:fen:
scriptencoding utf-8

" Saving 'cpoptions' {{{
let s:save_cpo = &cpo
set cpo&vim
" }}}

" Functions {{{

" TODO OO interface


function! simpletap#util#really_exists_func(Fn, ...) "{{{
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


function! simpletap#util#locked_call(...) "{{{
    return call('s:locked_call', a:000)
endfunction "}}}

function! simpletap#util#locked_call_silent(...) "{{{
    silent return call('s:locked_call', a:000)
endfunction "}}}

function! s:locked_call(Fn, args, ...) "{{{
    if a:0 == 0
        StatLock
        try
            return call(a:Fn, a:args)
        finally
            StatUnlock
        endtry
    else
        let obj = a:1
        call obj.stat.lock()
        try
            return call(a:Fn, a:args, obj)
        finally
            call obj.stat.unlock()
        endtry
    endif
endfunction "}}}

" }}}

" Restore 'cpoptions' {{{
let &cpo = s:save_cpo
" }}}
