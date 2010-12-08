" vim:foldmethod=marker:fen:
scriptencoding utf-8

" Saving 'cpoptions' {{{
let s:save_cpo = &cpo
set cpo&vim
" }}}



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

function! simpletap#util#get_local_func(pat, funcname) "{{{
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



" Restore 'cpoptions' {{{
let &cpo = s:save_cpo
" }}}
