set path=,,src/**,resources/**,test/**

augroup repl_close
    autocmd!
    autocmd ExitPre * silent! call zepl#send("\<CR>\<C-d>\<CR>", 1) | sleep 20m
augroup END

if !filereadable('.clj_port')
    echohl ErrorMsg
    echom 'REPL server has not been started...'
    echohl NONE
else
    if empty(execute('args'))
        edit src/org/enqueue/api/core.clj
    endif

    if winwidth('%') < 160
        keep 12 Repl clj-socket
    else
        keep botright vertical 80 Repl clj-socket
    endif

    if bufname('%') =~# '.clj$'
        call clojure#ChangeNs()
        sleep 20m
        ReplClear
    endif
endif
