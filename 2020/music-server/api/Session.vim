set path=,,src/**,resources/**,test/**

augroup repl_close
    autocmd!
    autocmd ExitPre * silent! call zepl#send("\<CR>\<C-d>\<CR>", 1)
augroup END

if !filereadable('.clj_port')
    echo 'Starting server and REPL...'
    let buf = term_start('clj-socket -X:axvr:dev:run', {
                \   'term_name': 'Server',
                \   'term_kill': 'int',
                \   'norestore': 1,
                \   'hidden': 1
                \ })
    call term_wait(buf, 4000)
else
    echo 'Starting REPL...'
endif

if winwidth('%') < 200
    16 Repl clj-socket
else
    botright vert 80 Repl clj-socket
endif

call clojure#ChangeNs('org.enqueue.api.core')
sleep 2
ReplClear
ReplSend ;; Note: server may still be starting.
wincmd p
edit src/org/enqueue/api/core.clj
