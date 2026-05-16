let g:conjure#client#clojure#nrepl#connection#auto_repl#cmd = 'do/clj -M:dev:repl/nrepl'
autocmd! BufRead,BufNewFile * set path=,,do/**,wiki/**,src/**,test/**,resources/**,dev/**,build/**,bench/**
