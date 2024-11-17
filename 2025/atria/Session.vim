set path=,,src/**,test/**,resources/**,build/**,perf/**,bench/**,wiki/**,do/**
let g:conjure#client#clojure#nrepl#connection#auto_repl#cmd = 'clojure -M:perf:repl/nrepl'
