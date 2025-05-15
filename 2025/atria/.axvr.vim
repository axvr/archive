set path=,,do/**,wiki/**,src/**,test/**,resources/**,devel/**,build/**,bench/**
let g:conjure#client#clojure#nrepl#connection#auto_repl#cmd = 'do/clj -M:devel:repl/nrepl'
