(ns org.enqueue.helpers)

(defn splat-apply
  "Like 'apply' but expands sequence arguments.
  (splat-apply `-> 1 [inc inc inc dec]) ; -> 3"
  [& fls]
  (eval (flatten fls)))
