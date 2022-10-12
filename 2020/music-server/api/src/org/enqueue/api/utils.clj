(ns org.enqueue.api.utils
  "Utility functions used throughout the Enqueue API.")

(defmacro on-shutdown!
  "Execute body on server shutdown.  Use for clean up tasks."
  [& body]
  `(.addShutdownHook
    (Runtime/getRuntime)
    (Thread. (bound-fn [] ~@body))))
