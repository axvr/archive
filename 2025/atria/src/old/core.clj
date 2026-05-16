(ns org.enqueue.catalyst.core)

;; (set! *compile-path* "target/classes")
;; (alter-var-root #'*compile-path* (constantly "target/classes"))

(println "Test")

(defn run [& _]
  (println "World"))

(defn shutdown! []
  (println "Shutting down!")
  (shutdown-agents))

(.addShutdownHook (Runtime/getRuntime) (Thread. ^Runnable shutdown!))
