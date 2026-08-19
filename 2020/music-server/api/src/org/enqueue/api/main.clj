(ns org.enqueue.api.main 
  (:require [clojure.main :as main]
            [org.enqueue.api.config :as config]
            [org.enqueue.api.server :as server]))

(defn- start-nrepl-server!
  "Start an nREPL server if the required dependencies are in the class."
  []
  (if (try
        (require 'org.enqueue.api.nrepl)
        true
        (catch Exception _ false))
    (let [start! (ns-resolve 'org.enqueue.api.nrepl 'start!)]
      (start! {}))
    (throw (ex-info "Cannot start nREPL server, API not started with `:nrepl` alias." {}))))

(defn run
  ([]
   (run {}))
  ([{:keys [nrepl? server?]
     :or {server? true}
     :as opts}]
   (when config/prod?
     ;; Disable assertions on production environment.
     (set! *assert* false))
   (when nrepl?
     ;; Start local nREPL server for editors to connect to.
     (start-nrepl-server!))
   (if server?
     (server/start! opts)
     (main/repl))))

(defn -main
  "Main entrypoint used for editor nREPL jack-in.  Not intended to be used from
   the CLI."
  [& _args]
  (run {:nrepl?  true
        :server? false}))
