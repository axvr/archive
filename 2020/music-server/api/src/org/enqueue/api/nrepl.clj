(ns org.enqueue.api.nrepl
  (:require cider.nrepl
            [clojure.java.io :as io]
            [nrepl.server :as nrepl]
            [org.enqueue.api.utils :as u]))

;; agent or ref?
(defonce server2 (agent nil))

(defonce server (atom nil))

(defn start! [opts]
  (when @server
    (throw (ex-info "nREPL server already running." @server)))
  (let [middleware (or (:middleware opts) cider.nrepl/cider-middleware)
        opts       (update opts :handler #(or % (apply nrepl/default-handler middleware)))
        port-file  (io/file ".nrepl-port")]
    (swap! server
           (fn [_]
             (let [server (nrepl/start-server opts)]
               (spit port-file (:port server))
               (println "[INFO] Started nREPL server on port" (:port server))
               (u/on-shutdown!
                (when server (nrepl/stop-server server))
                (when (.exists port-file) (.delete port-file)))
               server)))))

(defn stop! []
  (swap! server nrepl/stop-server))
