(ns org.enqueue.api.db
  (:require [org.enqueue.api.config :as config]
            [org.enqueue.api.utils  :as u]
            [uk.axvr.refrain        :as r]
            [honey.sql              :as sql]
            [next.jdbc              :as jdbc]
            [next.jdbc.connection   :as conn]
            [next.jdbc.plan         :as jdbc-plan]
            next.jdbc.date-time
            ragtime.jdbc
            ragtime.repl)
  (:import [com.zaxxer.hikari HikariDataSource]))

;;; -----------------------------------------------------------
;;; DB connection pool

(defn- init-db-conn [conf]
  (let [^HikariDataSource ds
        (conn/->pool HikariDataSource
                     ;; HikariCP uses `:username` instead of `:user`.
                     (assoc conf :username (:user conf)))]
    ;; Initialise the pool and validate config.
    (when-not config/test?
      (.close (jdbc/get-connection ds)))
    ;; Close the connection pool on JVM shutdown.
    (u/on-shutdown! (when ds (.close ds)))
    ds))

(defonce data-source (init-db-conn config/db))
(defonce ^:dynamic *conn* data-source)

;;; -----------------------------------------------------------
;;; DB interaction

(defmacro ^:private def-db-fn
  "Macro that generates a wrapper function around a next.jdbc function to
  automatically inject the DB connection source and compile HoneySQL."
  [fn-name fn-base]
  `(let [meta# (meta #'~fn-base)
         args# (:arglists meta#)
         ;; Get index that HoneySQL parameter may be passed in at.
         sql-idx# (some #(let [i# (.indexOf % (symbol "sql-params"))]
                           (when-not (neg? i#) (dec i#)))
                        args#)
         fqn# (str (:ns meta#) "/" (:name meta#))]
     (defn ~fn-name
       {:doc (str "\n  This is a wrapper function around `" fqn# "` that automatically"
                  "\n  injects the DB connection source and compiles HoneySQL expressions."
                  "\n\n-------------------------\n"
                  fqn# "\n" args# "\n\n  " (:doc meta#))
        :arglists (->> args#
                       (filter #(r/in? % (symbol "connectable")))
                       (map (comp vec rest)))}
       [& params#]
       (apply
        ~fn-base
        *conn*
        (if sql-idx#
           ;; If HoneySQL was passed in, compile it.
          (update (vec params#) sql-idx# #(if (map? %) (sql/format %) %))
          params#)))))

(def-db-fn exec! jdbc/execute!)
(def-db-fn exec1! jdbc/execute-one!)
(def-db-fn plan jdbc/plan)
(def-db-fn select! jdbc-plan/select!)
(def-db-fn select1! jdbc-plan/select-one!)

(defn insert!
  "Insert a record into the specified DB table."
  [table data]
  (exec1! {:insert-into [table]
           :columns     (keys data)
           :values      [(vals data)]}))

(defn update!
  "Update values in the specified DB table."
  [table where changes]
  (exec1! {:update [table]
           :set    changes
           :where  where}))

(defn delete!
  "Delete values in the specified DB table."
  [table where]
  (exec1! {:delete-from [table]
           :where       where}))

(defmacro with-transaction
  "Perform all database operations in body as a transaction.

  If the first parameter is a map (and more than 1 param was given), it will be
  passed as options to `next.jdbc/with-transaction`."
  [& body]
  (let [[opts body] (r/macro-body-opts body)]
    `(jdbc/with-transaction [tx# data-source ~opts]
       (binding [*conn* tx#]
         ~@body))))

(defmacro with-connection
  "Perform all database operations in body using a single connection.

  If the first parameter is a map (and more than 1 param was given), it will be
  passed as options to `next.jdbc/get-connection`."
  [& body]
  (let [[opts body] (r/macro-body-opts body)]
    `(with-open [conn# (jdbc/get-connection data-source ~opts)]
       (binding [*conn* conn#]
         ~@body))))

;;; -----------------------------------------------------------
;;; Migrations

(def ^:private ragtime-config
  {:datastore  (ragtime.jdbc/sql-database config/db
                                          {:migrations-table "ragtime_migrations"})
   :migrations (ragtime.jdbc/load-resources "migrations")})

(defn migrate [& _]
  (ragtime.repl/migrate ragtime-config))

(defn rollback [& _]
  (ragtime.repl/rollback ragtime-config))
