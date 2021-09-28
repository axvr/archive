(ns org.enqueue.api.db
  (:require [org.enqueue.api.config :as config]
            [next.jdbc :as jdbc]
            next.jdbc.date-time
            [honey.sql :as sql]
            ragtime.jdbc
            ragtime.repl))


;;; -----------------------------------------------------------
;;; DB interaction functions


(def ds (jdbc/get-datasource config/db))

(def ^:dynamic *conn* ds)


(def sql-format sql/format)
(def execute! (partial jdbc/execute! *conn*))
(def execute-one! (partial jdbc/execute-one! *conn*))


(defn query
  ([sql]
   (jdbc/execute! *conn* (sql/format sql)))
  ([sql opts]
   (jdbc/execute! *conn* (sql/format sql) opts)))


(defn query-first
  ([sql]
   (jdbc/execute-one! *conn* (sql/format sql)))
  ([sql opts]
   (jdbc/execute-one! *conn* (sql/format sql) opts)))


(defn insert! [table values]
  (query-first {:insert-into [table]
                :columns     (keys values)
                :values      [(vals values)]}))


(defn update! [table where changes]
  (query-first {:update [table]
                :set    changes
                :where  where}))


(defmacro with-transaction
  "Perform all database operations in body as a transaction."
  [& body]
  `(jdbc/with-transaction [tx# ds]
      (binding [*conn* tx#]
        ~@body)))


(defmacro with-connection
  "Perform all database operations in body using a single connection."
  [& body]
  `(with-open [conn# (jdbc/get-connection ds)]
     (binding [*conn* conn#]
       ~@body)))


;;; -----------------------------------------------------------
;;; Migrations


(def ragtime-config
  {:datastore  (ragtime.jdbc/sql-database
                 config/db
                 {:migrations-table "ragtime_migrations"})
   :migrations (ragtime.jdbc/load-resources "migrations")})


(defn migrate []
  (ragtime.repl/migrate ragtime-config))


(defn rollback []
  (ragtime.repl/rollback ragtime-config))


(comment
  (require 'org.enqueue.db)

  ;; Run migration.
  (org.enqueue.db/migrate)

  ;; Rollback migration.
  (org.enqueue.db/rollback))
