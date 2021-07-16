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


(def sql-format sql/format)
(def execute! (partial jdbc/execute! ds))
(def execute-one! (partial jdbc/execute-one! ds))


(defn query
  ([sql]
   (jdbc/execute! ds (sql/format sql)))
  ([sql opts]
   (jdbc/execute! ds (sql/format sql) opts)))


(defn query-first
  ([sql]
   (jdbc/execute-one! ds (sql/format sql)))
  ([sql opts]
   (jdbc/execute-one! ds (sql/format sql) opts)))


(defn insert! [table values]
  (query-first {:insert-into [table]
                :columns (keys values)
                :values [(vals values)]}))


(defn update! [table where changes]
  (query-first {:update [table]
                :set changes
                :where where}))


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
