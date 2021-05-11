(ns org.enqueue.db
  (:require [next.jdbc :as jdbc]
            next.jdbc.date-time
            [honey.sql :as sql]
            ragtime.jdbc
            ragtime.repl))


;; TODO: Get per-environment settings from EDN.
(def db-spec
  {:dbtype "postgresql"
   :dbname "enqueue"
   :host "localhost"
   :user "postgres"
   :password "postgres"})


(def ds (jdbc/get-datasource db-spec))


;;; -----------------------------------------------------------
;;; DB interaction functions


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


(defn insert [table values]
  (query-first {:insert-into [table]
                :columns (keys values)
                :values [(vals values)]}))


;;; -----------------------------------------------------------
;;; Migrations


(def ragtime-config
  {:datastore  (ragtime.jdbc/sql-database db-spec)
   :migrations (ragtime.jdbc/load-resources "migrations")})
;; NOTE: default migration history table is "ragtime_migrations"


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
