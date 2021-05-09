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


(comment
  (jdbc/execute!
    ds
    ["INSERT INTO artists (name)
      VALUES ('The Smashing Pumpkins')"])

  (insert! :artists {:name "The Smashing Pumpkins"})
  (insert! :artists {:name "Pearl Jam"})
  (query ["SELECT * FROM [artists]"])
  (execute-one! ["SELECT * FROM [artists]"]))


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
  ;; Execute migration.
  (require 'org.enqueue.db)
  (org.enqueue.db/migrate)

  ;; Rollback migration.
  (require 'org.enqueue.db)
  (org.enqueue.db/rollback))
