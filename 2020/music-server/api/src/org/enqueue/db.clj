(ns org.enqueue.db
  (:require [next.jdbc :as jdbc]
            [next.jdbc.sql :as sql]
            [honeysql.core :as honey]
            ragtime.jdbc
            ragtime.repl))

(def db-spec  ; TODO: DB credentials.
  {:dbtype "sqlite"
   :dbname "resources/example.db"})

(def ds (jdbc/get-datasource db-spec))


;;;; -----------------------------------------------------------
;;;; DB interaction functions.

;; TODO: doc strings.
(def insert! (partial sql/insert! ds))
(def insert-multi! (partial sql/insert-multi! ds))
(def delete! (partial sql/delete! ds))
(def update! (partial sql/update! ds))
(def get-by-id (partial sql/get-by-id ds))
(def find-by-keys (partial sql/find-by-keys ds))
(def query (partial sql/query ds))

(defn execute! [sql]
  (jdbc/execute! ds (honey/format sql)))

(defn execute-one! [sql]
  (jdbc/execute-one! ds (honey/format sql)))

(comment
  (jdbc/execute!
    ds
    ["INSERT INTO artists (name)
      VALUES ('The Smashing Pumpkins')"])

  (insert! :artists {:name "The Smashing Pumpkins"})
  (insert! :artists {:name "Pearl Jam"})
  (query ["SELECT * FROM [artists]"])
  (execute-one! ["SELECT * FROM [artists]"]))


;;;; -----------------------------------------------------------
;;;; Migrations.

(def ragtime-config
  {:datastore (ragtime.jdbc/sql-database db-spec)
   :migrations (ragtime.jdbc/load-resources "migrations")})

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
