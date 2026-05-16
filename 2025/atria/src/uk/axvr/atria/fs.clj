(ns uk.axvr.atria.fs
  "File system abstraction.  All file system operations are to be performed by
  this namespace."
  (:refer-clojure :exclude [meta name])
  (:require [clojure.java.io :as io]
            [next.jdbc :as jdbc])
  (:import (clojure.lang IMeta IDeref IBlockingDeref)
           (java.io File)
           (java.util UUID)))

(set! *warn-on-reflection* true)
(set! *compile-path* "target/classes")

(def +root+
  "Absolute root of the real file system available to Atria."
  (doto (case File/separatorChar
          #_ unix \/ (io/file (System/getProperty "user.home") ".local" "state" "uk" "axvr" "atria")
          ;; TODO
          #_ win  \\ (throw (UnsupportedOperationException. "Windows arenas are not yet implemented.")))
    (.mkdirs)))

(def +arena-root+
  (doto (io/file +root+ "arenas") (.mkdirs)))

(defn- init-arena [path]
  (doto (jdbc/get-datasource {:dbtype "sqlite", :dbname (io/file +arena-root+ "arena.db")})
    ;; TODO: on fail, delete?
    (jdbc/execute! ["BEGIN TRANSACTION;
                     PRAGMA user_version = 1;
                     CREATE TABLE resources
                       (name TEXT PRIMARY KEY,
                        upper_uuid INTEGER, lower_uuid INTEGER,
                        media_type_ns TEXT, media_type_name TEXT)
                       STRICT, WITHOUT ROWID;
                     CREATE INDEX rsc_uuid ON resources(upper_uuid, lower_uuid);
                     COMMIT TRANSACTION;"])))

(defn- load-arena [path]
  (let [ds (jdbc/get-datasource {:dbtype "sqlite", :dbname (io/file root-arena "arena2.db")})]
    (jdbc/with-transaction [tx ds]
      ;; Schema version.
      (jdbc/execute! tx ["PRAGMA user_version;"])
                           )))

;; Name -> UUID + created, updated, owner, ...

(defn- uuid->path
  "Converts a UUID into a partitioned file path to maximise compatibility with
  file system maximum files per-directory issues.  This pattern is similar to
  that used by Git, except that we use UUIDs."
  ^File [^UUID uuid]
  (let [fname (.. uuid toString (replaceAll "-" ""))]
    (io/file (.substring fname 0 2) fname)))

(comment
  (= "6a/6ad55676d1674b66a1d0dc257728b666"
     (str (uuid->path #uuid "6ad55676-d167-4b66-a1d0-dc257728b666")))
  )


(def ds
  (jdbc/get-datasource {:dbtype "sqlite", :dbname (io/file +arena-root+ "arena.db")}))

(jdbc/execute! (init-arena nil)
               ["SELECT * FROM sqlite_master WHERE type = 'index';"])


(let [uuid #uuid "4832d9fe-7f75-4247-873b-e8537aad9320"]
  (jdbc/execute! ds ["INSERT INTO resources (id, name, upper_uuid, lower_uuid) VALUES (1, 'My file', $1, $2);"
                     (.getMostSignificantBits uuid)
                     (.getLeastSignificantBits uuid)]))
