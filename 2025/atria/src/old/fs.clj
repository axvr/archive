(ns org.enqueue.catalyst.fs
  (:refer-clojure :exclude [meta name])
  (:require [clojure.java.io :as io]
            [next.jdbc :as jdbc]
            [uk.axvr.refrain :as r])
  (:import (clojure.lang IMeta IDeref IBlockingDeref)
           (java.io File)
           (java.util UUID)))

(set! *warn-on-reflection* true)
(set! *compile-path* "target/classes")

;; Java's NIO "FileSystem" and "FileStore" abstractions are too complex, so
;; here we build our own that is better suited for Catalyst.

;; $ORG_ENQUEUE_CATALYST_ROOT
;;   /arenas
;;     /

(def root
  (doto (case File/separatorChar
          #_ unix \/ (io/file (System/getProperty "user.home") ".local" "state" "org" "enqueue" "catalyst")
          ;; TODO
          #_ win)
    (.mkdirs)))

(def root-arena
  (doto (io/file root "arenas") (.mkdirs)))


(def ds
  (jdbc/get-datasource {:dbtype "sqlite", :dbname (io/file root-arena "arena.db")}))

(defn- init-arena [path]
  (doto (jdbc/get-datasource {:dbtype "sqlite", :dbname (io/file root-arena "arena.db")})
    ;; TODO: on fail, delete?
    (jdbc/execute! ["BEGIN TRANSACTION;
                     PRAGMA user_version = 1;
                     CREATE TABLE resources
                       (id INTEGER PRIMARY KEY,
                        name STRING,
                        upper_uuid INTEGER, lower_uuid INTEGER,
                        media_type_ns STRING, media_type_name STRING);
                     CREATE INDEX rsc_uuid ON resources(upper_uuid, lower_uuid);
                     CREATE INDEX rsc_name ON resources(name);
                     COMMIT TRANSACTION;"])))

(defn- load-arena [path]
  (let [ds (jdbc/get-datasource {:dbtype "sqlite", :dbname (io/file root-arena "arena2.db")})]
    (jdbc/with-transaction [tx ds]
      ;; Schema version.
      (jdbc/execute! tx ["PRAGMA user_version;"])
                           )))

(load-arena nil)

(jdbc/execute! (init-arena nil)
               ["SELECT * FROM sqlite_master WHERE type = 'index';"])

(jdbc/execute! ds ["DROP TABLE resources;"])
(jdbc/execute! ds ["CREATE TABLE resources
                     (id INTEGER PRIMARY KEY,
                      name STRING,
                      upper_uuid INTEGER, lower_uuid INTEGER,
                      media_type_ns STRING, media_type_name STRING);"])
(jdbc/execute! ds ["CREATE INDEX rsc_uuid_idx ON resources(upper_uuid, lower_uuid);"])
(jdbc/execute! ds ["CREATE INDEX rsc_name ON resources(name);"])

(jdbc/execute! ds ["SELECT * FROM sqlite_master WHERE type = 'index';"])

(let [uuid #uuid "4832d9fe-7f75-4247-873b-e8537aad9320"]
  (jdbc/execute! ds ["INSERT INTO resources (id, name, upper_uuid, lower_uuid) VALUES (1, 'My file', $1, $2);"
                     (.getMostSignificantBits uuid)
                     (.getLeastSignificantBits uuid)]))

(mapv
 (fn [{:as rsc, :keys [:resources/upper_uuid :resources/lower_uuid]}]
   (-> rsc
       (assoc :uuid (UUID. upper_uuid lower_uuid))))
 (jdbc/execute! ds ["SELECT * FROM resources;"]))


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

(defn- acq-arena [^File root name]
  (let [path (. root ...)]))

(defprotocol IArena
  (acq [this rsc-name write?]
       "Attempt to acquire a capability for a file resource from the arena.")
  (tmp [this]
       "Acquire a capability for a temporary file resource within the arena.")
  (reg [this]
       "Return a register of resources within the arena.")
  (fork [this name]
        "Create a sub-arena from this one.")
  (wipe [this]
        "Destroy the given arena."))

(defn- raw->rsc [raw]
  (let [uuid (UUID. (:resources/upper_uuid raw) (:resources/lower_uuid raw))
        media-type (r/when-let* [ns (:resources/media_type_ns raw)
                                 name (:resources/media_type_name raw)]
                                (keyword ns name))]
    {:uuid uuid
     :name (:name raw)
     :path (uuid->path uuid)
     :media-type media-type}))

;; TODO: Replace name with name + namespace?

(deftype Arena [j-file db write?]
  ;; TODO: store a weak reference to the connection to temporary persist connections for performance.
  IArena
  (acq [this rsc-name write?]
       (-> db
           (jdbc/execute-one! ["SELECT * FROM resources WHERE name = $1;" rsc-name])
           raw->rsc))
  (reg [this]
       (-> db
           (jdbc/execute! ["SELECT name, media_type_ns, media_type_name FROM resources;"])))
  ;; (fork [this name]
  ;;       (->Arena ...))
  IMeta
  (meta [this]
        {:created 1
         :updated (.lastModified j-file)  ; TODO: convert to Instant.
         #_:size :used-space (.getTotalSpace j-file)
         #_:max-size :total-space (.getUsableSpace j-file)})
  )

;; (clojure.java.javadoc/javadoc java.io.File)

(defprotocol IResource
  (del [this] "Request deletion of file resource.")
  (name [this] "Get the name of the file resource.")
  )

(deftype Resource [arena name write?]
  IResource
  IMeta
  (meta [this] {:created 1, :updated 2, :media-type})
  IDeref
  IBlockingDeref)
