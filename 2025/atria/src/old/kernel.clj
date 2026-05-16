(ns org.enqueue.catalyst.kernel)

(s/def dependency-conf
  (s/keys :req-un []))

(defprotocol DependencyManager
  (supported? [this] "This type of dependency can be handled.")
  (fetch [this] "Fetch a dependency."))


;; TODO: file API.  temp, peristent, etc.

;; TODO: TPM API.  jTSS?


;; Despite popular belief, DoS is not a security issue, its a reliability issue.


(defprotocol INqPath
  (real [this])
  (norm [this]))

;; Each module has its own "file system".  The "fs" objects are capabilities
;; into their region of the actual file system.
(defprotocol INqFileArena
  (acq [this file] "Attempt to acquire a capability for a file within the arena.")
  (tmp [this] "Acquire a capability for a temporary file within the arena.  Uses finalize to close and destroy it.")
  (fork [this path] "Create an arena under this one.")
  (reify [this] "Provides a reified name for this file system")
  (stats [this] "Provides stats on the arena.  E.g. storage used, file sizes, etc."))

(def nq-root
  (clojure.java.io/dir ""))

;; Munge file names such that they are cross-OS compatible.  This could be done
;; through some variant of punycode that further restricts the character set?

;; Can avoid even needing to do name munging by storing names to destination in
;; a SQLite DB or other file-based DB-like thing.
;;
;; Maybe a separate SQLite DB per-arena?  Makes sense for improved isolation
;; and data locality.  Also prevents sub-arena naming clashes, reduces
;; concurrency issues.

;; TODO: need to follow Unix symbolic links for those who wish their modules to
;; use separate physical disks or partitions.
;;
;; For this to work, arenas need to have a well-defined structure on disk.
;;
;; NQ_ROOT/arenas/
;;    com.module.foo/
;;      __sub-arenas/
;;        ...
;;
;; NQ_ROOT/state/...

;; Detect files not managed by the arena, but are still in it?

;; Auto-close SQLite connections after inactivity?
;; Include meta data on the arena itself, such as "last accessed", "created date", "catalyst version", etc.

;; Set up a suite of modules via a "system definition".
;; Should modules really get their own arena?  Maybe only root modules within a system?

;; Sqlite DB of arena + name -> some file UUID on disk.  Automatically
;; partition into subdirs like Git does for performance.

;; TODO: differing module versions from dependencies?

;; TODO: file arenas for storing dependencies?
;; TODO: file arenas for including resources?

;; TODO: custom query language for searching within an arena.

;; Kernel modules should be able to have a slightly more open version of file
;; arenas, that give more Unix-like access for use in things like dependency
;; fetching and loading.

;; TODO: temporary file arenas.

(deftype NqFileArena [real-path]
  INqFileArena
  (acq [this file]
    ))

;; The root arena, is literally just another arena.  All module arenas are sub-arenas.
(defn acq-root-arena []
  (reify INqFileArena
    (acq [this file]
         ...)
    (fork [this path]
          )
    ...))

(defprotocol INqFile
  (del [this] "Request deletion of file.")
  (name [this] "...")
  (write [this data])
  (read [this data])
  (owner [this])
  (created [this])
  (updated [this])
  (meta [this])
  (swap [this]))

(deftype NqFile
  INqFile
  IDeref
  )


;; Logs in sqlite DB?



