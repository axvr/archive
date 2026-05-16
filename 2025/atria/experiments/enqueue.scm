(module 
  ...)

;; module, component, object, section
;; service, group

Modules are a type of object, but act as configurable/instantiable dependencies.

Modules have their own environment.

Dependencies are built out of modules.  Most dependencies will contain one module.

Dependencies are just folders of source files.  _mod.nq files denote a module definition at that directory level.

Dependencies are pulled via Git.

Wire modules together in _mod.nq files.

self.nq files,  this.nq files?

Enqueue prototype will be constructed atop of Clojure.

Begin with dependency fetcher and module loader.  Extend module loader to allow hot reloading and transactional loading.

Need the concept of persistent state.

Mod files will start as EDN which do basic things like define persistent state, module metadata, child modules, etc.  versioning

Part of this will become the Git dep Kernel Module.

(defprotocol KernelInterface
  (register-module []))

Load and unload Kernel modules.
Kernel modules implement an interface.

create the file API.

(fs/nq-basis)

(fs/open ...)
(fs/close ...)

file metadata.
file name munging.

(fs/open '(foo ../ bar biz.txt))

(fs/tmp "foo")

(fs/cap ...)

Need 2 file APIs, one for the Kernel and its modules and another for the language.

pathos based file paths?
[org enqueue example paths file.txt]

Each module has a personal "folder" matching its RFQDN

NQFSROOT/rsc/com/example/module/...

NQFSMODROOT

(fs/root)
(fs/mod-root)

;; uses local module namespace as
;; the root, unless an alternate root
;; capability is specified.
(fs/acquire #fs/path [foo.txt])
(fs/path '[foo.txt])
#fs/path[foo.txt]

;; difference between files and folders or unify them?

(fs/file)

(fs/find #(= "txt" (fs/ext %)))

Pass capabilities to access others.

cannot have multiple modules with the same name.

fs.path  fs.file  fs.folder

NQ_CONF
  - org.enqueue/root
  
Should the file arena be referenced by the capability to prevent GC of it?

detect unmanaged files within an arena?  no ignore them.  they shouldn't be there.  maybe provide an import option?

persistent cdb implementation atop of file arenas?

(cdb/conn (fs/acq *fa* ["cdb"]))
cdb/get
cdb/

fs/swap!  atomic file swapping?  deref to get the handle