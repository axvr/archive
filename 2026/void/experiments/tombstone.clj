(ns tombstone)

(deftype Tombstone [])
(defonce void (Tombstone.))

(defprotocol Tombstoneable
  (update ))

(deftype TombstoneableMap [map]
  Tombstoneable
  )

(defmacro ->map [[k v] & kvs]
  `(...))

(defmacro ->vec [& elems]
  `(TombstoneableVec. ~elems))
