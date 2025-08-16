(ns hurl)

;; (hash-map)
;; (array-map)

(defprotocol CaseInsentiveKey
  (normalise [this] "Normalise a given key."))

(extend-protocol CaseInsentiveKey
  String
  (normalise [this] (.toLowerCase this))
  Keyword
  (normalise [this]
    (-> this .toString .toLowerCase (.substring 1)))
  Object
  (normalise [this] this))

;; clojure.lang.PersistentArrayMap
;; clojure.lang.PersistentHashMap
;; clojure.lang.PersistentHashSet

(gen-class
 ())

[:GET "https://example.com"
 {}]

(identical? (keyword "foo") (keyword "foo"))
