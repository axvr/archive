;; nil and void

(defn assoc-some [m k v]
  (if (and k v)
    (assoc m k v)
    m))

(defn update-user [user]
  (-> {:username (:username user)}
      (assoc-some :email-addr (:email user))
      (assoc-some :first-name (get-in user [:name :first]))
      http/patch))

(defn update-user [user]
  (-> #v/voidable
      {:username   (:username user)
       :email-addr (:email user v/VOID)
       :first-name (get-in user [:name :first] v/VOID)}
      http/patch))

(defn update-user [user]
  (-> #voidable
      {:username   (:username user)
       :email-addr #void (:email user)
       :first-name #void (get-in user [:name :first])}
      http/patch))

(map (fn [x] (if (= x 12) !void (inc x))))
(map #(if (= % 12) !void (inc %)))

(comp (reject #(= % 12)) (map inc))

;; keep but what if we want to keep nils?
;;
;; reduce does not always make intent clear.
;;
;; make several data structure operations redundant.
;;
;; Would be good to attach metadata to nil but unable in Clojure.
;; wrap nil in a void object? then not compatible elsewhere.
;;
;; Voidable as a protocol?
;;   .voidable to create a voidable version.
;;   .nilVoidable to automatically void nils.
;;
;; Vector example too.
;;
;; Can I use a reader tag/macro to swap out a data structure literal?
;; alternatively there is record syntax?

#void-if-nil
;; no point in creating many different variants of this.  if it is a value it
;; is more flexible than a reader tag pretending to be a function.

;; blog post structure:
;; - problem statement with example (Clojure and Elixir?) even worse when the
;;   languages don't include an assoc-some, update-some or maybe_update, etc.
;; - wouldn't it be nice to...
;; - proposed solution
;;   - alternative solutions
;;     - values or tags?
;; - potential problems
;;   - if it exists needs to be default in data structures and language and
;;     have similar nil properties. (needs to be a superset of nil) which is
;;     impossible in Java and most lisps/languages.
;;   - what happens when you use void elsewhere?  should void usage be
;;     contrained?
;;   - if void is not a singleton object it could be accidentally or
;;     maliciously used
;; - later
;;   - propagate upwards?  supervoid!
;;   - assist in macro writing unquote
;;     void.

;; (reject nil?) or (strip-nils) don't work when you need to include some nil
;; values.  but even if they do work for your use, they don't make the intent
;; clear on which values are supposed to be optional and which are not.
;;
;; Performance too.  why construct the data structure to immediately remove
;; things you just added?
