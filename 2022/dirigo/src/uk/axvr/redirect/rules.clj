(ns uk.axvr.redirect.rules)

(defn ->host [to]
  #(assoc % :host to))

(defn ->port [to]
  #(assoc % :port to))

(defn ->path [to]
  #(assoc % :path to))

(defn ->scheme [to]
  #(assoc % :scheme to))

(defn ->tls [uri]
  (if (= :http (:scheme uri))
    ((->scheme :https) uri)
    uri))

(defn ->type [redir-type]
  #(assoc % :type redir-type))

(def ->temp (->type :temporary))
(def ->perm (->type :permanent))

(defn !path [url]
  (dissoc url :path))

(defn rules->ruleset [rules]
  (update-vals
    rules
    (fn [v]
      (if (coll? v)
        (apply comp v)
        v))))

(defonce ruleset
  (-> {"axvr.uk"         [->tls (->host "www.axvr.uk")]
       "www.axvr.uk"     [->tls (->host "www.alexvear.com")]
       "alexvear.com"    [->tls (->host "www.alexvear.com")]

       "ascribe.axvr.uk" [->tls (->host "www.alexvear.com") (->path "/projects/ascribe/")]
       "cereal.axvr.uk"  [->tls (->host "github.com") (->path "/axvr/cereal")]
       "halogen.axvr.uk" [->tls (->host "github.com") (->path "/axvr/halogen")]

       "axvr.io"         [->tls (->host "axvr.uk")]
       "www.axvr.io"     [->tls (->host "www.axvr.uk")]
       "ascribe.axvr.io" [->tls (->host "ascribe.axvr.uk")]

       ;; TODO: Buy and move to nq.to
       "enqueue.org"     [->tls (->host "www.enqueue.org")]
       "dequeue.org"     [->tls (->host "www.dequeue.org") ->temp]
       "www.dequeue.org" [->tls (->host "www.enqueue.org") ->temp]

       "***REMOVED***.com"     [->tls (->host "www.***REMOVED***.com")]
       "www.***REMOVED***.com" [->tls (->host "www.***REMOVED***.com") (->path "/shop/MessyMammoth")]}
      rules->ruleset
      atom))
