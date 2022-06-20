(ns uk.axvr.redirect.rules
  (:require [clojure.string :as str]))

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

(def forge-details
  {:github {:root {:host "github.com" :path "/axvr"}
            :repo {:host "github.com" :path "/axvr/%s"}}
   :sr.ht  {:root {:host "sr.ht"      :path "/~axvr"}
            :repo {:host "git.sr.ht"  :path "/~axvr/%s"}}})

(defn repo->forge [repo]
  (case repo
    :github))

(defn ->git [url]
  (let [repo  (str/replace (:path url) #"(^/|\.git$)" "")
        forge (forge-details (repo->forge repo))
        {:keys [host path]} ((if (seq repo) :repo :root) forge)]
    (assoc url :host host :path (format path repo))))

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
       "git.axvr.uk"     [->tls ->git]

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
