(ns org.enqueue.api.docs
  "Enqueue documentation system."
  (:require [clojure.java.io :as io]
            [clojure.edn     :as edn]
            [clojure.string  :as str]
            [clojure.core.memoize :as memo]
            [org.enqueue.api.config :as config]
            [hiccup.core     :refer [html]]
            [org.enqueue.api.router.middleware :refer [wrap-async]]))


(def ^:private not-found-response
  {:status  404
   :headers {"Content-Type" "text/plain; charset=UTF-8"}
   :body    "404"})


(defn- to-fname [n]
  (str "docs/" (str/replace n #"\W+" "_") ".edn"))


(defn- read-doc [n]
  (some-> n
          to-fname
          io/resource
          slurp
          edn/read-string))


(defn- build-page
  [{:keys [title description keywords content]}]
  (let [title (if title
                (str title " | Enqueue API Docs")
                "Enqueue API Docs")]
    [:html {:lang "en-GB"}
     [:head
      [:meta {:charset "utf-8"}]
      [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
      [:link {:rel "stylesheet" :type "text/css" :href "/main.css"}]
      [:title title]
      [:meta {:name "description" :content description}]
      [:meta {:name "keywords" :content (str/join ", " keywords)}]
      [:meta {:name "copyright" :content "Copyright © 2021, Alex Vear."}]]
     [:body
      [:header
       [:h1 [:a {:href "/docs"} "Enqueue API"]]]
      content
      [:footer
       [:p
        "Copyright © 2021, "
        [:a {:href "https://www.alexvear.com"} "Alex Vear"]
        ".&emsp;All code snippets are dedicated to the public domain."]]]]))


(defn docs-handler
  [request]
  (let [page (get-in request [:uri-params :page] "index")
        doc  (read-doc page)]
    (if doc
      {:status  200
       :headers {"Content-Type" "text/html; charset=UTF-8"}
       :body    (html (build-page (eval doc)))}
      not-found-response)))


(def ^:private memo-docs-handler
  (if config/prod?
    (memo/lru docs-handler {} :lru/threshold 10)
    docs-handler))


(def doc-routes
  [["/docs"       {:get {:handler memo-docs-handler
                         :middleware [wrap-async]}}]
   ["/docs/:page" {:get {:handler memo-docs-handler
                         :middleware [wrap-async]}}]])
