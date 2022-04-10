(ns org.enqueue.api.docs
  "Enqueue documentation system."
  (:require [clojure.string          :as str]
            [clojure.core.memoize    :as memo]
            [org.enqueue.api.config  :as config]
            [hiccup.core             :refer [html]]
            [org.enqueue.api.helpers :refer [read-edn-resource]]))


(defn- read-doc [path]
  (as-> path doc
      (str/replace doc #"[^\w/]+" "_")
      (str "docs/" doc ".edn")
      (read-edn-resource doc)
      (eval doc)))


(defn- build-page
  [{:keys [title description keywords content]}]
  [:html {:lang "en-GB"}
   [:head
    [:meta {:charset "utf-8"}]
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
    [:link {:rel "stylesheet" :type "text/css" :href "/main.css"}]
    [:title (if title
              (str title " | Enqueue API Docs")
              "Enqueue API Docs")]
    (when description
      [:meta {:name "description" :content description}])
    (when keywords
      [:meta {:name "keywords" :content (str/join ", " keywords)}])
    [:meta {:name "copyright" :content "Copyright © 2021, Alex Vear."}]]
   [:body
    [:header
     [:h1 [:a {:href "/docs"} "Enqueue API"]]]
    content
    [:footer
     [:p
      "Copyright © 2022, "
      [:a {:href "https://www.alexvear.com"} "Alex Vear"]
      ".&emsp;All code snippets are dedicated to the public domain."]]]])


(def ^:private not-found-response
  "Response returned when no doc page was found."
  {:status  404
   :headers {"Content-Type" "text/html; charset=UTF-8"}
   :body    (html (build-page (read-doc "404")))})


(def ^:dynamic *request*
  "Currently active HTTP request."
  nil)


(defn api-uri
  "Returns the URI for the current API with an optional path on the end."
  ([] (api-uri nil))
  ([path]
   (str (name (:scheme *request*))
        "://"
        (get-in *request* [:headers "host"])
        path)))


(defn get-docs-response [page request]
  (binding [*request* request]
    (if-let [doc (read-doc page)]
      {:status  200
       :headers {"Content-Type" "text/html; charset=UTF-8"}
       :body    (html (build-page doc))}
      not-found-response)))


(defonce ^:private memo-get-docs-resp
  (if config/prod?
    (memo/lru get-docs-response {} :lru/threshold 10)
    get-docs-response))


(def docs-handler
  {:name ::page
   :enter
   (fn [context]
     (let [page (get-in context [:request :path-params :page] "index")]
       (assoc context :response (memo-get-docs-resp page (:request context)))))})


(def docs-routes
  #{["/docs" :get docs-handler :route-name ::index]
    ["/docs/*page" :get docs-handler]})
