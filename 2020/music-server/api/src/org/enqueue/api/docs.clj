(ns org.enqueue.api.docs
  "Enqueue documentation system."
  (:require [clojure.string          :as str]
            [clojure.core.memoize    :as memo]
            [org.enqueue.api.config  :as config]
            [hiccup.core             :refer [html]]
            [org.enqueue.api.helpers :refer [read-edn-resource]]))


(defn- read-doc [n]
  (read-edn-resource
    (str "docs/" (str/replace n #"[^\w/]+" "_") ".edn")
    :eval? true))


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
      ".&emsp;All code snippets are dedicated to the public domain."]]]])


(def ^:private not-found-response
  "Response returned when no doc page was found."
  {:status  404
   :headers {"Content-Type" "text/html; charset=UTF-8"}
   :body    (html (build-page (read-doc "404")))})


(defn- get-docs-response [page]
  (if-let [doc (read-doc page)]
    {:status  200
     :headers {"Content-Type" "text/html; charset=UTF-8"}
     :body    (html (build-page doc))}
    not-found-response))


(def ^:private memo-get-docs-response
  (if config/prod?
    (memo/lru get-docs-response {} :lru/threshold 10)
    get-docs-response))


(defn- docs-handler
  ([request]
   (let [page (get-in request [:uri-params :*] "index")]
     (memo-get-docs-response page)))
  ([request respond _]
   (respond (docs-handler request))))


(def doc-routes
  [["/docs"   {:get {:handler docs-handler}}]
   ["/docs/*" {:get {:handler docs-handler}}]])
