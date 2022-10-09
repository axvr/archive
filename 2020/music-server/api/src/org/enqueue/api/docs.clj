(ns org.enqueue.api.docs
  "Enqueue documentation system."
  (:require [clojure.string         :as str]
            [clojure.core.memoize   :as memo]
            [org.enqueue.api.config :as config]
            [uk.axvr.refrain        :as r]
            [hiccup.page            :refer [html5]]))

(defn- read-doc [path]
  (as-> path doc
    (str/replace doc #"[^\w/]+" "_")
    (str "docs/" doc ".edn")
    (r/read-edn-resource doc)
    (eval doc)))

(defn- build-page [{:keys [title description keywords content]}]
  [:html {:lang "en-GB"}
   [:head
    [:meta {:charset "utf-8"}]
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
    [:link {:rel "shortcut icon" :type "image/png" :href "/favicon.png"}]
    [:link {:rel "stylesheet" :type "text/css" :href "/main.css"}]
    [:link {:ref "stylesheet" :type "text/css" :href "https://www.enqueue.org/fonts/inter.css"}]
    [:title (str (when title (str title " | ")) "Enqueue API Docs")]
    (when description
      [:meta {:name "description" :content description}])
    (when keywords
      [:meta {:name "keywords" :content (str/join ", " keywords)}])
    [:meta {:name "copyright" :content "Alex Vear"}]]
   [:body
    [:header
     [:a {:id "skip-link" :href "#main-content"} "Skip to content"]
     [:nav {:class "section"}
      [:div {:class "branding"}
       [:a {:href "/docs"}
        [:img {:src "/logo.svg" :class "logo"}]]
       [:a {:href "/docs"} [:h1 "Enqueue API"]]]
      [:ul
       [:li [:a {:href "/docs"} "Docs"]]
       [:li [:a {:href "https://github.com/nq-music" :rel "nofollow"} "Code"]]
       [:li [:a {:href "/docs/support" :rel "nofollow"} "Support"]]
       [:li [:a {:href "https://www.enqueue.org" :title "Go to Enqueue" :rel "nofollow"} "Enqueue ➜"]]]]]
    [:main {:id "main-content"}
     [:div {:class "section"}
      (when title
        [:section
         [:p [:a {:href "/docs"} "\u27F5 back"]]])
      content]]
    [:footer
     [:div {:class "section"}
      [:span
       "© 2022 " [:a {:href "https://www.alexvear.com"} "Alex Vear"]]
      [:span [:a {:title "Back to top of page" :href "#top" :rel "nofollow"} "Going up?"]]]]]])

(def ^:private not-found-response
  "Response returned when no doc page was found."
  {:status  404
   :headers {"Content-Type" "text/html; charset=UTF-8"}
   :body    (html5 (build-page (read-doc "404")))})

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
       :body    (html5 (build-page doc))}
      not-found-response)))

(defonce ^:private memo-get-docs-resp
  (if config/prod?
    (memo/lru get-docs-response {} :lru/threshold 10)
    get-docs-response))

(def handler
  {:name ::page
   :enter
   (fn [context]
     (let [page (get-in context [:request :path-params :page] "index")]
       (assoc context :response (memo-get-docs-resp page (:request context)))))})

(def routes
  #{["/docs" :get handler :route-name ::index]
    ["/docs/*page" :get handler]})
