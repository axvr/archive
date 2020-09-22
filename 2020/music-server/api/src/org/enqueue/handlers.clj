(ns org.enqueue.handlers
  (:require
    [hiccup.core :refer [html]]))

(defn not-found-handler [request]
  {:status 404
   :headers {"Content-Type" "text/html"}
   :body "<h1>404</h1>"})

(defn home-handler [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (html [:html
                [:head
                 [:title "Enqueue API"]]
                [:body
                 [:h1 "Enqueue API"]
                 [:p "YOUR digital music collection, anywhere."]]])})

(defn about-handler [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body "<h1>About Enqueue</h1>"})
