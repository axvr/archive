(ns org.enqueue.handlers
  (:require
    [hiccup.core :refer [html]]))

(defn home-handler [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (html [:html
                [:head
                 [:title "Enqueue API"]]
                [:body
                 [:h1 "Enqueue API"]
                 [:p "Your digital music collection, anywhere."]]])})
