(ns hal
  "Helper functions for interacting with HAL services.

  HAL specification: <https://stateless.group/hal_specification.html>"
  (:refer-clojure :exclude [resolve])
  (:require [com.grzm.uri-template :as uri-template])
  (:import (java.net URI)))

;; Created: 2023-08-04
;; Updated: 2023-08-07

(defn- ->uri ^URI [uri]
  (if (uri? uri) uri (URI. uri)))

(defn resolve
  "Resolve a HAL link (\"relation\") in \"resource\" and template it using
  \"variables\".  Uses \"base\" as the base URI for resolving relative URIs.

  A relation can be the name of a relation (e.g., `:foo`) or a name + index
  (e.g., `[:foo 1]`), for relations with multiple links.

  Caveats:
    - Assumes that the `_links`, `href` and `templated` keys are keywords.
    - Does not respect \"CURIEs\", as they complect rel names with docs."
  (^URI [base resource relation]
   (resolve base resource relation nil))
  (^URI [base resource relation variables]
   (let [path-to-rel ((if (vector? relation) concat conj) [:_links] relation)
         {:keys [href templated]} (get-in resource path-to-rel)
         error-data {:base      base
                     :relation  relation
                     :href      href
                     :templated templated
                     :variables variables}]
     (if href
       (.resolve
        (->uri base)
        (URI. (if (or (and (boolean? templated) templated)
                      (and (string? templated) (parse-boolean templated)))
                (let [expansion (uri-template/expand href variables)]
                  (if (map? expansion)
                    (throw (ex-info "Failed to expand URI template"
                                    (assoc error-data :expansion expansion)))
                    expansion))
                href)))
       (throw (ex-info "No matching HAL link found"
                       (assoc error-data :links (:_links resource))))))))

(defn properties
  "Strip \"_links\" and \"_embedded\" from a HAL resource, returning a map of
  only properties."
  [resource]
  (dissoc resource :_links :_embedded))
