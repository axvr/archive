(ns stpa)

;; TODO: something like the VS Code PASTA extension, but in
;; Clojure using Clerk as the UI and d2 as diagram
;; generator?  Clojure STPA DSL?
;; https://github.com/kieler/pasta/blob/master/extension/README.md

(defrecord Loss [id desc rationale assumption])
(defrecord Hazard [id desc refs children])
(defrecord SystemConstraint [id desc refs children])

(defmacro L [id desc]
  `(def ^::loss L.id
     (->Loss ~id ~desc)))


;; Goals
(G :1 "Achieve something.")

;; Losses
(L :1 "Loss of life or serious injury to people."
   {:assumption "People are human."})

;; Hazards
(H :1 "Vessel's exposure to major damage or breakdown" [#L :1]
   (H :1 "Vessel enters No-Go area."))

(SC :1 "Vessel must not be exposed to major damage or breakdown." [#H :1]
    (SC :1 "Vessel must not enter a No-Go area." [#H :1.1]))
