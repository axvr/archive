(ns uk.axvr.catalyst.renderer
  (:require [dorothy.core :as dot]
            [dorothy.jvm :as dot-io]
            [uk.axvr.examples.personal-loans :as-alias loans]
            [uk.axvr.examples.personal-loans.application :as-alias loans.appl]
            [uk.axvr.catalyst :as-alias cat]))

(-> (dot/digraph
     [#_{:rankdir "LR"}
      (dot/subgraph :cluster_0
                    [{:label "uk.axvr.examples.personal-loans.application"}
                     [:start!
                      {:style :dashed
                       :tooltip "Start of the personal loan application workflow."}]
                     [:decline_in_0
                      {:tooltip "Decline personal loan application."
                       :style :dashed
                       :label :decline}]
                     [:decline
                      {:tooltip "Decline personal loan application."
                       :style :dashed
                       :label :decline}]
                     [:record-decision
                      {:tooltip "Run pre-eligibility checks."}]
                     [:pre-eligibility-rules
                      {:tooltip "Run pre-eligibility checks."}]
                     [:acceptance-rules
                      {:tooltip "Run acceptance checks."}]
                     [:affordability-rules
                      {:tooltip "Run affordability checks."}]
                     [:fetch-credit-file
                      {:tooltip "Fetch the customer's credit file from a credit agency."}]
                     [:fetch-open-banking-income
                      {:tooltip "Fetch the customer's income via Open Banking."}]
                     ;; Logical blocks
                     [:cond_0 {:shape :diamond :label ""}]
                     [:point_0 {:shape :triangle :label ""}]
                     [:point_1 {:shape :invtriangle :label ""}]])
      ;; Connections
      [:start! :pre-eligibility-rules]
      [:pre-eligibility-rules :cond_0]
      [:cond_0 :point_0 {:label ":passed-acceptance?"}]
      [:point_0 :fetch-credit-file]
      [:point_0 :fetch-open-banking-income {:arrowhead "open"}]
      [:cond_0 :decline_in_0 {:arrowhead "odot" :label "else"}]
      [:decline :record-decision]
      [:fetch-credit-file :acceptance-rules]
      [:acceptance-rules :point_1]
      [:fetch-open-banking-income :point_1]
      [:point_1 :affordability-rules]
      [:affordability-rules :record-decision]
      [:record-decision :uk.axvr.catalyst/halt!]])
    dot/dot
    (dot-io/save! "out.svg" {:format :svg}))


(comment
  (import '[net.sourceforge.plantuml
            SourceStringReader
            FileFormat
            FileFormatOption]
          '[java.io ByteArrayOutputStream])

  (let [s (StringBuilder.)]
    (.append s "@startuml\n")
    (.append s "start\n")
    (.append s "::start;\n")
    (.append s "stop\n")
    (.append s "@enduml\n")
    (let [rdr (SourceStringReader. (.toString s))]
      (with-open [os (ByteArrayOutputStream.)]
        (.generateImage rdr os (FileFormatOption. FileFormat/SVG))
        (spit "plantuml.svg" (String. (.toByteArray os) "UTF-8")))))
  )
