(ns personal-loan-stage-1-workflow
  (:require [uk.axvr.catalyst :as cat]
            [uk.axvr.examples.personal-loans :as-alias loans]
            [uk.axvr.examples.personal-loans.application :as-alias loans.appl]
            [uk.axvr.examples.personal-loans.repayment :as-alias loans.repay]))

(def components
  #{#::cat{:id   ::loans.appl/start!
           :doc  "Start of the personal loan application workflow."
           :type ::cat/interface}  ; Interfaces have no implementation.  When entered, they continue immediately.

    #::cat{:id   ::loans.appl/decline
           :doc  "Decline personal loan application."
           :type ::cat/interface}

    ;; #::cat{:id   ::loans.repay/reschedule!
    ;;        :type ::cat/queue}

    #::cat{:id   ::loans.appl/pre-eligibility-rules
           :doc  "Run pre-eligibility checks."
           :type ::cat/stage}  ; This is the default.

    #::cat{:id  ::loans.appl/acceptance-rules
           :doc "Run acceptance checks."}

    #::cat{:id  ::loans.appl/affordability-rules
           :doc "Run affordability checks."}

    #::cat{:id  ::loans.appl/fetch-credit-file
           :doc "Fetch the customer's credit file from a credit agency."
           :retry-strategy {:retries 5, :backoff :exponential}}

    #::cat{:id  ::loans.appl/fetch-transactions
           :doc "Fetch the customer's financial transactions for analysis."
           :retry-strategy {:retries 5, :backoff :exponential}}})

(def workflow
  #::cat{:workflow-id ::loans/submit-application
         :description "Personal loan application submission (stage 1)."
         :stages      [[::cat/flow
                        ::loans.appl/start!
                        ::loans.appl/pre-eligibility-rules
                        ::loans.appl/fetch-credit-file
                        ::loans.appl/acceptance-rules
                        ::loans.appl/fetch-transactions
                        ::loans.appl/affordability-rules]]})
;;                      ^ sequential workflow.

;; add some parallelism (below)

;;                             / fetch-transactions \
;; start! -> pre-eligibility -{                      }-> acceptance -> affordability
;;                             \ fetch-credit-file  /
[[::cat/flow
  ::loans.appl/start!
  ::loans.appl/pre-eligibility-rules
  [::cat/fork
   ::loans.appl/fetch-credit-file
   ::loans.appl/fetch-transactions]
  [::cat/join
   ::loans.appl/fetch-credit-file
   ::loans.appl/fetch-transactions]
  ::loans.appl/acceptance-rules
  ::loans.appl/affordability-rules]]
;; flow, fire, await, listen, pause, halt!, wait (timer), clear-queue, suspend!, resume!, replace!, retry!

;;                            /------- fetch-transactions ------\
;; start! -> pre-eligibility -> fetch-credit-file -> acceptance -> affordability
(comment
  ;; This is possible because the data flow is purely accretive.  Nothing is ever removed.
  (cat/do!!
   ::loans.appl/start!
   ::loans.appl/pre-eligibility-rules
   (cat/cond
    :passed-pre-eligibility?
    (cat/do!!
     (cat/fire! ::loans.appl/fetch-transactions)
     ::loans.appl/fetch-credit-file
     ::loans.appl/acceptance-rules
     (cat/cond
      #(:passed-acceptance? %)
      (cat/do!!
       (cat/await!! ::loans.appl/fetch-transactions)
       ::loans.appl/affordability-rules
       (cat/cond
        #(:passed-affordability? %))))))
   ::cat/halt!))

(cat/register workflow components)

;; loan-worker
(cat/subscribe
 #::cat{:workflow-id ::loans/submit-application
        :stage-id    ::loans.appl/acceptance-rules
        :handler-id  (random-uuid)
        :handler     (fn [{:as context, :keys [application-data]}]
                       ;; ... Run rules ...
                       ;; Should catalyst values be in the `context` or as metadata?
                       (assoc context
                              :passed-acceptance? false
                              :acceptance-fail-reasons [...]))})
;; (cat/unsubscribe handler-id)

;; loan-service
;; Start workflow instance.
(cat/send
 {::cat/workflow-id  ::loans/submit-application
  ::cat/instance-id  (random-uuid)
  ::cat/business-key (random-uuid)
  ::cat/stage-id     ::loans.appl/start!
  ::cat/message      ::cat/start!
  :customer-id       (random-uuid)
  :application-data  {:net-monthly-income 1000M
                      ...}})

;; Send a message to a queue in a workflow instance.
(cat/send #::cat{:workflow-id  ::loans/repayment-cycle
                 :instance-id  (random-uuid)
                 :business-key (random-uuid)
                 :message      ::loans.repay/reschedule!})

;; Stop running workflow instance(s) by sending the `::cat/halt!` message.
(cat/send
 #::cat{:workflow-id  ::loans/repayment-cycle
        :instance-id  (random-uuid)
        :business-key (random-uuid)
        :message      ::cat/halt!})

;; Suspend a *running* workflow instance.
;; Resume a *running* workflow instance.

;; Replace a running workflow instance.  Keep accumulated data.  Any new data in the message will be added.
;; If `::cat/workflow-version` was specified, change the version (`:latest` to promote to latest workflow version).
(cat/send
 #::cat{:workflow-id  ::loans/submit-application
        :instance-id  (random-uuid)
        :business-key (random-uuid)
        :message      ::cat/replace!
        :stage-id     ::loans.appl/acceptance-rules})

;; Suspend/resume entire workflows.
(cat/send #::cat{:workflow-id ::loans/submit-application
                 :message     ::cat/suspend!})

(cat/send!
 #::cat{:message ::cat/register-workflow!
        ...})

;; Send multiple messages at once to be completed atomically.
(cat/send
 [#::cat{:message ::cat/register-workflow!
         ...}
  #::cat{:workflow-id ::loans/submit-application
         :message     ::cat/suspend!}])

;; Retry a failed stage.
(cat/send #::cat{:workflow-id ::loans/submit-application
                 :instance-id (random-uuid)
                 :error-id    (random-uuid)
                 ;; :stage-id    ::loans.appl/fetch-credit-file
                 :message     ::cat/retry!})

;; Web UI would have a little message sending console.

(cat/render)

;; Can attach idempotency key to a message.

;; Need a way of distinguishing between "start only if instance exists", vs. "start, if no instance start one".
;; `::cat/start!` vs. `::cat/trigger!` (or `activate!`)
(cat/send #::cat{:message     ::cat/trigger!
                 :workflow-id ::loans/submit-application
                 :instance-id (random-uuid)
                 :stage-id    ::loans.appl/decline})

;; `::cat/halt!` as a message or a stage?  Stop execution of the workflow instance.

;; Whole system is based around messaging and queues.
;; stages, interfaces and queues.

;; Prototype can make use of agents and core.async.
;; (defn send [message]
;;   (clojure.core/send sys handle-message message))

;; This is the whole public API!
cat/send
cat/register
cat/subscribe
cat/unsubscribe

;; TODO: await semantics when "looping".

;; Network protocol.
;;   - HTTP is far too heavy and complex.
;;   - ZeroMQ?
;;   - Kafka or RabbitMQ.

;; Custom data encoding.
;;   - JSON lacks much needed extensibility.
;;   - Transit and/or Msgpack might be good options.
;;   - Something custom influenced by LoCal and Gibbon?
