(ns uk.axvr.examples
  (:require [clojure.core.async :as a]
            [uk.axvr.catalyst :as cat]))

(defclass PersonalLoan
  {:application [:submitted :under-review :closed :approved :rejected]
   :offer       [:given :accepted :declined :expired :withdrawn]
   })

;; Differentiate between states and labels/tags?

(defclass PersonalLoan
  (states #{:application :offer :loan})
  (labels #{}))

;; Or perhaps these should be separate classes?


(visualise my-customer)


(defclass PersonalLoan
  [:application/submitted
   :application/under-review
   :application/closed
   :application/approved
   :application/rejected

   :offer/given
   :offer/accepted
   :offer/declined
   :offer/expired
   :offer/withdrawn

   :loan/disbursed

   :loan/paid-off])

;; --> <-- !-> <-! <<< >>>

(defproc PersonalLoan
  [:application/submitted >> process-application]
  [:application/submitted <! ])

(defprocess PersonalLoan
  ([:offer/given (after (days 14))] expire-loan-offer))

;; Events:
;;   - Timer
;;   - Message
;;   - State change

;; Need to use the DB as the source of truth.


;; >> state change
;; !! async action (dispatch)
;; at timer
;; && become
;; ?? predicate
;; // add label
;; !/ remove label

;; yield

;; (defmacro defclass [name states & procs]
;;   `(defrecord ~name []))

;; states and tags/labels.
;; assertions during compilation.

(defn ->cde! [entity]
  (send! entity))

(defprocess PersonalLoan
  #{})

(defclass PersonalLoanOffer
  #{:given :accepted :declined :expired :withdrawn}
  (>> :-new :given)
  (!! :given notify-of-offer!)
  (?? :given (at #days 2) ->remind-of-offer))

(defclass PersonalLoanApplication
  #{:submitted :under-review :closed :approved :rejected}
  ;; [name dob]
  (>> :-new :submitted)
  (!! :submitted ->cde!)
  (&& :approved PersonalLoanOffer))

;; TODO: malli schemas?  Extract top level keys from map to use as fields on class.


(defclass PersonalLoan
  #{:active ...}
  )


;; Need an object registry.




(def PersonalLoanApplication__Registry
  (agent {}))


;; Hybrid of Clojure, Smalltalk and Erlang.


(defclass PersonalLoanApplication
  "A class for a personal loan application."
  #{:submitted :under-review :closed :approved :rejected})

(defmethod PersonalLoanApplication [:-init ...]
  [this ...]
  )


(defclass PersonalLoanOffer
  "A class representing a personal loan offer."
  #{:given :accepted :declined :expired :withdrawn}
  (to :-new :given)
  (do :given notify-of-offer!)
  (on :given (at #days 2) ->remind-of-offer))

(defmethod PersonalLoanOffer [:given (at )])


;; Design to integrate with process orchestrator.  Don't try to replace them, complement them.

;; Can we dynamically set up timers on a process orchestrator.


;; Virtual instalments for loans.


;; at
;; every

;; Autogenerate an ID.

(defclass PersonalLoanOffer
  "Class representing a personal loan offer."
  :inherit [CatalystClass]
  :states #{:given :accepted :declined :expired :withdrawn}
  )

(let [plo (cat/new PersonalLoanOffer)]
  (cat/send plo ...))

;; (chime/at [time] (send plo ...))

;; Personal loan versions.  E.g. early settlement fees only apply to loans created after a certain date.


(defclass PersonalLoan_2023)

(defclass PersonalLoan_2024-08-01
  :inherit [PersonalLoan2023]
  )


;; Process orchestrator callback workflow!  send a timestamp to call us back at.

;; idempotency key generation.


;; Upon an action being performed, we check if there is anything to do and then schedule a callback.

;; Auto save changes to DB.  Auto apply scheduled operations.


{:meta {:entity #uuid "94179fa1-8dc0-45c1-b93f-783cfaeb7ea5"
        :created-at ""
        :owner "cust_12345"
        :class PersonalLoan_2024-08-01}
 :data {}}



(defclass PersonalLoanOffer
  "Class representing a personal loan offer."
  :inherit [CatalystClass]
  :states #{:given :accepted :declined :expired :withdrawn}
  )

(cat/restore [{::class 'uk.axvr.examples.personal-loans.PersonalLoanOffer
               ::id    #uuid "94179fa1-8dc0-45c1-b93f-783cfaeb7ea5"
               ::state ::new
               ::time  ...}
              {}
              {}])

(let [plo (cat/new PersonalLoanOffer)]
  )

(require '[uk.axvr.catalyst :refer [defclass on at]])

(defclass PersonalLoanOffer
  "Class representing a personal loan offer."
  :states #{:given :withdrawn :expired :declined :accepted}
  :on-save (fn [])    ; Save new event to DB transaction.
  :set-timer (fn [])  ; Send to process orchestrator.
  (on [:new constructor-schema]
      :new :given)
  (at [:expire (days 14)]  ; If no timer, do imediately.
      :given :expired optional-expire-action)
  (on [:withdraw]
      :given :withdrawn withdraw-offer)
  (on [:decline decline-offer-schema]
      #{:given} #{:declined} decline-offer)
  (on [:accept accept-offer-schema]
      :given :accepted accept-offer))
