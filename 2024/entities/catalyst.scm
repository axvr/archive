;; This file contains a mix of Scheme and Clojure code written on my phone at
;; the same time I was working on this concept.

;; define process
;; define object (entity)

(defclass personal-loan


  (states [new disbursed])
  (substates [])

  (new -> disbursed) disburse)

(defproc move-to-arrears)

timed procs with conditionals

(defclass PersonalLoan s/personal-loan
  [new
   disbursed
   closed
   [active [in-arrears
            good-standing
            defaulted]]]
  (new -> disbursed | disburse)
  (disbursed -> active good-standing | activate)
  (active good-standing -> active in-arrears | in-arrears))

(defmethod PersonalLoan
  [:new :disbursed state ]
  )

;; conditions on arcs
;; timers for arcs?

;; if conditions are met, execute procedure.
;; events could be used to trigger it?

()

controller model.

effect handlers, e.g. trigger reschedule.

always comes back to catalyst.

these "entities" are living.  we need a system to control/facilitate state changes and trigger actions.

we incorrectly model "entities" as dead objects that require mutation.

live objects.

nancy leveson's controller concept.  catalyst is the controller.

auto persist to db.

handle rollbacks.

fix mistakes while running!

component reliability is not as critical as system safety nor does one have any influence over the other.

debuggability is critical.

dynamic!

we keep replicating crud-style everywhere.  there must be a better abstraction, at least for some cases, right?

adjourn?  catalyst?  ???

yield

;; capability tokens can be issued to async callback actions

;; >> state change
;; !! async action
;; @@ on timer
;; && become
;; ?? predicate

(defclass PersonalLoanApplication
  '[submitted approved declined
    referred closed])

(defproc PersonalLoanApplication
  (>> '-init 'submitted)
  (!! 'submitted ->cde)
  (&& 'approved PersonalLoanOffer))


(defclass PersonalLoanOffer
  '[offered withdrawn])

(defproc PersonalLoanOffer
  (>> '-init 'approved)
  (!! 'approved ->notify-of-offer)
  (@@ 'approved #days 2
    (!! 'approved ->remind-of-offer))
  ())


(defclass PersonalLoan
  '[active disbursed closed])

'{tags [closed/repaid disbursed]}

;; Process orchestrator listener/trigger system?




The problem:
- Need a centralised place to track the various statuses that a loan may be in.
- Need to be able to trigger actions based on these state transitions, conditions and timers.
- Need to distinguish between event types, statuses and labels/tags.
- Other systems need to learn these states and perform actions based on them.


(defauto ...)  (deftrigger ...)



(defclass PersonalLoanOffer
  #{:given :withdrawn :accepted :declined :expired})

(defproc PersonalLoanOffer
  [:given #{:accepted :declined}])

(defmethod PersonalLoanOffer :accept [])

;; method name
;; from state
;; to state
;; guard/predicate
;; timer (all durations?)
;; times based on data.
;;   ^ to be calculated
;; action
;; payload schema
;; run once?
;; callbacks and capabilites

(defclass PersonalLoanOffer
  :states #{:given :withdrawn :accepted :declined :expired}
  :on-save ...
  :set-timer ...

  (on [:new constructor-schema]
    :new :given)

  (on [:accept accept-offer-schema]
    :given :accepted
    accept-offer)

  (on [:decline decline-offer-schema]
    #{:given} :declined
    decline-offer)

  (at [:expire (days 2)]  ; if no timer, do on state change.
    :given :expired
    optional-expire-action))

(defn- accept-offer [this data]
  )



(define-model (PLA_1)
  ...)

(define-process (PLA_1 'approve schema)
  ())

(define (approve-application self payload)
  (let ((self (cons self `(decision . ,payload))))
    ;; TODO: what if it already exists?
    ;; TODO: fetch and return if so?
    ;; TODO: deterministic IDs?
    ;; TODO: locks so that only one approve can run at a time.  (lock by writing event to pla table? and impl CAS on methods)
    ;; IO cannot be performed by the constructor?  Maybe don't pass self.
    (cat/new! PLO `((application . ,self)))
    self))


;; todo aliases or super classes need to exist to support querying across versions and minimise duplicate tables.
;;
;; effectively a special purpose partial implementation of the MOP.
;;
;;
;; layers of state machines.  higher level state machine that controls the whole life cycle of a loan, and refinancing, etc.  High level state machines where each state is another state machine.


(define-model (PLO_1)
  "Personal loan offer."
  `((cons . (give))
    (states . (given accepted declined withdrawn expired))))

(define-model-alias PLO PLO_1)

(define (give-offer self payload)
  (let* ((appl (assoc payload 'application))
         (appl-id (assoc appl 'id))
         (decn (assoc appl 'decision)))
    `((amount (assoc decn 'amount))
      (term (assoc decn 'term))
      (rate (assoc decn 'rate))
      (application-id ,appl-id))))

(define-process (PLO_1 'give schema)
  "Give a loan offer to a customer."
  `((when
      ((state . new)))
    (then
      ((state . given)
       (action . give-offer)))))
