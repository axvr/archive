(import (lispkit match))

(define (last lst)
  (if (null? (cdr lst))
    (car lst)
    (last (cdr lst))))

(define (ends-with? str ch)
  (char=? ch (last (string->list str))))

(define (method? sym)
  (ends-with? (symbol->string sym) #\:))

;; create an interface/protocol
(define (intf name)
  (lambda (_intf )))

(define (obj)
  '())

(define (->method sym)
  '())

;; who?
;; resolve
;; bind
;; macro
;; undefined?
;; true?
;; false?
;; then
;; else
;; or
;; and

(define (var ns name)
  (let foo ()
    ))

set!

(define (var ns name)
  (match-lambda*
    (('resolve:) self)
    (('who?) `(ok ,ns ,name))
    (('bind: x) `(ok ,x))
    (('macro: x) 'macro)
    (('undefined?) '(ok #t))
    (else '(error does-not-understand))))

(define foo (var 'user 'foo))

(define cat (var 'animal 'cat))

