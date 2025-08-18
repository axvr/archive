;; depict DSL for STAMP diagrams.
;; Experimenting with representing Depict in Scheme.
;; https://github.com/mstone/depict

;; Created: 2024-12-07.  Later morphed into `2025/depict`.

;; person microwave food: open, start, stop / beep : heat
;; person food: stir

;; (depict/->d2
;;  `'([person microwave food
;;      : open . start . stop / beep
;;      : heat]
;;     [person food
;;      : stir]))

(define (separ? x)
  (eq? x ':))

(define (partition-by pred coll)
  (fold-left
    (lambda (acc elm)
      (if (pred elm)
        (append acc '())
       ;; (cons (cons elm (car acc))
         ;;     (cdr acc))
        ;acc
       ; (cons elm acc)
        ;(append acc '())
        (append (append (car acc) elm)
               (cdr acc))
      ))
    '(())
    coll))

(partition-by even?
  '(1 3 2 3 5 1 4 7))

(define (parse-line line)
  (partition-by separ? line))

(define (->dictim depict)
  (map parse-line depict))

(->dictim
'((person microwave food
    : open & start & stop / beep
    : heat)
    (person food : stir))
  )
