(define (wrap)
  (write "wrap\n")
  (ccd))

(define *cc '())

(define (ccd)
  (write "Cc'd")
  (write (call/cc
    (lambda (cc)
      (set! *cc cc)
      '())))
  (write 'end))

(wrap)