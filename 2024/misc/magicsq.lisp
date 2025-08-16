
(defun sq (x)
  (* x x))

(mapcar #'sq '(1 2 3))

(defun build-sq (a b c d e f g h i)
  `((,a ,b ,c)
    (,a ,e ,i)
    (,a ,d ,g)
    (,d ,e ,f)
    (,g ,h ,i)
    (,b ,e ,h)
    (,c ,f ,i)
    (,c ,e ,g)))

;; 4: e
;; 3: a i / c g
;; 2: b d f h

(build-sq 1 2 3 4 5 6 7 8 9)

(setq sq! (lambda (x) (mapcar #'sq x)))

(mapcar sq! (build-sq 1 2 3 4 5 6 7 8 9))

(defun valid-sq-p (x)
  (mapcar (lambda (y) (apply #'+ y)) x))

(valid-sq-p (build-sq 1 2 3 4 5 6 7 8 9))

(error "Testing!")
(/ 1 0)






;; (defmacro def (&body body)
;;   `(setq ,@body))

*last-package*
