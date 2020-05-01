;;;; Better association lists.
;;;; Public domain.  No rights reserved.

(declare (unit alist))

(module alist (get contains? keys vals
               assoc dissoc merge
               map-on map-on-vals map-on-keys)

  (import (except scheme assoc))

  (define (empty? list)
    "Check if a list is empty."
    (eq? list '()))

  (define (get alist key)
    "Retrieve value from a alist."
    (cond ((empty? alist) '())
          ((equal? key (caar alist))
           (cdar alist))
          (else (get (cdr alist) key))))

  (define (contains? alist key)
    "Check if a alist contains a specfied key."
    (cond ((empty? alist) #f)
          ((equal? key (caar alist)) #t)
          (else (contains? (cdr alist) key))))

  (define (assoc alist key value)
    "Associate a key in a alist with a value."
    (cond ((empty? alist) (list (cons key value)))
          ((equal? key (caar alist))
           (cons (cons key value)
                 (cdr alist)))
          (else (cons (car alist)
                      (assoc (cdr alist) key value)))))

  (define (dissoc alist key)
    "Dissociate a key from a alist."
    (cond ((empty? alist) '())
          ((equal? key (caar alist))
           (dissoc (cdr alist) key))
          (else (cons (car alist)
                      (dissoc (cdr alist) key)))))

  (define (merge base override)
    "Merge 2 alists into 1."
    (if (empty? override)
      base
      (merge (assoc base
                    (caar override)
                    (cdar override))
             (cdr override))))

  (define (keys alist)
    (map car alist))

  (define (vals alist)
    (map cdr alist))

  (define (map-on fn alist)
    (map (lambda (x) (fn (car x) (cdr x))) alist))

  (define (map-on-vals fn alist)
    (map-on (lambda (k v) (cons k (fn v))) alist))

  (define (map-on-keys fn alist)
    (map-on (lambda (k v) (cons (fn k) v)) alist)))
