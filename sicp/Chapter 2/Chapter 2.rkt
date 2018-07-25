#lang racket

;; From Section 2.3 of SICP
;; https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-16.html#%_sec_2.3

;; Exercise 2.54

(define (equal? a b)
  (cond
    ((and (null? a) (null? b)) #t)
    ((or (null? a) (null? b)) #f)
    ((and (list? (car a)) (list? (car b))) (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
    ((eq? (car a) (car b)) (equal? (cdr a) (cdr b)))
    (else
     #f)))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))
(equal? '(this (is a) list) '(this (is a) list))
(equal? '() '(foo bar))

;; Exercise 2.55
(expand (datum->syntax #f '(car ''abracadabra)))
