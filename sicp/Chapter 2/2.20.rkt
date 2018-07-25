#lang racket

;; Exercise 2.20
(define (same-parity x . y)
  (letrec [(sp (lambda (l)
                          (cond
                            ((null? l) '())
                            ((eq? (odd? x) (odd? (car l))) (cons (car l) (sp (cdr l))))
                            (else
                             (sp (cdr l))))))]
    (cons x (sp y))))


(same-parity 2 3 4 5 6 7 8)
(same-parity 1 2 3 4 5 6 7)