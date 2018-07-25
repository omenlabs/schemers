#lang racket

(define (pascal row col)
  (cond
    ((= row col) 1)
    ((= row 1) 1)
    ((= col 1) 1)
    (else
     (+ (pascal (sub1 row) col)
        (pascal (sub1 row) (sub1 col))))))

(pascal 5 2)
