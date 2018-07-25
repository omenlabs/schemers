#lang racket

(define (even? n)
    (= (remainder n 2) 0))

(define (exp b n)
  (letrec [(exp-iter (lambda (a n)
    (cond
    ((= n 1) a)
    (else
     (exp-iter (* a (* b b)) (/ n 2))))))]
  (cond
    ((even? n) (exp-iter 1 n))
    (else
     (* b (exp-iter 1 (sub1 n)))))))

