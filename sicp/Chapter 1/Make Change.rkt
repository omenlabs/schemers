#lang racket

(require math/array)

(define coins #(1 5 10 25 50))

(define (make-change amount)
  (letrec [(chg (lambda (amount coin-idx)
                  (cond
                    ((eq? amount 0) 1)
                    ((< amount 0) 0)
                    ((< coin-idx 0) 0)
                    (else
                     (+ (chg (- amount (vector-ref coins coin-idx)) coin-idx)
                        (chg amount (sub1 coin-idx)))))))]
    (chg amount 4)))

(make-change 100)