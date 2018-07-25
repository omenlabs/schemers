#lang racket

(let ([kn (call/cc (lambda (k) (cons k 0)))])
  (let ([k (car kn)]
        [n (cdr kn)])
    (display n)
    (newline)
    (k (cons k (+ 1 n)))))