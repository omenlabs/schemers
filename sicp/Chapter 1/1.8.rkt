#lang racket

(define (cubert-iter check guess x)
  (if (check guess x)
      guess
      (cubert-iter check (improve guess x)
                 x)))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (square x) (* x x))
         (define (cube x) (* x (square x)))

(define (cubert x)
  (cubert-iter good-enough? 1.0 x))

(define (good-enough? guess x)
  (< (abs (- guess (improve guess x))) (* guess 0.0000001)))
