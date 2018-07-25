#lang racket

(define (sqrt-iter check guess x)
  (if (check guess x)
      guess
      (sqrt-iter check (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))


(define (square x) (* x x))

(define (sqrt x)
  (sqrt-iter good-enough? 1.0 x))

(define (sqrt2 x)
  (sqrt-iter percent-enough 1.0 x))

(define (percent-enough guess x)
  (< (abs (- 1 (/ guess (improve guess x)))) 0.0000001))

(define (compare x)
(abs (- (sqrt x) (sqrt2 x))))

(compare 5)
(compare 0.0001)
(compare 4458877050500055)
