#lang racket

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)

(define (f n) (A 0 n))
;; f(n) = 2y
(define (g n) (A 1 n))
;; g(n) = f(g(n - 1)) = 2*g(n-1) = 2*f(g(n - 2)) = 2^n
(define (h n) (A 2 n))
;; h(n) = A(1 A(2 (n-1)) = g(h(n-1)) = 2^(g(h(n-2)) = 2^(2^(h(n-2)) = 2^2^2...n
(define (k n) (* 5 n n))
;; k(n) = 5n^2
