#lang racket

(define (cube x) (* x x x))
(define count 0)
(define (p x)
  (set! count (add1 count))
  (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

(sine 27.15)
(display count)