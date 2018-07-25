#lang racket

(define stream-car
  (lambda (s)
    (car (force s))))

(define stream-cdr
  (lambda (s)
    (cdr (force s))))

(define counters
  (let next ([n 1])
    (delay (cons n (next (+ n 1))))))

(define stream-add
  (lambda (s1 s2)
    (delay (cons
             (+ (stream-car s1) (stream-car s2))
             (stream-add (stream-cdr s1) (stream-cdr s2))))))

(define (integer-stream)
  (let next [(n 1)]
    (delay (cons n (next (add1 n))))))

(define somenumbers (integer-stream))

(define (call-next)
  (let* [(stream (force somenumbers))
         (num (car stream))
         (rest (force (cdr stream)))]
    (set! somenumbers rest)
    num))
(for [(i (in-range 10))]
  (println (call-next)))