#lang racket

(provide (all-defined-out))

(define page 0)
(define start-page
  (lambda (x)
    (begin
      (set! page (sub1 x))
      (next-page))))

(define next-page
  (lambda ()
    (begin
      (set! page (add1 page))
      (printf "~n Page ~a~n" page))))
