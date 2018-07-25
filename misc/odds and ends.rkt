#lang racket

;; Case lambda for different args
(define caselambda
  (case-lambda
    [(n) (display "One Arg")]
    [(n x) (display "Two Arg")]
    [(n x . f) (display (+ 2 (length f)))]))

;; Using the => operator inside of cond
(define weirdcond
  (lambda (b)
    (cond
      [b => display]
      (else
       (display "I dunno")))))

;; Let with a proc-id
(define (countdown start)
  (let next [(n start)]
    (display n)
    (if (eq? n 0)
        (void)
        (next (sub1 n)))))
