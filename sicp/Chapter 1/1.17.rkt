#lang racket

(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))



(define (mul a b)
  (let [(count 0)]
    (letrec [(fast-mul (lambda (a b)
                       (set! count (add1 count))
                       (cond
                         ((= a 1) b)
                         ((even? a) (fast-mul (/ a 2) (+ b b)))
                         (else
                          (+ b (fast-mul (/ (sub1 a) 2) (+ b b)))))))]
      (set! count 0)
      (display (if (< a b)
          (fast-mul a b)
          (fast-mul b a)))
      (printf "~n ~s" count)
      )))

(mul 300 300)