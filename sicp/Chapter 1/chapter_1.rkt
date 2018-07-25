#lang racket

;; Exercise 1.2

(/ (+ 5 (+ 4 (- 2 (- 3 (+ 6 (/ 4.0 5))))))
   (* 3 (* (- 6 2) (- 2 7))))

;; Exercise 1.3
(define sum-of-squares
  (lambda (a b)
    (+ (* a a) (* b b))))

(define larger
  (lambda (a b)
    (if (> a b)
	a
	b)))

(define sq-sum-of-larger
  (lambda (a b c)
    (if (= a (larger a b))
	(sum-of-squares a (larger b c))
	(sum-of-squares b (larger a c)))))
     
;; Exercise 1.4
;; If b is greater than 0 we add, otherwise subtract

;; Exercise 1.5

;; This will go infinite on applicative order but work on normal order


;; Exercise 1.6

;; Since we are doing applicative order evaluation, both arguments to
;; the new-if are evaulated before making the choice.  This means that
;; we sqrt-iter forever.

;; Exercise 1.7

;; Very small numbers are very close to the .001 already
;; Very large numbers may never get close enough to being .001 apart

(define (percent-enough guess x)
  (< (abs (- 1 (/ guess (improve guess x)))) 0.0000001))

;; Exercise 1.8

(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))
;; This process is recursive because inc acts on the result of a
;; recursive call to +

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

;; This is iterative.  This is because the recursive call to + only
;; acts on results that are not recursive

;; Exercise 1.10

(define (f n) (A 0 n))
;; f(n) = 2y
(define (g n) (A 1 n))
;; g(n) = f(g(n - 1)) = 2*g(n-1) = 2*f(g(n - 2)) = 2^n
(define (h n) (A 2 n))
;; h(n) = A(1 A(2 (n-1)) = g(h(n-1)) = 2^(g(h(n-2)) = 2^(2^(h(n-2)) = 2^2^2...n-1
(define (k n) (* 5 n n))
;; k(n) = 5n^2

;; Exercise 1.11

see 1.11.rkt

;; Exercise 1.12

see 1.12.rkt

;; Exercise 1.13

skipped

;; Exercise 1.14

We branch out for each kind of coin.  So O(n^5)

;; Exercise 1.15

a. 5.  This is easy to see since we divide by 3 each recursion

b. Every iteration reduces the angle by 3.  This means that if we had 27,
it would take three steps to decrease the angle to less than .1.  We
call since twice for every step.  so 2log3,n or O(log(n))

;; Exercise 1.16

(define (even? n)
    (= (remainder n 2) 0))

(define (exp b n)
  (letrec [(exp-iter (lambda (a n)
    (cond
    ((= n 1) a)
    (else
     (exp-iter (* a (* b b)) (/ n 2))))))]
  (cond
    ((even? n) (exp-iter 1 n))
    (else
     (* b (exp-iter 1 (sub1 n)))))))

;; 1.17

see 1.17.rkt

