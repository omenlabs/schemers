#lang racket

(require "schemerisms.rkt")

(define toppings '())

(define deepB
  (lambda (m)
    (cond
     ((zero? m) (let/cc jump
			(set! toppings jump)
			(quote pizza)))
     (else
      (cons (deepB (sub1 m)) '())))))
			
		 
(deepB 4)

(cons
 (toppings 'cake)
 (cons (toppings 'mozzarelle)
       (cons (toppings 'pizza)
	     '())))

(define deep&co
  (lambda (m k)
    (cond
     ((zero? m) (k 'pizza))
     (else
      (deep&co (sub1 m)
	       (lambda (x)
		 (k (cons x '()))))))))


(define deep&coB
  (lambda (m k)
    (cond
     ((zero? m)
      (let ()
	(set! toppnigs k)
	(k 'pizza)))
     (else
      (deep&coB (sub1 m)
		(lambda (x)
		  (k (cons x '()))))))))


(define two-in-a-row?
  (letrec [(W (lambda (a lat)
		(cond
		 ((null? lat) #f)
		 (else
		  (let [(nxt (car lat))]
		    (or (eq? nxt a)
			(W nxt (cdr lat))))))))]
    (lambda (lat)
      (cond
       ((null? lat) #f)
       (else (W (cat lat) (cdr lat)))))))

(define leave '())

(define walk
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (leave (car l)))
     (else
      (begin
	(walk (car l))
	(walk (cdr l)))))))


	    

     
     
