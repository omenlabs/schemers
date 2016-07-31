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
	(set! toppings k)
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
       (else (W (car lat) (cdr lat)))))))

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

(define start-it
  (lambda (l)
    (let/cc here
	    (set! leave here)
	    (walk l))))

(define fishnchips '((potato) (chips (chips (with))) fish))

(start-it fishnchips)

(define fill '())
(define waddle
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (begin
	(let/cc rest
		(set! fill rest)
		(leave (car l)))
	(waddle (cdr l))))
     (else
      (begin
	(waddle (car l))
	(waddle (cdr l)))))))

(define start-it2
  (lambda (l)
    (let/cc here
	    (set! leave here)
	    (waddle l))))

(start-it2 fishnchips)

(define get-next
  (lambda ()
    (let/cc here-again
	    (set! leave here-again)
	    (fill))))

(define get-first
  (lambda (l)
    (let/cc here
	    (set! leave here)
	    (waddle l)
	    (leave '()))))

(define ptwo-in-a-row*?
  (lambda (l)
    (let [(fst (get-first l))]
      (if (atom? fst)
	  (ptwo-in-a-row-b*? fst)
	  #f))))

(define ptwo-in-a-row-b*?
  (lambda (a)
    (let [(n (get-next))]
      (if (atom? n)
	  (or (eq? n a)
	      (ptwo-in-a-row-b*? n))
	  #f))))

(define two-in-a-row*?
  (letrec
      [(T? (lambda (a)
	     (let [(n (get-next))]
	       (if (atom? n)
		   (or (eq? n a)
		       (T? n))
		   #f))))
       (get-next
	(lambda ()
	  (let/cc here-again
		  (set! leave here-again)
		  (fill))))
       (fill (lambda (x) x))
       (waddle
	(lambda (l)
	  (cond
	   ((null? l) '())
	   ((atom? (car l))
	    (begin
	      (let/cc rest
		      (set! fill rest)
		      (leave (car l)))
	      (waddle (cdr l))))
	   (else
	    (begin
	      (waddle (car l))
	      (waddle (cdr l)))))))
       (leave (lambda (x) x))]
    (lambda (l)
      (let [(fst (let/cc here
			 (set! leave here)
			 (waddle l)
			 (leave '())))]
	(if (atom? fst) (T? fst) #f)))))

(two-in-a-row*? '(((food) ()) (((food)))))



     
     
