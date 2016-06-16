#lang racket

(require "schemerisms.rkt")

(define leftmost
  (λ (l)
    (cond
     ((null? (car l)) '())
     ((atom? (car l)) (car l))
     (else
      (let [(a (leftmost (car l)))]
        (cond
          ((atom? a) a)
          (else (leftmost (cdr l)))))))))

;(leftmost '((()) 2 3))

(define rember1*
  (λ (a l)
    (letrec [(R (λ (l)
		  (cond
		   ((null? l) '())
		   ((atom? (car l))
		    (cond
		     ((eq? (car l) a) (cdr l))
		     (else
		      (cons (car l) (R (cdr l))))))
		   (else
		    (let [(a (R (car l)))]
		      (cond
		       ((eqlist? a (car l)) (cons (car l) (R (cdr l))))
		      (else
		       (cons a (cdr l)))))))))]
      (R l))))
		   
(define depth*
  (λ (l)
    (cond
     ((null? l) 1)
     ((atom? (car l)) (depth* (cdr l)))
     (else
      (let [(adep (add1 (depth* (car l))))
	    (ddep (depth* (cdr l)))
	    (max (λ (a b) (if (> a b) a b)))]
	(max ddep adep))))))

(define scramble
  (letrec ((pick (λ (n lat)
                   (cond
                     ((eq? n 1) (car lat))
                     (else
                      (pick (sub1 n) (cdr lat))))))
           (scram (λ (tup rev-pre)
                     (cond
                       ((null? tup) '())
                       (else
			(let [(tp (cons (car tup) rev-pre))]
			  (cons (pick (car tup) tp)
				(scram (cdr tup) tp))))))))
    (λ (tup)
      (scram tup '()))))

(define leftmost2
  (λ (l)
    (let/cc out
	    (letrec [(lm (lambda (l)
			   (cond
			    ((null? l) '())
			    ((atom? (car l)) (out (car l)))
			    (else
			     (begin
			       (lm (car l))
			       (lm (cdr l)))))))]
	      (lm l)))))
