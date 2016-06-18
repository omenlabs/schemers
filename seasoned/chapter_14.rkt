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

(define rm
  (λ (a l oh)
	(cond
	 ((null? l) (oh 'no))
	 ((atom? (car l))
	  (if (eq? (car l) a)
		  (cdr l)
		  (cons (car l)
				(rm a (cdr l) oh))))
	 (else
	  (if (atom?
		   (let/cc oh
				  (rm a (car l) oh)))
		  (cons (car l)
				(rm a (cdr l) oh))
		  (cons (rm a (car l) 0)
				(cdr l)))))))

(define rember2*
  (λ (a l)
	(let [(newl (let/cc meh (rm a l meh)))]
	  (if (atom? newl)
		  l
		  newl))))

;; Oh, we don't have try.  Macro time!
(define-syntax-rule
  (try x α β)
  (let/cc success
		  (let/cc x  ;; if a calls x, we continue on from here, returning B
				  (success α))  ;; jump up to success, returning the result of a
		  β))

;; So, if we don't find what we are looking for, we call oh.
;; In this case, if we don't change the list, we call x which
;; returns β.  If we do change the list, we hand that changed
;; list off to success which returns it to us.
(define rember3*
  (λ (a l)
	(try oh (rm a l oh) l)))
