#lang racket

(require "schemerisms.rkt")

(define deep
  (λ (m)
	(if (zero? m)
		(quote pizza)
		(cons (deep (sub1 m)) '()))))


(define deepM
  (let [(Rs '())
	(Ns '())]
      (λ (n)
	(let [(exists (find n Ns Rs))]
	  (if (atom? exists)
	      (let [(result (if (zero? n)
				   'pizza
				   (cons (deepM (sub1 n)) '())))]
		(set! Rs (cons result Rs))
		(set! Ns (cons n Ns))
		result)
	      exists)))))


(define counter '())
(define reset '())

(define consC
  (let [(count 0)]
    (set! counter (λ () count))
    (set! reset (λ () (set! count 0)))
    (λ (a l)
      (set! count (add1 count))
      (cons a l))))

(define deepC
  (λ (m)
	(if (zero? m)
		(quote pizza)
		(consC (deepC (sub1 m)) '()))))

(define supercounter
  (λ (func)
    (letrec [(L (λ (n)
		  (func n)
		  (if (zero? n)
		      (counter)
		      (L (sub1 n)))))]
      (reset)
      (L 1000))))


(define deepMC
  (let [(Rs '())
	(Ns '())]
      (λ (n)
	(let [(exists (find n Ns Rs))]
	  (if (atom? exists)
	      (let [(result (if (zero? n)
				   'pizza
				   (consC (deepMC (sub1 n)) '())))]
		(set! Rs (cons result Rs))
		(set! Ns (cons n Ns))
		result)
	      exists)))))

(define rember1*C
  (λ (a l)
    (letrec [(R (λ (l oh)
		  (cond
		   ((null? l) (oh 'no))
		   ((atom? (car l))
		    (if (eq? (car l) a)
			(cdr l)
			(consC (car l)
			      (R (cdr l) oh))))
		   (else
		    (let [(new-car
			   (let/cc oh
				  (R (car l) oh)))]
		      (if (atom? new-car)
			  (consC (car l)
				(R (cdr l) oh))
			  (consC new-car
				(cdr l))))))))]
      (let [(new-l (let/cc oh (R l oh)))]
	(if (atom? new-l)
		   l
		   new-l)))))


		       
(define rember1*C2
  (λ (a l)
    (letrec [(R (λ (l)
          (cond
           ((null? l) '())
           ((atom? (car l))
            (cond
             ((eq? (car l) a) (cdr l))
             (else
              (consC (car l) (R (cdr l))))))
           (else
            (let [(a (R (car l)))]
              (cond
               ((eqlist? a (car l)) (consC (car l) (R (cdr l))))
              (else
               (consC a (cdr l)))))))))]
      (R l))))
