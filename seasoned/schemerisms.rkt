#lang racket

(provide (all-defined-out)) ; export all the things!

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define eqan?
  (λ (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((and (atom? a1) (atom? a2) (eq? a1 a2)))
      (else #f))))

(define eqlist?
  (λ (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and
        (eqan? (car l1) (car l2))
        (eqlist? (cdr l1) (cdr l2))))
      ((or (atom? (car l1)) (atom? (car l2))) #f)
      (else
       (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(define member?
  (λ (a lat)
    (letrec
        ((mem (λ (l)
              (cond
              ((null? l) #f)
              ((eq? (car l) a) #t)
              (else
               (mem (cdr l)))))))
    (mem lat))))

(define Y
  (λ (X)
    ((λ (procedure)
       (X (λ (arg) ((procedure procedure) arg))))
     (λ (procedure)
       (X (λ (arg) ((procedure procedure) arg)))))))

(define find
  (lambda (n indicies results)
	(letrec
		[(rfind (λ (ns rs)
				  (cond
				   ((null? ns) #f)
				   ((eq? (car ns) n) (car rs))
				   (else
					(rfind (cdr ns) (cdr rs))))))]
	  (rfind indicies results))))


(define cons-counter '())
(define cons-counter-reset '())
(define cons-count
  (let [(count 0)]
    (set! cons-counter (λ () count))
    (set! cons-counter-reset (λ () (set! count 0)))
    (λ (a l)
      (set! count (add1 count))
      (cons a l))))
