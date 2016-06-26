#lang racket

(require "schemerisms.rkt")

;; Start off with the meat on P146, skipping counting eggs

(define kons1
  (λ (kar kdr)
    (λ (selector)
      (selector kar kdr))))

(define kar1
  (λ (kons-func)
    (kons-func (λ (first second) first))))

(define kdr1
  (λ (kons-func)
    (kons-func (λ (first second) second))))

(define bons
  (λ (kar)
    (let [(kdr '())]
      (λ (selector)
	(selector
	 (λ (x) (set! kdr x))
	 kar
	 kdr)))))

(define kar
  (λ (bons-func)
    (bons-func (λ (s a d) a))))

(define kdr
  (λ (bons-func)
    (bons-func (λ (s a d) d))))

(define set-kdr
  (λ (bons-func val)
    (bons-func (λ (s a d) (s val)))))

(define kons
  (λ (kar kdr)
    (let [(x (bons kar))]
      (set-kdr x kdr)
      x)))

(define counter '())
(define reset '())

(define konsC
  (let [(count 0)]
    (set! counter (λ () count))
    (set! reset (λ () (set! count 0)))
    (λ (kar kdr)
      (set! count (add1 count))
      (kons kar kdr))))

;; now we can go back
(define lots
  (λ (m)
    (cond
     ((zero? m) '())
     (else
      (konsC 'eggs (lots (sub1 m)))))))

(define lenkth
  (λ (l)
    (cond
     ((null? l) 0)
     (else
      (add1 (lenkth (kdr l)))))))

(printf "Dozen\n")
(define dozen (lots 12))
(counter)
(reset)

(define add-at-end
  (λ (l)
    (cond
     ((null? (kdr l))
      (konsC (kar l)
	     (konsC 'egg '())))
     (else
      (konsC (kar l)
	     (add-at-end (kdr l)))))))

(define bakers-dozen (add-at-end dozen))
(printf "Bakers Dozen\n")
(counter)
(reset)

(define add-at-end-too
  (λ (l)
    (letrec
	[(A (λ (ls)
	      (cond
	       ((null? (kdr ls)) (set-kdr ls (konsC 'egg '())))
	       (else
		(A (kdr ls))))))]
      (A l)
      l)))

(printf "Bakers Dozen Too\n")
(define bakers-dozen-too (add-at-end-too dozen))
(counter)
(reset)
					  
(printf "Bakers Dozen Again\n")
(define bakers-dozen-again (add-at-end dozen))
(counter)
(reset)
	
(define eklist?
  (λ (ls1 ls2)
    (cond
     ((null? ls1) (null? ls2))
     ((null? ls2) #f)
     (else
      (and (eq? (kar ls1) (kar ls2))
	   (eklist? (kdr ls1) (kdr ls2)))))))

(printf "eklist\n")
(eklist? bakers-dozen bakers-dozen-too)

(define same?
  (λ (c1 c2)
    (let [(t1 (kdr c1))
	  (t2 (kdr c2))]
      (set-kdr c1 1)
      (set-kdr c2 2)
      (let [(v (= (kdr c1) (kdr c2)))] ; if they are the same, we changed kdr to 1 then 2
	(set-kdr c1 t1) ; put things back
	(set-kdr c2 t2)
	v))))

(printf "same?\n")
(same? dozen bakers-dozen-too) ; ERRATA: Book has bakers-dozen instead of dozen, but it is nice

(printf "same of two different kons\n")
(same? (kons 'egg '())
       (kons 'egg '()))

(define last-kons
  (λ (ls)
    (cond
     ((null? (kdr ls)) ls)
     (else
      (last-kons (kdr ls))))))

(define long (lots 12))

;; loopy time
(set-kdr (last-kons long) long)

(define finite-length
  (λ (p)
    (let/cc infinite
	    (letrec [(C (λ (p q)
			  (cond
			   ((same? p q) (infinite #f))
			   ((null? q) 0)
			   ((null? (kdr q)) 1)
			   (else
			    (+ (C (sl p) (qk q)) 2)))))
		     (qk (λ (x) (kdr (kdr x))))
		     (sl (λ (x) (kdr x)))]
	      (cond
	       ((null? p) 0)
	       (else
		(add1 (C p (kdr p)))))))))

(finite-length long)
