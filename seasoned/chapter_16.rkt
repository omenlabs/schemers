#lang racket

(require "schemerisms.rkt")

(define sweet-tooth
  (λ (food)
	(cons food
		  (cons 'cake '()))))

(define last 'anglefood)


(define sweet-toothL
  (λ (food)
	(set! last food)
	(cons food
		  (cons 'cake '()))))

(printf "sweet-toothL\n")
(sweet-toothL 'chocolate)
last

(define ingredients '())

(define sweet-toothR
  (λ (food)
	(set! ingredients (cons food ingredients))
	(cons food
		  (cons 'cake '()))))

(printf "sweet-toothR\n")
(sweet-toothR 'laphroaig)
(sweet-toothR 'ardbeg)
ingredients

(define deep
  (λ (m)
	(cond
	 ((zero? m) (quote pizza))
	 (else
	  (cons (deepM (sub1 m)) (quote ()))))))

(define Ns '())
(define Rs '())

(define deepR
  (λ (n)
	(let [(result (deep n))]
	  (set! Ns (cons n Ns))
	  (set! Rs (cons result Rs))
	  result)))

;; memoize time
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

(deepR 2)
(deepR 3)
(deepR 5)
(find 2 Ns Rs)

(define deepM
  (let [(Rs '())
		(Ns '())]
	(λ (n)
	  (if
	   (member? n Ns)
	   (find n Ns Rs)
	   (let [(result (deep n))]
		 (set! Ns (cons n Ns))
		 (set! Rs (cons result Rs))
		 result)))))

(deepM 4)

(find 3 '() '())

(define deepM2
  (let [(Rs '())
		(Ns '())]
	(λ (n)
	  (let [(exists (find n Ns Rs))]
		(if (atom? exists) ; got #f
			(let [(result (deep n))]
			  (set! Ns (cons n Ns))
			  (set! Rs (cons result Rs))
			  result)
			exists))))) ; exists is a sexp

(define length
  (λ (l)
	0))

(set! length
	  (lambda (l)
		(cond
		 ((null? l) 0)
		 (else
		  (add1 (length (cdr l)))))))

(define length2
  (let [(h (lambda (l) 0))]
	(set! h
		  (lambda (l)
			(cond
			 ((null? l) 0)
			 (else
			  (add1 (h (cdr l)))))))
	h))

;; eliminate the parts of length2 related to length
;; (define length2
;;   (let [(h (lambda (l) 0))]
;; 	(set! h
;; 		  ...)
;;   h))

(define L
  (λ (length)
	(lambda (l)
	  (cond
	   ((null? l) 0)
	   (else
		(add1 (length (cdr l))))))))

(define length3
  (let [(h (lambda (l) 0))]
	(set! h
		  (L (lambda (arg) (h arg))))
	h))

;; what is the value (L (lambda (arg) (h arg)))
;; (lambda (l)
;;   (cond
;;     ((null? l) 0)
;;     (else
;;       (add1 ((lambda (arg) (h arg)) (cdr l))))

(define Y!
  (λ (L)
	(let [(h (lambda (l) '()))]
	  (set! h
			(L (lambda (arg) (h arg))))
	  h)))

(define Y-bang
  (λ (f)
	(letrec
		[(h (f (λ (arg) (h arg))))]
	  h)))

;; Y! == Y-bang
(define length4
  (Y! L))


;; Can we take this further?
;; https://docs.racket-lang.org/reference/let.html

(define is-even?f
  (λ (oddf)
	(λ (n)
	  (or (zero? n)
		  (oddf (sub1 n))))))

(define is-odd?f
  (λ (evenf)
	(λ (n)
	  (and (not (zero? n))
		   (evenf (sub1 n))))))

;; mutual recursion
(define Y2
  (λ (F1 F2)
	(let [(x1 (λ (n) '()))
		  (x2 (λ (n) '()))]
		(set! x1 (F1 (λ (n) (x2 n))))
		(set! x2 (F2 (λ (n) (x1 n))))
		x1)))

(define is-odd?
  (Y2 is-odd?f is-even?f))
