#lang racket

(require "common.rkt")

(require "TheReasonedSchemer/mk.scm")
(require "TheReasonedSchemer/mkextraforms.scm")

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define-syntax show
  (syntax-rules ()
    ((_ expression)
     (let [  (result (eval expression ns))
	  ]
       (printf "~a is ~a ~n" expression result)))))


(start-page 4)
(show '(run* (q) fail))
(show '(run* (q) (== #t q)))
(show '(run* (q) fail (== #t q)))

(next-page) ; page 5
(show '(run* (q) succeed (== #t q)))
(show '(run* (r) succeed (== 'corn r)))
(show '(run* (r) fail (== 'corn r)))
(show '(run* (q) succeed (== #f q)))

(next-page) ; page 6
(show '(let [(x #f)] (== #f x)))
(show '(run* (x)
		   (let [(x #f)]
		     (== #t x))))
(show '(run* (q)
		   (fresh (x)
			  (== #t x)
			  (== #t q))))

(next-page) ; page 7
(show '(run* (q)
		   (fresh (x)
			  (== x #t)
			  (== #t q))))
(show '(run* (q)
		   (fresh (x)
			  (== x #t)
			  (== q #t))))

(show '(run* (x) succeed))

(next-page) ; page 8
(show '(run* (x)
		   (let [(x #f)]
		     (fresh (x)
			    (== #t x)))))

(show '(run* (r)
		   (fresh (x y)
			  (== (cons x (cons y '())) r))))

(show '(run* (s)
		   (fresh (t u)
			  (== (cons t (cons u '())) s))))


(show '(run* (r)
		   (fresh (x)
			  (let [(y x)]
			    (fresh (x)
				   (== (cons y (cons x (cons y '()))) r))))))

(show '(run* (r)
		   (fresh (x)
			  (let [(y x)]
			    (fresh (x)
				   (== (cons x (cons y (cons x '()))) r))))))

(next-page) ; page 9
(show '(run* (q)
		   (== #f q)
		   (== #t q)))

(show '(run* (q)
		   (== #f q)
		   (== #f q)))

;; co-refer or share
(show '(run* (r)
		   (fresh (x)
			  (== x r))))

(show '(run* (q)
		   (fresh (x)
			  (== #t x)
			  (== x q))))

(show '(run* (q)
		   (fresh (x)
			  (== x q)
			  (== #t x))))

(next-page) ; page 10

(show '(cond (#f #t) (else #f)))

(next-page) ; page 11
(show '(conde (#f succeed) (else fail)))
(show '(conde (fail fail) (else succeed)))
(show '(conde (succeed succeed) (else fail)))
(show '(run* (x)
	     (conde
	      ((== 'olive x) succeed)
	      ((== 'oil x) succeed)
	      (else fail))))

(next-page) ; page 12
(show '(run 1 (x)
	     (conde
	      ((== 'olive x) succeed)
	      ((== 'oil x) succeed)
	      (else fail))))

(show '(run* (x)
	     (conde
	      ((== 'virgin x) fail)
	      ((== 'olive x) succeed)
	      (succeed succeed)
	      ((== 'oil x) succeed)
	      (else fail))))

(next-page) ;page 13

(show '(run 2 (x)
	    (conde
	     ((== 'extra x) succeed)
	     ((== 'virgin x) fail)
	     ((== 'olive x) succeed)
	     ((== 'oil x) succeed)
	     (else fail))))

(show '(run* (r)
	     (fresh (x y)
		    (== 'split x)
		    (== 'pea y)
		    (== (cons x (cons y '())) r))))


(show '(run* (r)
	     (fresh (x y)
		    (conde
		     ((== 'split x) (== 'pea y))
		     ((== 'navy x) (== 'bean y))
		     (else fail))
		    (== (cons x (cons y '())) r))))

(show '(run* (r)
	     (fresh (x y)
		    (conde
		     ((== 'split x) (== 'pea y))
		     ((== 'navy x) (== 'bean y))
		     (else fail))
		    (== (cons x (cons y '(soup))) r))))

(next-page) ; page 14

(define teacup
  (lambda (x)
    (conde
     ((== 'tea x) succeed)
     ((== 'cup x) succeed)
     (else fail))))

(show '(run* (x) (teacup x)))

(show '(run* (r)
	     (fresh (x y)
		    (conde
		     ((teacup x) (== #t y) succeed)
		     ((== #f x) (== #t y))
		     (else fail))
		    (== (cons x (cons y '())) r))))

(show '(run* (r)
	     (fresh (x y z)
		    (conde
		     ((== y x) (fresh (x) (== z x)))
		     ((fresh (x) (== y x)) (== z x))
		     (else fail))
		    (== (cons y (cons z '())) r))))

(show '(run* (r)
	     (fresh (x y z)
		    (conde
		     ((== y x) (fresh (x) (== z x)))
		     ((fresh (x) (== y x)) (== z x))
		     (else fail))
		    (== #f x)
		    (== (cons y (cons z '())) r))))

(show '(run* (q)
	     (let [(a (== #t q))
		   (b (== #f q))]
	       b)))
				 

(show '(run* (q)
	     (let [(a (== #t q))
		   (b (fresh (x)
			     (== x q)
			     (== #f x)))
		   (c (conde
		       ((== #t q) succeed)
		       (else (== #f q))))]
	       b)))
