#lang racket

(load "TheReasonedSchemer/mk.scm")
(load "TheReasonedSchemer/mkextraforms.scm")

(define show-print
  (lambda (x)
    (printf "~a is ~a ~n" x (eval x))))

(define page 0)
(define start-page
  (lambda (x)
    (begin
      (set! page (sub1 x))
      (next-page))))

(define next-page
  (lambda ()
    (begin
      (set! page (add1 page))
      (printf "~n Page ~a~n" page))))

(start-page 4)
(show-print '(run* (q) fail))
(show-print '(run* (q) (== #t q)))
(show-print '(run* (q) fail (== #t q)))

(next-page) ; page 5
(show-print '(run* (q) succeed (== #t q)))
(show-print '(run* (r) succeed (== 'corn r)))
(show-print '(run* (r) fail (== 'corn r)))
(show-print '(run* (q) succeed (== #f q)))

(next-page) ; page 6
(show-print '(let [(x #f)] (== #f x)))
(show-print '(run* (x)
		   (let [(x #f)]
		     (== #t x))))
(show-print '(run* (q)
		   (fresh (x)
			  (== #t x)
			  (== #t q))))

(next-page) ; page 7
(show-print '(run* (q)
		   (fresh (x)
			  (== x #t)
			  (== #t q))))
(show-print '(run* (q)
		   (fresh (x)
			  (== x #t)
			  (== q #t))))

(show-print '(run* (x) succeed))

(next-page) ; page 8
(show-print '(run* (x)
		   (let [(x #f)]
		     (fresh (x)
			    (== #t x)))))

(show-print '(run* (r)
		   (fresh (x y)
			  (== (cons x (cons y '())) r))))

(show-print '(run* (s)
		   (fresh (t u)
			  (== (cons t (cons u '())) s))))


(show-print '(run* (r)
		   (fresh (x)
			  (let [(y x)]
			    (fresh (x)
				   (== (cons y (cons x (cons y '()))) r))))))

(show-print '(run* (r)
		   (fresh (x)
			  (let [(y x)]
			    (fresh (x)
				   (== (cons x (cons y (cons x '()))) r))))))

(next-page) ; page 9
(show-print '(run* (q)
		   (== #f q)
		   (== #t q)))

(show-print '(run* (q)
		   (== #f q)
		   (== #f q)))

;; co-refer or share
(show-print '(run* (r)
		   (fresh (x)
			  (== x r))))

(show-print '(run* (q)
		   (fresh (x)
			  (== #t x)
			  (== x q))))

(show-print '(run* (q)
		   (fresh (x)
			  (== x q)
			  (== #t x))))

(next-page) ; page 10

(show-print '(cond (#f #t) (else #f)))

(next-page) ; page 11
(show-print '(conde (fail succeed) (else fail)))
