#lang racket/base

(provide run* run == conde succeed fail)


;; frame 1
(define-syntax run
  (syntax-rules ()
    ((_ n_hat (x) g ...)
     (let [(n n_hat)
	   (x (var x))]
       (if (or (not n) (> n 0))
	   (map_inf n
		    (lambda (s)
		      (reify (walk* x s)))
		    ((all g ...) empty-s))
	   '())))))

(define-syntax case-inf
  (syntax-rules ()
    ((_ e on-zero ((a_hat) on-one) ((a f) on-choice))
     (let [(a_inf e)]
       (cond
	((not a_inf) on-zero)
	((not (and
	       (pair? a_inf)
	       (procedure? (cdr a_inf))))
	 (let [(a_hat a_inf)]
	   on-one))
	(else (let [(a (car a_inf))
		    (f (cdr a_inf))]
		on-choice)))))))

(define-syntax mzero
  (syntax-rules ()
    ((_) #f)))

(define-syntax unit
  (syntax-rules ()
    ((_ a) a)))

(define-syntax choice
  (syntax-rules ()
    ((_ a f) (cons a f))))

(define-syntax map_inf
  (lambda (n p a_inf)
    (case_inf a_inf
	      '()
	      ((a) (cons (p a) '()))
	      ((a f) (cons (p a)
			   (cond
			    ((not n) (map_inf n p (f)))
			    ((> n 1) (map_inf (sub1 n) p (f)))
			    (else '())))))))


;; frame 2

(define succeed (lambda (s) (unit s))) ; lambda_g
(define fail (lambda (s) (mzero))) ; lambda_g

(define ==
  (lambda (v w)
    (lambda (s)			; lambda_g
	      (cond
	       ((unify v w s) => succeed)
	       (else (fail s))))))
	      
      
(define-syntax fresh
  (syntax-rules ()
    ((_ (x ... ) g ... )
     (lambda (s)			; lambda_g
	       (let [(x (var x)) ... ]
		 ((all g ... ) s))))))

(define-syntax cond_e
  (syntax-rules ()
    ((_ c ... ) (cond-aux if_e c ... ))))

(define-syntax all
  (syntax-rules ()
    ((_ g ... ) (all-aux bind g ... ))))

(define-syntax all_i
  (syntax-rules ()
    ((_ g ... ) (all-aux bind_i g ... ))))

(define-syntax cond_i
  (syntax-rules ()
    ((_ c ... ) (cond-aux if_i c ... ))))

(define-syntax cond_a
  (syntax-rules ()
    ((_ c ... ) (cond-aux if_a c ... ))))

(define-syntax cond_u
  (syntax-rules ()
    ((_ c ... ) (cond-aux if_u c ... ))))

;; page 3
(define mplus
  (lambda (a_inf f)
    (case_inf a_inf
	      (f)
	      ((a) (choice a f))
	      ((a f_0) (choice a
			       (lambda () (mplus (f_0) f))))))) ; lambda_f
(define bind
  (lambda (a_inf g)
    (case_inf a_inf
	      (mzero)
	      ((a) (g a))
	      ((a f) (mplus (g a)
			    (lambda () (bind (f) g))))))) ; lambda_f

(define mplus_i
  (lambda (a_inf f)
    (case_inf a_inf
	      (f)
	      ((a) (choice a f))
	      ((a f_0) (choice a
			       (lambda () ; lambda_f
					 (mplus_i (f) f_0)))))))

(define bind_i
  (lambda (a_inf g)
    (case_inf a_inf
	      (mzero)
	      ((a) (g a))
	      ((a f) (mplus_i (g a)
			      (lambda () (bind_i (f) g))))))) ; lambda_f

;; frame 4
(define-syntax cond-aux
  (syntax-rules (else)
    ((_ ifer) fail)
    ((_ ifer (else g ... )) (all g ... ))
    ((_ ifer (g ... )) (all g ... ))
    ((_ ifer (g_0 g ... ) c ... )
     (ifer g_0
	   (all g ... )
	   (cond-aux ifer c ... )))))

(define-syntax all-aux
  (syntax-rules ()
    ((_ bnd) succeed)
    ((_ bnd g) g)
    ((_ bnd g_0 g ... )
     (let [(g_hat g_0)]
       (lambda (s)			;lambda_g
	 (bnd (g_hat s)
	      (lambda (s)		;lambda_g
		((all-aux bnd g ...) s))))))))

;;frame 5

(define-syntax if_e
  (syntax-rules ()
    ((_ g_0 g_1 g_2)
     (lambda (s) 			;lambda_g
       (mplus ((all g_0 g_1) s)
	      (lambda ()		; lambda_f
		(g_2 s)))))))

(define-syntax if_i
  (syntax-rules ()
    ((_ g_0 g_1 g_2)
     (lambda (s)			;lambda_g
       (mplus_i ((all g_0 g_1) s)
		(lambda ()		;lambda_f
		  (g_2 s)))))))

;; frame 6
(define-syntax if_a
  (syntax-rules ()
    ((_ g_0 g_1 g_2)
     (lambda (s)			;lambda_g
       (let [(s_inf (g_0 s))]
	 (case_inf s_inf
		   (g_2 s)
		   ((s) (g_1 s))
		   ((s f) (bind s_inf g_1))))))))

(define-syntax if_u
  (syntax-rules ()
    ((_ g_0 g_1 g_2)
     (lambda (s) 			;lambda_g
       (let [(s_inf (g_0 s))]
	 (case_inf s_inf
		   (g_2 s)
		   ((s) (g_1 s))
		   ((s f) (g_1 s))))))))
	  
