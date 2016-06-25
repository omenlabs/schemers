#lang racket

(require racket/trace)

(define Y
  (λ (F)
    ((λ (x) (x x))
     (λ (z) (F (λ (y) ((z z) y)))))))

(define biz
  (let [(x 0)]
	(λ (f)
	  (set! x (add1 x))
          (printf "x is ~a\n" x)
	  (lambda (a)
		(if (= a x)
			0
			(f a))))))

(trace Y)
(trace biz)
((Y biz) 5)