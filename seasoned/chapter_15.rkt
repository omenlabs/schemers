#lang racket

(require "schemerisms.rkt")

;; completely unrelated, but wanted to write another syntax rule
(define-syntax-rule
  (looper start stop func)
  (let/cc done
          (letrec [(loopy (λ (n)
                            (begin
                              func
                              (if (eq? n stop)
                                  (done)
                                  (loopy (add1 n))))))]
            (loopy start))))

;;(looper 1 10 (printf "Hi!\n"))
(define x 'fooo)

(define dinerR
  (λ (food)
    (set! x food)
    (cons 'milkshake
          (cons food '()))))

(printf "Setting global x\n")
(dinerR 'onion)
x

(define omnivore
  (let [(x 'minestrone)]
    (λ (food)
      (set! x food)
      (cons food
            (cons x '())))))

(printf "Omnivore let\n")
(omnivore 'daikon)
x

(define gobbler
  (let [(x 'minestrone)]
    (λ (food)
      (set! x food)
      (cons food
            (cons x '())))))

(printf "Gobbler let\n")
(gobbler 'asdf)
x

(define food 'none)
(define glutton
  (λ (x)
	(set! food x)
	(cons 'more
		  (cons x
				(cons 'more
					  (cons x '()))))))

(printf "Glutton\n")
(glutton 'garlic)

(define chez-nous
  (λ ()
	(set! food x)
	(set! x food)))
(printf "chez-nous\n")
(chez-nous)
x
food
