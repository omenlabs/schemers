#lang racket

(require "schemerisms.rkt")

(define deep
  (λ (m)
	(if (zero? m)
		(quote pizza)
		(cons (deep (sub1 m)) '()))))
