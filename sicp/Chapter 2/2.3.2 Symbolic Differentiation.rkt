#lang racket

;; 2.3.2 Example Symbolic Differentiation

;; definitions from the text
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? exp)
  (and (pair? exp) (eq? (car exp) '+)))
(define (product? exp)
  (and (pair? exp) (eq? (car exp) '*)))

(define (make-sum a1 a2)
  (cond
    ((and (number? a1) (number? a2)) (+ a1 a2))
    ((eqv? a1 0) a2)
    ((eqv? a2 0) a1)
    (else
     (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond
    ((and (number? m1) (number? m2)) (* m1 m2))
    ((or (eqv? 0 m1) (eqv? 0 m2)) 0)
    ((eqv? m1 1) m2)
    ((eqv? m2 1) m1)
    (else
     (list '* m1 m2))))

(define (addend exp) (cadr exp))
(define (augend exp) (caddr exp))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(deriv '(+ (* 3 x) (* 4 y)) 'x)

;; Exercise 2.56: Add handling of exponents

;; Exponents will be of the form (** base power)
(define (exponentiation? exp)
  (and (pair? exp) (eq? '** (car exp))))

(define (base exp)
  (cadr exp))

(define (exponent exp)
  (caddr exp))

(define (make-exponentiation base power)
  (cond
    ((eqv? power 1) base)
    ((eqv? power 0) 1)
    (else
     (list '** base power))))

(define (deriv2 exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp) (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
          (deriv2 (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; only handles power rule, not derivative of exponential function or log
(deriv2 '(** x '3) 'x)