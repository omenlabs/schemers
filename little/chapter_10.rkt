#lang racket

; Assumptoins for chapter 10
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define 1st-sub-exp
  (λ (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (λ (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (λ (aexp)
    (car aexp)))

(define x
  (lambda (a b)
    (cond
      ((zero? b) 0) ;note how we don't have to worry about 1
      (else (+ a (x a (sub1 b)))))))

(define ↑
  (λ (n m)
    (cond
      ((eq? m 0) 1)
      (else (x n (↑ n (sub1 m)))))))

(define first
  (λ (p) (car p)))

(define second
  (λ (p) (car (cdr p))))

; Chapter 10

(define lookup-in-entry
  (λ (name entry entry-f)
    (lookup-in-entry-help
     name
     (first entry)
     (second entry)
     entry-f)))

(define lookup-in-entry-help
  (λ (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car values))
      (else
       (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))

; Tests for lookup-in-entry
(lookup-in-entry 'asdf '((a asdf b) (beef cow murder)) (λ (name) (display 'fail)))
(lookup-in-entry 'adf '((a asdf b) (beef cow murder)) (λ (name) (display 'fail)))

; onward
(define extend-table cons)

(define example-table '(((entrée dessere)
                         (spaghetti spumoni))
                        ((appetizer entrée beverage)
                         (food tastes good))))

(define lookup-in-table
  (λ (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else
       (lookup-in-entry
        name
        (car table)
        (λ (name) (lookup-in-table name (cdr table) table-f)))))))

; reaclling value from chapter 6
(define value
  (λ (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) '+) (+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
      ((eq? (operator nexp) 'x) (x (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
      ((eq? (operator nexp) '↑) (↑ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))))))

(define expression-to-action
  (λ (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else
       (list-to-action e)))))

(define atom-to-action
  (λ (a)
    (cond
      ((eq? a 'quote) quote)
      ((eq? a 'car) car)
      ((eq? a 'cdr) cdr)
      ((eq? a 'cond) cond)
      ((eq? a 'lambda) lambda)
       