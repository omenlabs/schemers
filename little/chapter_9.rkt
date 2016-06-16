#lang racket

; chapter 9

(define pick
  (λ (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else
       (pick (sub1 n) (cdr lat))))))

(define looking
  (λ (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (λ (want item lat)
    (cond
      ((number? item) (keep-looking want (pick item lat) lat))
      (else
       (eq? want item)))))

(looking 'caviar '(6 2 4 caviar 5 7 3))
(looking 'caviar '(6 2 poo caviar 5 7 3))

; This is unnatural recursion.  Looking is a partial function since there are some (many) sets that
; won't stop.  All functions up until now have been total functoins.
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define a-pair?
  (λ (set)
    (cond
      ((atom? set) #f)
      ((null? set) #f)
      ((null? (cdr set)) #f)
      ((null? (cdr (cdr set))) #t)
      (else
       #f))))

(define first
  (λ (p) (car p)))

(define second
  (λ (p) (car (cdr p))))

(define build
  (λ (x y) (cons x (cons y '()))))


(define shift
  (λ (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(define align
  (λ (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (align (shift pora)))
      (else
       (build (first pora)
              (align (second pora)))))))

(define length*
  (λ (pora)
    (cond
      ((null? pora) 0)
      ((atom? (car pora)) (+ 1 (length* (cdr pora))))
      (else (+ (length* (car pora)) (length* (cdr pora)))))))

(define C
  (λ (n)
    (cond
      ((= 1 n) 1)
      ((even? n) (C (/ n 2)))
      (else
       (C (+ 1 (* 3 n)))))))

(define A
  (λ (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else
       (A (sub1 n)
          (A n (sub1 m)))))))

(define length
  (λ (l)
    (cond
      ((null? l) 0)
      (else
       (add1 (length (cdr l)))))))

((λ (mk-length) (mk-length 'eternity)) (λ (length) (λ (l) (cond ((null? l) 0)
                                                                (else (add1 (length (cdr l))))))))

; Chapter 10

