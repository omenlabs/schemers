#lang racket

; (two-in-a-row? '(a b c)) is #f

(define two-in-a-row?
  (λ (lat)
    (two-in-a-row-b? (car lat) (cdr lat))))

(define two-in-a-row-b?
  (λ (prev lat)
    (cond
      ((null? lat) #f)
      ((eq? prev (car lat)) #t)
      (else
       (two-in-a-row-b? (car lat) (cdr lat))))))

(two-in-a-row? '(a b c d e f))
(two-in-a-row? '(a b c c e f))

; (sum-of-prefixes '(1 1 1 1 1)) is '(1 2 3 4 5)

(define sum-of-prefixes
  (λ (lat)
   (sum-of-prefixes-b 0 lat)))

(define sum-of-prefixes-b
  (lambda (prev lat)
    (cond
      ((null? lat) '())
      (else
       (cons (+ prev (car lat))
             (sum-of-prefixes-b (+ prev (car lat)) (cdr lat)))))))

(sum-of-prefixes '(2 1 9 17 0))
(sum-of-prefixes '(1 1 1 1 1))

; scramble

(define pick
  (λ (n lat)
    (cond
      ((eq? n 1) (car lat))
      (else
       (pick (sub1 n) (cdr lat))))))

(pick 4 '(4 3 1 1 1))
(pick 2 '(2 4 3 1 1 1))

(define scramble-b
  (lambda (tup rev-pre)
    (cond
      ((null? tup) '())
      (else
       (cons (pick (car tup) (cons (car tup) rev-pre))
             (scramble-b (cdr tup)
                         (cons (car tup) rev-pre)))))))
(define scramble
  (lambda (tup)
    (scramble-b tup '())))


(scramble '(1 1 1 3 4 2 1 1 9 2))
(scramble '(1 2 3 4 5 6 7 8 9))
(scramble '(1 2 3 1 2 3 4 1 8 2 10))
