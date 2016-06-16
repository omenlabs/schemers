#lang racket

(provide member?)

;; The non-Y version of multrember

(define multirember
  (lambda (a lat)
    ((letrec
         ((mr (lambda (lat)
              (cond
                ((null? lat) '())
                ((eq? a (car lat)) (mr (cdr lat)))
                (else
                 (cons (car lat) (mr (cdr lat))))))))
       mr)
     lat)))

;; Trying factorial with letrec

(define fact
  (lambda (n)
    (letrec ((f (lambda (x)
                   (cond
                     ((eq? x 0) 1)
                     (else
                      (* x (f (- x 1)))))))) (f n)))) ; This tastes weird

;; multirember-f

(define multirember-f
  (λ (test?)
      (letrec
         ((mf (λ (a lat)
              (cond
                ((null? lat) '())
                ((test? a (car lat)) (mf a (cdr lat)))
                (else
                 (cons (car lat) (mf a (cdr lat))))))))
       mf)))

;; letrec member
(define member?
  (λ (a lat)
    (letrec
        ((mem (λ (l)
              (cond
              ((null? l) #f)
              ((eq? (car l) a) #t)
              (else
               (mem (cdr l)))))))
    (mem lat))))

;(member? 'a '(1 3 f))
;(member? 'a '(1 3 a f))

;; letrec union
(define union
  (λ (set1 set2)
    (letrec
        ((un (λ (s)
               (cond
                 ((null? s) set2)
                 ((member? (car s) set2) (un (cdr s)))
                 (else
                  (cons (car s) (un (cdr s)))))))) (un set1))))

;(union '(1 2 3) '(4 5 6))
;(union '(1 2 3) '(2 3 4))

;; Protected union
(define union-p
  (λ (set1 set2)
    (letrec ((un (λ (s)
               (cond
                 ((null? s) set2)
                 ((mem? (car s) set2) (un (cdr s)))
                 (else
                  (cons (car s) (un (cdr s)))))))
             (mem? (λ (a l)
              (cond
              ((null? l) #f)
              ((eq? (car l) a) #t)
              (else
               (mem? a (cdr l))))))) (un set1))))

;(union-p '(1 2 3) '(4 5 6))
;(union-p '(1 2 3) '(2 3 4))

;; Protected union, following the twelth commandment (don't pass down things that don't change)
(define union-p2
  (λ (set1 set2)
    (letrec ((un (λ (s)
               (cond
                 ((null? s) set2)
                 ((mem? (car s) set2) (un (cdr s)))
                 (else
                  (cons (car s) (un (cdr s)))))))
             (mem? (λ (a lat)
              (letrec ((m (λ (l)
                            (cond
                              ((null? l) #f)
                              ((eq? (car l) a) #t)
                              (else
                               (mem? a (cdr l))))))) (m lat))))) (un set1))))



;(union-p2 '(1 2 3) '(4 5 6))
;(union-p2 '(1 2 3) '(2 3 4))

;; two-in-a-row
(define two-in-a-row?
  (λ (lat)
    (letrec
        ((twoism (λ (a l)
                   (cond
                     ((null? l) #f)
                     ((eq? (car l) a) #t)
                     (else
                      (twoism (car l) (cdr l)))))))
(cond
  ((null? lat) #f)
  (else
      (twoism (car lat) (cdr lat)))))))

;(two-in-a-row? '(beep beep))
;(two-in-a-row? '(empty safe))

;; sum of prefix

;; (sum-of-prefixes '(1 1 1 1 1)) is '(1 2 3 4 5)

(define sum-of-prefixes
 (letrec ((summy (λ (sum tup)
                   (cond
                     ((null? tup) '())
                     (else
                      (cons (+ sum (car tup))
                            (summy (+ sum (car tup)) (cdr tup))))))))
   (λ (tup)
     (summy 0 tup))))

;(sum-of-prefixes '(1 1 1 1 1))

;; scramble

(define scramble
  (letrec ((pick (λ (n lat)
                   (cond
                     ((eq? n 1) (car lat))
                     (else
                      (pick (sub1 n) (cdr lat))))))
           (scram (λ (tup rev-pre)
                     (cond
                       ((null? tup) '())
                       (else
                        (cons (pick (car tup) (cons (car tup) rev-pre))
                              (scram (cdr tup)
                                          (cons (car tup) rev-pre))))))))
    (λ (tup)
      (scram tup '()))))

;(scramble '(1 1 1 3 4 2 1 1 9 2))
;(scramble '(1 2 3 4 5 6 7 8 9))
;(scramble '(1 2 3 1 2 3 4 1 8 2 10))
