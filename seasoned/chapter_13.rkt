#lang racket

(require "chapter_12.rkt")

;; intersect, letrecy

(define intersect
  (λ (set1 set2)
    (letrec ((I (λ (set)
                  (cond
                    ((null? set) '())
                    ((member? (car set) set2) (cons (car set) (I (cdr set))))
                    (else
                     (I (cdr set)))))))
      
      (cond
        ((null? set2) '())
        (else
         (I set1))))))

;(intersect '(future understand) '(belief understand))

(define intersectall
  (λ (lset)
    (let/cc hop
      (letrec
          ((I (λ (set1 set2)
                (letrec ((I (λ (set)
                              (cond
                                ((null? set) '())
                                ((member? (car set) set2) (cons (car set) (I (cdr set))))
                                (else
                                 (I (cdr set)))))))
                  
                  (cond
                    ((null? set2) (hop '()))
                    (else
                     (I set1))))))
           (A (lambda (lset)
                (cond
                  ((null? (car lset)) (hop '()))
                  ((null? (cdr lset)) (car lset))
                  (else
                   (I (car lset)
                      (A (cdr lset))))))))
        (cond
          ((null? lset) '())
          (else
           (A lset)))))))

(intersectall '((3 mangoes and)
                ()
                (3 diet hamburgers)))

(intersectall '((3 steaks and)
                (no food and)
                (three baked pototoes)
                (3 diet hamburgers)))

;; rember with letrec

(define rember
  (λ (a lat)
    (letrec ((R (lambda (l)
                (cond
                  ((null? l) '())
                  ((eq? (car l) a) (cdr l))
                  (else
                   (cons (car l) (R (cdr l)))))))) (R lat))))

(rember 'a '(a s d f))

(define rember-beyond-first
  (λ (a lat)
    (letrec ((R (λ (l)
                  (cond
                    ((null? l) '())
                    ((eq? (car l) a) '())
                    (else
                     (cons (car l) (R (cdr l)))))))) (R lat))))

(rember-beyond-first 'root '(woot woot root boot toot))

(define rember-upto-last
  (λ (a lat)
    (let/cc skip
      (letrec ((R (λ (l)
                    (cond
                      ((null? l) '())
                      ((eq? (car l) a) (skip (R (cdr l)))) ; this is awesome
                      (else
                       (cons (car l) (R (cdr l)))))))) (R lat)))))

(rember-upto-last 'a '(a beer a bar brat a great time))