#lang racket


(define multirember&co
  (λ (a lat col)
    (cond
      ((null? lat) (col '() '()))
      ((eq? (car lat) a) (multirember&co a (cdr lat)
                                         (λ (newlat seen)
                                           (col newlat (cons (car lat) seen)))))
      (else
       (multirember&co a (cdr lat) (λ (newlat seen)
                                     (col (cons (car lat) newlat) seen)))))))

(define a-friend
  (λ (x y)
    (null? y)))

(multirember&co 'tuna '(strawberries tuna and swordfish) a-friend)


(define multiinsertLR
  (λ (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) oldL)
       (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR)
       (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else
       (cons (car lat) ((multiinsertLR new oldL oldR (cdr lat))))))))

(define multiinsertLR&co
  (λ (new oldL oldR lat col)
    (cond
      ((null? lat) (col '() 0 0))
      ((eq? (car lat) oldL)
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (λ (newlat numl numr)
                           (col (cons new (cons (car lat) newlat))
                                (+ 1 numl)
                                numr))))
      ((eq? (car lat) oldR)
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (λ (newlat numl numr)
                           (col (cons (car lat (cons new newlat))
                                      numl
                                      (+ 1 numr))))))
      (else
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (λ (newlat numl numr)
                           (col (cons (car lat) newlat)
                                numl
                                numr)))))))

(define even?
  (λ (n)
    (= (remainder n 2) 0)))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define evens-only*
  (λ (lat)
    (cond
      ((null? lat) '())
      ((atom? (car lat))
       (cond
         ((even? (car lat)) (cons (car lat) (evens-only* (cdr lat))))
         (else
          (evens-only* (cdr lat)))))
      ((list? (car lat))
       (cons (evens-only* (car lat)) (evens-only* (cdr lat)))))))

(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))

(define evens-only*&co
  (λ (lat col)
    (cond
      ((null? lat) (col '() 1 0))
      ((atom? (car lat))
       (cond
         ((even? (car lat)) (evens-only*&co (cdr lat)
                                            (λ (elat prod_even sum_odd)
                                              (col (cons (car lat) elat)
                                                   (* (car lat) prod_even)
                                                   sum_odd))))
         (else
          (evens-only*&co (cdr lat)
                          (λ (elat prod_even sum_odd)
                            (col elat
                                 prod_even
                                 (+ (car lat) sum_odd)))))))
      (else
       (evens-only*&co (car lat) (λ (elat prod_even sum_odd)
                                   (evens-only*&co (cdr lat)
                                                   (λ (clat cprod_even csum_odd)
                                                     (col (cons elat clat)
                                                          (* prod_even cprod_even)
                                                          (+ sum_odd csum_odd))))))))))

(define the-last-friend
  (λ (newl prod sum)
    (cons sum (cons prod newl))))

(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)