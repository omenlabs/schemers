#lang racket

;Chapter 1
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; Chapter 2
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (l lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? l (car lat))
                (member? l (cdr lat)))))))
; Chapter 3
(define rember
  (lambda (l lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) l) (cdr lat))
      (else (cons (car lat) (rember l (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((eq? (car lat) old) (cons old (cons new (cdr lat))))
         (else
          (cons (car lat) (insertR new old (cdr lat)))))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cons old (cdr lat))))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(define eq2
  (lambda (x a b)
    (cond
      ((eq? x a) #t)
      ((eq? x b) #t)
      (else #f))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((eq2 (car lat) o1 o2) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (l lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) l) (multirember l (cdr lat)))
      (else (cons (car lat) (multirember l (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))

; Chapter 4

(define add1
  (lambda (n)
    (+ 1 n)))

(define sub1
  (lambda (n)
    (- n 1)))

(define o+
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (add1 (o+ a (sub1 b)))))))

(define o-
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (sub1 (o- a (sub1 b)))))))

(define addtup
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (o+ (car lat) (addtup (cdr lat)))))))

(define x
  (lambda (a b)
    (cond
      ((zero? b) 0) ;note how we don't have to worry about 1
      (else (o+ a (x a (sub1 b)))))))

(define tup+
  (lambda (lat1 lat2)
    (cond
      ((null? lat1) lat2)
      ((null? lat2) lat1)
      (else (cons
             (o+ (car lat1) (car lat2))
             (tup+ (cdr lat1) (cdr lat2)))))))

(define >
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (> (sub1 n) (sub1 m))))))

(define <
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (< (sub1 n) (sub1 m))))))

(define =
  (lambda (n m)
    (cond
      ((> n m) #f)
      ((< n m) #f)
      (else #t))))

(define ↑
  (λ (n m)
    (cond
      ((eq? m 0) 1)
      (else (x n (↑ n (sub1 m)))))))

(define ÷
  (λ (n m)
    (cond
      ((< n m) 0)
      (else (add1 (÷ (- n m) m))))))

(define length
  (λ (lat)
    (cond
      ((null? lat) 0)
      (else
       (add1 (length (cdr lat)))))))

(define pick
  (λ (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else
       (pick (sub1 n) (cdr lat))))))

(define rempick
  (λ (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else
       (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (λ (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (λ (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(define eqan?
  (λ (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((and (atom? a1) (atom? a2) (eq? a1 a2)))
      (else #f))))

(define occur
  (λ (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(define one?
  (λ (n)
    (= n 1)))

; Chapter 5

; The text orders this differently and uses atom? instead of pair?
(define rember*
  (λ (a l)
    (cond
      ((null? l) '())
      ((pair? (car l)) (cons (rember* a (car l)) (rember* a (cdr l))))
      (else
       (cond
         ((eq? (car l) a) (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l)))))))))

; Going to do this with atom? like the text
(define insertR*
  (λ (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else
       (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(define occur*
  (λ (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else ;handle list
       (+
        (occur* a (car l))
        (occur* a (cdr l)))))))

(define subst*
  (λ (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else ;handle list
       (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(define insertL*
  (λ (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
         (else (cons (car l) (insertL* new old (cdr l))))))
      (else
       (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(define member*
  (λ (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or
        (eq? (car l) a)
        (member* a (cdr l))))
      (else
       (or
        (member* a (car l))
        (member* a (cdr l)))))))

(define leftmost
  (λ (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(define eqlist?
  (λ (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and
        (eqan? (car l1) (car l2))
        (eqlist? (cdr l1) (cdr l2))))
      ((or (atom? (car l1)) (atom? (car l2))) #f)
      (else
       (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (λ (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eq? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else
       (eqlist2? s1 s2)))))

(define eqlist2?
  (λ (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
       (and
        (equal? (car l1) (car l2))
        (equal? (cdr l1) (cdr l2)))))))

(define rember5 ; remove first s exp
  (λ (s l)
    (cond
      ((null? l) '())
      ((equal? (car l) s) (cdr l))
      (else
       (cons (car l) (rember s (cdr l)))))))

(define rember5* ; recursive s-expression removal, not in text.
  (λ (s l)
    (cond
      ((null? l) '())
      ((equal? (car l) s) (rember5 s (cdr l)))
      ((atom? (car l)) (cons (car l) (rember5 s (cdr l))))
      (else
       (cons (rember5 s (car l)) (rember5 s (cdr l)))))))

; Chapter 6

(define numbered?
  (λ (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and
        (numbered? (car aexp))
        (numbered? (car (cdr (cdr aexp)))))))))

(define value-orig
  (λ (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) '+) (+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) 'x) (x (value (car nexp)) (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) '↑) (↑ (value (car nexp)) (value (car (cdr (cdr nexp)))))))))

(define value-second
  (λ (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car nexp) '+) (+ (value (car (cdr nexp))) (value (car (cdr (cdr nexp))))))
      ((eq? (car nexp) 'x) (x (value (car (cdr nexp))) (value (car (cdr (cdr nexp))))))
      ((eq? (car nexp) '↑) (↑ (value (car (cdr nexp))) (value (car (cdr (cdr nexp)))))))))

(define 1st-sub-exp
  (λ (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (λ (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (λ (aexp)
    (car aexp)))

(define value
  (λ (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) '+) (+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
      ((eq? (operator nexp) 'x) (x (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
      ((eq? (operator nexp) '↑) (↑ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))))))

(define sero?
  (λ (n)
    (null? n)))

(define edd1
  (λ (n)
    (cons '() n)))

(define zub1
  (λ (n)
    (cdr n)))

(define splus
  (λ (a b)
    (cond
      ((sero? b) a)
      (else
       (splus (edd1 a) (zub1 b)))))) 

; Chapter 7
(define set?
  (λ (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat)))))) 

(define makeset
  (λ (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

(define makeset2
  (λ (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat) (makeset2 (multirember (car lat) (cdr lat))))))))

(define subset?
  (λ (set1 set2)
    (cond
      ((null? set1) #t)
      (else
       (and
        (member? (car set1) set2)
        (subset? (cdr set1) set2))))))

(define eqset?
  (λ (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(define intersect? ; do the sets intersect?
  (λ (set1 set2)
    (cond
      ((null? set1) #f)
      (else
       (or (member? (car set1) set2)
           (intersect? (cdr set1) set2))))))

(define intersect 
  (λ (set1 set2)
    (cond
      ((or (null? set1) (null? set2)) '())
      ((member (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
      (else
       (intersect (cdr set1) set2)))))

(define union
  (λ (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else
       (cons (car set1) (union (cdr set1) set2))))))

(define intersect-all
  (λ (sets)
    (cond
      ((null? (cdr sets)) (car sets))
      (else
       (intersect (car sets) (intersect-all (cdr sets)))))))

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

(define third
  (λ (p) (car (cdr (cdr p)))))

(define fun?
  (λ (l) (set? (firsts l))))

(define revpair
  (λ (p)
    (build (second p) (first p))))

(define revrel
  (λ (l)
    (cond
      ((null? l) '())
      (else
       (cons
        (revpair (car l))
        (revrel (cdr l)))))))

(define seconds
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (second (car l)) (seconds (cdr l)))))))

(define fullfun?
  (λ (l)
    (set? (seconds l))))

(define one-to-one?
  (λ (l)
    (one-to-one? (revrel l))))

; Chapter 8

(define rember-f-orig
  (λ (test? a l)
    (cond
      ((null? l) '())
      ((test? a (car l)) (cdr l))
      (else
       (cons (car l) (rember-f test? a (cdr l)))))))

(define rember-f
  (λ (test?)
    (λ (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) (cdr l))
        (else
         (cons (car l) ((rember-f test?) a (cdr l))))))))

(define insertR-f
  (λ (test?)
    (λ (new old lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) old) (cons old (cons new (cdr lat))))
        (else
         (cons (car lat) ((insertR-f test?) new old (cdr lat))))))))

(define insertL-f
  (λ (test?)
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) old) (cons new (cons old (cdr lat))))
        (else (cons (car lat) ((insertL-f test?) new old (cdr lat))))))))

(define insert-g
  (λ (test? seq)
    (λ (new old lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) old) (seq new old (cdr lat)))
        (else
         (cons (car lat) ((insert-g test? seq) new old (cdr lat))))))))

(define seqL
  (λ (new old lat)
    (cons new (cons old lat))))

(define seqR
  (λ (new old lat)
    (cons old (cons new lat))))

(define seqS
  (λ (new old lat)
    (cons new lat)))

; skipped some of the piecing together of old functions

(define atom-to-function
  (λ (a)
    (cond
      ((eq? a '+) +)
      ((eq? a 'x) x)
      (else ↑))))

(define value-f
  (λ (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
       ((atom-to-function (operator nexp)) (value-f (1st-sub-exp nexp)) (value-f (2nd-sub-exp nexp)))))))

(define multirember-f
  (λ (test?)
    (λ (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) ((multirember-f test?) a (cdr l)))
        (else
         (cons (car l) ((multirember-f test?) a (cdr l))))))))

(define mutlirember-eq? (multirember-f eq?))

(define eq?-c
  (λ (c)
    (λ (a)
      (eq? c a))))

(define eq?-tuna (eq?-c 'tuna))

(define multiremberT
  (λ (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat)) (multiremberT test? (cdr lat)))
      (else
       (cons (car lat) (multiremberT test? (cdr lat)))))))


; baked in test

(define woodchuck '((how much (wood))
                    could
                    ((a (wood) chuck))
                    (((chuck)))
                    (if (a) ((wood chuck)))
                    could chuck wood))
(define banana '((banana)
                 (split ((((banana ice)))
                         (cream (banana))
                         sherbet))
                 (banana)
                 (bread)
                 (banana brandy)))
(define chips '((potato) (chips ((with) fish) (chips))))
