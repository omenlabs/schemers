#lang racket

; Following http://www.ece.uc.edu/~franco/C511/html/Scheme/ycomb.html

; Step 0
(define fact-named
  (λ (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))

; Step 1
(define fact-maker
  (lambda (procedure)
    (lambda (n)
      (if (zero? n)
          1
          (* n ((procedure procedure) (sub1 n)))))))

((fact-maker fact-maker) 5)

; Step 2: We moved the name out of the body of the code, but still have to pass
; fact-maker to fact-maker

; We merely expand out (fact-maker fact-maker)
(define fact-nameless
  ((λ (procedure)
     (λ (n)
       (if (zero? n)
           1
           (* n ((procedure procedure) (sub1 n))))))
   (λ (procedure)
     (λ (n)
       (if (zero? n)
           1
           (* n ((procedure procedure) (sub1 n))))))))

(fact-nameless 5)

; We achieve namelessness!
(((λ (procedure)
     (λ (n)
       (if (zero? n)
           1
           (* n ((procedure procedure) (sub1 n))))))
   (λ (procedure)
     (λ (n)
       (if (zero? n)
           1
           (* n ((procedure procedure) (sub1 n))))))) 5)

; Step 3: There is a meta pattern here
; ((procedure procedure) (sub1 n)) is the same as ((λ (arg) ((procedure procedure) arg)) (sub1 n))

;(define F
;  ((λ (func-arg)
;     (λ (n)
;       (if (zero? n)
;           1
;           (* n (func-arg (sub1 n))))))
;   (λ (arg) ((procedure procedure) arg))))

; Step 4: apply step 3 to step 2
(define fact32
  ((λ (procedure)
     ((λ (func-arg)
        (λ (n)
          (if (zero? n)
              1
              (* n (func-arg (sub1 n))))))
      (λ (arg) ((procedure procedure) arg))))
   (λ (procedure)
     ((λ (func-arg)
        (λ (n)
          (if (zero? n)
              1
              (* n (func-arg (sub1 n))))))
      (λ (arg) ((procedure procedure) arg))))))

(define F*
  (λ (func-arg)
    (λ (n)
      (if (zero? n)
          1
          (* n (func-arg (sub1 n)))))))

(define fact-simp
  ((λ (procedure)
     (F* (λ (arg) ((procedure procedure) arg))))
   (λ (procedure)
     (F* (λ (arg) ((procedure procedure) arg))))))

; Step 5
(define Y
  (λ (X)
    ((λ (procedure)
       (X (λ (arg) ((procedure procedure) arg))))
     (λ (procedure)
       (X (λ (arg) ((procedure procedure) arg)))))))

(define fact (Y F*))

