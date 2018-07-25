#lang racket

(define fib
    (λ (n)
      (cond
        ((eq? n 0) 1)
        ((eq? n 1) 1)
        (else
         (+ (fib (sub1 n)) (fib (- n 2)))))))

(time (fib 23))

(define fib-n
  (let [(mem (make-hash))]
    (λ (n)
        (cond
          ((eq? n 0) 1)
          ((eq? n 1) 1)
          (else
           (let [(result (hash-ref mem n #f))]
             (if (integer? result)
                 result
                 (begin
                   (set! result (+ (fib-n (sub1 n)) (fib-n (- n 2))))
                   (hash-set! mem n result)
                   result))))))))
                         
(time (fib-n 23))

(define memoize
  (λ (func)
    (printf "Making hash\n")
    (let [(mem (make-hash))]
      (λ (n)
        (let [(result (hash-ref mem n #f))]
            (if (eq? result #f)
                (begin
                  (set! result (func n))
                  (printf "Setting hash ~a\n" n)
                  (hash-set! mem n result)
                  result)
                result))))))

(define F
  (λ (func)
    (λ (n)
      (cond
        ((eq? n 0) 1)
        ((eq? n 1) 1)
        (else
         (+ (func (sub1 n)) (func (- n 2))))))))


(define Y!
  (λ (func)
    (let [(x (λ (n) '()))]
      (set! x (memoize (func (lambda (n) (x n)))))
      x)))

(time ((Y! F) 23))

