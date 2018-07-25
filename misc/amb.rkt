#lang racket

(define (amb-backtrack)
  (error "no solution found"))

(define (amb . args)
  (call/cc (lambda (return)
             (let ((backtrack amb-backtrack))
               (map (lambda (x)
                      (call/cc (lambda (k)
                                 (set! amb-backtrack k)
                                 (return x))))
                    args)
               (backtrack 'fail)))))

(define (require p)
  (if (not p) (amb) #t))


(define (deweet)
  (let [(mynumber (amb 1 2 3 4 5))
        (yournumber (amb 1  3 4 5))]
    (require (< yournumber mynumber))
    (printf "~a\n" mynumber)
    (printf "~a\n" yournumber)
    ))

(deweet)
