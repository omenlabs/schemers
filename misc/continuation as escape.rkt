#lang racket


(define (list-mult a-list)
  (call/cc (lambda (return)
             (letrec ((listrec (lambda (list)
                                 (cond
                                   ((empty? list) 1)
                                   ((zero? (car list)) (return #f))
                                   (else
                                    (* (car list) (listrec (cdr list))))))))
               (listrec a-list)))))

(list-mult '(1 0 2 3 ))