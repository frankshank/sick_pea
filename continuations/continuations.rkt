#lang racket

; CPS-style factorial
(define (factorial cc n)
       (if (= n 0)
           (cc 1)
           (factorial (lambda (x) (cc (* n x))) (- n 1))))

(factorial displayln 3)
(factorial displayln 5)