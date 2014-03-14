#lang racket
;; factorial
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1))) ))

;; tests
(display (fact 3))
(newline)
(display (fact 5))
(newline)