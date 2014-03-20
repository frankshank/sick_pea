#lang racket
;;;;;;;;;;;;;;
;; section 2.1.3
;; defined cons car and cdr for pairs
;; as procedures
;;;;;;;;;;;;;;

(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))

;; tests
(define pair (cons 3 4))
(displayln pair)
(displayln (pair 0))
(displayln (pair 1))

(display "car: ")
(displayln (car pair))
(display "cdr: ")
(displayln (cdr pair))

