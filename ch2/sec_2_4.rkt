#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Excercise 2.73
;;
;; The derive program for symbolic differentiation
;; can be transformed into a data directed style by
;; re-writing the deriv procedure.
;; Our job is to write the procedures for sums and
;; products and the auxilliary code required to install
;; them into our optable dispatch table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; first up the dispatch table as a (note non-functional) hashtable
(define +optable+ (make-hash))
(define (put op type proc) (hash-set! +optable+ (list op type) proc))
(define (get op type) (hash-ref +optable+ (list op type)))

;; the re-written deriv procedure
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

;; the selectors
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

<<<<<<< HEAD
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z) exp)
    (sqrt (+ (square (real-part z))
             (square (imag-part z))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag ’rectangular x))
    (put ’real-part ’(rectangular) real-part)  
    (put ’imag-part ’(rectangular) imag-part)
    (put ’magnitude ’(rectangular) magnitude)
    (put ’angle ’(rectangular) angle)
    (put ’make-from-real-imag ’rectangular
      (lambda (x y) (tag (make-from-real-imag x y))))
    (put ’make-from-mag-ang ’rectangular
      (lambda (r a) (tag (make-from-mag-ang r a))))
’done)

; derivative sum
(define (sum-d exp var)
  (make-sum (deriv (addend exp) var)
            (deriv (augend exp) var)))
=======
; other pish
(define (attach-tag tag exp)
  (cons tag exp))

;; the predicates that we had before from section 2.3.2
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) 
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;; the sum derivative package in bonkers closure fashion
(define (install-sum-package) 
  (define (sum-deriv expr var)
    (make-sum (deriv (addend expr) var) 
              (deriv (augend expr) var)))
  (define (addend expr) (car expr))
  (define (augend expr) (cadr expr))
  (define (make-sum x1 x2)
    (cond ((and (number? x1) (number? x2)) (+ x1 x2))
          ((eq? x1 0) x2)
          ((eq? x2 0) x1)
          (else (list '+ x1 x2))))
  (define (mul-deriv expr var)
    (make-sum (make-product (multiplier expr)
                            (deriv (multiplicand expr) var))
              (make-product (multiplicand expr)
                            (deriv (multiplier expr) var))))
  (define (multiplier expr) (car expr))
  (define (multiplicand expr) (cadr expr))
  (define (make-product x1 x2)
    (cond ((and (number? x1) (number? x2)) (* x1 x2))
          ((= x1 1) x2)
          ((= x2 1) x2)
          ((or (= x1 0) (= x2 0)) 0)
          (else (list '* x1 x2))))
  (put 'deriv '+ sum-deriv)
  (put 'deriv '* mul-deriv)
  'done)

(define (make-sum x y) 
  ((get 'make-sum '+) x y)) 
>>>>>>> 94ed6f7996b9b7b02e552a1601bbb37057b79204


;; the product package
(define (install-product-package) 
  (define (make-product m1 m2) (cons m1 m2)) 
  (define (multiplier p) (cadr p)) 
  (define (multiplicand p) (caddr p)) 
  (define (deriv-product exp var) 
    (make-sum 
     (make-product (multiplier exp) 
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))
  
  (define (tag x) (attach-tag '* x)) 
  (put 'deriv '(*) deriv-product) 
  (put 'make-product '* 
       (lambda (x y) (tag (make-product x y)))) 
  'done) 

(define (make-product x y) 
  ((get 'make-product '*) x y))


<<<<<<< HEAD
(put 'deriv 'exponent exp-d)
 
; real example 
(define poly (* 3 (square x)))
(get 'deriv 'sum poly x)
=======
;; add the exponentiation rule
(define (exponentiation-deriv expr var) 
  (make-product (exponent expr) 
                (make-product 
                 (make-exponentiation (base expr)
                                      (make-sum (exponent expr) -1))
                 (deriv (base expr) var))))
(define (exponent expr) (cadr expr))
(define (base expr) (car expr))
(define (make-exponentiation base exponent)
  (cond ((= exponent 0) 1)
        ((= exponent 1) base)
        ((= base 1) 1)
        (else (list '** base exponent))))

(put 'deriv '** exponentiation-deriv)  

; real example 3x^2 +4x
(define poly '(3x**2 + 4x))
(display "Derivative of ")
(displayln poly)
(display " is ")
(displayln (deriv poly 'x))
>>>>>>> 94ed6f7996b9b7b02e552a1601bbb37057b79204
