#lang racket

;; hash table
(define +optable+ (make-hash))
(define (put op type proc) (hash-set! +optable+ (list op type) proc))
(define (get op type_ (hash-ref +optable+) (list op type)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
`
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))


(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z) exp)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
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

(put 'deriv 'sum sum-d)

; derivative of a product
(define (prod-d exp var)
  (make-prod (deriv (addend exp) var)
             (deriv (augend exp) var)))

(put 'deriv 'prod prod-d)

; derivative of an exponent
(define (exp-d exp var)
   (* (exponent exp)
      (coeff exp)
      (- (exponent exp) 1)))

(put 'deriv 'exponent exp-d)
 
; real call 
(define poly (* 3 (square x)))
(get 'deriv 'sum poly x)