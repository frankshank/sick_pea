#lang racket
;;;;;;;;;;;;;;;;;;
;; Section 2.5
;;
;; complex numbers
;;;;;;;;;;;;;;;;;;

; helpers 
(define (square x)
  (* x x))

; constructors
(define (make-from-real-imag real-part imaginary-part)
  (cons real-part imaginary-part))

(define (make-from-mag-ang mag ang)
  (cons mag angle))

; selectors
(define (real-part z)
  (car z))

(define (imaginary-part z)
  (cdr z))

(define (magnitude z)
  (sqrt (+ (square (real-part z))
        (square (imaginary-part z)))))

(define (angle z)
  ; TODO implement
  (z))
  
 
; complex arithmetic (add)
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imaginary-part z1) (imaginary-part z2))))

; complex arithmetic (subtract)
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imaginary-part z1) (imaginary-part z2))))


; some tests
(define z1 (make-from-real-imag 3 4))
(define z2 (make-from-real-imag 4 5))

(display "z1 -> ")
(displayln z1)
(display "z2 -> ")
(displayln z2)

(display "addition of z1 and z2 -> ")
(displayln (add-complex z1 z2))

(display "subtraction of z1 from z2 -> ")
(displayln (sub-complex z1 z2))


; tagged data support
(define (attach-tag tag z)
  (cons tag z))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (tag z)
  (car z))

(define (rectangular? z)
  (eq? (tag z) 'rectangular))

(define (polar? z)
  (eq? (tag z) 'polar))

(define (display-complex z)
  (display "tag: ")
  (displayln (car z))
  (display "real: ")
  (displayln (cadr z))
  (display "imaginary: ")
  (displayln (cddr z)))

; tests
(define z4 (make-from-real-imag-rectangular 3 4))
(display "z4 -> ")
(display-complex z4)
             