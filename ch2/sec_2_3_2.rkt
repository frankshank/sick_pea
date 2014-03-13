#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lecture 3B
;; symbolic differentiation
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dx 0.00001)

; define a derivative procedure that
; takes a procedure as its argument 
; and  produces a procedure as its value
(define (deriv f)
  (lambda (x)
    (/ (- (f (+ x dx))
          (f x))
        (dx))))

(define d (deriv (lambda (x) (* x x))))
;(d 3)
     

; start building the primitives of our language
(define (atom? x) 
  (not (list? x)))

(atom? 3)
(atom? 'a)
(atom? '(a b c))

; constant?
(define (constant? exp var)
  (and (atom? exp)
       (not (eq? exp var))))
 
(constant? '3x 'x)

; same-var?
(define (same-var? exp var)
  (and (atom? exp)
       (eq? exp var)))

(same-var? 1 1)

; sum?
(define (sum? exp)
  (and (not (atom? exp))
       (eq? (car exp) '+))) 
    
(sum? '(+ 3x 6))

; make a sum
(define (make-sum a1 a2)
  (list '+ a1 a2))

(make-sum '4x 5)

; pull out the elements of our sum
(define addend cadr)
(define augend caddr)

(define my-sum (make-sum '5x 3))
(display "sum: ") my-sum
(display "addend: ") (addend my-sum)
(display "augend: ") (augend my-sum)

; is it a product
(define (product? exp)
  (and (not (atom? exp))
       (eq? (car exp) '*)))

(product? '(* 14x 9))

; make a product
(define (make-product m1 m2)
  (list '* m1 m2))

(make-product '7x 12)

; pull out the elements of our product
(define m1 cadr)
(define m2 caddr)
