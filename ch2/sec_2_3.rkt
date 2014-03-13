#lang racket
;;;;;;;;;;;;;;
;; Section 2.3
;;;;;;;;;;;;;;
(define a 1)
(define b 2)

(list a b)
(list 'a 'b)
(list 'a b)

(car '(a b c))
(cdr '(a b c))

; checks if a symbol is in the list
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)))
        (else (memq item (cdr x)))))

(memq 'a '(b c d a g))
(memq 'a '(b c d h g))

;; define equal? in terms of eq for lists
;; if car of a an b and cdr of a and b same
;; we can consider them to be equal
(define (equal? a b)
  (cond ((and (null? a) (null? b) #t))
        ((or 
          (and (null? a) (pair? b))
          (and (null? b) (pair? a)))
         #f)          
        ((eq? (car a) (car b)) (equal? (cdr a) (cdr b)))
        (else #f)))

(displayln " ++ Oak")
(equal? '(a b c) '(a b c))
(equal? '(a b c) '(a b d))
(equal? '(a b c) '(a b c d))


; Grabbed from vlad much more concise
(define (equal2? a b) 
   (if (and (pair? a) (pair? b)) 
       (and (equal2? (car a) (car b)) (equal2? (cdr a) (cdr b))) 
       (eq? a b))) 

(displayln " ++ Vlad")
(equal2? '(a b c) '(a b c))
(equal2? '(a b c) '(a b d))
(equal2? '(a b c) '(a b c d))
