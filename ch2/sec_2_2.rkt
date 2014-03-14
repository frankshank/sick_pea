#lang racket
(define nil '())

; grab a particular item in the list (0-indexed)
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1)) ))

; length of a list here we use an auxiliary iterative method
(define (length items)
  (define (length-iter a count) 
  (if (null? a)
      count
      (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

; recursive strategy for append by consing the car of list1 with list2
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2)) ))
<<<<<<< HEAD
=======
             
; Ex 2.17 last pair that returns the list with only last element 
; of a non-empty list:
; (last-pair (list 23 72 149 34)) --> (34)
(define (last-pair items)
  (if (null? (cdr items))
      (list (car items))
      (last-pair (cdr items)) ))
      
             
>>>>>>> 505157c1bbf7d347e9cc86b536c01cf88cbbb87d
