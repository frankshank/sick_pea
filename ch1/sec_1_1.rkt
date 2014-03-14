#lang racket

;; average of two vals
(define (avg x y)
  (/ (+ x y) 
     2))

;; segment
(define (make-segment a b) (cons a b))
(define (start-segment s) (car s))
(define (end-segment z) (cdr z))

;; point
(define (make-point x y) (cons x y))
(define (x-cord p) (car p))
(define (y-cord p) (cdr p))

;; midpoint of a segment
(define (midpoint-segment s)
  (let 
    ((dx (- (x-cord (end-segment s))
            (x-cord (start-segment s))))
     (dy (- (y-cord (end-segment s))
            (y-cord (start-segment s)))) )

  (make-point (avg dx) (avg dy)))) 

;; find the base
(define (find-power base n)
  (if (> (remainder n base) 0)
      0
      (+ 1 (find-power base (/ n base) ))))
