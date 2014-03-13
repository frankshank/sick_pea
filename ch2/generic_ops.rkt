#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 3 different ways to slice the table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (type-tag x)
  (car x))
  
(define (contents x)
  (cdr x))

(define (put type-tag operation procedure)
  (displayln " ++ put procedure Not implemented yet"))

(define (get type op)
  (displayln "get not implemented yet")
  (lambda (x) x))

;; conventional style
(define (area shape)
  (cond ((eq? (type-tag shape) 'square)
         (* (contents shape) (contents shape)))
        ((eq? (type-tag shape) 'circle)
         (* pi (contents shape) (contents shape)))
        (else (error "Unknown shape"))))

;; data directed programmin
(put 'square 'area (lambda (s) (* s s)))
(put 'circle 'area (lambda (r) (* pi (* r r))))

(get 'square 'area)
((get 'square 'area) 4)

; generic operator procedure
(define (operate op obj)
  (let ((proc (get (type-tag obj) op)))
    (if proc
        (proc (contents op))
        (error "Unknown operator for type"))))
  
; syntactic sugaring that wraps operate
(define (area2 shape)
  (operate 'area shape))
 
;; message passing
;; return the data as represented as a procedure
;; no longer useing tagged data
(define (make-square side)
  (lambda (message)
    (cond ((eq? message 'area)
           (* side side))
          ((eq? message 'perimeter)
           (* 4 side))
          (else (error "Unknown message")))))
          
; note we have to re-define operate
(define (operate2 op obj)
  (obj op))