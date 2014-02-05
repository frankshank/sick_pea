#lang racket
; ex 1.2 arsey fraction --> -37/150
(/ (+ 5 4
      (- 2 
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (* (- 6 2)
         (- 2 7))) )
