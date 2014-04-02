#lang racket

;;;;;;;;;;;;;;;;
;; section 2.3.3
;;;;;;;;;;;;;;;;

;; representation of trees using lists
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;; tests
(define my-tree (make-tree 3 '(1 () () 2 () ()) '(4 () () 5 () ())))
(display " ++ tree procs for: ")
(displayln my-tree) 
(entry my-tree)
(left-branch my-tree)
(right-branch my-tree)


;; check to see if x is in set
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

;; tests
`c
(element-of-set? 5 my-tree)
(element-of-set? 99 my-tree)
(element-of-set? 1 my-tree)

;; ex 2.63 - one method to convert binary tree to list
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
;; test
(displayln " ++ tree-list1")
(tree->list-1 my-tree)

;; ex 2.63 - another method to convert binary tree to a list
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;; test
(displayln " ++ tree-list2")
(tree->list-2 my-tree)


;; ex 2.6.4 convert an ordered list to a tree
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
;; helper function partial-tree
;; The result returned by partial-tree is a pair (formed with cons) 
;; whose car is the constructed tree and whose 
;; cdr is the list of elements not included in the tree
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;; test
(display " ++ Converting the oprdered list --> ")
(define slist '(2 4 6 8 10 12))
(displayln slist)

(displayln "... to a tree:")
(define ntree (list->tree slist))
(display ntree)

;; key fake
(define (key x) x)
;; lookup a database of records by its key
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))
