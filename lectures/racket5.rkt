#lang racket

(require racket/trace)

;; deep recursion

(define s '((2 3 (4) ((5))) ((4)) (3 4 5)))

;; returning a single value

(define (add-tree lst)
  (cond ((null? lst) 0)
	((number? lst) lst)
	((list? lst)
	 (+ (add-tree (car lst))
	    (add-tree (cdr lst))))
	))

;; transforming a tree retaining its structure

;; using hw2 problem
;;(define (treeadd1 tree)
;;   (map-tree (lambda (x) (+ x 1)) tree))


;; assuming only numeric leaves to the tree
(define (treeadd1 tree)
  (cond ((null? tree) '())
	((not (pair? tree))
	 (+ 1 tree))
	(else
	 (cons (treeadd1 (car tree))
	       (treeadd1 (cdr tree))))))

;; assuming only numeric leaves to the tree
(define (double-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree))
	 (+ tree tree))
	(else
	 (cons (double-tree (car tree))
	       (double-tree (cdr tree))))))




;; transforming a tree to a list

(define (o-f value)
  (cond
   [(not (list? value)) (list value)]
   [(empty? value) '()]
   [else
    (append (o-f (first value))
	    (o-f (rest value)))]))