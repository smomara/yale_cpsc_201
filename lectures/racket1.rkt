#lang racket

;; defining racket variables and functions

;; global variables
(define age 18)

(define new-age (+ age 4))

;; global functions

;; defining a function as a lambda expression
(define plus-four (lambda (n) (+ n 4)))

;; alternative syntax for defining functions
(define (plus-five n) (+ n 5))


;; x takes exactly two arguments and returns their sum
;; (x 3 4) => 7
(define x (lambda (a b) (+ a b)))

;; or equivalently:
(define (x2 a b) (+ a b))

;; y takes any number of arguments and returns them as a list
;; (y 1 2 3 4) => (1 2 3 4)
(define y (lambda n n))

;; or equivalently:
(define (y2 . n) n)

;; z takes any number of arguments and returns their sum
;; (z 1 2 3 4) => 10
(define z (lambda n (apply + n)))

;; or equivalently:
(define (z2 . n) (apply + n))



;; default binary operation is +
;; can specify another operator
;; (binaryop 10 2) => 12
;; (binaryop 10 2 *) => 20
;; (binaryop 10 2 /) => 5
;; (binaryop 10 2 <) => #f
;; (binaryop 10 2 >) => #t
(define (binaryop a b . operator)
  (let ((op (if (null? operator) +
		(car operator))))
    (op a b)))

(define (betterbinaryop a b [operator +])
  (operator a b))