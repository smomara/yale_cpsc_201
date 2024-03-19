#lang racket

;; various numeric functions


(define examples
  '(
    (number? 1)
    (complex? 2+3i)
    (real? 3.14159)
    (real? +inf.0)
    (rational? 1)
    (integer? 1)
    (integer? +inf.0)
    (integer? 2.0)
    (exact-integer? 2.0)
    (exact-nonnegative-integer? 0)
    (exact-nonnegative-integer? -1)
    (exact-positive-integer? 0)
    (inexact-real? 3.4)
    (inexact-real? 3.5)
    (flonum? 3.4)
    (double-flonum? 3.4)
    (double-flonum? 3.4444444444)
    (single-flonum? 3.4)
    (zero? 0.0)
    (positive? 1)
    (negative? 1)
    (even? 1)
    (odd? 1)
    (exact? 3.14159)
    (inexact? 3.14159)
    (inexact->exact 3.14159)

    )
  )

(define (demo)
  (map
   (lambda (lst)
     (list (car lst)
	   (cadr lst)
	   '==>
	   (apply (eval (car lst)) (cdr lst))))
   examples))


(define examples2
  '(
    (remainder 10 3)
    (remainder -10 3)
    (remainder 10 -3)
    (remainder -10 -3)
    (modulo 10 3)
    (modulo -10 3)
    (modulo 10 -3)
    (modulo -10 -3)
    )
  )

(define (demo2)
  (map
   (lambda (lst)
     (list (car lst)
	   (cadr lst)
	   (caddr lst)
	   '==>
	   (apply (eval (car lst)) (cdr lst))))
   examples2))