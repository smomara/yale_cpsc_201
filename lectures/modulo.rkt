#lang racket

;; modulo.rkt


(define examples
  '(
    (remainder 10 3)
    (remainder -10.0 3)
    (remainder 10.0 -3)
    (remainder -10 -3)
    (modulo 10 3)
    (modulo -10.0 3)
    (modulo 10.0 -3)
    (modulo -10 -3)
    (/ 10 3)
    (/ -10.0 3)
    (/ 10.0 -3)
    (/ -10 -3)
    (quotient 10 3)
    (quotient -10.0 3)
    (quotient 10.0 -3)
    (quotient -10 -3)
    )
  )

(define (demo)
  (map
   (lambda (lst)
     (list (car lst)
	   (cadr lst)
	   (caddr lst)
	   '==>
	   (apply (eval (car lst)) (cdr lst))))
   examples))