#lang racket

(require racket/trace)

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;;; use cond instead of if
(define (fact n)
  (cond ((= n 1) 1)
	(else
	 (* n (fact (- n 1))))))