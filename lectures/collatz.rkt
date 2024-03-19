#lang racket

(require racket/trace)

;; The Collatz conjecture

(define (collatz n)
  (if (= (modulo n 2) 0) (/ n 2)
      (+ 1 (* n 3))))


;; generate the series based on Collatz
(define (c-series n)
  (print n)
  (newline)
  (if (equal? n 1) 'done
      (let ((next (collatz n)))
	(c-series next))))

;; generate the series based on Collatz
(define (c-series2 n)
  (print n)
  (newline)
  (if (equal? n 1) 'done
      (c-series2 (collatz n))))

;; generate the series based on Collatz without printing
;; try tracing this one.
(define (c-series3 n)
  (if (equal? n 1) 'done
      (c-series3 (collatz n))))

