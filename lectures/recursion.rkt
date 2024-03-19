#lang racket

;; recursion.rkt


(require racket/trace)

;; optional argument example; default value is length
(define (size lst . proc)
  (let ((func (if (null? proc)
		  length
		  (car proc))))
    (func lst)))

;; (size '(1 2 8 4 5))
;; (size '(1 2 8 4 5) max)

;; optional argument example; default value is length
(define (size2 lst [proc length])
  (proc lst))

;; (size2 '(1 2 8 4 5))
;; (size2 '(1 2 8 4 5) max)


(define d 736241)
(define x '(7 3 6 2 4 1))

(define (last-digit n)
  (abs (remainder n 10)))

;; break a decimal number into a list of decimal digits

(define (split n)
  (reverse (split-aux n)))

(define (split-aux n)
  (if (< n 10)
      (cons n '())
      (cons (last-digit n) (split-aux (quotient n 10)))))

;; convert a list of positive integers to a single decimal number
(define (join lst)
  (join-aux lst 0))

(define (join-aux lst num)
  (if (null? lst)
      num
      (join-aux (cdr lst) (+ (car lst) (* 10 num)))))
      

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))


;; append can be recursively defined
;; compare to (cons list1 list2)
(define (append list1 list2)
  (if (null? list1) list2
      (cons (car list1) (append (cdr list1) list2))))

;; replicate an object n times in a list
(define (replicate obj n)
  (if (zero? n)
      empty   ;; '()
      (cons obj (replicate obj (- n 1)))))


(define (trreplicate obj n)
  (trrepaux obj n empty))

(define (trrepaux obj n result)
  (if (zero? n)
      result
      (trrepaux obj (- n 1) (cons obj result))))
  

(define (length lst)
  (if (null? lst) 0
      (+ 1 (length (cdr lst)))))

(define (trlength lst)
  (trlength-aux lst 0))

(define (trlength-aux lst length)
  (if (null? lst)
      length
      (trlength-aux (cdr lst) (+ 1 length))))

(define (sum lst)
  (if (null? lst) 0
      (+ (car lst) (sum (cdr lst)))))

(define (trsum lst)
  (trsum-aux lst 0))

(define (trsum-aux lst sum)
  (if (null? lst)
      sum
      (trsum-aux (cdr lst) (+ (car lst) sum))))

;; there are a couple of problems here
(define (max lst)
  (if (null? lst)
      0
      (if (> (car lst) (max (cdr lst)))
	  (car lst)
	  (max (cdr lst)))))

(define (trmax lst)
  (trmax-aux lst -inf.0))

(define (trmax-aux lst max)
  (if (null? lst)
      max
      (if (> (car lst) max)
	  (trmax-aux (cdr lst) (car lst))
	  (trmax-aux (cdr lst) max))))


;; Ackermann Function see https://en.wikipedia.org/wiki/Ackermann_function
(define (ack m n)
  (cond ((= m 0) (+ n 1))
	((= n 0) (ack (- m 1) 1))  ;; m > 0
	(else
	 (ack (- m 1) (ack m (- n 1))))))

;; power function (racket has expt function)
(define (power x n)
  (if (zero? n)
      1
      (* x (power x (- n 1)))))

(define (trpower x n)
  (trpower-aux x n 1))

(define (trpower-aux x n result)
  (if (zero? n)
      result
      (trpower-aux x (- n 1) (* x result))))

;; page 48 https://link.springer.com/content/pdf/10.1007%2F978-1-84800-070-4.pdf
;; this is faster than expt!;
; The Algorithm Design Manual, Skienna
(define (lgpower a n)
  (if (zero? n)
      1
      (let ((x (lgpower a (quotient n 2))))
	(if (even? n)
	    (* x x)
	    (* a x x)))))


(define (memoize func [table (make-hash)])
  (lambda (arg1 arg2)
    (cond ((hash-has-key? table (list arg1 arg2))
	   (begin
	     (display "used hash value")
	     (newline)
	     (hash-ref table (list arg1 arg2))))
	  (else
	   (hash-set! table (list arg1 arg2) (func arg1 arg2))
	   (hash-ref table (list arg1 arg2))))))

; (define table (make-hash))
; (define ack (memoize ack table))