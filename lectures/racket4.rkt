#lang racket

(require racket/trace)

;;; let creates local variables within your procedure.
;;; it does not pollute the global namespace

(define (f x)
  (let ((y (* x 2))
	(z (+ x 2)))
    (list x y z)))




;;; let* is like let, but you can refer to other local variables within let

(define (g x)
  (let* ((y (* x 2))
	 (z (+ y 2)))
    (list x y z)))

;;; this is what you would need to do if you did not have let*

(define (h x)
  (let ((y (* x 2)))
    (let ((z (+ y 2)))
      (list x y z))))


(define (our-length lst)
  (if (empty? lst)
      0
      (+ 1 (our-length (rest lst)))))

(define (tr-length lst)
  (tr-length-aux lst 0))

(define (tr-length-aux lst result)
  (if (empty? lst)
      result
      (tr-length-aux (rest lst) (+ 1 result))))


(define (member? item lst)
  (cond
   [(empty? lst) #f]
   [(equal? item (first lst)) #t]
   [else (member? item (rest lst))]))


(define (double-each lst)
  (if (null? lst)
      '()
      (cons (* 2 (first lst))
	    (double-each (rest lst)))))

(define (tr-double-each lst)
;  (reverse (tr-de-aux lst '())))
   (tr-de-aux (reverse lst) '()))

(define (tr-de-aux lst result)
  (if (null? lst)
      result
      (tr-de-aux (rest lst)
		 (cons (* 2 (first lst)) result))))

(define (our-append lst1 lst2)
  (if (empty? lst1)
      lst2
      (cons (first lst1) (our-append (rest lst1) lst2))))

(define (square-each lst)
  (if (empty? lst)
      '()
      (cons (* (first lst) (first lst))
	    (square-each (rest lst)))))

 (define (double-each2 lst)
   (map (lambda (x) (* 2 x))
	lst))

(define (square-each2 lst)
  (map (lambda (x) (* x x))
       lst))

(define (square-each3 lst)
  (map sqr lst))

(define (our-map proc lst)
  (if (empty? lst)
      '()
      (cons (proc (first lst))
	    (our-map proc (rest lst)))))

(define (o-f value)
  (cond
   [(not (list? value)) (list value)]
   [(empty? value) '()]
   [else
    (append (o-f (first value))
	    (o-f (rest value)))]))