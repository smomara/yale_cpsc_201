#lang racket

;; This is a comment
;; define a list comprising 10 digits
(define digits '(0 1 2 3 4 5 6 7 8 9))


;; define a function, num-to-word, which takes a single digit, num,
;; and returns the corresponding English word
;; ----------------------------------------------------------------
(define (num-to-word num)
  (if (> num 9) #f
      (let ((numbers '("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine")))
	(list-ref numbers num))))

;; define a function, word-to-num, which is the inverse of num-to-word
;; Note that we define a sub-function within the function itself.
;; The sub-function, find-index, is recursive.
;; -------------------------------------------------------------------
(define (word-to-num word)
  (let ((numbers '("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine")))
    (define (find-index lst n)
      (cond ((null? lst) #f)
	    ((equal? (first lst) word) n)
	    (else 
	     (find-index (rest lst) (+ n 1)))))
    (find-index numbers 0)))

;; test out the functions

(define x (map num-to-word digits))

(define y (sort x string<?))

(define z (map word-to-num y))







