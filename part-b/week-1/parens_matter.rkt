#lang racket

(provide (all-defined-out))

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

;Don't do something like this
;(define (fact1 n)
;  (if (= n 0)
;      (1)
;      (* n (fact1 (- n 1)))))