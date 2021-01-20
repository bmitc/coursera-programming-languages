#lang racket

(provide (all-defined-out))

; This is a silly addition function that purposely runs slow
; for demonstration purposes.
(define (slow-add x y)
  (letrec ([slow-id (lambda (y z)
                      (if (= 0 z)
                          y
                          (slow-id y (- z 1))))])
    (+ (slow-id x 50000000) y)))

; Multiplies x and result of y-thunk, calling y-thunk x times
(define (my-mult x y-thunk) ;; assumes x is >= 0
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [#t (+ (y-thunk) (my-mult (- x 1) y-thunk))]))

; slow
(slow-add 3 4)

; fast
(my-mult 0 (lambda () (slow-add 3 4)))

; slow
(my-mult 1 (lambda () (slow-add 3 4)))

; slower
(my-mult 2 (lambda () (slow-add 3 4)))

; slow
(my-mult 0 (let ([x (slow-add 3 4)]) (lambda () x)))

; just slow
; slow
(my-mult 2 (let ([x (slow-add 3 4)]) (lambda () x)))