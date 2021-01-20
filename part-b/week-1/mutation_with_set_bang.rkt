#lang racket

(define b 3)
(define f (lambda (x) (* 1 (+ x b))))
(define c (+ b 4)) ; 7
(set! b 5)
(define z (f 4))   ; 9
(define w c)       ; 7

; Makes a local copy of a
(define a 3)
(define g
  (let ([a a])
    (lambda (x) (* 1 (+ x a)))))