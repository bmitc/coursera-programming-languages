#lang typed/racket

(define-type TwoInts (Pair Integer Integer))
(define-type mytype (U (TwoInts 'Str 'Pizza)))