#lang racket

(provide (all-defined-out))

(define (funny-sum xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (funny-sum (cdr xs)))]
        [(string? (car xs)) (+ (string-length (car xs))
                               (funny-sum (cdr xs)))]))

; Impelement the same idea as
; datatype exp = Const of int | Negate of exp
;              | Add of exp * exp | Multiply of exp * exp

; Helper functions that make lists where first element is a symbol
(define (Const i) (list 'Const i))
(define (Negate e) (list 'Negate e))
(define (Add e1 e2) (list 'Add e1 e2))
(define (Multipley e1 e2) (list 'Multiply e1 e2))

; Helper functions that test what "kind of exp"
; Note: could raise better errors for non-exp values.
(define (Const? x) (eq? (car x) ('Const)))
(define (Negate? x) (eq? (car x) ('Negate)))
(define (Add? x) (eq? (car x) 'Add))
(define (Multiply? x) (eq? (car x) 'Multiply))

; Helper functions that get the pieces for "one kind of exp"
; Note: more robust could check "what kind of exp"
(define (Const-int e) (car (cdr e))) ; Could just use cadr here
(define (Negate-e e) (car (cdr e)))
(define (Add-e1 e) (car (cdr e)))
(define (Add-e2 e) (car (cdr (cdr e)))) ; Could use caddr
(define (Multiply-e1 e) (car (cdr e)))
(define (Multiply-e2 e) (car (cdr (cdr e))))

; Could have used the short-hand functions for accessing the elements.
;(define Const-int cadr)
;(define Negate-e cadr)
;(define Add-e1 cadr)
;(define Add-e2 caddr)
;(define Multiply-e1 cadr)
;(define Multiple-e2 caddr)

(define (eval-exp e)
  (cond [(Const? e) e] ; Note: returning an exp, not a number
        [(Negate? e) (Const (- (Const-int (eval-exp (Negate-e e)))))]
        [(Add? e) (let ([v1 (Const-int (eval-exp (Add-e1 e)))]
                        [v2 (Const-int (eval-exp (Add-e2 e)))])
                    (Const (+ v1 v2)))]
        [(Multiply? e) (let ([v1 (Const-int (eval-exp (Multiply-e1 e)))]
                             [v2 (Const-int (eval-exp (Multiply-e2 e)))])
                         (Const (* v1 v2)))]
        [else (error "eval-exp expected an exp")]))