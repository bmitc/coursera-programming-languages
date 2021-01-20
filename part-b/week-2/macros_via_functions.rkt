#lang racket

(provide (all-defined-out))

(struct const (int) #:transparent) ; int should hold a number
(struct negate (e1) #:transparent) ; e1 should hold an expression
(struct add (e1 e2) #:transparent) ; e1, e2 should hold expressions
(struct multiply (e1 e2) #:transparent) ; e1, e2 should hold expressions
(struct bool (b) #:transparent) ; b should hold #t or #f
(struct eq-num (e1 e2) #:transparent) ; e1, e2 should hold expressions
(struct if-then-else (e1 e2 e3) #:transparent) ; e1, e2, e3 should hold expressions
; A value in this language is a legal const or bool.

(define (eval-exp e)
  (cond [(const? e) e]
        [(negate? e) (let ([v (eval-exp (negate-e1 e))])
                       (if (const? v)
                           (const (- (const-int v)))
                           (error "negate applied to non-number")))]
        [(add? e) (let ([v1 (eval-exp (add-e1 e))]
                        [v2 (eval-exp (add-e2 e))])
                    (if (and (const? v1) (const? v2))
                        (const (+ (const-int v1) (const-int v2)))
                        (error "add applied to non-number")))]
        [(multiply? e) (let ([v1 (eval-exp (multiply-e1 e))]
                             [v2 (eval-exp (multiply-e2 e))])
                         (if (and (const? v1) (const? v2))
                             (const (* (const-int v1) (const-int v2)))
                             (error "multiply applied to non-number")))]
        [(bool? e) e]
        [(eq-num? e) (let ([v1 (eval-exp (eq-num-e1 e))]
                           [v2 (eval-exp (eq-num-e2 e))])
                       (if (and (const? v1) (const? v2))
                           (bool (= (const-int v1) (const-int v2)))
                           (error "eq-num applied to non-number")))]
        [(if-then-else? e) (let ([v-test (eval-exp (if-then-else-e1 e))])
                             (if (bool? v-test)
                                 (if (bool-b v-test)
                                     (eval-exp (if-then-else-e2 e))
                                     (eval-exp (if-then-else-e3 e)))
                                 (error "if-then-else applied to non-boolean")))]
        [else (error "eval-exp-wrong expected an exp")])) ; not strictly necessary

(define (andalso e1 e2)
  (if-then-else e1 e2 (bool #f)))

(define (double e)
  (multiply e (const 2)))

(define (list-product es) ; take in a Racket list of expressions
  (if (null? es)
      (const 1)
      (multiply (car es) (list-product (cdr es)))))

(define test (andalso (eq-num (double (const 4))
                              (list-product (list (const 2)
                                                  (const 2)
                                                  (const 1)
                                                  (const 2))))
                      (bool #t)))