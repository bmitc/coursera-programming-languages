
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (cond [(> low high) null]
        [(= low high) (cons high null)]
        [#t (cons low (sequence (+ low stride) high stride))]))

(define (string-append-map xs suffix)
  (map (lambda(x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (= n 0) null (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream
  (letrec ([f (lambda(x) (cond [(= 0 (remainder x 5)) (cons (- 0 x) (lambda() (f (+ x 1))))]
                               [#t (cons x (lambda() (f (+ x 1))))]))])
     (lambda() (f 1))))

(define dan-then-dog
  (letrec ([dan (lambda() (cons "dan.jpg" (lambda() (dog))))]
           [dog (lambda() (cons "dog.jpg" (lambda() (dan))))])
    (lambda() (dan))))

(define (stream-add-zero s)
  (letrec ([f (lambda(m)
                (cons (cons 0 (car (m))) (lambda() (f (cdr (m))))))])
    (lambda() (f s))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda(x) (cons (cons (list-nth-mod xs x) (list-nth-mod ys x)) (lambda() (f (+ x 1)))))])
    (lambda() (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda(v vec n)
                (cond [(>= n (vector-length vec)) #f]
                      [#t (letrec ([val (vector-ref vec n)])
                            (if (pair? val)
                                (if (equal? (car val) v)
                                    val
                                    (f v vec (+ n 1)))
                                (f v vec (+ n 1))))]))])
    (f v vec 0)))

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [v 0]
           [f (lambda(x)
                (if (vector-assoc x cache)
                    (vector-assoc x cache)
                    (if (assoc x xs)
                        (begin (vector-set! cache v (assoc x xs))
                               (if (= v (- n 1))
                                   (set! v 0)
                                   (set! v (+ v 1)))
                               (assoc x xs))
                        #f)))])
    f))
