
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))


(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [else (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([s-result (s)])
        (cons (car s-result) (stream-for-n-steps (cdr s-result) (- n 1))))))

(define (funny-number-stream)
  (define (funny-number x)
    (if (= (remainder x 5) 0) (- x) x))
  (define (cons-stream x)
    (cons (funny-number x) (lambda () (cons-stream (+ x 1)))))
  (cons-stream 1))

(define (dan-then-dog)
  (cons "dan.jpg"
        (lambda () (cons "dog.jpg"
                         dan-then-dog))))

(define ((stream-add-zero s))
  (let ([s-result (s)])
    (cons (cons 0 (car s-result))
          (stream-add-zero (cdr s-result)))))

(define ((cycle-lists xs ys))
  (define (loop n)
    (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
          (lambda () (loop (+ n 1)))))
  (loop 0))

(define ((cycle-lists-1 xs ys))
  (define (cycle-lists-aux xs-aux ys-aux)
    (let ([xs-next (if (null? xs-aux) xs xs-aux)]
          [ys-next (if (null? ys-aux) ys ys-aux)])
      (cons (cons (car xs-next) (car ys-next))
            (lambda () (cycle-lists-aux (cdr xs-next) (cdr ys-next))))))
  (cycle-lists-aux xs ys))

(define (vector-assoc v vec)
  (define (assoc i length)
    (if (= i length)
        #f
        (let ([x (vector-ref vec i)])
          (if (and (pair? x) (equal? (car x) v))
              x
              (assoc (+ i 1) length)))))
  (assoc 0 (vector-length vec)))

(define (cached-assoc xs n)
  (let ([cache (make-vector n #f)]
        [index 0]
        [cicle-index (lambda (i) ( if (= i n) 0 i))])
    (lambda (v) (or (vector-assoc v cache)
                    (let ([result (assoc v xs)])
                      (and result
                           (begin (vector-set! cache index result)
                                  (set! index (cicle-index (+ index 1)))
                                  result)))))))

