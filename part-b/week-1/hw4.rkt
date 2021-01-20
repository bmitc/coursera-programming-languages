#lang racket

(provide (all-defined-out))

; NOTE: In this homework I have used the Racket function thunk. This is equivalent
; to lambda with zero arguments. To be precise (thunk <expression>) is identical to
; (lambda () <expression>). I think this is clearer and is obviously shorter, so I
; have used it often when defining streams.

; Takes low, high, and stride and returns a list of numbers from low to
; a number not exceeding high incremented with stride. If high is less
; than low, then return the empty list.
(define (sequence low high stride)
  (if (> low high)
      empty
      (cons low (sequence (+ low stride) high stride))))

; Takes a list of strings and a suffix and returns a list of each element
; of the original list with suffix appened to the end of the element.
(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

; Takes a list and a number n and returns the ith element where i is the
; remainder produced by dividing n by the list's length. Errors are returned
; if n is negative or the list is empty.
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(empty? xs) (error "list-nth-mod: empty list")]
        [else (let ([rem (remainder n (length xs))])
                (list-ref xs rem))]))
; The course videos may be old becase Racket has a special keyword "else" that
; catches the last clause. So I use this instead of "#t" for the last clause.

; A stream of the natural numbers: 1, 2, 3, ...
; Used for testing other functions that operate on streams.
; Taken from the course video lectures.
(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

; Takes a stream and a number n and returns a list holdig the first n
; elements of the stream.
(define (stream-for-n-steps s n)
  (if (= n 0)
      empty              ; we've got n elements, so end the list
      (let ([pair (s)])  ; evaluate the stream s
        (cons (car pair) ; get the first element
              (stream-for-n-steps (cdr pair) (- n 1)))))) ; recurse on the remaining stream

; It is my opinion that defining streams using thunk with an internal local
; helper function is more clear than using (lambda () <expression>) and a single
; letrec local binding. See my comments at the top of the file as well.
; For example, compare my definition of funny-number-stream with the definition
; closer to that used in the course video lectures. Note that these definitions
; are basically equivalent aside from style.

; This makes it easy to create a stream definition template. For example:
;(define <stream-name>
;  (thunk
;   (define <local-helper-function)
;   (<local-helper-function-with-some-base-argument>)))

; A stream identical to the natural numbers except multiples of 5 are negated.
; For example: 1, 2, 3, 4, -5, 6, 7, 8, 9, -10, 11, ...
(define funny-number-stream
  (thunk
   (define (f x)
     (cons (if (= 0 (remainder x 5)) ; Create a value for the stream in the first pair element
               (- x)
               x)
           (thunk (f (+ x 1)))))     ; Return a thunk to compute the next value for the second pair element
   (f 1)))

; Definition similar to that found in the course video lectures.
;(define funny-number-stream
;  (letrec ([f (lambda (x)
;                (cons (if (= 0 (remainder x 5))
;                          (- x)
;                          x)
;                      (thunk (f (+ x 1)))))])
;    (thunk (f 1))))

; Stream that produces "dan.jpg", "dog.jpg", "dan.jpg", "dog.jpg", ...
(define dan-then-dog
  (thunk
   (define dan-next (thunk (cons "dan.jpg" dog-next)))
   (define dog-next (thunk (cons "dog.jpg" dan-next)))
   (dan-next))) ; Evaluates the dan-next thunk to get "dan.jpg" consed with the dog-next thunk

; Takes a stream and returns a new stream with each element being a pair
; (0 . v) where v is the value of each original element.
(define (stream-add-zero s)
  (thunk
   (define (add-zero x)
     (let ([v (x)])
       (cons (cons 0 (car v)) ; make the pair (0 . v) where v is the next value of the stream
             (thunk (add-zero (cdr v)))))) ; return a thunk that will compute the next pair
   (add-zero s)))

; Takes two lists and returns a stream where each value returned by the stream
; is a pair made up of an element from each list. The list elements are cycled
; through and then wrap back around.
(define (cycle-lists xs ys)
  (thunk
   (define (f n)
     (cons (cons (list-nth-mod xs n)
                 (list-nth-mod ys n))
           (thunk (f (+ n 1)))))
   (f 0)))

; Takes a value and a vector and returns the first pair in which the first
; element matches the value. Accepts vectors that have elements that aren't
; always pairs.
(define (vector-assoc v vec)
  (define (check-element n)
    (if (= n (vector-length vec))
        #f
        (let ([element (vector-ref vec n)])
          (if (and (pair? element) (equal? v (car element)))
              element
              (check-element (+ n 1))))))
  (check-element 0))

; I liked this particular solution with the cond but there is no way to make
; a local binding inside a cond clause, and so I had to evaluate
; (vector-ref vec n) multiple times, which I didn't like.
;(define (vector-assoc v vec)
;  (define (check-element n)
;    (cond [(> (+ n 1) (vector-length vec)) #f]
;          [(and (pair? (vector-ref vec n)) (equal? v (car (vector-ref vec n))))
;           (vector-ref vec n)]
;          [#t (check-element (+ n 1))]))
;  (check-element 0))

; Takes a list xs and a cache size and returns a function that takes a value v
; and returns (assoc v xs) but utilizes a cache to compute (assoc v xs).
(define (cached-assoc xs n)
  (let* ([cache (make-vector n #f)] ; initialize cache vector
         [slot 0] ; initialize slot
         [increment-slot (lambda () (set! slot (modulo (add1 slot) n)))])
    (lambda (v) (let ([in-cache (vector-assoc v cache)])
                  (or in-cache ; check if the cache returned something and return it if so
                      (let ([answer (assoc v xs)]) ; otherwise, compute the new answer
                        (and answer ; if there's an answer, continue to update the cache, otherwise return #f
                             (begin (vector-set! cache slot answer) ; update the cache
                                    (increment-slot)
                                    answer))))))))

; Macro that takes two expressions e1 and e2 and then executes e2 until e2
; returns a value greater than or equal to e1. Returns #t onces the evaluation
; terminates. Executes e1 exactly once and e2 at least once.
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([stop-condition e1]
              [body (thunk e2)] ; Thunk e2 to delay evaluation until the loop
              [loop (thunk (if (< (body) stop-condition)
                               (loop)
                               #t))])
       (loop))]))