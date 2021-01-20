#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file
;; Simple macro for lambda (), easier reading.
(define-syntax-rule (thunk body) (lambda () body))

;; Problema 1
;; put your code below
;; defining sequence function
(define (sequence inicio fin inc)
  (cond
    [(> inicio fin) null]
    [else    (cons inicio (sequence (+ inicio inc) fin inc))]))

;; Problema 2
;; Listof string -> String -> Listof String
(define (string-append-map list suffix)
 (map (lambda (e) (string-append e suffix)) list))

; Problema 3
; Auxiliar function to extract element of index
; Listof A -> Integer -> A
(define (extract xs n (cont 0))
  (cond 
    [(and
      (= n cont)                               ; base case
      (not (null? xs)))                        ; guard
     (car xs)]                                 ; element found
    [else (extract (cdr xs) n (+ cont 1))]))   ; natural recursion


; Function definition
; Listof A -> Integer -> A
(define (list-nth-mod  xs n)
  (cond
    [(< n 0)    (error "list-nth-mod: negative number")]
    [(null? xs) (error "list-nth-mod: empty list")]
    [else (let* 
      ([length (length xs)]
       [reman (remainder n length)])
        (extract xs reman))]))



;; Problema 4
;; Streamof A -> Integer -> Listof A
(define (stream-for-n-steps stream n)
  (cond 
    [(= n 0) null]                  ; Base Case
    [else (cons (car (stream))      ; Taking first element of the pair
                (stream-for-n-steps ; Recursion on:
                 (cdr (stream))     ;    - Procedure of the pair 
                 (- n 1)))]))       ;    - number of elements to take - 1

; Problema 5
; (fn[A] -> A -> A -> A) -> A -> Streamof A
(define (stream-maker fn arg)
  (define f (lambda (x)                         ; using define instead of letrec
              (cons x (thunk (f (fn x arg)))))) ; forming the pair 
  (thunk (f arg)))                              ; forming a thunk to avoid eval

;; Auxiliar function for using with stream-maker. Every 5 change change to negative
;; [A]: A -> A -> A
(define (for-fun a b)
  (let*
      ([suma  (apply + (map abs (list a b)))] ; Only positives
       [resto (remainder suma 5)])            ; Remainder to repeat each 5
       (cond                                  ; Conditional
         [(= resto 0) (* -1 suma)]            ; Special Case, each 5 negative
         [else        suma])))                ; Normal case
;; definimos ahora el stream
;; Streamof A
(define funny-number-stream (stream-maker for-fun 1)) ; starting with one

; Problema 6
; Creation of a stream from a list
; (Streamof A: (Pair A  (fn: Number -> A))
(define (stream-cycle-maker lista inicio)
  (define f (lambda (n) (cons  (list-nth-mod  lista n); first element of the pair
                               (thunk (f (+ n 1)))))) ; plus one increment
  (thunk (f inicio)))                                 ; thunk to avoid evaluation

;; Creation of the Stream
;; Listof A -> Number -> Stream A
(define dan-then-dog (stream-cycle-maker
                      (list "dan.jpg" "dog.jpg")
                      0))
;; Problema 7
(define (stream-add-zero stream)
  (letrec ([f (λ (stream)
                (let*
                    ([call  (stream)]         ;; Evaluates stream
                     [value (car call)]       ;; Obtain value
                     [next  (cdr call)])      ;; Next stream
                  (cons (cons 0 value)        ;; Create first element  of pair
                        (thunk (f next)))))]) ;; thunk for lazy evaluation
    (thunk (f stream))))
 

;; Problema 8

;; Listof A -> Listof A -> A -> Streamof A
(define (stream-cicles-maker lst1 lst2 inicio)
  (define f (lambda (n) (cons  (cons                       ;; Creation of the pair
                                    (list-nth-mod  lst1 n) ;; It is a pair too
                                    (list-nth-mod  lst2 n));; Element of the pair
                               (thunk (f (+ n 1))))))      ;; adding 
  (thunk (f inicio)))                                      ;; devolvemos un thunk

;; Now we use the former function for creating the stream
;; Listof A -> Listof A -> Streamof A
(define (cycle-lists xs ys)
  (stream-cicles-maker xs ys 0))

;; Problema 9
(define (vector-assoc index vector)
  (let ([len (vector-length vector)])            ; length of the vector
  ; Auxilar function
    (define (aux (n 0))                          ; n with default value.
      (cond
        [(= n len) #f]                           ; did not found. Base Case.
        [else                                    ; look for it in the vector
         (let*
             ([act-element (vector-ref vector n)]); actual pair in the vector

           (cond
             [(equal? act-element 0) #f]         ; For the next exercise         
             [(= index (car act-element))
              act-element]                       ; get the element
             [else (aux (+ n 1))]))]))           ; natural recursion
    (aux)))                                      ; Using auxilar function

; vector-assoc test
;   

;; Problema 10

; cached-assoc tests
;   (check-equal? ((cached-assoc (list (cons 1 2) (cons 3 4)) 3) 3) (cons 3 4) "cached-assoc test")
; listOfPairs -> Int -> (Int -> Pair)
; Inner creation of a struct for cache 
; cached-assoc tests
;   (check-equal? ((cached-assoc (list (cons 1 2) (cons 3 4)) 3) 3) (cons 3 4) "cached-assoc test")
; listOfPairs -> Int -> (Int -> Pair)
; Inner creation of a struct for cache 
(define (cached-assoc lista n)
  (struct control                           ; Struct for cache
    ([cache #:mutable]                      ; cache, vector of size n
     [indice #:mutable]) #:transparent)     ; to implement Round-Robin using remainder
  (let* 
      ([cache           (make-vector n)]    ; Creation of vector for cache #(0 0 0) 
       [a-cache-control (control cache 0)]) ; Index for inserting in clycle mode
    ; Creation of the function with these values.
    (lambda (index)
      (let*   
            ([cache (control-cache a-cache-control)] 
             [index-cache  (control-indice a-cache-control)]
             [elemento (vector-assoc index cache)])
        (println "Cache structure:")
        (println a-cache-control)
        (cond
          [(false? elemento)                      ; Not in cache
           (let
               ([newElemento (assoc index lista)])
             (cond
               [(false? newElemento) newElemento]
               [else 
                (begin               ;;  Round robin implementation
                  (let*
                      ([new-indice (remainder (+ index-cache 1) n)])
                    (vector-set! cache index-cache newElemento)
                    (set-control-indice! a-cache-control new-indice)
                    newElemento))]))]
          [else    elemento])))))

;; Problema 11
(define-syntax while-less
  (syntax-rules (do)                ; word do used in syntax
    [(while-less e1 do e2)          ; rule, each time is found..
      (letrec ([f (lambda (x1 x2)   ; defining a lecrec with a function
              (cond              
                [(<= x1 (x2)) #t]   ; Base condition, evaluation of x2
                [else (f x1 x2)]))]); Natural recursion
        (f e1 (thunk e2)))]))       ; Use function with thunk to avoid
                                    ; evaluation of e2 in the function f
