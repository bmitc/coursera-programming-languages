#lang racket
  
(provide (all-defined-out))

; Definition of structures for MUPL programs. Provided by course.
(struct var (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int (num) #:transparent)  ;; a constant number, e.g., (int 17)
(struct add (e1 e2) #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4) #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual) #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2) #:transparent) ;; make a new pair
(struct fst (e) #:transparent) ;; get first part of a pair
(struct snd (e) #:transparent) ;; get second part of a pair
(struct aunit () #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

; A closure is not in "source" programs but is a MUPL value. It is what functions
; evaluate to. Provided by course.
(struct closure (env fun) #:transparent)

;; Problem 1

; Takes a Racket list and returns a MUPL list with the same elements.
(define (racketlist->mupllist lst)
  (if (empty? lst)
      (aunit)
      (apair (car lst)
             (racketlist->mupllist (cdr lst)))))

; Takes a MUPL list and returns a Racket list with the same elements.
(define (mupllist->racketlist mlist)
  (if (aunit? mlist)
      empty
      (cons (apair-e1 mlist) ; Note the use of the struct functions and not car and cdr
            (mupllist->racketlist (apair-e2 mlist)))))

;; Problem 2

; Takes an environment and a variable name as a string and returns the value
; bound to the variable. Provided by the course. Do NOT change this function.
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

; Helper function. Takes an environment and list of mappings (i.e., pairs of
; a name and value) and returns the extended environment including the old
; environment and the new mappings.
(define (add-to-env env mappings)
  (append mappings env))

; Takes a MUPL expression and environment and returns the value obtained by
; evaluating the expression in the given environment.
(define (eval-under-env e env)
  (cond [(var? e) (envlookup env (var-string e))] ; Simply look up the variable in the environments
        [(int? e) e] ; Integers are values
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1) ; Check if we have two integers to add
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) ; Check if we have two integers to compare
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)  ; Only evaluate e3 or e4 depending
                   (eval-under-env (ifgreater-e4 e) env)) ; on the comparison result
               (error "MUPL ifgreater applied to non-number")))]
        [(fun? e) (closure env e)] ; Evaluating functions yields closures
        [(call? e)
         (let ([closure-val (eval-under-env (call-funexp e) env)] ; Evaluate the function expression passed to call
               [argument-val (eval-under-env (call-actual e) env)]) ; Evaluate the argument expression passed to call
           (if (closure? closure-val) ; Check if the function expression evaluated to a closure
               (let* ([closure-environ (closure-env closure-val)] ; These local bindings are for code
                      [function-exp (closure-fun closure-val)]    ; cleanliness and readability.
                      [function-name (fun-nameopt function-exp)]
                      [function-parameter (fun-formal function-exp)]
                      [function-body (fun-body function-exp)]
                      [extended-env (if function-name ; If the function is named, add the name and parameter to the environment.
                                        (add-to-env closure-environ (list (cons function-name closure-val)
                                                                          (cons function-parameter argument-val)))
                                        (add-to-env closure-environ (list (cons function-parameter argument-val))))])
                 (eval-under-env function-body extended-env))
               (error (format "MUPL call error. First argument must be a closure. Given: ~v" e))))]
        [(mlet? e)
         (let ([v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v)
                                               env)))]
        [(apair? e) (apair (eval-under-env (apair-e1 e) env)   ; Simply build the pair by evaluating
                           (eval-under-env (apair-e2 e) env))] ; the two expressions passed to apair
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)]) ; Evaluate the expression passed to fst
           (if (apair? v)
               (apair-e1 v) ; If v is a pair, return the value in the first position
               (error (format "MUPL fst expects an apair and was given: ~v" (fst-e e)))))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)]) ; Evaluate the expression passed to snd
           (if (apair? v)
               (apair-e2 v) ; If v is a pair, return the value in the second position
               (error (format "MUPL snd expects an apair and was given: ~v" (snd-e e)))))]
        [(aunit? e) e] ; (aunit) is a value
        [(isaunit? e) (if (aunit? (eval-under-env (isaunit-e e) env))
                          (int 1)
                          (int 0))]
        [(closure? e) e] ; Closures are values
        [else (error (format "bad MUPL expression: ~v" e))]))

;; Provided by course. Do NOT change.
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

; Takes three MUPL expressions. If e1 evaluates to aunit then e2 is evaluated and returned,
; otherwsie e3 is evaluated and returned.
(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3)) ; Simply test isaunit (returns 1 or 0) against 0 using ifgreater

; Takes a list of local bindings and a MUPL expression and returns the expression
; with an environment made out of the local bindings.
(define (mlet* bindings e2)
  (if (empty? bindings)
      e2 ; Done making the local bindings, so return the expression
      (let* ([binding (car bindings)]    ; Get the next local binding to make
             [name (car binding)]        ; Get its name
             [expression (cdr binding)]) ; Get its expression
        (mlet name expression (mlet* (cdr bindings) e2)))))
        ; Build an mlet expression with the current binding and then
        ; recursively build the rest of the local bindings with mlet*.

; Takes four MUPL expressions. If e1 and e2 are equal integers when evaluated, then
; return e3 evaluated, otherwsie return e4 evaluated.
(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2)) ; Only evaluate e1 and e2 once
         (ifgreater (var "_x") (var "_y")
                    e4
                    (ifgreater (var "_y") (var "_x")
                               e4
                               e3))))

;; Problem 4

; A variable holding a MUPL function that takes a MUPL function f and returns a MUPL
; function that takes a MUPL list and applies f to each element of the list.
(define mupl-map
  (fun "map" "f"
       (fun "iterator" "mlist"
            (ifaunit (var "mlist")
                     (aunit)
                     (apair (call (var "f") (fst (var "mlist")))
                            (call (var "iterator") (snd (var "mlist"))))))))

; A variable holding a MUPL function that takes a MUPL integer  i and returns a MUPL
; function that takes a MUPL list and returns a list with i added to each element.
(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun "mapAddN" "i"
             (call (var "map") (fun #f "x" (add (var "x") (var "i")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))