;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent)


;; Problem 1
;; (a)
(define (racketlist->mupllist lst)
  (if (null? lst)
      (aunit)
      (apair (car lst) (racketlist->mupllist (cdr lst)))))

;; (b)
(define (mupllist->racketlist lst)
  (if (aunit? lst)
      null
      (cons (apair-e1 lst) (mupllist->racketlist (apair-e2 lst)))))


;; Problem 1 tests
(module+ test
  (require rackunit)

  (define tests-for-problem-1
    (test-suite
     "Tests for problem 1"

     ; racketlist->mupllist
     (check-equal? (racketlist->mupllist (list (apair (int 2) (int 3))))  ; '((2 . 3))
                   (apair (apair (int 2) (int 3)) (aunit))
                   "problem-1a-test-1")
     (check-equal? (racketlist->mupllist (list (int 2) (int 3)))  ; '(2 3)
                   (apair (int 2) (apair (int 3) (aunit)))
                   "problem-1a-test-2")

     ; mupllist->racketlist
     (check-equal? (mupllist->racketlist (apair (apair (int 2) (int 3)) (aunit)))
                   (list (apair (int 2) (int 3)))
                   "problem-1b-test-1")
     (check-equal? (mupllist->racketlist (apair (int 2) (apair (int 3) (aunit))))
                   (list (int 2) (int 3))
                   "problem-1b-test-2"))))


;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) (envlookup env (var-string e))]

        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]

        ;; CHANGE add more cases here
        [(int? e) e]

        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1)
                      (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number test value")))]

       [(fun? e) (closure env e)]

       [(call? e)
        (let ([cls (eval-under-env (call-funexp e) env)])
          (if (closure? cls)
              (let* ([arg (eval-under-env (call-actual e) env)]
                     [f (closure-fun cls)]  ; fun is assumed to be a MUPL fun
                     [f-name (fun-nameopt f)]  ; nameopt is assumed to be a string
                     [f-param (fun-formal f)]
                     [cls-env (closure-env cls)]
                     [new-cls-env (if (not (equal? f-name #f))
                                      ; note: f-name can shadow f-param here
                                      (cons (cons f-name cls)
                                            (cons (cons f-param arg) cls-env))
                                      (cons (cons f-param arg) cls-env))])
                (eval-under-env (fun-body f) new-cls-env))
              (error "MUPL call first argument is not a closure")))]

       [(mlet? e)
        (let* ([val (eval-under-env (mlet-e e) env)]
               [new-env (cons (cons (mlet-var e) val) env)])  ; var is assumed to be a string
          (eval-under-env (mlet-body e) new-env))]

       [(apair? e)
        (let ([e1 (apair-e1 e)]
              [e2 (apair-e2 e)])
          ; if the pair is already a value, just return it
          (if (and (or (int? e1) (closure? e1) (aunit? e1))
                   (or (int? e2) (closure? e2) (aunit? e2)))
              e
              (let ([v1 (eval-under-env e1 env)]
                    [v2 (eval-under-env e2 env)])
                (apair v1 v2))))]

       [(fst? e)
        (let ([pr (eval-under-env (fst-e e) env)])
          (if (apair? pr)
              (apair-e1 pr)
              (error "MUPL fst argument is not a pair")))]

       [(snd? e)
        (let ([pr (eval-under-env (snd-e e) env)])
          (if (apair? pr)
              (apair-e2 pr)
              (error "MUPL snd argument is not a pair")))]

       [(aunit? e) e]

       ; isaunit reduces its argument
       [(isaunit? e)
        (let ([val (eval-under-env (isaunit-e e) env)])
          (if (aunit? val)
              (int 1)
              (int 0)))]

       ; for the autograder
       [(closure? e) e]

       [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))


;; Problem 2 tests
(module+ test
  (define tests-for-problem-2
    (test-suite
     "Tests for problem 2"

     ; ifgreater
     (check-equal? (eval-exp (ifgreater (add (int 1) (int 2))
                                        (add (int 1) (int 1))
                                        (int 1)
                                        (int 0)))
                   (int 1))

     ; fun
     (check-equal? (eval-exp (fun "times2" "x"
                                  (add (var "x") (var "x"))))
                   (closure '() (fun "times2" "x"
                                     (add (var "x") (var "x"))))
                   "fun-test-1")

     (check-equal? (eval-exp (mlet "a" (int 10)
                                   (fun "times2" "x"
                                        (add (var "x") (var "x")))))
                   (closure `(("a" . ,(int 10)))
                            (fun "times2" "x"
                                 (add (var "x") (var "x"))))
                   "fun-test-2")

     ; call
     (check-equal? (eval-exp (call (fun "multiply-by-2" "a"
                                        (add (var "a") (var "a")))
                                   (int 1)))
                   (int 2))

     ; mlet
     (check-equal? (eval-exp (mlet "x" (int 1)
                                   (var "x")))
                   (int 1))
     (check-equal? (eval-exp (mlet "x" (add (int 1) (int 2))
                                   (add (var "x") (int 3))))
                   (int 6))

     ; apair
     (check-equal? (eval-exp (apair (int 1) (int 1)))
                   (apair (int 1) (int 1)))
     (check-equal? (eval-exp (apair (add (int 1) (int 1))
                                    (add (int 1) (int 1))))
                   (apair (int 2) (int 2)))

     ; fst
     (check-equal? (eval-exp (fst (apair (int 1) (int 2))))
                   (int 1))

     ; snd
     (check-equal? (eval-exp (snd (apair (int 1) (int 2))))
                   (int 2))

     ; isaunit
     (check-equal? (eval-exp (isaunit (aunit)))
                   (int 1))
     (check-equal? (eval-exp (isaunit (apair (int 1) (int 2))))
                   (int 0))
     (check-equal? (eval-exp (isaunit (call (fun "returns-aunit" "not-used"
                                                 (aunit))
                                            (aunit))))
                   (int 1)))))



;; Problem 3

;; (a)
(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

;; (b)
(define (mlet* lstlst e2)
  (letrec ([loop (lambda (bindings)
                   (if (null? bindings)
                       e2
                       (let ([s (caar bindings)]
                             [e (cdar bindings)])
                         (mlet s e (loop (cdr bindings))))))])
    (loop lstlst)))

;; (c)
;; I wonder whether there is a more elegant solution, but this works.
;; The challenge is to define equality in terms of our available structs
;; - let's begin with: e1=e2 iff ((e1+x) > e2) AND ((e2+x) > e1).
;; We have addition and "greater than" comparison, but we also have to implement
;; boolean AND with MUPL structs (let true=1 and false=0, that means
;; true && true can be translated as boolexp1 + boolexp2 = 2, i.e.,
; boolexp1 + boolexp2 > 1).
(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1)
               (cons "_y" e2)
               (cons "v1" (ifgreater (add (var "_x") (int 1))
                                     (var "_y")
                                     (int 1)
                                     (int 0)))
               (cons "v2" (ifgreater (add (var "_y") (int 1))
                                     (var "_x")
                                     (int 1)
                                     (int 0))))
         (ifgreater (add (var "v1") (var "v2")) (int 1) e3 e4)))


;; Problem 3 tests
(module+ test
  (define tests-for-problem-3

    (test-suite
     "Tests for problem 3"

     ; ifaunit
     (check-equal? (eval-exp (add (int 1) (ifaunit (aunit) (int 3) (int 4))))
                   (int 4))
     (check-equal? (eval-exp (add (int 1) (ifaunit (int 0) (int 3) (int 4))))
                   (int 5))

     ; mlet*
     (check-equal? (eval-exp (mlet* (list (cons "a" (int 1))
                                          (cons "b" (add (var "a") (int 1))))
                                    (add (var "a") (var "b"))))
                   (int 3))

     ; ifeq
     (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4)))
                   (int 4))
     (check-equal? (eval-exp (ifeq (int 1) (int 1) (int 3) (int 4)))
                   (int 3)))))
                                   


;; Problem 4

;; (a)
(define mupl-map
  (fun #f "f"
       (fun "loop" "mupllst"
            (ifeq (isaunit (var "mupllst")) (int 1)
                  (aunit)
                  (apair (call (var "f") (fst (var "mupllst")))
                         (call (var "loop") (snd (var "mupllst"))))))))


;; (b)
(define mupl-mapAddN
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map")
                   (fun #f "elem"
                        (add (var "elem") (var "i")))))))


;; Problem 4 tests
(module+ test
  (define tests-for-problem-4
    (test-suite
     "Tests for problem 4"

     ; mupl-map (provided test)
     (check-equal? (eval-exp (call (call mupl-map
                                         (fun #f "x"
                                              (add (var "x") (int 7))))
                                   (apair (int 1) (aunit))))
                   (apair (int 8) (aunit))
                   "mupl-map test")

     ; mupl-mapAddN
     (check-equal? (eval-exp (call (call mupl-mapAddN (int 3))
                                   (apair (int 1) (aunit))))
                   (apair (int 4) (aunit))
                   "mupl-mapAddN test"))))



;; Running tests
(module+ test
  (require rackunit/text-ui)
  (run-tests tests-for-problem-1)
  (run-tests tests-for-problem-2)
  (run-tests tests-for-problem-3)
  (run-tests tests-for-problem-4))
