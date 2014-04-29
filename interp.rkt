#lang plai-typed
;;Grammar
;
;S -> <number>
;S -> <ident>
;S -> (+ S S)
;S -> (fundef <ident> S)
;S -> (S S)
;
;;Data Definition : 
;
;A sentence is either;  number,
;	                identifier,
;	                addition,
;	                fundef,
;	                funapp
(define-type sentence 
  [sentence-num (n : number)]
  [sentence-ident (id : symbol)]
  [sentence-addition (lhs : sentence) (rhs : sentence)]
  [sentence-fundef (param : symbol) (body : sentence)]
  [sentence-funapp (applier : sentence) (applied : sentence)])




;An environment is either; empty,
; OR
;	                   identifier, value, env
(define-type env
  [env-empty]
  [env-ident (id : symbol) (val : value) (next : env)])

;A value is either; number,
;                   function value
(define-type value
  [value-number (n : number)]
  [value-func-val  (fp : symbol) (body : sentence) (e : env) ]
  [value-error (reason : symbol)])


;; parser s-expression -> sentence
;;
;; examples
;;
;; '5 -> (sentence-num 5)
;; '(+ 3 4) -> (sentence-addition (sentence-num 3) (sentence-num 4))
;; '(+ x x) ->(sentence-addition (sentence-ident 'x) (sentence-ident 'x)

;; template

;(define (parse [s : s-expression])
;  (cond
;    [s-exp-number? ...]
;    [s-exp-symbol? ...]
;    [s-exp-list? ...first ...second]
;    [sentence-fundef ...first ...second]
;    [sentence-funapp ...first ...second]
;    [else ... ]
;    ))


;;Function
(define (parse [s : s-expression])
  (cond
    ((s-exp-number? s) (sentence-num (s-exp->number s)))
    ((s-exp-symbol? s) (sentence-ident (s-exp->symbol s)))
    ((and (s-exp-list? s) (symbol=? (s-exp->symbol (first (s-exp->list s))) '+)
          (= (length (s-exp->list s)) 3))
     (sentence-addition (parse (second (s-exp->list s))) (parse (third (s-exp->list s)))))
    ((and (s-exp-list? s) (symbol=? (s-exp->symbol (first (s-exp->list s))) 'fundef)
          (s-exp-symbol? (second (s-exp->list s))) (= (length (s-exp->list s)) 3))
     (sentence-fundef (s-exp->symbol (second (s-exp->list s))) (parse (third (s-exp->list s)))))
    ((and (s-exp-list? s) (= (length (s-exp->list s)) 2))
     (sentence-funapp (parse (first (s-exp->list s))) (parse (second (s-exp->list s)))))
    (else (error 'parse (s-exp->string s)))))

;; Tests :
(test (parse (number->s-exp 5))(sentence-num 5))
(test (parse (symbol->s-exp 'x))(sentence-ident 'x))
(test (parse '(+ 3 4)) (sentence-addition (sentence-num 3) (sentence-num 4)))
(test (parse '(+ x 8))(sentence-addition (sentence-ident 'x)(sentence-num 8)))
(test (parse '(+ x x))(sentence-addition (sentence-ident 'x) (sentence-ident 'x)))
(test (parse '(fundef n (+ n 3))) (sentence-fundef 'n (sentence-addition (sentence-ident 'n) (sentence-num 3))))
(test (parse '(n 5)) (sentence-funapp (sentence-ident 'n) (sentence-num 5)))

;; Contract :  interpreter
;Sentence, Env -> value

;;Purpose :  
;Evaluate the sentence in the given environment to produce a value.

;;Examples : 
; 5 (emty Env) -> 5 (value-number 5)
; 5 (x = 5) -> 5 (value-number 5)
; x (x = 2) -> 2 (value-number 2)
; (+ x y) (x=4 y=3) -> 7 (value-number 7)
; (fundef f S) (empty Env) -> (Env, Sentence, Symbol)
; (f x) (x=3) -> ERROR
; (f x) (x=3, f=(fundef n (+ n 3)) -> 6
; ((fundef x (+ x 3)) 3) (empty Env) -> 6
; ((fundef x (+ x 5)) 5) (empty Env) -> 6

;;Template
; (define (interp s : sentence env : Env)
;   (type-case s
;     [sentence-num (n) ... ]
;     [sentence-ident (id) ... ]
;     [sentence-addition (lhs rhs) ... ]
;     [sentence-fundef (param body) ... ]
;     [sentence-funapp (applier applied) ... ]))


(define (interp [s : sentence] [e : env])
  (type-case sentence s
    [sentence-num (n) (value-number n)]
    [sentence-ident (id) (look-up id e)]
    [sentence-addition (lhs rhs) (value-number (+ (value-number-n (interp lhs e))
                                                  (value-number-n (interp rhs e))))]
    [sentence-fundef (param body) (value-func-val param body e)]
    [sentence-funapp (applier applied) (apply (interp applier e)
                                              (interp applied e)
                                              e)]))

;;;helper functions :  
;; look-up
(define (look-up [id : symbol] [e : env])
  (type-case env e
    [env-empty () (value-error 'undefined-symbol)]
    [env-ident (env-id env-val next-env)
               (if (symbol=? id env-id) env-val (look-up id next-env))]))

;; apply :      value value env -> value
(define (apply [applier : value] [applied : value] [e : env])
  (type-case value applier
    [value-number (n) (value-error 'applied-but-not-a-function-value)]
    [value-error (err) (value-error 'applied-but-not-a-function-value)]
    [value-func-val (fv-fp fv-body fv-env) (interp fv-body (env-ident fv-fp applied fv-env))]))

;; Tests:

(test (interp (parse (number->s-exp 5)) (env-empty)) (value-number 5)) 
(test (interp (parse (number->s-exp 5)) (env-ident 'x (value-number 5) (env-empty))) (value-number 5))
(test (interp (parse (symbol->s-exp 'x)) (env-ident 'x (value-number 2) (env-empty))) (value-number 2))
(test (interp (parse '(+ x y)) (env-ident 'x (value-number 4) (env-ident 'y (value-number 3) (env-empty)))) (value-number 7))
(test (interp (parse '(fundef f s)) (env-empty)) (value-func-val 'f (sentence-ident 's) (env-empty)))
