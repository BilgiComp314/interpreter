#lang plai-typed
;;Grammar
;
;S -> <number>
;S -> <ident>
;S -> (+ S S)
;S -> (fundef <ident> S)
;S -> (S S)
;
;;Data Definition:
;
;A sentence is either;  number,
;	                identifier,
;	                addition,
;	                fundef,
;	                funapp
(define-type sentence 
  [sentence_num (n:number)]
  [sentence_ident (id:symbol)]
  [sentence_addition (lhs:sentence rhs:sentence)]
  [sentence_funDef (param:symbol body:sentence)]
  [sentence_funApp (applier:sentence applied:sentence)])

;An environment is either; empty,
;	                   identifier,
;	                   value,
;	                   Env
(define-type Env
  [env_empty ()]
  [env_ident (id:symbol) (val:value) (env:Env)])

;A value is either; number,
;                   function value
(define-type value
  [value_number (n:number)]
  [value_funcVal (env:Env) (body:Sentence) (fp:symbol)])

;; Contract: interpreter
;Sentence, Env -> value

;;Purpose: 
;Evaluate the sentence in the given environment to produce a value.

;;Examples:
; 5 (emty Env) -> 5 (value-number 5)
; 5 (x = 5) -> 5 (value-number 5)
; x (x = 2) -> 2 (value-number 2)
; (+ x y) (x=4 y=3) -> 7 (value-number 7)
; (fundef f S) (empty Env) -> (Env, Sentence, Symbol)
; (f x) (x=3) -> ERROR
; (f x) (x=3, f=(funDef n (+ n 3)) -> 6
; ((fundef x (+ x 3)) 3) (empty Env) -> 6

;;Template
; (define (interp s:sentence env:Env)
;   (type-case s
;     [sentence_num (n) ... ]
;     [sentence_ident (id) ... ]
;     [sentence_addition (lhs rhs) ... ]
;     [sentence_funDef (param body) ... ]
;     [sentence_funApp (applier applied) ... ]))


(define (interp s:sentence env:Env)
  (type-case s
    [sentence_num (n) (value-number (n))]
    [sentence_ident (id) (look-up id env)]
    [sentence_addition (lhs rhs) (value-number (+ (value-number-n (interp lhs env))
                                                  (value-number-n (interp rhs env))))]
    [sentence_funDef (param body) (value-fv env body param)]
    [sentence_funApp (applier applied) (apply (interp applier env)
                                              (interp applied env)
                                              env)]))

;;;helper functions: 
;; look-up
(define (look-up id symbol env:Env)
  (type-case env
    [env_empty ERROR]
    [env_full ...]))

;; apply:     
;value value env -> value
(define (apply applier:value applied:value e:Env)
  (type-case applier
    [value-number error]
    [value_fv (fv_env fv_body fv_fp) (interp fv_body (Env fv_fp applied fv_env))]))
