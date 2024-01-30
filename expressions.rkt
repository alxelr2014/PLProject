#lang racket

(require (lib "eopl.ss" "eopl"))
(require racket/lazy-require)
(require "datatypes.rkt")
(#%require "datatypes.rkt")
(require "state.rkt")
(require "stack.rkt")
(require "helper.rkt")
(lazy-require ["interpreter.rkt" (value-of-stm-list)])
(require racket/trace)


(define (value-of-exp expr)
    (cases expression expr
        (binary_op (op left right) (binop op left right))
        (unary_op (op operand) (unop op operand))
        (function_call (func params) (func-call func params))
        (list_ref (ref index) null)
        (ref (var) (ref-var var))
        (atomic_bool_exp (bool) (bool-val bool))
        (atomic_num_exp (num) (num-val num))
        (atomic_null_exp () (null-val))
        (atomic_list_exp (l) (list-val l))
        (else (println "error in expression."))
    ))

(define (lazy-eval expr)
    (cases expression expr
        (binary_op (op left right) (binary_op op (lazy-eval left) (lazy-eval right)))
        (unary_op (op operand) (unary_op op (lazy-eval operand)))
        (function_call (func params) (func-call func params))
        (list_ref (ref index) (list_ref ref (lazy-eval index)))
        (ref (var) (lazy-eval (deref (apply-stack! var))))
        (atomic_list_exp (l) null)
        (else expr)))


(define (binop op left right)
    (let ([val-left (expval->scheme (value-of-exp left ))]
        [val-right (expval->scheme (value-of-exp right))])
            (scheme->expval (op val-left val-right))))

(define (unop op operand)
    (let ([val-operand (expval->scheme (value-of-exp operand ))])
        (scheme->expval (op val-operand))))

(define (ref-var var)
    (let ([refe (apply-stack! var)])
        (let ([expr (deref refe)])
            (value-of-exp expr))))

(define (set-param param expr)
    (setref! (getref! param) expr))

(define (set-params def-params act-params)
    (if (null? def-params) null
        (cases func_param (car def-params) 
                (with_default (var expr) 
                    (if (null? act-params)
                        (begin 
                            (set-param var expr)
                            (set-params (cdr def-params) act-params))
                        (begin
                            (set-param var (car act-params))
                            (set-params (cdr def-params) (cdr act-params))))))))

(define (get-function-statement funct)
    (cases expression funct
        (ref (var) (deref (getref! var)))
        (else (println "error in function name."))
    ))

(define (func-call funct params)
    (let ([func-def (get-function-statement funct)])
        (cases statement  func-def
            (func (name param stmts)
                (begin
                (new-stack! (funct-block))
                (set-params (param*->list param)  (expr*->list params))
                (new-stack! (normal-block))
                (value-of-stm-list stmts)
                (cases flow-control (get-controller!)
                    (re-val (expr) (begin (set-controller! (non)) expr))
                    (re-void () (set-controller! (non)))
                    (else (println "Non returning function!")))))
            (else (println "Bad function call")))))
(if etracing (begin 
(trace value-of-exp)
(trace func-call)) (println "Expression tracking is off"))
(provide (all-defined-out))