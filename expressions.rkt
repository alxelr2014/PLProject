#lang racket

(require (lib "eopl.ss" "eopl"))
(require "datatypes.rkt")
(#%require "datatypes.rkt")
(require "state.rkt")
(require "stack.rkt")
(require "helper.rkt")

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
        (function_call (func params) null);; need to fill
        (list_ref (ref index) (list_ref ref (lazy-eval index)))
        (ref (var) (deref (apply-stack! var)))
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


(define (func-call func params)
    (debugger func))

(provide (all-defined-out))