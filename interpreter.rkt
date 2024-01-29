#lang racket

(require (lib "eopl.ss" "eopl"))
(require "datatypes.rkt")
(#%require "datatypes.rkt")
(require "state.rkt")
(require "stack.rkt")
(require "helper.rkt")


(define (value-of-program pgm)
    (begin
        (initialize-store!)
        (init-stack!)
        (new-stack!)
        (value-of-stm-list pgm)))

(define (value-of-stm-list stmlist)
    (if (null? stmlist) (print-state)
        (begin 
            (value-of-stm (car stmlist) )
            (value-of-stm-list (cdr stmlist)))))


(define (value-of-stm stm)
    (begin 
    (println stm)
    (print-state)
    (cases statement stm
        (assign (var expr) (assignment var expr))
        (global (var) (glob var))
        (return (expr) null)
        (return_void () null)
        (pass () null)
        (break () null)
        (continue () null)
        (func (name params statements) null)
        (if_stmt (cond_exp if_sts else_sts) null)
        (for_stmt (iter list_exp sts) null)
        (print_stmt (exprs) (printer exprs))
        (else (println "error in statement."))
    )))

(define (value-of-exp expr)
    (cases expression expr
        (binary_op (op left right) (binop op left right))
        (unary_op (op operand) null)
        (function_call (func params) null)
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

(define (assignment var expr)
    (setref!  (getref! var) (lazy-eval expr)))

(define (glob var)
    (extend-global! var (newref 0)))

(define (binop op left right)
    (let ([val-left (expval->scheme (value-of-exp left ))]
        [val-right (expval->scheme (value-of-exp right))])
        (begin (println (scheme->expval (op val-left val-right)))
            (scheme->expval (op val-left val-right)))))

(define (ref-var var)
    (let ([refe (apply-stack! var)])
        (let ([expr (deref refe)])
            (value-of-exp expr))))

(define (printer exprs)
    (cases expression* exprs
        (expressions (expr rest-exprs) (begin (print (value-of-exp expr)) (printer rest-exprs)))
        (empty-expr () (println ""))))

(provide (all-defined-out))
