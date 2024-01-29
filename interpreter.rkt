#lang racket

(require (lib "eopl.ss" "eopl"))
(require "datatypes.rkt")
(#%require "datatypes.rkt")
(require "state.rkt")
(require "stack.rkt")
(require "helper.rkt")
(require "expressions.rkt")

; initialize the memory and stack.
(define (value-of-program pgm)
    (begin
        (initialize-store!)
        (init-stack!)
        (new-stack! (normal-block))
        (init-controller!)
        (value-of-stm-list pgm)
    ))

; evaluate a block of statements
(define (value-of-stm-list stmlist)
    (if (null? stmlist) (pop-stack!)
        (begin 
            (value-of-stm (car stmlist) )
            (value-of-stm-list (cdr stmlist)))))

; evaluate a statement
(define (value-of-stm stm)
    (begin 
    (println stm)
    (print-state)
    (cases statement stm
        (assign (var expr) (assignment var expr))
        (global (var) (glob var))
        (return (expr) (ret-value expr))
        (return_void () null)
        (pass () null)
        (break () null)
        (continue () null)
        (func (name params statements) (func-stms name params statements))
        (if_stmt (cond_exp if_sts else_sts) (if-stms cond_exp if_sts else_sts))
        (for_stmt (iter list_exp sts) (for-init iter list_exp sts))
        (print_stmt (exprs) (printer exprs))
        (else (println "error in statement.")))
    (print-state)))


(define (assignment var expr)
    (setref!  (getref! var) (value-of-exp expr)))

(define (glob var)
    (extend-global! var (newref 0)))

(define (printer exprs)
    (cases expression* exprs
        (expressions (expr rest-exprs) (begin (display "Printing: ") (print (value-of-exp expr)) (printer rest-exprs)))
        (empty-expr () (displayln " " ))))

(define (if-stms cond_exp if_sts else_sts)
    (let ([cond_result (value-of-exp cond_exp)])
        (cases expval cond_result
            (bool-val (bool) 
                (begin
                    (new-stack!)
                    (if bool (value-of-stm-list if_sts) (value-of-stm-list else_sts))))
            (else (println "Invalid if condition")))))

(define (for-init iter list_exp sts)
    (let ([eval_list (expval->scheme (value-of-exp list_exp))])
        (for-stms iter eval_list sts)))

(define (for-stms iter eval_list sts)
    (if (null? eval_list) null 
        (begin 
            (new-stack! (for-block))
            (setref! (getref! iter) (car eval_list))
            (value-of-stm-list sts)
            (for-stms iter (cdr eval_list) sts))))

(define (func-stms name params statements)
    (setref! (getref! name) (func name params statements)))


(define (ret-value expr)
    (set-controller! 'ret-val))

(provide (all-defined-out))
