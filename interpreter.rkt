#lang racket

(require (lib "eopl.ss" "eopl"))
(require "datatypes.rkt")
(#%require "datatypes.rkt")
(require "state.rkt")


(define (value-of-program pgm)
    (value-of-stm-list pgm 0))

(define (value-of-stm-list stmlist env)
    (if (null? stmlist) (println "now is empty") (begin 
    (value-of-stm (car stmlist) env )
    (value-of-stm-list (cdr stmlist) env))))


(define (value-of-stm stm env)
    (cases statement stm
        (assign (var expr) null)
        (global (var) null)
        (return (expr) null)
        (return_void () null)
        (pass () null)
        (break () null)
        (continue () null)
        (func (name params statements) null)
        (if_stmt (cond_exp if_sts else_sts) null)
        (for_stmt (iter list_exp sts) null)
        (print_stmt (exprs) null)
        (else (println "error"))
    ))


(provide (all-defined-out))
