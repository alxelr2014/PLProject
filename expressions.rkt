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

; turns expression to expval
(define (value-of-exp expr stck)
    (cases expression expr
        (binary_op (op left right) (binop op left right stck))
        (unary_op (op operand) (unop op operand stck))
        (function_call (func params) (func-call func params stck))
        (list_ref (ref index) (ref-list ref index stck))
        (ref (var) (ref-var var stck))
        (atomic_bool_exp (bool) (bool-val bool))
        (atomic_num_exp (num) (num-val num))
        (atomic_null_exp () (null-val))
        (atomic_list_exp (l) (list-val (list->expv* (map (lambda (ex) (value-of-exp ex stck)) (expr*->list  l)))))
        (else (error-msg "Not a valid expression."))
    ))


(define (value-of-thunk thk)
    (cases thunks thk
        (a-thunk (expr stck) (value-of-exp expr stck))))


(define (can-lazy-binop op left stck)
    (let 
        ([val-left (expval->scheme (value-of-exp left stck))])
        (cond 
            [(and (eqv? op *) (number? val-left) (zero? val-left)) #t]
            [(and (eqv? op bor) (boolean? val-left) val-left) #t]
            [(and (eqv? op band) (boolean? val-left) (not val-left)) #t]
            [else #f])))


(define (operand-type left right)
    (or (and (number? left) (number? right)) (and (boolean? left) (boolean? right)) (and (list? left) (list? right))))

(define (binary-operator-type optr opnd)
    (cond
        [(is-in-operator optr (list add))  (or (number? opnd) (list? opnd))]
        [(is-in-operator  optr (list - * / expt < > equal?))  (number? opnd)]
        [(is-in-operator optr (list not bor band)) (boolean? opnd)]))

(define (binop op left right stck)
    (if (can-lazy-binop op left stck) 
        (cond
            [(eqv? op *) (scheme->expval 0)]
            [(eqv? op band) (scheme->expval #t)]
            [(eqv? op bor) (scheme->expval #f)])
        (let ([val-left (expval->scheme (value-of-exp left stck))]
            [val-right (expval->scheme (value-of-exp right stck))])
            (cond
                [(not  (operand-type val-left val-right)) (error-msg "Operands are not the same type." (list op val-left val-right))]
                [(not  (binary-operator-type op val-left)) (error-msg "Operator and Operands are not the same type." (list op val-left val-right))]
                [else (scheme->expval (op val-left val-right))]))))

(define (unop op operand stck)
    (let ([val-operand (expval->scheme (value-of-exp operand stck))])
        (if (number? val-operand) (scheme->expval (op val-operand)) (error-msg "Unary operator only applies to numbers." (list op operand)))))

(define (ref-var var stck)
    (let ([refe (apply-stack stck var)])
        (let ([thk (deref refe)])
            (value-of-thunk thk))))

(define (ref-list ref index stck) 
    (let ([val-ind (expval->scheme (value-of-exp index stck))]
        [val-list (expval->scheme (value-of-exp ref stck))])
        (cond
            [(not (number? val-ind)) (error-msg "An index must be a number.")]
            [(not (>= val-ind 0)) (error-msg "Index must be nonnegative.")]
            [(not (list? val-list)) (error-msg "Variable is not a list" (list ref))]
            [(>= val-ind (length val-list)) (error-msg "Index is out of bound." (list ref index))]
            [(list-ref val-list val-ind)])))

(define (set-param param expr stck)
    (setref! (getref! param) (a-thunk expr stck)))

(define (set-params def-params act-params stck)
    (if (null? def-params) null
        (cases func_param (car def-params) 
                (with_default (var expr) 
                    (if (null? act-params)
                        (begin 
                            (set-param var expr stck)
                            (set-params (cdr def-params) act-params stck))
                        (begin
                            (set-param var (car act-params) stck)
                            (set-params (cdr def-params) (cdr act-params) stck)))))))

(define (get-function-statement funct stck)
    (cases expression funct
        (ref (var) (deref (apply-stack stck var)))
        (else (error-msg "Procedure name is invalid."))
    ))

(define (func-call funct params stck)
    (let ([func-def (get-function-statement funct stck)])
        (cases statement func-def
            (func (name param stmts)
                (let ([copy-stack '()])
                (begin 
                    (set! copy-stack main-stack)
                    (set-mainstack! stck)
                    (new-stack! (funct-block))
                    (set-params (param*->list param)  (expr*->list params) stck)
                    (new-stack! (normal-block))
                    (value-of-stm-list stmts)
                    (pop-stack!)
                    (fupdate-mainstack! copy-stack)
                    (cases flow-control (get-controller!)
                        (re-val (expr) (begin (set-controller! (non)) expr))
                        (re-void () (set-controller! (non)))
                        (else null)))))
            (else (error-msg "Invalid expression for a function call.")))))

(if etracing (begin 
(trace value-of-exp)
(trace value-of-thunk)
(trace binary-operator-type)
(trace func-call)) (println "Expression tracing is off"))
(provide (all-defined-out))