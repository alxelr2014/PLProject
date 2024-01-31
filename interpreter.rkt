#lang racket

(require (lib "eopl.ss" "eopl"))
(require "datatypes.rkt")
(#%require "datatypes.rkt")
(require "state.rkt")
(require "stack.rkt")
(require "helper.rkt")
(require "expressions.rkt")
(require racket/trace)



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
            (if (signal-checker) (value-of-stm-list (cdr stmlist))
                (begin 
                    (value-of-stm (car stmlist))
                    (value-of-stm-list (cdr stmlist))))))

; evaluate a statement
(define (value-of-stm stm)
    (begin 
    ;(println stm)
    ;(print-state)
    (cases statement stm
        (assign (var expr) (assignment var expr))
        (global (var) (glob var))
        (return (expr) (ret-value expr))
        (return_void () (ret-void))
        (pass () null)
        (break () (breaking))
        (continue () (continuing))
        (func (name params statements) (func-stms name params statements))
        (if_stmt (cond_exp if_sts else_sts) (if-stms cond_exp if_sts else_sts))
        (for_stmt (iter list_exp sts) (for-init iter list_exp sts))
        (print_stmt (exprs) (printer-init exprs))
        (else (error-msg "Not a valid statement.")))
    ))


(define (assignment var expr)
    (let ([thk (a-thunk expr main-stack)])
    (setref!  (getref! var) thk)))

(define (glob var)
    (extend-global! var (newref 0)))


(define (print-expval expv)
    (cases expval expv
        (num-val (num) (display num))
        (bool-val (bool) (if bool (display "True") (display "False")))
        (null-val () (display "None"))
        (list-val (l) (begin (display "[") (printer l) (display "]")))))

(define (printer-init exprs)
    (begin (display "Print: ") (printer exprs) (displayln "")))

(define (printer exprs)
    (cases expression* exprs
        (empty-expr () null)
        (expressions (expr rest-exprs) 
            (begin  
                (printer rest-exprs) 
                (cases expression* rest-exprs 
                    (empty-expr () null)
                    (expressions (ex1 res1) (display ", ")))
                (print-expval (value-of-exp expr main-stack))))))

(define (if-stms cond_exp if_sts else_sts)
    (let ([cond_result (value-of-exp cond_exp main-stack)])
        (cases expval cond_result
            (bool-val (bool) 
                (begin
                    (new-stack! (normal-block))
                    (if bool (value-of-stm-list if_sts) (value-of-stm-list else_sts))))
            (else (error-msg "Non boolean if condition!")))))

(define (for-init iter list_exp sts)
    (let ([eval_list (expval->scheme (value-of-exp list_exp main-stack ))])
        (for-stms iter eval_list sts)))

(define (for-stms iter eval_list sts)
    (if (list? eval_list)
        (if (null? eval_list) null 
            (begin 
                (new-stack! (for-block))
                (setref! (getref! iter) (a-thunk (car eval_list) main-stack))
                (new-stack! (normal-block))
                (value-of-stm-list sts)
                (cases flow-control (get-controller!)
                        (cont () (begin
                            (set-controller! (non))
                            (for-stms iter (cdr eval_list) sts)))
                        (brk () (set-controller! (non)))
                        (else (for-stms iter (cdr eval_list) sts)))))
        (error-msg "Non list for iteration!")))
                

(define (func-stms name params statements)
    (setref! (getref! name) (func name params statements)))


(define (ret-value expr)
    (set-controller! (re-val (value-of-exp expr main-stack))))

(define ret-void (lambda() (set-controller! (re-void))))

(define continuing (lambda () (set-controller! (cont))))
(define breaking (lambda () (set-controller! (brk))))


(define signal-checker (lambda()
    (cases flow-control (get-controller!)
        (non () #f)
        (re-void ()
            (cases stack-type (top-type!)
                (funct-block () #f)
                (else #t)))  
        (re-val (expr)
            (cases stack-type (top-type!)
                (funct-block () #f)
                (else #t)))   
        (cont ()  
            (cases stack-type (top-type!)
                (for-block () #f)
                (else #t)))
        (brk ()  
            (cases stack-type (top-type!)
                (for-block () #f)
                (else #t))))))

(if itracing (begin 
(trace value-of-program)
(trace value-of-stm)
(trace value-of-stm-list)
(trace signal-checker)) (println "Intrepeter tracing is off"))

(provide (all-defined-out))
