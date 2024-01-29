#lang racket
(require "datatypes.rkt")
(require (lib "eopl.ss" "eopl"))

(define-datatype stack-type stack-type?
    (normal-block)
    (for-block)
    (func-block))
(define-datatype flow-control flow-control?
    (non)
    (cont)
    (brk)
    (ret-val)
    (ret-void))


(define-datatype stack stack?
    (empty-stack)
    (push-stack (env environment?) (prev stack?) (typ stack-type?)))

(define-datatype environment environment?
    (empty-env)
    (extend-env (var (lambda(v) #t)) (val reference?) (prev environment?)))


(define main-stack '())
(define controller '())

(define init-controller! (lambda() (set! controller (non))))
(define set-controller! (lambda (signal) (set! controller signal)))

(define reference?
    (lambda (v) (integer? v)))


(define (pop-stack!)
    (set! main-stack (cases stack main-stack
        (empty-stack () empty-stack)
        (push-stack (env prev type) prev))))


(define (apply-env env var)
    (cases environment env
        (empty-env () #f)
        (extend-env (vari val prev) 
            (if (equal? vari var) val (apply-env prev var)))))

(define (apply-stack stck var)
    (cases stack stck
        (empty-stack () #f)
        (push-stack (env prev type) 
            (if (apply-env env var) (apply-env env var) (apply-stack prev var)))))

(define (extend-stack var val stck)
    (cases stack stck
        (push-stack (env prev type) 
            (push-stack (extend-env var val env) prev type))
        (empty-stack () #f)))

(define init-stack! (lambda() (set! main-stack (empty-stack))))

(define extend-stack! 
    (lambda(var val)
        (cases stack main-stack
            (push-stack (env prev type) 
                (set! main-stack (extend-stack var val main-stack)))
            (else println "Bad stack keeping!"))))

(define new-stack! (lambda (type) (set! main-stack (push-stack (empty-env) main-stack type))))

(define apply-stack! (lambda(var) (apply-stack main-stack var)))

(define extend-global! 
    (lambda (var val) 
        (set! main-stack
            (letrec ([go-down (lambda (stck)
                (cases stack stck
                    (push-stack (env prev type)
                        (cases stack prev
                            (empty-stack () (extend-stack var val stck))
                            (push-stack (env prev type) (push-stack env (go-down prev)) type )))
                    (empty-stack () (println "Bad global stack keeping"))))])
                (go-down main-stack)))))


(define-datatype thunks thunks?
    (a-thunk (expr expression?) (stck stack?)))
(provide (all-defined-out))