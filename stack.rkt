#lang racket
(require "datatypes.rkt")
(require (lib "eopl.ss" "eopl"))

(define reference?
    (lambda (v) (integer? v)))

(define main-stack '())

(define-datatype stack stack?
    (empty-stack)
    (push-stack (env environment?) (prev stack?)))

(define (pop-stack stkc)
    (cases stack stkc
        (empty-stack () null)
        (push-stack (env prev) prev)))


(define-datatype environment environment?
    (empty-env)
    (extend-env (var (lambda(v) #t)) (val reference?) (prev environment?)))

(define (apply-env env var)
    (cases environment env
        (empty-env () #f)
        (extend-env (vari val prev) 
            (if (equal? vari var) val (apply-env prev var)))))

(define (apply-stack stck var)
    (cases stack stck
        (empty-stack () #f)
        (push-stack (env prev) 
            (if (apply-env env var) (apply-env env var) (apply-stack prev var)))))

(define (extend-stack var val stck)
    (cases stack stck
        (push-stack (env prev) 
            (push-stack (extend-env var val env) prev))
        (empty-stack () #f)))

(define init-stack! (lambda() (set! main-stack (empty-stack))))

(define extend-stack! 
    (lambda(var val)
        (cases stack main-stack
            (push-stack (env prev) 
                (set! main-stack (extend-stack var val main-stack)))
            (else println "Bad stack keeping!"))))

(define new-stack! (lambda () (set! main-stack (push-stack (empty-env) main-stack))))

(define apply-stack! (lambda(var) (apply-stack main-stack var)))

(define extend-global! 
    (lambda (var val) 
        (set! main-stack
            (letrec ([go-down (lambda (stck)
                (cases stack stck
                    (push-stack (env prev)
                        (cases stack prev
                            (empty-stack () (extend-stack var val stck))
                            (push-stack (env prev) (push-stack env (go-down prev)))))
                    (empty-stack () (println "Bad global stack keeping"))))])
                (go-down main-stack)))))


(define-datatype thunks thunks?
    (a-thunk (expr expression?) (stck stack?)))
(provide (all-defined-out))