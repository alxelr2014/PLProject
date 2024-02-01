#lang racket
(require "datatypes.rkt")
(require (lib "eopl.ss" "eopl"))
(require "helper.rkt")
(require racket/trace)


(define-datatype stack-type stack-type?
    (normal-block)
    (for-block)
    (funct-block))

(define-datatype flow-control flow-control?
    (non)
    (cont)
    (brk)
    (re-void)
    (re-val (expr expval?)))


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
(define get-controller! (lambda () controller))
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


(define (apply-stack-info stck var k)
    (cases stack stck
        (empty-stack () (cons k (list #f)))
        (push-stack (env prev type) 
            (if (apply-env env var) (cons k (list (apply-env env var)))
                (apply-stack-info prev var (+ k 1))))))


(define apply-stack-info! (lambda (var) (apply-stack-info main-stack var 0)))


(define (extend-stack var val stck)
    (cases stack stck
        (push-stack (env prev type) 
            (push-stack (extend-env var val env) prev type))
        (empty-stack () #f)))


(define (extend-stack-info var val ind stck)
    (if (zero? ind) (extend-stack var val stck)
        (cases stack stck
            (empty-stack () #f)
            (push-stack (env prev type) 
                (push-stack env (extend-stack-info var val (- ind 1) prev) type)))))

(define extend-stack-info!     
    (lambda(var val ind)
        (cases stack main-stack
            (push-stack (env prev type) 
                (set! main-stack (extend-stack-info var val ind main-stack)))
            (else (error-msg "Bad stack keeping!")))))

(define init-stack! (lambda() (set! main-stack (empty-stack))))

(define extend-stack! 
    (lambda(var val)
        (cases stack main-stack
            (push-stack (env prev type) 
                (set! main-stack (extend-stack var val main-stack)))
            (else (error-msg "Bad stack keeping!")))))

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
                    (empty-stack () (error-msg "Bad global stack keeping"))))])
                (go-down main-stack)))))


(define top-type! (lambda () 
    (cases stack main-stack
        (empty-stack () (error-msg "Bad stack type."))
        (push-stack (env prev typ)  typ))))

(define (size-of-stack stck)
    (cases stack stck
        (empty-stack () 0)
        (push-stack (env prev type) (+ 1 (size-of-stack prev)))))

(define (replace-stack old-stack new-stack ind)
    (if (zero? ind) new-stack
        (cases stack old-stack
            (empty-stack () null)
            (push-stack (env prev type) 
                (push-stack env (replace-stack prev new-stack (- ind 1)) type)))))

(define (construct-stack old-stack new-stack)
    (let ([size-old (size-of-stack old-stack)]
        [size-new (size-of-stack new-stack)])
        (if (>= size-old size-new) 
            (replace-stack old-stack new-stack (- size-old size-new))
            (error-msg "Index of stack is out of bound."))))

(define set-mainstack! (lambda (stck) (set! main-stack stck)))
(define fupdate-mainstack! (lambda (stck) (set! main-stack (construct-stack stck main-stack))))

(define-datatype thunks thunks?
    (a-thunk (expr expression?) (stck stack?)))


(if stracing (begin 
(trace replace-stack)
(trace construct-stack)
(trace fupdate-mainstack!)
;(trace extend-stack-info)
) (println "Stack tracing is off"))


(provide (all-defined-out))