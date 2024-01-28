#lang racket

(define the-store '())

(define empty-store
    (lambda () '()))

(define get-store
    (lambda () the-store))

(define initialize-store!
    (lambda ()
        (set! the-store (empty-store))))

(define reference?
    (lambda (v) (integer? v)))

(define newref
    (lambda (val)
        (let ((next-ref (length the-store)))
        (set! the-store (append the-store (list val)))
        next-ref)))

(define deref
    (lambda (ref)
        (list-ref the-store ref)))

(define setref!
    (lambda (ref val)
        (set! the-store
            (letrec
                ((setref-inner (lambda (store1 ref1)
                    (cond
                    ((null? store1) (report-invalid-reference ref the-store))
                    ((zero? ref1) (cons val (cdr store1)))
                    (else (cons (car store1) (setref-inner (cdr store1) (- ref1 1))))))))
                (setref-inner the-store ref)))))


(define (report-no-binding-found search-var)
    (print "Environment not having it today!"))

(define (report-invalid-reference ref the-stor)
    (print "Store not having it today!"))

(define empty-env 
    (lambda () (lambda (search-var) (report-no-binding-found search-var))))

(define extend-env
    (lambda (var val env) (lambda(search-var) 
    (if (eqv? var search-var) (val)  (apply-env env search-var)))))

(define apply-env
    (lambda (env var) (env var)))

(provide (all-defined-out))
