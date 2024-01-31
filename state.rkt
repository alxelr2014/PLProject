#lang racket

(require "stack.rkt")
(require "helper.rkt")

(define the-store '())

(define empty-store
    (lambda () '()))

(define initialize-store!
    (lambda ()
        (set! the-store (empty-store))))


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
                ([setref-inner (lambda (store1 ref1)
                    (cond
                    ((null? store1) (error-msg "The store is not having it!"))
                    ((zero? ref1) (cons val (cdr store1)))
                    (else (cons (car store1) (setref-inner (cdr store1) (- ref1 1))))))])
                (setref-inner the-store ref)))))


; (define getref!
;     (lambda (val)
;         (if (apply-stack! val) (apply-stack! val) 
;             (let ([ref  (newref val)])
;                 (begin (extend-stack! val ref) ref)))))

(define getref!
    (lambda (val) (let ([ref  (newref val)])
                (begin (extend-stack! val ref) ref))))


(define (report-invalid-reference ref the-stor)
    (error-msg "Store not having it today!"))



(define print-state (lambda () (begin (displayln " ") (println "The current state is:") (println (get-controller!)) (println main-stack) (println the-store) (displayln " "))))

(provide (all-defined-out))
