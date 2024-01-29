#lang racket

(define (debugger x)
    (begin (println "In debugger") (println x) x))

(define (extra-debug lst x)
    (if (null? lst) x (begin (car lst) (extra-debug (cdr lst) x ))))

(provide (all-defined-out))