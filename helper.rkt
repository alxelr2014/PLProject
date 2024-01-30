#lang racket


(define (debugger x)
    (begin (println "In debugger") (println x) x))

(define (extra-debug lst x)
    (if (null? lst) x (begin (car lst) (extra-debug (cdr lst) x ))))

(define etracing #f)
(define itracing #f)

(provide (all-defined-out))