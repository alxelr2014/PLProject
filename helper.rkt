#lang racket


(define (debugger x)
    (begin (println "In debugger") (println x) x))

(define (extra-debug lst x)
    (if (null? lst) x (begin (car lst) (extra-debug (cdr lst) x ))))

(define (error-msg msg [options 0]) 
    (begin (println options) (error (string-append "Error occured: " msg))))

(define (is-in-operator op lst)
    (if (null? lst) #f (or (eqv? op (car lst)) (is-in-operator op (cdr lst)))))

(define etracing #f)
(define itracing #f)

(provide (all-defined-out))