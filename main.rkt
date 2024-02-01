#lang racket

(require "passes/parser.rkt")
(require "passes/lexer.rkt")
(require "interpreter.rkt")
(define (parse-scan prog-string)
  (python-parser (lex-this prog-string))
  )

(define (evaluate file-name)
  (let ([parsed (parse-scan (string-join (file->lines file-name)))])
    (value-of-program parsed)))

(evaluate "simples/lists.py")
(provide (all-defined-out))
