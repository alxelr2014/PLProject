#lang racket
(require (lib "eopl.ss" "eopl"))

(define-datatype statement statement?
  (assign (var string?) (expr expression?))
  (global (var string?))
  (return (expr expression?))
  (return_void)
  (pass)
  (break)
  (continue)
  (func (name string?) (params func_param*?) (statements list?))
  (if_stmt (cond_exp expression?) (if_sts list?) (else_sts list?))
  (for_stmt (iter string?) (list_exp expression?) (sts list?))
  (print_stmt (expressions expression*?))
  )



(define-datatype func_param func_param?
  (with_default (var string?) (expr expression?))
  )

(define-datatype func_param* func_param*?
  (empty-param)
  (func_params (param func_param?) (rest-params func_param*?))
  )

(define-datatype fnuction function?
  (a-function (name string?) (param func_param*?) (stmts list?)))

(define-datatype expression expression?
  (binary_op (op procedure?) (left expression?) (right expression?))
  (unary_op (op procedure?) (operand expression?))
  (function_call (func expression?) (params expression*?))
  (list_ref (ref expression?) (index expression?))
  (ref (var string?))

  (atomic_bool_exp (bool boolean?))
  (atomic_num_exp (num number?))
  (atomic_null_exp)
  (atomic_list_exp (l expression*?))
  )

(define-datatype expression* expression*?
  (empty-expr)
  (expressions (expr expression?) (rest-exprs expression*?))
  )


(define-datatype expval expval?
  (num-val
    (num number?))
  (bool-val
    (bool boolean?))
  (null-val)
  (list-val (lst expression*?)))

(define (expr*->list lst)
  (cases expression* lst
    (empty-expr () null)
    (expressions (expr rest-exprs) (append (expr*->list rest-exprs)  (list expr)))))

(define (list->expr* lst)
  (if (null? lst) (empty-expr) 
    (expressions (car lst) (list->expr* (cdr lst)))))

(define expval->scheme
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (bool-val (bool) bool)
      (list-val (lst) (expr*->list lst))
      (null-val () null)
      (else (println "Not a expval")))))

(define scheme->expval
  (lambda (val)
    (cond
      [(number? val) (num-val val)]
      [(boolean? val) (bool-val val)]
      [(list? val) (list->expr* val)]
      [(null? val) (null-val)])))

(provide (all-defined-out))
(#%provide (all-defined))