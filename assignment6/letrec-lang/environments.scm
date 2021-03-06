#lang eopl

  ;; builds environment interface, using data structures defined in
  ;; data-structures.scm. 

  (require "data-structures.scm")

  (provide init-env empty-env extend-env apply-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;
  
  ;; init-env : () -> Env
  ;; usage: (init-env) = [i=1, v=5, x=10]
  ;; (init-env) builds an environment in which i is bound to the
  ;; expressed value 1, v is bound to the expressed value 5, and x is
  ;; bound to the expressed value 10.
  ;; Page: 69

  (define init-env 
    (lambda ()
      (extend-env 
       'i (num-val 1)
       (extend-env
        'v (num-val 5)
        (extend-env
         'x (num-val 10)
         (empty-env))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

  ;; Page: 86

;(define (apply-env env var)
 ; (cond ((null? env) (eopl:error 'apply-env "Variable not found"))
  ;      ((eqv? (caar env) var)
   ;      (let ((val (cadar env)))
    ;       (if (vector? val)
     ;          (vector-ref val 0)
      ;         val)))
       ; (#t (apply-env (cdr env) var))))
  (define apply-env
    (lambda (env search-sym)
      (cases environment env
             (empty-env ()
                        (eopl:error 'apply-env "No binding for ~s" search-sym))
             (extend-env (var val saved-env)
                         (if (eqv? search-sym var)
                             (if (vector? val) (vector-ref val 0) val)
                             (apply-env saved-env search-sym)))
      ;  (extend-env-rec (p-name b-var body saved-env)
       ;                 (if (eqv? p-name search-sym)
        ;                    (if (vector? (cadar env)) (vector-ref val 0) (apply-env (cdr env) search sym))
         ;                   (apply-env saved-env search-sym)))
        ;)))
             (else (cond 
        ((eqv? (cadr env) search-sym)
        (let ((val (caddr env)))
           (if (vector? val)
               (vector-ref val 0)
               (apply-env (cdr env) search-sym)))))))))
        ;(else (apply-env (cdr env) search-sym)))))))
          ;                   (if (eqv? search-sym p-name)
           ;                      (proc-val (procedure b-var p-body env))          
 ;                                (apply-env saved-env search-sym))))))
;

