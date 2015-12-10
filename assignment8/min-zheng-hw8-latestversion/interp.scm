#lang eopl
;min zheng
;I pledge my honor that I have abided by the Stevens Honor System
  ;; interpreter for the IMPLICIT-REFS language

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)

  ;; show the contents of define-datatype values
  (print-struct #t)

  (require racket/pretty)
  ;(provide (all-from (lib "pretty.ss")))

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)
      (cases program pgm
        ;(a-program (exp1)
         ; (value-of exp1 (init-env))))))
        (a-program (stat)
                   (execute-stat stat (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 118, 119
  (define value-of
    (lambda (exp env)
      (cases expression exp

        (const-exp (num) (num-val num))

        (var-exp (var) (deref (apply-env env var)))

        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))
        ;new added cases
        (add-exp (exp1 exp2)
                 (let ((val1 (value-of exp1 env))
                       (val2 (value-of exp2 env)))
                   (num-val (+ (expval->num val1)
                               (expval->num val2)))))
        ;new added cases
        (multi-exp (exp1 exp2)
                 (let ((val1 (value-of exp1 env))
                       (val2 (value-of exp2 env)))
                   (num-val (* (expval->num val1)
                               (expval->num val2)))))
        ;new added cases
        (not-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (if (expval->bool val1) (bool-val #f) (bool-val #t))))
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        (let-exp (var exp1 body)       
          (let ((v1 (value-of exp1 env)))
            (value-of body
              (extend-env var (newref v1) env))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))
        ;modified for multiple arguments
        (call-exp (rator rands)
          (let ((proc (expval->proc (value-of rator env)))
                (args (map (lambda (rand) (value-of rand env)) rands)))
            (apply-procedure-m proc args)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (assign-exp (var exp1)
          (begin
            (setref!
              (apply-env env var)
              (value-of exp1 env))
            (num-val 27)))

        )))


  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 119

  ;; uninstrumented version
  ;;  (define apply-procedure
  ;;    (lambda (proc1 val)
  ;;      (cases proc proc1
  ;;        (procedure (var body saved-env)
  ;;          (value-of body
  ;;            (extend-env var (newref val) saved-env))))))
  
  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
          (let ((r (newref arg)))
            (let ((new-env (extend-env var r saved-env)))
              (when (instrument-let)
                (begin
                  (eopl:printf
                    "entering body of proc ~s with env =~%"
                    var)
                  (pretty-print (env->list new-env)) 
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))  

  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (list
            (car p)
            (expval->printable (cadr p))))
        l)))
;apply-procedure1 for multiple arguemts of proc and call expression
(define apply-procedure-m
  (lambda (proc1 vals)
    (cases proc proc1
      (procedure (vars body saved-env)
                 (value-of body (extend-env* vars vals saved-env))))))
;execute a statement
(define execute-stat
  (lambda (stat env)
    (cases statement stat
	   (print-stat (exp)
		       (print-exp (value-of exp env)))
                       ;(value-of exp env))
	   (declare-stat (vars inner-stat)
			 (execute-stat inner-stat (extend-state-env* vars env)))

	   (if-stat (test-exp true-stat false-stat)
                    (let ((val (value-of test-exp env)))
                       (if (expval->bool val)
                           (execute-stat true-stat env)
                           (execute-stat false-stat env))))
		   

	   (set-stat (var exp)
		     (setref!
		      (apply-env env var)
		      (value-of exp env)))

	   (block-stat (stats)
		       (execute-stats stats env))

	   (while-stat (exp stat)
		       (execute-while-statement exp stat env)
                       ))))
;execute statements
(define execute-stats
  (lambda (stats env)
    (cond ((not (null? stats))
	  (execute-stat (car stats) env)
	  (execute-stats (cdr stats) env)))))
;execute while statement
(define execute-while-statement
  (lambda (exp stat env)
    (let ((val (value-of exp env)))
      (cond ((expval->bool val)
	    (execute-stat stat env)
	    (execute-while-statement exp stat env))))))
;print value of statement
(define print-exp
  (lambda (exp)
    (cases expval exp
	  (bool-val (val)
		    (eopl:printf "   ~a\n" val))
	  (num-val (val)
		   (eopl:printf "  ~a\n" val))
	  (proc-val (val)
		    (eopl:printf "  ~a\n" val))
	  (ref-val (val)
		   (eopl:error "can not trying to print ref\n")))))


  


  
