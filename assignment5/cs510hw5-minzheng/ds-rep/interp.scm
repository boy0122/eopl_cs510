#lang eopl
  
  ;; interpreter for the PROC language, using the data structure
  ;; representation of procedures.

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  (define value-of
    (lambda (exp env)
      (cases expression exp

        (const-exp (num) (num-val num))

        (var-exp (var) (apply-env env var))

        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

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
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env #f)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))
        ;new added exercise 3.27
        (traceproc-exp (var body)
                       (proc-val (procedure var body env #t)))
        
        ;exercise 3.10
        (list-exp (exprs)
           (list-val (map (lambda (x) (value-of x env))  exprs)))
        (cond-exp (condi act)
		     (condfun condi act env))
        )))

;condfun for cond expression
(define (condfun condi act env)
  (cond ((null? condi)
         (bool-val #f))
        ((expval->bool (value-of (car condi) env))
         (value-of (car act) env))
        (else
         (condfun (cdr condi) (cdr act) env))))


  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
  (define apply-procedure
    (lambda (proc1 val)
      (cases proc proc1
        ;modified
        (procedure (var body saved-env trace?)
                   (if trace? (eopl:printf "Entry: trace message, ~a = ~a\n" var val) (display ""))
                   (let ((procval (value-of body (extend-env var val saved-env))))
                     (if trace? (eopl:printf "Exit: trace message, result value = ~s\n" procval) (display ""))
                     procval)))))


