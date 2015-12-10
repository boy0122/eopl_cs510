#lang eopl

  (require "lang.scm")

  (provide type-of type-of-program)
;min zheng && changjiang cai

  ;; check-equal-type! : Type * Type * Exp -> Unspecified
  ;; Page: 242
  (define check-equal-type!
    (lambda (ty1 ty2 exp)
      (if (not (equal? ty1 ty2))
        (report-unequal-types ty1 ty2 exp)
	#t))) ; DN added arbitrary since the EOPL book uses syntax removed in Racket 5.3.3

  ;; report-unequal-types : Type * Type * Exp -> Unspecified
  ;; Page: 243
  (define report-unequal-types
    (lambda (ty1 ty2 exp)
      (eopl:error 'check-equal-type!  
          "Types didn't match: ~s != ~a in~%~a"
          (type-to-external-form ty1)
          (type-to-external-form ty2)
          exp)))

  ;;;;;;;;;;;;;;;; The Type Checker ;;;;;;;;;;;;;;;;
  
  ;; type-of-program : Program -> Type
  ;; Page: 244
  (define type-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1) (type-of exp1 (init-tenv))))))

  ;; type-of : Exp * Tenv -> Type
  ;; Page 244--246
  (define type-of
    (lambda (exp tenv)
      (cases expression exp

        (const-exp (num) (int-type))

        (var-exp (var) (apply-tenv tenv var))

        (diff-exp (exp1 exp2)
          (let ((ty1 (type-of exp1 tenv))
                (ty2 (type-of exp2 tenv)))
            (check-equal-type! ty1 (int-type) exp1)
            (check-equal-type! ty2 (int-type) exp2)
            (int-type)))

        (zero?-exp (exp1)
          (let ((ty1 (type-of exp1 tenv)))
            (check-equal-type! ty1 (int-type) exp1)
            (bool-type)))

        (if-exp (exp1 exp2 exp3)
          (let ((ty1 (type-of exp1 tenv))
                (ty2 (type-of exp2 tenv))
                (ty3 (type-of exp3 tenv)))
            (check-equal-type! ty1 (bool-type) exp1)
            (check-equal-type! ty2 ty3 exp)
            ty2))

        (let-exp (var exp1 body)
          (let ((exp1-type (type-of exp1 tenv)))
            (type-of body
              (extend-tenv var exp1-type tenv))))

        (proc-exp (var var-type body)
          (let ((result-type
                  (type-of body
                    (extend-tenv var var-type tenv))))
            (proc-type var-type result-type)))

        
        (call-exp (rator rand) 
          (let ((rator-type (type-of rator tenv))
                (rand-type  (type-of rand tenv)))
            (cases type rator-type
              (proc-type (arg-type result-type)
                (begin
                  (check-equal-type! arg-type rand-type rand)
                  result-type))
              (else
                (report-rator-not-a-proc-type rator-type rator)))))

        (letrec-exp (p-result-type p-name b-var b-var-type p-body
                      letrec-body)
          (let ((tenv-for-letrec-body
                  (extend-tenv p-name
                    (proc-type b-var-type p-result-type)
                    tenv)))
            (let ((p-body-type 
                    (type-of p-body
                      (extend-tenv b-var b-var-type
                        tenv-for-letrec-body)))) 
              (check-equal-type!
                p-body-type p-result-type p-body)
              (type-of letrec-body tenv-for-letrec-body))))
        
        ;; newly added for EXPLICIT-REFS;
        ;; typing rule for newref
        ;; (type-of (var-exp var) tenv ) = t1
        ;; --------------------------------------------
        ;; (type-of (newref-exp var) tenv) = refto t1
        (newref-exp (var) 
                    (let ((t-var (type-of var tenv)))
                      (ref-type t-var)))
        
        ;; typing rule for deref
        ;; (type-of exp tenv) = refto t1
        ;; --------------------------------------------
        ;; (type-of (deref-exp exp) tenv) = t1
        (deref-exp (exp1)
                   (let ((t1 (type-of exp1 tenv)))
                     (cases type t1
                      (ref-type (t2) t2)
                      (else (report-expression-not-a-refto-type t1 exp)))
                     ))
        ;(setref-exp (exp1 exp2)
	;	       (let ((exp-type (type-of exp1 tenv)))
	;		 (cases type exp-type
	;			(ref-type (arg-type)
	;				    (void-type))
	;			(else
	;			 (report-expression-not-a-refto-type exp1)))))
        
        ;typing rule
        ; (begin-exp (exp1 exps))
        ;(type-of exp1 tenv) = t1
        ;(type-of 1rst-exp-in-exps tenv) = t2
        ; ...
        ;(type-of last-exp-in-exps tenv)= tn
        ;---------------------------------------
        ; (type-of (begin-exp (exp1 exps)))=tn
        (begin-exp (exp1 exps)
                   (if (null? exps) (type-of exp1 tenv) (begin
                                                          (type-of exp1 tenv) 
                                                          (begin-type-helper exps tenv)))
                   )
        ;; typing rule for setref
        ;; (type-of ref-exp tenv) = refto t1
        ;; (type-of exp2 tenv) = t1
        ;; --------------------------------------------
        ;; (type-of (setref-exp ref-exp exp2) tenv) = void
        (setref-exp (ref-exp var-exp)
                    (let ((t1 (type-of ref-exp tenv))
                          (t2 (type-of var-exp tenv)))
                     (cases type t1
                      ; syntax for begin in scheme
                      ; (begin exp1 exp2 ...)
                      ; In terms of control flow, a (begin ... ) expression is rather like a begin ... end block in Pascal,
                      ; or a { ... } block in C. 
                      ; (We don't need an end keyword, because the closing parenthesis does the job.)
                      ; A begin returns the value of the last expression in the sequence.
                      (ref-type (t-temp) (begin (check-equal-type! t2 t-temp var-exp) 
                                                (void-type)))
                        (else (report-expression-not-a-refto-type t1 exp)))))
                     
        )))
;to check type for every expressions in exps for begin expression
  (define begin-type-helper
    (lambda (exps tenv)
      ;(begin (type-of  exps tenv))))
   ;   (if (null? (cdr exps)) ((type-of (car exps) tenv) (begin (type-of (cdr exps) tenv))))))
      (if (null? (cdr exps)) (type-of (car exps) tenv) (begin
                                                         (type-of (car exps) tenv) 
                                                        (begin-type-helper (cdr exps)) tenv))))
      ;    (cons (type-of (car exps) tenv) (begin-type-helper (cdr exps) tenv)))))
    ;  (if (not(null? exps)) (cons )))
    
  (define report-rator-not-a-proc-type
    (lambda (rator-type rator)
      (eopl:error 'type-of-expression
        "Rator not a proc type:~%~s~%had rator type ~s"   
           rator 
           (type-to-external-form rator-type))))
    
    ; helper function
    (define report-expression-not-a-refto-type
      (lambda (exp-type exp)
        (eopl:error 'type-of-expression
        "Expression not a refto type:~%~s~%had rator type ~s"   
           exp 
           (type-to-external-form exp-type))))

  ;;;;;;;;;;;;;;;; type environments ;;;;;;;;;;;;;;;;
    
  (define-datatype type-environment type-environment?
    (empty-tenv-record)
    (extended-tenv-record
      (sym symbol?)
      (type type?)
      (tenv type-environment?)))
    
  (define empty-tenv empty-tenv-record)
  (define extend-tenv extended-tenv-record)
    
  (define apply-tenv 
    (lambda (tenv sym)
      (cases type-environment tenv
        (empty-tenv-record ()
          (eopl:error 'apply-tenv "Unbound variable ~s" sym))
        (extended-tenv-record (sym1 val1 old-env)
          (if (eqv? sym sym1) 
            val1
            (apply-tenv old-env sym))))))
  
  (define init-tenv
    (lambda ()
      (extend-tenv 'x (int-type) 
        (extend-tenv 'v (int-type)
          (extend-tenv 'i (int-type)
            (empty-tenv))))))
