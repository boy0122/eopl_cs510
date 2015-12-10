#lang eopl

  ;; data structures for proc-lang/ds-rep

  (require "lang.scm")                  ; for expression?

  (provide (all-defined-out))           ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?))
    (list-val
     (lst list?))
    ;new added
    )

;;; extractors:


;; expval->scheme-val : ExpVal -> Scheme value,  for testing
(define expval->scheme-val
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (bool-val (bool) bool)
      (list-val (lst) lst)
      (proc-val (proc) "procedure value cannot be displayed")
      ;new added
     ; (cond-val (cond))
      (else (expval-extractor-error 'TypeError v)))))

  ;; expval->num : ExpVal -> Int
  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

 ;; expval->list : ExpVal -> List
  (define expval->list
    (lambda (v)
      (cases expval v
	(list-val (lst) lst)
        (else (eopl:error "not expresseddd val ~s" v)))))
;	(else (expval-extractor-error 'lst v)))))

  ;; expval->bool : ExpVal -> Bool
  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  ;; expval->proc : ExpVal -> Proc
  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))
;new added
 
  ;(define cond-val
   ; (lambda (conds opr env)
    ;  (cond 
     ;   ((null? conds) (eopl:error "no expresssions for cond"))
      ;  ((expval->bool (value-of (car conds) env))
       ;  (value-of (car opr) env))
        ;(else
         ;(cond-val (cdr conds) (cdr opr) env)))))



;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  ;; proc? : SchemeVal -> Bool
  ;; procedure : Var * Exp * Env -> Proc
  (define-datatype proc proc?
    (procedure
      (var symbol?)
      (body expression?)
      (env environment?)
      ;new added
      (trace boolean?)))

;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

;; example of a data type built without define-datatype

  (define empty-env-record
    (lambda () 
      '()))

  (define extended-env-record
    (lambda (sym val old-env)
      (cons (list sym val) old-env)))
  
  (define empty-env-record? null?)
  
  (define environment?
    (lambda (x)
      (or (empty-env-record? x)
          (and (pair? x)
               (symbol? (car (car x)))
               (expval? (cadr (car x)))
               (environment? (cdr x))))))

  (define extended-env-record->sym
    (lambda (r)
      (car (car r))))

  (define extended-env-record->val
    (lambda (r)
      (cadr (car r))))

  (define extended-env-record->old-env
    (lambda (r)
      (cdr r)))

