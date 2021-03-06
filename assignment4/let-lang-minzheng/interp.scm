#lang eopl
;student name: Min Zheng
;Assignment 4
;I pledge my honor that I have abided by the stevens honor system.
;On my honor, I have neither recieved nor given any unauthorized assistance on 
;this homework.
(require "lang.scm")
(require "data-structures.scm")
(require "environments.scm")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : program -> expval

(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (value-of body (init-env))))))

;; value-of : expression * environment -> expval

(define value-of
  (lambda (exp env)
    (cases expression exp
      
      (const-exp (num) (num-val num))
      
      (var-exp (id) (apply-env env id))
      
      (diff-exp (exp1 exp2)
                (let ((val1
                       (expval->num
                        (value-of exp1 env)))
                      (val2
                       (expval->num
                        (value-of exp2 env))))
                  (num-val
                   (- val1 val2))))
      
      (zero?-exp (exp1)
                 (let ((val1 (expval->num (value-of exp1 env))))
                   (if (zero? val1)
                       (bool-val #t)
                       (bool-val #f))))
      
      (if-exp (exp0 exp1 exp2) 
              (if (expval->bool (value-of exp0 env))
                  (value-of exp1 env)
                  (value-of exp2 env)))
      
      (let-exp (id rhs body)       
               (let ((val (value-of rhs env)))
                 (value-of body
                           (extend-env id val env))))
      ;new added
      (plus-exp (exp1 exp2)
                (let ((val1
                       (expval->num (value-of exp1 env)))
                      (val2 
                       (expval->num (value-of exp2 env))))
                  (num-val
                   (+ val1 val2))))
      (minus-exp (exp1)
                 (let ((val1 (expval->num (value-of exp1 env))))
                   (num-val
                    (- 0 val1))))
      (sign-exp (exp1 exp2)
                (let ((val1 (expval->bool (value-of exp1 env)))
                      (val2 (expval->num (value-of exp2 env))))
                  (if (eqv? #t val1)
                      (num-val
                       val2)
                      (num-val
                       (- 0 val2)))))
      (less?-exp (exp1 exp2)
             (let ((val1 (expval->num (value-of exp1 env)))
                   (val2 (expval->num (value-of exp2 env))))
               (bool-val
                (< val1 val2))))
      
      )))
