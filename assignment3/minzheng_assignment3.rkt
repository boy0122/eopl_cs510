#lang eopl
;student name: Min Zheng
;Assignment 3
;I pledge my honor that I have abided by the stevens honor system.
;On my honor, I have neither recieved nor given any unauthorized assistance on 
;this homework.

;Exercise 2.4
; I use [] to simulate a stack, specify stack as list
;(emcpty-stack)             = [()]
;(push sdata [stack])         = [(cons sdata stack)] 
;(pop [stack])                = {error if stack is [()], means stack is empty}
;                             {[(cdr stack)]}
;(top [stack])                = {error if stack is [()], means stack is empty}
;                           = {[car stack] }
;(empty-stack? [stack])       = {#t if stack is [()], means stack is empty}
;                           = {#f if stack is not empty}
;empty-stack? and top are observers, all others are constructors

;Exercise 2.5
;Env = ()|((Var . SchemeVal)  Env)

;empty-env
;empty-env: () --> Env
;usage: (empty-env ) It creating a empty environment by creating a empty list
(define empty-env
  (lambda ()
    '()))
;extend-env
;extend-env: Var * SchemeVal * Env --> Env
;usage (extend-env var val env) It produces a new enviroment that behave like env by creating a association
;list
(define extend-env 
  (lambda (var val env)
    (cons (cons var val)
          env)))

;apply-env
;apply-env: Env * Var --> SchemeVal
;usage (apply-env env search-var) It applies a representation of an enviroment to a variable by returning
;the scheme value of that environment
(define apply-env
  (lambda (env search-var)
    (cond
      ((null? env) (eopl:error 'apply-env "No bounding for ~s" search-var))
      (else (cond
              ((eqv? search-var (car (car env))) (cdr (car env)))
              (else (apply-env (cdr env) search-var)))))))

;Exercise 2.8 
;empty-env? 
;empty-env?: Env --> Boolean
;usage (empty-env? env) It check whether the environment env is empty
(define empty-env?
  (lambda (env)
    (cond
      ((null? env) #t)
      (else #f))))

;Exercise 2.9
;has-binding?
;has-binding?: Env * Var --> Boolean
;usage: (has-binding? env s) It checks if the varialbe s has associated value in environment env
(define has-binding?
  (lambda (env s)
    (cond
      ((null? env) #f)
      (else (cond
              ((eq? s (car (car env))) #t)
              (else (has-binding? (cdr env) s)))))))

;Exercise 2.10
;extend-env*
;extend-env*: List * List * Env --> Env
;usage: (extend-env* varlist vallist env) It produces a new environment
(define extend-env*
  (lambda (varlist vallist env)
    (cond
      ((null? varlist) env)
      (else (extend-env (car varlist) (car vallist) (extend-env* (cdr varlist) (cdr vallist) env))))))

;Exercise 2.12
;implementing stack data type using a procedural representation
;Stack = Stack --> SchemeData

;empty-stack
;empty-stack: ()--> stacks
;usage: (empty-stack) It builds a empty stack 
(define empty-stack
  (lambda ()
    (lambda (oper)
      (cond
        ((eq? oper 'empty-stack?) #t)
        (else (eopl:error "Invalid operations on empty stacks"))))))
;push
;push: SchemeData * Stack --> Stack
;usage: (push sdata stack) It returns a new stack by pushing a new element in the stack
(define push
  (lambda (sdata stack)
    (lambda (oper)
      (cond
        ((eq? oper 'empty-stack?) #f)
        ((eq? oper 'pop) stack)
        ((eq? oper 'top) sdata)
        (else (eopl:error "unaccpeted operation"))))))
;empty-stack?
;empty-stack?: Stack --> Boolean
;usage: (empty-stack? stack) It checks whether the stack is empty
(define empty-stack?
  (lambda (stack)
    (stack 'empty-stack?)))
;pop
;pop: Stack --> Stack
;usage: (pop stack) It builds a new stack by remvoing the top element
(define pop 
  (lambda (stack)
    (stack 'pop)))
;top
;top: Stack --> SchemData
;usage: (top stack) It returns the toppest element in a stack
(define top
  (lambda (stack)
    (stack 'top)))

;testcase for the procedural implementation
(define stack-testcase
  (lambda ()
  (and (eq? #t (empty-stack? (empty-stack))) 
       (empty-stack? (pop (push 5 (empty-stack))))
       (eq? 5 (top (push 5 (empty-stack)))))))

;Exercise 2.24
;datatype of bintree
(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

;bintree-to-list
;bintree-to-list: Bintree --> List
;Usage: (bintree-to-list tree) It returns a list with all interior nodes and leaf nodes in it.
(define bintree-to-list
  (lambda (tree)
    (cases bintree tree
      (leaf-node (node)
                 (list 'leaf-node node))
      (interior-node (key left right)
                     (list 'interior-node
                           key
                           (bintree-to-list left)
                           (bintree-to-list right)))
      (else (eopl:error "Illegal type")))))

;test bintree
(define ea (interior-node 'a (leaf-node 3) (leaf-node 4)))





















