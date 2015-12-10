#lang eopl
;I pledge my honor that I have abided by the Stevens Honor System
;min zheng
  ;; language for IMPLICIT-REFS

  (provide (all-defined-out))
  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  (define the-grammar
    ;modified
    '((program (statement) a-program)

      (expression (number) const-exp)
      (expression
        ("-" "(" expression "," expression ")")
        diff-exp)
      
      ;new added for addition
      (expression
       ("+" "(" expression "," expression ")")
       add-exp)
      
      ;new added for multiplication
      (expression
       ("*" "(" expression "," expression ")")
       multi-exp)
      
      ;new added for not operation
      (expression
       ("not" "(" expression ")")
       not-exp)
      
      (expression
       ("zero?" "(" expression ")")
       zero?-exp)

      (expression
       ("if" expression "then" expression "else" expression)
      ; ("if" expression  expression  expression)
       if-exp)

      (expression (identifier) var-exp)

      (expression
       ("let" identifier "=" expression "in" expression)
       let-exp)   
      ;support mulitple arguments
      (expression
       ("proc" "(" (separated-list identifier ",") ")" expression)
       proc-exp)
      ; support mulitple arguments
      (expression
       ("(" expression (arbno expression) ")")
       call-exp)

      (expression
        ("letrec"
          (arbno identifier "(" (separated-list identifier ",") ")" "=" expression)
           "in" expression)
        letrec-exp)
      
      (expression
        ("begin" expression (arbno ";" expression) "end")
        begin-exp)

      ;; new for implicit-refs

      (expression
        ("set" identifier "=" expression)
        assign-exp)
      
        ;new added
      (statement
       (identifier "=" expression)
       set-stat)
  
      ;new added
      (statement
       ("print" expression)
       print-stat)
      
          ;new added
      (statement
       ("{" (separated-list statement ";") "}")
       block-stat)
      
      ;new added
       (statement
       ("if" expression statement statement)
       if-stat)
       
       ;new added
      (statement
       ("while" expression statement)
       while-stat)
      
      ;new added 
      (statement
       ("var" (separated-list identifier ",") ";" statement)
       declare-stat)

      ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
