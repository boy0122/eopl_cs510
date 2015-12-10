#lang eopl
;student name: Min Zheng
;Assignment 1
;I pledge my honor that I have abided by the stevens honor system.
;On my honor, I have neither recieved nor given any unauthorized assistance on 
;this homework.
;Exercise 1.11
;The last line of subst-in-s-exp call the subst procedure which will reduce 
;the subst problem to a smaller subproblem. Thus the recursion will finally 
;reduce to a sexp that is a symbol or empty slist '().
;Thus, the recursion is guranteed to halt.

;Exercise 1.12
;subst with inlining technique 
(define subst 
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons
         (if (symbol? (car slist))
             (if (eqv? (car slist) old) new (car slist))
             (subst new old (car slist)))
         (subst new old (cdr slist))))))


