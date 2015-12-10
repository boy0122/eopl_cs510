#lang eopl
;student name: Min Zheng
;Assignment 2
;I pledge my honor that I have abided by the stevens honor system.
;On my honor, I have neither recieved nor given any unauthorized assistance on 
;this homework.

;Exercise 1.17
;down
;down: ListOf(SchemeVal) --> ListOf(SchemeVal)
;usage: (down lst) wraps parentheses around each top-level element of lst
(define down 
  (lambda (lst)
    (cond
      ((null? lst) '())
      (else (cons (cons 
                   (car lst) 
                   '())
                  (down (cdr lst)))))))
;Exercise 1.18
;swapper: SchemeVal * SchemeVal * Slist --> Slist
;usage: (swapper s1 s2 slist) It builds a new list with all s1 replaced by s2
;and all s2 replaced by s1
(define swapper
  (lambda (s1 s2 slist)
    (cond
      ((null? slist) '())
      ((symbol? (car slist)) (cond
                               ((eq? s1 (car slist)) (cons s2 
                                               (swapper s1 s2 (cdr slist))))
                               ((eq? s2 (car slist)) (cons s1 
                                               (swapper s1 s2 (cdr slist))))
                               (else (cons (car slist)
                                           (swapper s1 s2 (cdr slist))))))
      (else (cons (swapper s1 s2 (car slist))
                  (swapper s1 s2 (cdr slist)))))))

;Exercise 1.19
;list-set
;list-set: ListOf(SchemeVal) * Int * SchemeVal --> ListOf(SchemeVal)
;usage: (list-set lst n x) It builds a list with the nth element replaced by x
; the index is zero-based
(define list-set
  (lambda (lst n x)
    (cond
      ((null? lst) '())
      ((eq? 0 n) (cons x 
                       (cdr lst)))
      (else (cons (car lst) 
                  (list-set (cdr lst) (- n 1) x))))))

;Exercise 1.24
;every?: Procedure * ListOf(SchemeVal) --> Boolean
;usage: (every? pred lst) returns #f if any element of lst fails to satisfy procedure pred
;otherwise, return #ti. Assuming that lst is not empty
(define every?
  (lambda (pred lst)
    (cond
      ((null? lst) #t)
      ((pred (car lst)) (every? pred (cdr lst)))
      (else #f))))

;Exercise 1.27
;flatten
;flatten: Slist --> ListOf(SchemeVal)
;usage: (flatten slist) It bulids a list with all symbols appeared in slist with all inner 
;parenthesises being removed
(define flatten
  (lambda (slist)
    (cond
      ((null? slist) '())
      ((symbol? (car slist)) (cons (car slist) 
                                   (flatten (cdr slist))))
      (else (append (flatten (car slist)) (flatten (cdr slist)))))))





