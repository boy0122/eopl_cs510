#lang eopl

;; tests for IMPLICIT-REFS
;I pledge my honor that I have abided by the Stevens Honor System
;min zheng
(require rackunit 
         rackunit/text-ui)
(require "environments.scm")
(require "data-structures.scm")  ; for expval constructors
(require "lang.scm")             ; for scan&parse
(require "interp.scm")           ; for value-of-program
(require "store.scm")            ; for value-of-program

;; predicate to test exceptions
;; exn? : any -> bool
(define exn? (lambda (exn) #t))
  
;; run : String -> ExpVal
(define run
  (lambda (string)
   ; (expval->scheme-val (value-of-program (scan&parse string)))))
    (value-of-program (scan&parse string))))

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
(define implicit-ref-new-tests
  (test-suite
   "tests for IMPLICIT-REFS language"
   
;1 test
  (display "expected:  1\n")
  (display "result: ")
  (run "var x,y; {x = 3; y = 4; print -(y,x)}")
  
  ; 2 test 
  (display "expected:  3\n")
  (display "result: ")
  (run "var x; {x = 3; print x}")
  
  ;3 test
  (display "expected:  12\n")
  (display "result: ")
   (run "var f,x; {f = proc(x,y) *(x,y); x =3; print (f 4 x)}")
   
   ;4test
   (display "expected:  3\n 4\n 3\n")
   (display "result: ")
   (run "var x; {x=3;print x; var x; {x = 4;print x};print x}")
   
   ;5 test
   (display "expected:  12\n")
   (display "result: ")
  (run "var x,y,z; {x = 3;
                    y = 4;
                    z = 0;
                    while not(zero?(x))
                      {z = +(z,y); x = -(x,1)};
       print z}")
 
))



;(run-tests let-tests 'verbose)
;(run-tests proc-tests 'verbose)
;(run-tests letrec-tests 'verbose)
;(run-tests implicit-ref-tests 'verbose)
(run-tests implicit-ref-new-tests 'verbose)
