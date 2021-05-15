#lang racket
(require "mulb.rkt")

(require rackunit)

(define (eval-exp-exn e)
  (with-handlers
      ([exn:fail? (lambda (e) (exn-message e))])
    (eval-exp e)))
      
(define tests
  (test-suite
   "Sample tests for Project MUL461 Part b"
   ;; test for pair/list related expressions
   (check-equal? (eval-exp-exn (mul461-null)) (mul461-null) "null test")
   
   (check-equal? (eval-exp-exn
                  (mul461-cons
                   (mul461-+ (mul461-int 1) (mul461-int 2))
                   (mul461-cons (mul461-+ (mul461-int 3) (mul461-int 4)) (mul461-null))))
                 (mul461-cons (mul461-int 3) (mul461-cons (mul461-int 7) (mul461-null)))
                 "cons test")

   (check-equal? (eval-exp-exn
                  (mul461-car
                   (mul461-cdr
                    (mul461-cons
                     (mul461-+ (mul461-int 1) (mul461-int 2))
                     (mul461-cons (mul461-+ (mul461-int 3) (mul461-int 4)) (mul461-null))))))
                 (mul461-int 7)
                 "car and cdr test")

   (check-equal? (eval-exp-exn
                  (mul461-cons
                   (mul461-isnull (mul461-cdr (mul461-cons (mul461-int 3) (mul461-null))))
                   (mul461-or (mul461-isnull (mul461-int 3))
                              (mul461-isnull (mul461-cons (mul461-int 1) (mul461-int 2))))))
                 (mul461-cons (mul461-bool #t) (mul461-bool #f))
                 "isnull test")

   (check-equal? (eval-exp-exn
                  (mul461-let "y"
                              (mul461-* (mul461-int 2) (mul461-int 3))
                              (mul461-fun "addy" "x" (mul461-+ (mul461-var "x") (mul461-var "y")))))
                 (mul461-closure (mul461-fun "addy" "x" (mul461-+ (mul461-var "x") (mul461-var "y")))
                                 (list (cons "y" (mul461-int 6))))
                 "fun-closure test")
   
   (check-equal? (eval-exp-exn
                  (mul461-call
                   (mul461-let "y"
                              (mul461-* (mul461-int 2) (mul461-int 3))
                              (mul461-fun "addy" "x" (mul461-+ (mul461-var "x") (mul461-var "y"))))
                   (mul461-- (mul461-int 10) (mul461-int 3))))
                 (mul461-int 13)
                 "fun-call test")
   
   (check-equal? (eval-exp-exn
                  (mul461-call
                   mul461-factorial
                   (mul461-int 4)))
                 (mul461-int 24)
                 "recursive fun-call test")
   
   (check-equal? (makelist null) (mul461-null)
                 "makelist null test")

   (check-equal? (makelist (list (mul461-isnull (mul461-int 1)) (mul461-+ (mul461-int 2) (mul461-int 3))))
                 (mul461-cons
                  (mul461-isnull (mul461-int 1))
                  (mul461-cons
                   (mul461-+ (mul461-int 2) (mul461-int 3))
                   (mul461-null)))
                 "makelist test")


   (check-equal? (eval-exp-exn
                   (mul461-call
                    (mul461-call
                     mul461-filter
                     (mul461-fun "gt5" "x" (mul461-< (mul461-int 5) (mul461-var "x"))))
                    (makelist (list (mul461-int 8) (mul461-int 3) (mul461-int 6) (mul461-int 5)))))
                 (makelist (list (mul461-int 8) (mul461-int 6)))
                 "mul461-filter test")
                  

   (check-equal? (eval-exp-exn
                   (mul461-call
                    (mul461-call
                     mul461-map
                     (mul461-fun "add5" "x" (mul461-+ (mul461-var "x") (mul461-int 5))))
                    (makelist (list (mul461-int 2) (mul461-int 3) (mul461-int 4)))))
                 (makelist (list (mul461-int 7) (mul461-int 8) (mul461-int 9)))
                 "mul461-map add5 test")
   
   ))
(require rackunit/text-ui)
;; runs the test
(run-tests tests)
