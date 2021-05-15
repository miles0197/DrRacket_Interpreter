#lang racket
(provide (all-defined-out)) ;; we will import this code and run tests in a different file
;; mul (made up language) part a
;; int and boolean expressions and operations, 
;; int constant and operations on ints
(struct mul461-int (value) #:transparent)
(struct mul461-+ (e1 e2) #:transparent) ;; given
(struct mul461-- (e1 e2) #:transparent)
(struct mul461-* (e1 e2) #:transparent)

;; boolean constant and operations on booleans
(struct mul461-bool (value) #:transparent)
(struct mul461-and (e1 e2) #:transparent)
(struct mul461-or (e1 e2) #:transparent)
(struct mul461-not (e) #:transparent)

;; comparisons between two int expressions
(struct mul461-< (e1 e2) #:transparent)
(struct mul461-= (e1 e2) #:transparent)

;; first evaluates e1 to a boolean value, if it is true, the whole expression evaluates to e2
;; otherwise the whole expression evaluates to e3
(struct mul461-if (e1 e2 e3) #:transparent)

;; variable and let
(struct mul461-var (name) #:transparent) ;; given
(struct mul461-let (name e body) #:transparent)

;; pair and list
(struct mul461-null () #:transparent)
(struct mul461-cons (e1 e2) #:transparent)
(struct mul461-car (e) #:transparent)
(struct mul461-cdr (e) #:transparent)
(struct mul461-isnull (e) #:transparent)

;; function declaration and function call
(struct mul461-fun (fname formal body) #:transparent)
(struct mul461-call (funexp actual) #:transparent)

;; closure is not a mul461-exp in the source code
;; it is a value that a mul461-fun evaluates to.
(struct mul461-closure (fun env) #:transparent)

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of mul461 expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  
  (cond [(mul461-var? e) ;variable 
         (envlookup env (mul461-var-name e))]
        [(mul461-int? e) e] ;integer
        [(mul461-bool? e) e] ;boolean
        
        [(mul461-+? e) ;addition
         (let ([v1 (eval-under-env (mul461-+-e1 e) env)]
               [v2 (eval-under-env (mul461-+-e2 e) env)])
           (if (and (mul461-int? v1)
                    (mul461-int? v2))
               (mul461-int (+ (mul461-int-value v1) 
                              (mul461-int-value v2)))
               (error "mul461-+ applied to non-integer")))]
        
        ;; CHANGE add more cases here
        [(mul461--? e) ;subtraction
         (let ([v1 (eval-under-env (mul461---e1 e) env)]
               [v2 (eval-under-env (mul461---e2 e) env)])
           (if (and (mul461-int? v1)
                    (mul461-int? v2))
               (mul461-int (- (mul461-int-value v1)
                              (mul461-int-value v2)))
               (error "mul461-- applied to non-integer")))]
        
        [(mul461-*? e) ;multiplication
         (let ([v1 (eval-under-env (mul461-*-e1 e) env)]
               [v2 (eval-under-env (mul461-*-e2 e) env)])
           (if (and (mul461-int? v1)
                    (mul461-int? v2))
               (mul461-int (* (mul461-int-value v1)
                              (mul461-int-value v2)))
               (error "mul461-* applied to non-integer")))]

        
        [(mul461-and? e) ;logical and
         (let ([v1 (eval-under-env (mul461-and-e1 e) env)]
               [v2 (eval-under-env (mul461-and-e2 e) env)])
           (if (and (mul461-bool? v1)
                    (mul461-bool? v2))
               (mul461-bool (and (mul461-bool-value v1)
                                 (mul461-bool-value v2)))
               (error "mul461-and applied to non-bool")))]
        
        [(mul461-or? e) ;logical or
         (let ([v1 (eval-under-env (mul461-or-e1 e) env)]
               [v2 (eval-under-env (mul461-or-e2 e) env)])
           (if (and (mul461-bool? v1)
                    (mul461-bool? v2))
               (mul461-bool (or (mul461-bool-value v1)
                                (mul461-bool-value v2)))
               (error "mul461-or applied to non-bool")))]

        ;;not sure 
        [(mul461-not? e) ;not
         (let ([v1 (eval-under-env (mul461-not-e e) env)])
           
           (if (mul461-bool? v1)
               (mul461-bool (not (mul461-bool-value v1)))
               
               (error "mul461-not applied to non-bool")))]

        ;;less than
        [(mul461-<? e)
         (let ([v1 (eval-under-env (mul461-<-e1 e) env)]
               [v2 (eval-under-env (mul461-<-e2 e) env)])
           (if (and (mul461-int? v1)
                    
                    (mul461-int? v2))
               
               (if (< (mul461-int-value v1) (mul461-int-value v2)) (mul461-bool #t) (mul461-bool #f))
               
               (error "mul461-< applied to non-int")))]

        ;;equal
        [(mul461-=? e)
         (let ([v1 (eval-under-env (mul461-=-e1 e) env)]
               [v2 (eval-under-env (mul461-=-e2 e) env)])
           (if (and (mul461-int? v1)
                    (mul461-int? v2))
               (if (= (mul461-int-value v1) (mul461-int-value v2)) (mul461-bool #t) (mul461-bool #f))

               (error "mul461-= applied to non-int")))]

        [(mul461-if? e)
         (let ([v1 (eval-under-env (mul461-if-e1 e) env)])
           (if (mul461-bool? v1)
               (if (mul461-bool-value v1)
                   (eval-under-env (mul461-if-e2 e) env)
                   (eval-under-env (mul461-if-e3 e) env))
               (error "mul461-if applied to non-bool")))]
        
        [(mul461-let? e)
         (define ss_en(mul461-let-name e))
         (let ([v1 (eval-under-env (mul461-let-e e) env)])
           (eval-under-env (mul461-let-body e) (cons (cons ss_en v1) env)))]

        [(mul461-cons? e) ;cons pair creator
         (let ([v1 (eval-under-env (mul461-cons-e1 e) env)]
               [v2 (eval-under-env (mul461-cons-e2 e) env)])
           (mul461-cons v1 v2))]

        [(mul461-car? e) ;first pair 
         (let ([v (eval-under-env (mul461-car-e e) env)])
           (if (mul461-cons? v)
               
               (mul461-cons-e1 v)
               (error "mul461-car applied to non-pair")))]
        
        [(mul461-cdr? e) ;second pair
         (let ([v (eval-under-env (mul461-cdr-e e) env)])
           (if (mul461-cons? v)
               (mul461-cons-e2 v)
               (error "mul461-cdr applied to non-par")))]

        [(mul461-null? e) ;no data, null
         (mul461-null)]

        [(mul461-isnull? e) ; null, expression, returns boolean
         (let ([v1 (eval-under-env (mul461-isnull-e e) env)])
           (if (mul461-null? v1) (mul461-bool #t)
               (mul461-bool #f)))]
        

        [(mul461-closure? e) ;closure evaluates to itself
         e]

        [(mul461-fun? e) ;function evaluates to closure with fun and env
         (mul461-closure (mul461-fun (mul461-fun-fname e) (mul461-fun-formal e) (mul461-fun-body e)) env)]

        [(mul461-call? e) ;evaluates first and second subexp to values, raise err if first pair is no closure, add the bindings to the env in closure
         (let ([v1 (eval-under-env (mul461-call-funexp e) env)]
               [v2 (eval-under-env (mul461-call-actual e) env)])
           (if (mul461-closure? v1)
               (eval-under-env (mul461-fun-body (mul461-closure-fun v1))
                               (cons (cons (mul461-fun-fname (mul461-closure-fun v1)) v1)
                                     (cons (cons (mul461-fun-formal (mul461-closure-fun v1)) v2)
                                           (mul461-closure-env v1))))
               (error "mul461-call applied to non-closure")))]
                              
        
        [#f (error (format "bad mul461 expression: ~v" e))]))
        


;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; CHANGE and implement makelet* and makefactorial
(define (makelet* t e)
  (if (null? t)
      e
      (let ([first-pairs (car t)]
            [remaining-pairs (cdr t)])
        (mul461-let (car first-pairs) (cdr first-pairs) (makelet* remaining-pairs e)))))


;;for this question i have reference from : https://stackoverflow.com/questions/19211310/scheme-car-and-cdr-recursion?rq=1 and https://docs.racket-lang.org/guide/let.html

(define (makefactorial n)
  (if (<= n 1)
      1
      (* mul461-* (makefactorial (- n 1))))) ;not real place hold purpose

;; A sample mul461 function factorial
(define mul461-factorial
  (mul461-fun "factorial" "n"
              (mul461-if (mul461-< (mul461-var "n") (mul461-int 2))
                         (mul461-int 1)
                         (mul461-*
                          (mul461-var "n")
                          (mul461-call (mul461-var "factorial")
                                       (mul461-- (mul461-var "n") (mul461-int 1)))))))

;; CHANGE and implement makelist
(define (makelist t)  ; list of pairs,exp or null
  (if (null? t) (mul461-null)
      (mul461-cons (car t) (makelist (cdr t)))))



;; CHANGE and define mul461 functions filter and map:
;not sure 
(define (mul461-filter m)
  (if (mul461-cons? m) null
      (cons (mul461-cons-e1 m) (mul461-filter
                                (mul461-cons-e2 m)))))
;not sure
(define mul461-map
  (mul461-fun 
   "map" "f"
   (mul461-fun
    "mapf" "t"
    (mul461-var "t"))))

;;test cases
