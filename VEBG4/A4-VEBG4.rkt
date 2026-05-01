#lang typed/racket
(require typed/rackunit)

#|
TODO: WRITE STATEMTN OF HOW MUCH WE IMPLEMENTED HERE
|#

(define-type ExprC (U numC idC stringC ifC lamC appC))
(struct numC ([n : Real]) #:transparent)
(struct stringC ([s : String]) #:transparent)
(struct idC ([s : Symbol]) #:transparent)
(struct ifC ([cond : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct lamC ([params : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct appC ([fn : ExprC] [args : (Listof ExprC)]) #:transparent)

(define-type Value (U numV stringV boolV cloV primV))
(struct numV ([n : Real]) #:transparent)
(struct stringV ([s : String]) #:transparent)
(struct boolV ([b : Boolean]) #:transparent)
(struct cloV ([params : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct primV ([op : Symbol] [arity : Real]) #:transparent) ; DELETE THIS COMMENT LATER: POTENTIALLY CHANGE ARITY TO BE INTEGER

(define-type Env (Listof binding))
(struct binding ([sym : Symbol] [val : Value]) #:transparent) 

(define top-env : Env
  (list (binding 'true (boolV #t))
        (binding 'false (boolV #f))
        (binding '+ (primV '+ 2))
        (binding '- (primV '- 2))
        (binding '* (primV '* 2))
        (binding '/ (primV '/ 2))))



(: lookup (Symbol Env -> Value))
#|
lookup takes a symbol and environment and returns the Value associated with
that symbol in the environment. 
|#
(define (lookup [sym : Symbol] [env : Env]) : Value
  (match env
    ['() (error 'lookup
                "[ VEBG ] Symbol not found in environment: ~e"
                sym)]
    [(cons (binding s v) rest) (if (equal? s sym)
                                   v
                                   (lookup sym rest))]))


(: serialize (Value -> String))
#|
serialize takes a Value and returns a string representation of that value.
|#
(define (serialize [v : Value]) : String
  (match v
    [(numV n) (~v n)]
    [(boolV b) (if b "true" "false")]
    [(stringV s) (~v s)]  ; ASK ABOUT THIS "\"hello\""
    [(cloV _ _ _) "#<procedure>"]
    [(primV _ _) "#<primop>"]))


(: interp (ExprC Env -> Value))
(define (interp [e : ExprC] [env : Env]) : Value
  (match e
    [(numC n) (numV n)]
    [(stringC s) (stringV s)]
    [(idC s) (lookup s env)] ; primV
    [(lamC params body) (cloV params body env)]
    [(ifC cond then else)
     (define cond-val (interp cond env))
     (match cond-val
       [(boolV b) (if b 
                      (interp then env)
                      (interp else env))]
       [other (error 'interp
                     "[ VEBG ] Expected boolean in if condition, but got: ~e"
                     cond-val)])]
    [(appC fn args)
     (define fn_val (interp fn env))
     (define args_vals (map
                        (lambda ([arg : ExprC])
                          (interp arg env))
                        args))
     
     (match fn_val
       [(cloV params body cloV_env)
        (define new_env (append
                         (bind_params_to_args params args_vals)
                         cloV_env)) ;have new bindings on top of the closure env
        (interp body new_env)] 
       [other (error 'interp
                     "[ VEBG ] Did not receive a function into appC interp: ~e"
                     fn_val)])]))


(define (bind_params_to_args [params : (Listof Symbol)] [args : (Listof Value)]) : Env
  (match* (params args)
    [('() '()) '()]
    [((cons param param-rest) (cons arg arg-rest)) 
     (cons (binding param arg) (bind_params_to_args param-rest arg-rest))]
    [(_ _) (error 'bind_params_to_args
                  "[ VEBG ] Number of parameters does not match number of arguments. Params: ~e, Args: ~e"
                  params args)]))


;-------CHECKS-------

;-------lookup check-------
(check-equal?
 (lookup 'true top-env)
 (boolV #t))

(check-equal?
 (lookup 'false top-env)
 (boolV #f))

(check-equal?
 (lookup '+ top-env)
 (primV '+ 2))

(check-equal?
 (lookup '* top-env)
 (primV '* 2))

(check-equal?
 (lookup 'x
         (list (binding 'x (numV 7))
               (binding 'y (stringV "hi"))))
 (numV 7))

(check-equal?
 (lookup 'y
         (list (binding 'x (numV 7))
               (binding 'y (stringV "hi"))))
 (stringV "hi"))

(check-exn
 (regexp
  (regexp-quote
   "[ VEBG ] Symbol not found in environment: 'missing"))
 (lambda ()
   (lookup 'missing top-env)))

;-------serialize check-------
(check-equal?
 (serialize (numV 5))
 "5")

(check-equal?
 (serialize (numV 3.5))
 "3.5")

(check-equal?
 (serialize (boolV #t))
 "true")

(check-equal?
 (serialize (boolV #f))
 "false")

(check-equal?
 (serialize (stringV "hello"))
 "\"hello\"")

(check-equal?
 (serialize (cloV '(x) (idC 'x) '()))
 "#<procedure>")

(check-equal?
 (serialize (primV '+ 2))
 "#<primop>")
 

;-------interp check-------
(check-equal? (interp (numC 5) top-env) (numV 5))
(check-equal? (interp (stringC "hi") top-env) (stringV "hi"))
(check-equal? (interp (idC 'true) top-env) (boolV #t))
(check-equal? (interp (idC '+) top-env) (primV '+ 2))
(check-equal?
 (interp (lamC '(x) (idC 'x)) top-env)
 (cloV '(x) (idC 'x) top-env))

(check-equal?
 (interp (ifC (idC 'true) (numC 1) (numC 2)) top-env)
 (numV 1))

(check-equal?
 (interp (ifC (idC 'false) (numC 1) (numC 2)) top-env)
 (numV 2))

(check-exn
 (regexp
  (regexp-quote
   "[ VEBG ] Expected boolean in if condition, but got: (numV 3)"))
 (lambda ()
   (interp (ifC (numC 3) (numC 1) (numC 2)) top-env)))