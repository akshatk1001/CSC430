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
#|
interp takes an ExprC and its Env, and evaluates it to return a Value (the answer)
|#
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
       [(primV op arity)
        (if (equal? (length args_vals) arity)
            (calc_primV op args_vals)
            ;; Throw error for incorrect number of PrimV arguments
            (error 'interp
                   "[ VEBG ] ~v expected ~e arguments, got ~e"
                   op arity (length args_vals)))]
       [other (error 'interp
                     "[ VEBG ] Did not receive a function into appC interp: ~e"
                     fn_val)])]))


(: bind_params_to_args ((Listof Symbol) (Listof Value) -> Env))
#|
bind_params_to_args takes in a list of params symbols and list of values and
creates a new Env with the bindings. 
|#
(define (bind_params_to_args [params : (Listof Symbol)] [args : (Listof Value)]) : Env
  (match* (params args)
    [('() '()) '()]
    [((cons param param-rest) (cons arg arg-rest)) 
     (cons (binding param arg) (bind_params_to_args param-rest arg-rest))]
    [(_ _) (error 'bind_params_to_args
                  "[ VEBG ] Number of parameters does not match number of arguments. Params: ~e, Args: ~e"
                  params args)]))


(: calc_primV (Symbol (Listof Value) -> Value))
#|
calc_primV takes in a Symbol representing the operation and a list of Values representing the args, 
and then performs the operationa and returns a Value. Helper used in interp. 
|#
(define (calc_primV [op : Symbol] [args : (Listof Value)]) : Value
  (match op
    ['+ (numV (match args
                [(list (numV n1) (numV n2)) (+ n1 n2)]
                [other (error 'calc_primV
                              "[ VEBG ] Expected two numV's for +, but got: ~e"
                              args)]))]
    ['- (numV (match args
                [(list (numV n1) (numV n2)) (- n1 n2)]
                [other (error 'calc_primV
                              "[ VEBG ] Expected two numV's for -, but got: ~e"
                              args)]))]
    ['* (numV (match args
                [(list (numV n1) (numV n2)) (* n1 n2)]
                [other (error 'calc_primV
                              "[ VEBG ] Expected two numV's for *, but got: ~e"
                              args)]))]
    ['/ (numV (match args
                [(list (numV n1) (numV n2)) (/ n1 n2)]
                [other (error 'calc_primV
                              "[ VEBG ] Expected two numV's for /, but got: ~e"
                              args)]))] 
    [other (error 'calc_primV
                  "[ VEBG ] Unknown symbol in calc_primV: ~v"
                  op)]))

(: parse (Sexp -> ExprC))
#|
TODO: ADD COMMENT HERE
|#
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real?) (numC s)]
    [(? string?) (stringC s)]
    [(? symbol?) (idC s)]
    [(list 'if cond then else) (ifC (parse cond) (parse then) (parse else))]
    [(list 'given (list (list (? symbol? params) '= args) ...) 'do body)
     (appC (lamC (cast params (Listof Symbol)) (parse body))
           (map parse (cast args (Listof Sexp))))]
    [(list 'fn (list (? symbol? params) ...) '-> body)
     (lamC (cast params (Listof Symbol)) (parse body))]
    [(cons fn-expr arg-exprs)
     (appC (parse fn-expr)
           (map parse (cast arg-exprs (Listof Sexp))))]
    [other (error 'parse
                  "[ VEBG ] Cannot parse Sexp into ExprC: ~e"
                  other)]))
                  
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
   "interp: [ VEBG ] Expected boolean in if condition, but got: (numV 3)"))
 (lambda ()
   (interp (ifC (numC 3) (numC 1) (numC 2)) top-env)))

(check-equal?
 (interp (appC (lamC '(x) (idC 'x)) 
               (list (numC 9))) 
         top-env)
 (numV 9))

(check-equal?
 (interp (appC (lamC '(x y) (idC 'y))
               (list (numC 1) (stringC "hello")))
         top-env)
 (stringV "hello"))
(check-equal?

 (interp (appC (ifC (idC 'true)
                    (lamC '(x) (idC 'x))
                    (lamC '(x) (numC 0)))
               (list (numC 42)))
         top-env)
 (numV 42))

(check-exn
 (regexp
  (regexp-quote
   "interp: [ VEBG ] Did not receive a function into appC interp: (numV 5)"))
 (lambda ()
   (interp (appC (numC 5) (list (numC 1))) top-env)))


(check-equal?
 (interp (appC (idC '+) (list (numC 1) (numC 2))) top-env)
 (numV 3))

(check-equal?
 (interp (appC (idC '-) (list (numC 5) (numC 2))) top-env)
 (numV 3))

(check-equal?
 (interp (appC (idC '*) (list (numC 3) (numC 4))) top-env)
 (numV 12))

(check-equal?
 (interp (appC (idC '/) (list (numC 8) (numC 2))) top-env)
 (numV 4))

(check-exn
 (regexp
  (regexp-quote
   "interp: [ VEBG ] '+ expected 2 arguments, got 1"))
 (lambda ()
   (interp (appC (idC '+) (list (numC 1))) top-env)))

;-------bind_params_to_args check-------   
(check-equal?
 (bind_params_to_args '(x y) (list (numV 1) (stringV "hi")))
 (list (binding 'x (numV 1))
       (binding 'y (stringV "hi"))))

(check-exn
 (regexp
  (regexp-quote
   "bind_params_to_args: [ VEBG ] Number of parameters does not match number of arguments. Params: '(y), Args: '()"))
 (lambda ()
   (bind_params_to_args '(x y) (list (numV 1)))))

(check-exn
 (regexp
  (regexp-quote
   "bind_params_to_args: [ VEBG ] Number of parameters does not match number of arguments. Params: '(), Args: (list (numV 2))"))
 (lambda ()
   (bind_params_to_args '(x) (list (numV 1) (numV 2)))))


;-------calc_primV checks-------
(check-equal? (calc_primV '+ (list (numV 1) (numV 2))) (numV 3))
(check-equal? (calc_primV '- (list (numV 5) (numV 2))) (numV 3))
(check-equal? (calc_primV '* (list (numV 3) (numV 4))) (numV 12))
(check-equal? (calc_primV '/ (list (numV 8) (numV 2))) (numV 4))

(check-exn
 (regexp
  (regexp-quote
   "calc_primV: [ VEBG ] Expected two numV's for +, but got: (list (numV 1) (stringV \"hi\"))"))
 (lambda ()
   (calc_primV '+ (list (numV 1) (stringV "hi")))))

(check-exn
 (regexp
  (regexp-quote
   "calc_primV: [ VEBG ] Expected two numV's for -, but got: (list (numV 1) (stringV \"hi\"))"))
 (lambda ()
   (calc_primV '- (list (numV 1) (stringV "hi")))))

(check-exn
 (regexp
  (regexp-quote
   "calc_primV: [ VEBG ] Expected two numV's for *, but got: (list (numV 1) (stringV \"hi\"))"))
 (lambda ()
   (calc_primV '* (list (numV 1) (stringV "hi")))))

(check-exn
 (regexp
  (regexp-quote
   "calc_primV: [ VEBG ] Expected two numV's for /, but got: (list (numV 1) (stringV \"hi\"))"))
 (lambda ()
   (calc_primV '/ (list (numV 1) (stringV "hi")))))

(check-exn
 (regexp
  (regexp-quote
   "calc_primV: [ VEBG ] Unknown symbol in calc_primV: 'bad-op"))
 (lambda ()
   (calc_primV 'bad-op (list (numV 1) (numV 2)))))



;-------parse checks-------
(check-equal? (parse 5) (numC 5))
(check-equal? (parse "hi") (stringC "hi"))
(check-equal? (parse 'x) (idC 'x))
(check-equal?
 (parse '(if true 1 2))
 (ifC (idC 'true) (numC 1) (numC 2)))

(check-equal?
 (parse '(fn (x y) -> (+ x y)))
 (lamC '(x y)
       (appC (idC '+) (list (idC 'x) (idC 'y)))))

      
(check-equal?
 (parse '((fn (z y) -> (+ z y)) (+ 9 14) 98))
 (appC
  (lamC '(z y)
        (appC (idC '+) (list (idC 'z) (idC 'y))))
  (list
   (appC (idC '+) (list (numC 9) (numC 14)))
   (numC 98))))

(check-equal?
 (parse '(given ([x = 10] [y = 20]) do (+ x y)))
 (appC
  (lamC '(x y)
        (appC (idC '+) (list (idC 'x) (idC 'y))))
  (list (numC 10) (numC 20))))

(check-exn
 (regexp
  (regexp-quote
   "parse: [ VEBG ] Cannot parse Sexp into ExprC: '()"))
 (lambda ()
   (parse '())))