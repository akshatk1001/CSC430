#lang typed/racket
(require typed/rackunit)

#|
Full Project Implemented. Incredible Victory!
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
(struct primV ([op : Symbol] [arity : Real]) #:transparent) 

(define-type Env (Listof binding))
(struct binding ([sym : Symbol] [val : Value]) #:transparent) 

(define top-env : Env
  (list (binding 'true (boolV #t))
        (binding 'false (boolV #f))
        (binding '+ (primV '+ 2))
        (binding '- (primV '- 2))
        (binding '* (primV '* 2))
        (binding '/ (primV '/ 2))
        (binding '<= (primV '<= 2))
        (binding 'equal? (primV 'equal? 2))
        (binding 'substring (primV 'substring 3))
        (binding 'strlen (primV 'strlen 1))
        (binding 'error (primV 'error 1))))

(: reserved-symbol? (Symbol -> Boolean))
#|
reserved-symbol? takes a Symbol as the arg and returns if that sybol is reserved or not
|#
(define (reserved-symbol? [s : Symbol]) : Boolean
  (or (symbol=? s 'if)
      (symbol=? s '=)
      (symbol=? s 'given)
      (symbol=? s 'fn)
      (symbol=? s '->)
      (symbol=? s 'do)))


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
                [(list (numV n1) (numV n2)) (if (equal? n2 0)
                                                (error 'calc_primV
                                                       "[ VEBG ] Tried to divide by 0: ~e"
                                                       args)
                                                (/ n1 n2))]
                [other (error 'calc_primV
                              "[ VEBG ] Expected two numV's for /, but got: ~e"
                              args)]))]
    ['<= (boolV (match args
                  [(list (numV n1) (numV n2)) (<= n1 n2)]
                  [other (error 'calc_primV
                                "[ VEBG ] Expected two numV's for <=, but got: ~e"
                                args)]))]
    ['equal? (boolV (match args
                      [(list (or (cloV _ _ _) (primV _ _)) (or (cloV _ _ _) (primV _ _))) #f]
                      [(list _ (or (cloV _ _ _) (primV _ _))) #f]
                      [(list (or (cloV _ _ _) (primV _ _)) _) #f] 
                      [(list n1 n2) (equal? n1 n2)]
                      [other (error 'calc_primV
                                    "[ VEBG ] Expected two arguments for equal?, but got: ~e"
                                    args)]))]
    ['substring (stringV (match args
                           [(list (stringV s) (numV start) (numV end))
                            (cond
                              [(not (exact-nonnegative-integer? start)) ; substring docs say exact nonnegative integer
                               (error 'calc_primV
                                      "[ VEBG ] Expected natural number for start index in substring, but got: ~e"
                                      start)]
                              [(not (exact-nonnegative-integer? end))
                               (error 'calc_primV
                                      "[ VEBG ] Expected natural number for end index in substring, but got: ~e"
                                      end)]
                              [(> start end)
                               (error 'calc_primV
                                      "[ VEBG ] End before start index in substring, got: ~e, end: ~e"
                                      start end)]
                              [(> end (string-length s))
                               (error 'calc_primV
                                      "[ VEBG ] Substring indexes out of range for string: ~e"
                                      s)]
                              [else
                               (substring s start end)])]
                           [other (error 'calc_primV
                                         "[ VEBG ] Expected string and two numbers for substring, but got: ~e"
                                         args)]))]
    ['strlen (numV (match args
                     [(cons (stringV s) '()) (string-length s)]
                     [other (error 'calc_primV
                                   "[ VEBG ] Expected a string for strlen, but got: ~e"
                                   args)]))]
    ['error (match args
              [(list v)
               (error 'user-error
                      "[ VEBG ] user-error: ~a"
                      (serialize v))]
              [other
               (error 'calc_primV
                      "[ VEBG ] Expected one argument for error, but got: ~e"
                      args)])]

    [other (error 'calc_primV
                  "[ VEBG ] Unknown symbol in calc_primV: ~v"
                  op)]))


(: parse (Sexp -> ExprC))
#|
Parse takes in an S-expression and converts it into an ExprC to be interpreted.
|#
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real?) (numC s)]
    [(? string?) (stringC s)]
    [(? symbol?) 
     (if (reserved-symbol? s)
         (error 'parse "[ VEBG ] Used reserved symbol: ~e" 
                s)
         (idC s))]
    [(list 'if cond then else) (ifC (parse cond) (parse then) (parse else))]
    [(list 'given (list (list (? symbol? params) '= args) ...) 'do body)
     (define params-list (cast params (Listof Symbol)))
     (cond
       [(check-duplicates params-list)
        (error 'parse "[ VEBG ] duplicate given names: ~e" params-list)]
       [(ormap reserved-symbol? params-list)
        (error 'parse "[ VEBG ] reserved symbol used as given name: ~e" params-list)]
       [else
        (appC (lamC params-list (parse body))
              (map parse (cast args (Listof Sexp))))])]
    [(list 'fn (list (? symbol? params) ...) '-> body)
     (define params-list (cast params (Listof Symbol)))
     (cond 
       [(check-duplicates params-list) (error 'parse 
                                              "[ VEBG ] Duplicate parameter names in definition: ~e" 
                                              params-list)]
       [(ormap reserved-symbol? params-list) (error 'parse 
                                                    "[ VEBG ] reserved symbol used as parameter: ~e" 
                                                    params-list)]
       [else (lamC params-list (parse body))])]
    [(cons fn-expr arg-exprs)
     (appC (parse fn-expr)
           (map parse (cast arg-exprs (Listof Sexp))))]
    [other (error 'parse
                  "[ VEBG ] Cannot parse Sexp into ExprC: ~e"
                  other)]))


(: top-interp (Sexp -> String))
#|
top-interp takes in an S-expression, parses it into an ExprC, evaluates it to a Value, and then
serializes that value into a string
|#
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))


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

;-------calc_primV checks-------
(check-equal? (calc_primV '+ (list (numV 1) (numV 2))) (numV 3))
(check-equal? (calc_primV '- (list (numV 5) (numV 2))) (numV 3))
(check-equal? (calc_primV '* (list (numV 3) (numV 4))) (numV 12))
(check-equal? (calc_primV '/ (list (numV 8) (numV 2))) (numV 4))

(check-exn
 (regexp
  (regexp-quote
   "calc_primV: [ VEBG ] Tried to divide by 0: (list (numV 1) (numV 0))"))
 (lambda ()
   (calc_primV '/ (list (numV 1) (numV 0)))))

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


;-------top-interp checks-------
(check-equal? (top-interp 5) "5")
(check-equal? (top-interp "hi") "\"hi\"")
(check-equal? (top-interp 'true) "true")
(check-equal? (top-interp '(+ 1 2)) "3")
(check-equal?
 (top-interp '((fn (x) -> x) 9))
 "9")

(check-equal?
 (top-interp '(given ([x = 10] [y = 20]) do (+ x y)))
 "30")

(check-equal?
 (top-interp '((fn (z y) -> (+ z y)) (+ 9 14) 98))
 "121")

(check-equal? (top-interp '(<= 1 2)) "true")
(check-equal? (top-interp '(<= 5 2)) "false")
(check-equal? (top-interp '(equal? 3 3)) "true")
(check-equal? (top-interp '(equal? 3 4)) "false")
(check-equal? (top-interp '(strlen "hello")) "5")
(check-equal? (top-interp '(substring "hello" 1 4)) "\"ell\"")

(check-exn
 (regexp
  (regexp-quote
   "user-error: [ VEBG ] user-error: \"boom\""))
 (lambda ()
   (top-interp '(error "boom"))))

(check-exn
 (regexp
  (regexp-quote
   "calc_primV: [ VEBG ] Expected one argument for error, but got: '()"))
 (lambda ()
   (calc_primV 'error '())))


;-------new calc_primV checks-------
(check-equal? (calc_primV '<= (list (numV 1) (numV 2))) (boolV #t))
(check-equal? (calc_primV '<= (list (numV 5) (numV 2))) (boolV #f))
(check-exn
 (regexp
  (regexp-quote
   "calc_primV: [ VEBG ] Expected two numV's for <=, but got: (list (numV 1) (stringV \"hi\"))"))
 (lambda ()
   (calc_primV '<= (list (numV 1) (stringV "hi")))))

(check-equal? (calc_primV 'equal? (list (numV 3) (numV 3))) (boolV #t))
(check-equal? (calc_primV 'equal? (list (numV 3) (numV 4))) (boolV #f))
(check-equal? (calc_primV 'equal? (list (stringV "hi") (stringV "hi"))) (boolV #t))
(check-equal? (calc_primV 'equal? (list (boolV #t) (boolV #t))) (boolV #t))
(check-equal? (calc_primV 'equal? (list (boolV #t) (boolV #f))) (boolV #f))
(check-equal? (calc_primV 'equal? (list (numV 1) (stringV "1"))) (boolV #f))
(check-equal? (calc_primV 'equal? (list (primV '+ 2) (primV '+ 2))) (boolV #f))
(check-equal? (calc_primV 'equal? (list (cloV '(x) (idC 'x) '()) (numV 1))) (boolV #f))
(check-equal? (calc_primV 'equal? (list (numV 1) (cloV '(x) (idC 'x) '()))) (boolV #f))

(check-exn
 (regexp
  (regexp-quote
   "calc_primV: [ VEBG ] Expected two arguments for equal?, but got: (list (numV 1))"))
 (lambda ()
   (calc_primV 'equal? (list (numV 1)))))

(check-equal?
 (calc_primV 'substring (list (stringV "hello") (numV 1) (numV 4)))
 (stringV "ell"))

(check-equal?
 (calc_primV 'substring (list (stringV "hello") (numV 2) (numV 2)))
 (stringV ""))

(check-exn
 (regexp
  (regexp-quote
   "calc_primV: [ VEBG ] Expected string and two numbers for substring, but got: (list (numV 1) (numV 0) (numV 1))"))
 (lambda ()
   (calc_primV 'substring (list (numV 1) (numV 0) (numV 1)))))

(check-equal?
 (calc_primV 'strlen (list (stringV "hello")))
 (numV 5))

(check-exn
 (regexp
  (regexp-quote
   "calc_primV: [ VEBG ] Expected a string for strlen, but got: (list (numV 5))"))
 (lambda ()
   (calc_primV 'strlen (list (numV 5)))))


(check-exn
 (regexp
  (regexp-quote
   "calc_primV: [ VEBG ] Expected natural number for start index in substring, but got: -1"))
 (lambda ()
   (calc_primV 'substring (list (stringV "hello") (numV -1) (numV 3)))))

(check-exn
 (regexp
  (regexp-quote
   "calc_primV: [ VEBG ] Expected natural number for end index in substring, but got: 2.5"))
 (lambda ()
   (calc_primV 'substring (list (stringV "hello") (numV 1) (numV 2.5)))))

(check-exn
 (regexp
  (regexp-quote
   "calc_primV: [ VEBG ] End before start index in substring, got: 4, end: 2"))
 (lambda ()
   (calc_primV 'substring (list (stringV "hello") (numV 4) (numV 2)))))

(check-exn
 (regexp
  (regexp-quote
   "calc_primV: [ VEBG ] Substring indexes out of range for string: \"hi\""))
 (lambda ()
   (calc_primV 'substring (list (stringV "hi") (numV 0) (numV 5)))))


;-------new parser error checks-------
(check-exn
 (regexp (regexp-quote "parse: [ VEBG ] Used reserved symbol: 'if"))
 (lambda () (parse 'if)))

(check-exn
 (regexp (regexp-quote "parse: [ VEBG ] Duplicate parameter names in definition: '(x x)"))
 (lambda () (parse '(fn (x x) -> x))))

(check-exn
 (regexp (regexp-quote "parse: [ VEBG ] reserved symbol used as parameter: '(if)"))
 (lambda () (parse '(fn (if) -> 1))))

(check-exn
 (regexp (regexp-quote "parse: [ VEBG ] duplicate given names: '(x x)"))
 (lambda () (parse '(given ([x = 1] [x = 2]) do x))))

(check-exn
 (regexp (regexp-quote "parse: [ VEBG ] reserved symbol used as given name: '(if)"))
 (lambda () (parse '(given ([if = 1]) do if))))
