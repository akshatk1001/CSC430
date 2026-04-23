#lang typed/racket
(require typed/rackunit)

#|
Full project implemented.
|#

(define-type ExprC (U numC binopC identifierC fncallC ifleq0C))
(struct numC ([n : Real]) #:transparent)
(struct binopC ([op : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
(struct identifierC ([s : Symbol]) #:transparent)
(struct fncallC ([funcname : Symbol] [args : (Listof ExprC)]) #:transparent)
(struct ifleq0C ([cond : ExprC] [then : ExprC] [else : ExprC]) #:transparent)

(struct fndefC ([funcname : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent)



#|
lookup accepts one argument, ex (Symbol), and returns the Racket operation
that the symbol represents (ie. '+ -> +).
|#
(: lookup (Symbol -> (Real Real -> Real)))

(define (lookup [ex : Symbol]) : (Real Real -> Real)
  (match ex
    ['+ +]
    ['* *]
    ['/ /]
    ['- -]
    [other (error 'lookup
                  "[ VEBG ] lookup expected a valid symbol (+, -, *, /), got ~e"
                  ex)]))

#|
valid-symbol accepts on argument, s (Symbol), and returns #t or #f depending
on if the symbol is a valid symbol for either BinopC or fndefC or ifleql0C
|#
(: binop-symbol (Symbol -> Boolean))

(define (binop-symbol [s : Symbol]) : Boolean
  (or (symbol=? s '+)
      (symbol=? s '*)
      (symbol=? s '-)
      (symbol=? s '/)))

#|
reserved-symbol takes one argument, s (Symbol), and returns if the symbol is
reserved to different ExprC's.
|#

(: reserved-symbol (Symbol -> Boolean))

(define (reserved-symbol [s : Symbol]) : Boolean
  (or (binop-symbol s)
      (symbol=? s '->)
      (symbol=? s 'named-fn)
      (symbol=? s 'ifleq0?)))

#|
interp accepts two arguments, arithexp (ArithC) and funs (Listof fndefC) and
evaluates the expression to return the answer using all our helper functions.
|#
(: interp (ExprC (Listof fndefC) -> Real))

(define (interp [arithexp : ExprC] [funcs : (Listof fndefC)]) : Real
  (match arithexp
    [(numC n) n]
    [(binopC op l r)
     (define left (interp l funcs))
     (define right (interp r funcs))
     (if (and (symbol=? op '/) (= right 0))
         (error 'interp
                "[ VEBG ] dividing by 0 error: ~e"
                arithexp)
         ((lookup op) left right))]
    [(identifierC s) (error 'interp
                            "[ VEBG ] received unknown identifier ~e"
                            s)]
    [(ifleq0C cond then else) (if (<= (interp cond funcs) 0)
                                  (interp then funcs)
                                  (interp else funcs))]
    [(fncallC name args)
     (define fd (get-fndefn name funcs))
     (define params (fndefC-args fd))
     (define body (fndefC-body fd))
     (define arg-values
       (map (lambda
                ([a : ExprC]) (numC (interp a funcs)))
            args)) ; eager eval
     (if (= (length arg-values) (length params))
         (interp (subst-full-body arg-values params body) funcs)
         (error 'interp
                "[ VEBG ] wrong number of arguments in function call ~e"
                arithexp))]))

#|
parse accepts one argument, exp (Sexp) and converts the expression into
an expression of type ArithC to be evaluated by the Arith interpreter. 
|#
(: parse (Sexp -> ExprC))

(define (parse [exp : Sexp]) : ExprC
  (match exp
    [(? real? r) (numC r)]
    [(? symbol? s) (if (reserved-symbol s)
                       (error 'parse
                              "[ VEBG ] reserved symbol used in parse as an identifier ~e"
                              s)
                       (identifierC s))]
    [(list 'ifleq0? cond then else) (ifleq0C
                                     (parse cond)
                                     (parse then)
                                     (parse else))]
    [(list 'ifleq0? args ...) (error 'parse
                                     "[ VEBG ] received more/less than 3 arguments for ifleq0? in ~e"
                                     exp)]
    [(list (? symbol? s) args ...)
     (define args-list (cast args (Listof Sexp)))
     (cond
       [(and (binop-symbol s) (= (length args-list) 2) ) ;check if its operation
        (binopC s
                (parse (list-ref args-list 0)) ;0th element
                (parse (list-ref args-list 1)))] ;1st element
       [(binop-symbol s)
        (error 'parse
               "[ VEBG ] parser requires two arguments to use the operator in ~e" exp)]
       [(reserved-symbol s)
        (error 'parse
               "[ VEBG ] tried to use reserved symbol as a function name in a call: ~e"
               exp)]
       [else
        (fncallC s (map parse args-list))])] ;otherwise its a func. call
    [other (error 'parse
                  "[ VEBG ] parser expected real number or expression like (<op> <expr> <expr>), got ~e"
                  exp)]))

#|
parse-fundef parses function definition expressions (Sexp) and converts them
into actual fndefC expressions
|#
(: parse-fundef (Sexp -> fndefC))

(define (parse-fundef [s : Sexp]) : fndefC
  (match s
    [(list 'named-fn
           (? symbol? name)
           (list (? symbol? params) ...)
           '->
           fnbody)
     (define params-list
       (cast params (Listof Symbol)))
     (cond
       [(check-duplicates params-list)
        (error 'parse-fundef
               "[ VEBG ] duplicate parameter names in function: ~e"
               s)]
       [(ormap reserved-symbol params-list)
        (error 'parse-fundef
               "[ VEBG ] reserved symbol used as parameter in function: ~e"
               s)]
       [(reserved-symbol name)
        (error 'parse-fundef
               "[ VEBG ] reserved symbol used as function name: ~e"
               s)]
       [else (fndefC name params-list (parse fnbody))])]
    [other (error 'parse-fundef "[ VEBG ] incorrect function definition ~e" s)]))

#|
get-fndefn gets the actual body of a provided function. It takes name (Symbol)
and fns (Listof fndefC) to get the provided function.
|#
(: get-fndefn (Symbol (Listof fndefC) -> fndefC))

(define (get-fndefn
         [name : Symbol]
         [fns : (Listof fndefC)]) : fndefC
  (match fns
    ['() (error 'get-fndefn "[ VEBG ] no function named ~e" name)]
    [(cons f r) (if (symbol=? name (fndefC-funcname f))
                    f
                    (get-fndefn name r))]))

#|
subst takes in 3 arguments, what (ExprC) for (Symbol) and in (ExprC), and
replaces the "for" symbol with "what" in the "in".
|#
(: subst (ExprC Symbol ExprC -> ExprC))

(define (subst
         [what : ExprC]
         [for : Symbol]
         [in : ExprC]) : ExprC
  (match in
    [(numC n) in]
    [(binopC op l r) (binopC op
                             (subst what for l)
                             (subst what for r))]
    [(identifierC x) (if (symbol=? x for)
                         what
                         in)]
    [(ifleq0C cond then else) (ifleq0C (subst what for cond)
                                       (subst what for then)
                                       (subst what for else))]
    [(fncallC fname args) (fncallC fname
                                   (map (lambda
                                            ([a : ExprC]) (subst what for a))
                                        args))]))

#|
subst-full-body takes in vals (Listof ExprC), params (Listof Symbol), and
body (ExprC), and substitutes in a parametes with its corresponding
value (using index)
|#
(: subst-full-body ((Listof ExprC) (Listof Symbol) ExprC -> ExprC))

(define (subst-full-body
         [vals : (Listof ExprC)]
         [params : (Listof Symbol)]
         [body : ExprC]) : ExprC
  (match* (vals params)
    [('() '()) body]
    [((cons val restvals) (cons param restparams))
     (subst-full-body restvals
                      restparams
                      (subst val param body))]
    [(_ _) (error 'subst-full-body
                  "[ VEBG ] substitution lists lengths are not the same in ~e"
                  body)]))

#|
parse-prog takes in a program (Sexp) that contains all of the function
definitions (including a main) and converts them to be actual function
fndefC expressions
|#
(: parse-prog (Sexp -> (Listof fndefC)))

(define (parse-prog [s : Sexp]) : (Listof fndefC)
  (match s
    [(list fundefs ...)
     (define fundefs-list (cast fundefs (Listof Sexp)))
     (define parsed-fundefs (map parse-fundef fundefs-list))
     (define parsed-names (map fndefC-funcname parsed-fundefs))
     (if (false? (check-duplicates parsed-names))
         parsed-fundefs
         (error 'parse-prog
                "[ VEBG ] received duplicate function name in ~e"
                s))]
    [other (error 'parse-prog
                  "[ VEBG ] program must contain list of function definitions, got ~e"
                  s)]))

#|
interp-fns takes in the list of function definitions (Listof fndefC), and
evaluates the main function
|#
(: interp-fns ((Listof fndefC) -> Real))

(define (interp-fns [funcs : (Listof fndefC)]) : Real
  (define main-fn (get-fndefn 'main funcs))
  (if (empty? (fndefC-args main-fn))
      (interp (fndefC-body main-fn) funcs)
      (error 'interp-fns
             "[ VEBG ] main function can not have arguments in it ~e"
             main-fn)))

#|
top-interp takes in one argument, fun-sexps (Sexp), and evaluates it using our
parser and interpreter to return the answer.
|#
(: top-interp (Sexp -> Real))

(define (top-interp [fun-sexps : Sexp]) : Real
  (interp-fns (parse-prog fun-sexps)))


;-------CHECKS-------

;-------lookup check-------
(check-equal? (lookup '+) +)
(check-equal? (lookup '*) *)
(check-equal? (lookup '/) /)
(check-exn (regexp
            (regexp-quote
             "lookup: [ VEBG ] lookup expected a valid symbol (+, -, *, /), got 'hi"))
           (lambda () (lookup 'hi)))



;-------interp check-------
(define test-funs
  (list
   (fndefC 'five '() (numC 5))
   (fndefC 'area '(w h)
           (binopC '* (identifierC 'w) (identifierC 'h)))))

(check-equal? (interp (numC 4) '()) 4)
(check-equal? (interp (binopC '+ (numC 5) (numC 8)) '()) 13)
(check-equal? (interp (binopC '- (numC 5) (numC 2)) '()) 3)
(check-equal? (interp (binopC '* (numC 5) (binopC '+ (numC 2) (numC 4))) '()) 30)
(check-equal? (interp (binopC '/ (binopC '* (numC 3) (numC 4)) (numC 6)) '()) 2)
(check-equal? (interp (fncallC 'five '()) test-funs) 5)
(check-equal? (interp (fncallC 'area (list (numC 3) (numC 4))) test-funs) 12)

(check-equal?
 (interp (ifleq0C (numC -1) (numC 10) (numC 20)) '())
 10)

(check-equal?
 (interp (ifleq0C (numC 0) (numC 10) (numC 20)) '())
 10)

(check-equal?
 (interp (ifleq0C (numC 5) (numC 10) (numC 20)) test-funs)
 20)

(check-equal?
 (interp (fncallC 'area (list
                         (binopC '+ (numC 1) (numC 2))
                         (binopC '+ (numC 3) (numC 4))))
         test-funs)
 21)


(check-exn
 (regexp
  (regexp-quote
   "[ VEBG ] dividing by 0 error: (binopC '/ (numC 4) (numC 0))"))
 (lambda ()
   (interp (binopC '/ (numC 4) (numC 0)) '())))

(check-exn
 (regexp
  (regexp-quote
   "interp: [ VEBG ] received unknown identifier 'x"))
 (lambda ()
   (interp (identifierC 'x) '())))


(check-exn
 (regexp
  (regexp-quote "interp: [ VEBG ] wrong number of arguments in function call (fncallC 'five (list (numC 4)))"))
 (lambda () (interp
             (fncallC 'five
                      (list (numC 4)))
             test-funs)))

(check-exn
 (regexp
  (regexp-quote
   "interp: [ VEBG ] wrong number of arguments in function call (fncallC 'area (list (numC 3)))"))
 (lambda ()
   (interp (fncallC 'area
                    (list (numC 3)))
           test-funs)))

(check-exn
 (regexp
  (regexp-quote
   "get-fndefn: [ VEBG ] no function named 'five"))
 (lambda ()
   (interp (fncallC 'five '()) '())))



;-------parser check-------
(check-equal? (parse 5) (numC 5))
(check-equal? (parse '(+ 4 1)) (binopC '+ (numC 4) (numC 1)))
(check-equal? (parse '(* 5 2)) (binopC '* (numC 5) (numC 2)))
(check-equal? (parse '(/ 8 (+ 3 1))) (binopC '/ (numC 8) (binopC '+ (numC 3) (numC 1))))
(check-equal? (parse 'x) (identifierC 'x))
(check-equal? (parse '(- 5 2)) (binopC '- (numC 5) (numC 2)))
(check-equal? (parse '(ifleq0? -1 10 20)) (ifleq0C (numC -1) (numC 10) (numC 20)))
(check-equal? (parse '(area 3 4))
              (fncallC 'area (list (numC 3) (numC 4))))
(check-equal? (parse '(f (+ 1 2) (/ 8 4)))
              (fncallC 'f
                       (list (binopC '+ (numC 1) (numC 2))
                             (binopC '/ (numC 8) (numC 4)))))

(check-exn (regexp
            (regexp-quote
             "parse: [ VEBG ] tried to use reserved symbol as a function name in a call: '(named-fn 1 2)"))
           (lambda ()
             (parse '(named-fn 1 2))))

(check-exn (regexp
            (regexp-quote
             "parse: [ VEBG ] parser expected real number or expression like (<op> <expr> <expr>), got '(4 2 bye)"))
           (lambda () (parse '(4 2 bye))))

(check-exn
 (regexp (regexp-quote
          "parse: [ VEBG ] reserved symbol used in parse as an identifier '+"))
 (lambda () (parse '+)))

(check-exn
 (regexp
  (regexp-quote
   "parse: [ VEBG ] received more/less than 3 arguments for ifleq0? in '(ifleq0? 1 2)"))
 (lambda ()
   (parse '(ifleq0? 1 2))))

(check-exn
 (regexp
  (regexp-quote
   "parse: [ VEBG ] parser requires two arguments to use the operator in '(+ 1)"))
 (lambda ()
   (parse '(+ 1))))

(check-exn
 (regexp
  (regexp-quote
   "parse: [ VEBG ] parser requires two arguments to use the operator in '(* 1 2 3)"))
 (lambda ()
   (parse '(* 1 2 3))))

(check-exn
 (regexp
  (regexp-quote
   "parse: [ VEBG ] parser requires two arguments to use the operator in '(+)"))
 (lambda ()
   (parse '(+))))



;-------parse-fundef check-------
(check-equal?
 (parse-fundef '(named-fn area (w h) -> (* w h)))
 (fndefC 'area '(w h)
         (binopC '* (identifierC 'w) (identifierC 'h))))
(check-equal?
 (parse-fundef '(named-fn five () -> 5))
 (fndefC 'five '() (numC 5)))

(check-exn
 (regexp
  (regexp-quote
   "parse-fundef: [ VEBG ] duplicate parameter names in function: '(named-fn bad (x x) -> x)"))
 (lambda ()
   (parse-fundef '(named-fn bad (x x) -> x))))

(check-exn
 (regexp
  (regexp-quote
   "parse-fundef: [ VEBG ] incorrect function definition '(named-fn f x -> x)"))
 (lambda ()
   (parse-fundef '(named-fn f x -> x))))

(check-exn
 (regexp
  (regexp-quote "parse-fundef: [ VEBG ] reserved symbol used as function name: '(named-fn + (n1 n2) -> (+ n1 n2))"))
 (lambda ()
   (parse-fundef '(named-fn + (n1 n2) -> (+ n1 n2)))))

(check-exn
 (regexp
  (regexp-quote "[ VEBG ] reserved symbol used as parameter in function: '(named-fn add (+ num2) -> (+ num1 num2))"))
 (lambda ()
   (parse-fundef '(named-fn add (+ num2) -> (+ num1 num2)))))



;-------get-fndef check-------
(check-equal? (get-fndefn 'five
                          (list (fndefC 'five '() (numC 5))))
              (fndefC 'five '() (numC 5)))
(check-equal?
 (get-fndefn 'area
             (list (fndefC 'five '() (numC 5))
                   (fndefC 'area '(w h)
                           (binopC '* (identifierC 'w) (identifierC 'h)))))
 (fndefC 'area '(w h)
         (binopC '* (identifierC 'w) (identifierC 'h))))

(check-exn
 (regexp
  (regexp-quote
   "get-fndefn: [ VEBG ] no function named 'missing"))
 (lambda ()
   (get-fndefn 'missing '())))



;-------subst check-------
(check-equal? (subst (numC 3) 'x (identifierC 'x)) (numC 3))
(check-equal? (subst (numC 3) 'x (identifierC 'y)) (identifierC 'y))
(check-equal? (subst (numC 3) 'x (binopC '+ (identifierC 'x) (numC 2))) (binopC '+ (numC 3) (numC 2)))
(check-equal?
 (subst (numC 3) 'x (ifleq0C (identifierC 'x) (numC 1) (numC 2)))
 (ifleq0C (numC 3) (numC 1) (numC 2)))

(check-equal? (subst (numC 3)
                     'x
                     (fncallC 'f (list (identifierC 'x) (identifierC 'y))))
              (fncallC 'f (list (numC 3) (identifierC 'y))))



;-------subst-full-body check-------
(check-equal?
 (subst-full-body (list (numC 3) (numC 4))
                  '(w h)
                  (binopC '* (identifierC 'w) (identifierC 'h)))
 (binopC '* (numC 3) (numC 4)))

(check-exn
 (regexp
  (regexp-quote
   "subst-full-body: [ VEBG ] substitution lists lengths are not the same in (numC 3)"))
 (lambda ()
   (subst-full-body (list (numC 3))
                    '(x y)
                    (identifierC 'x))))



;-------parse-prog check-------
(check-equal?
 (parse-prog '((named-fn five () -> 5)
               (named-fn main () -> (five))))
 (list
  (fndefC 'five '() (numC 5))
  (fndefC 'main '() (fncallC 'five '()))))

(check-exn
 (regexp
  (regexp-quote
   "parse-prog: [ VEBG ] received duplicate function name in '((named-fn five () -> 5) (named-fn five () -> 4))"))
 (lambda ()
   (parse-prog '((named-fn five () -> 5)
                 (named-fn five () -> 4)))))

(check-exn
 (regexp
  (regexp-quote
   "parse-prog: [ VEBG ] program must contain list of function definitions, got 5"))
 (lambda ()
   (parse-prog 5)))



;-------interp-fns check-------
(check-equal?
 (interp-fns
  (list
   (fndefC 'five '() (numC 5))
   (fndefC 'main '() (fncallC 'five '()))))
 5)

(check-exn
 (regexp
  (regexp-quote
   "interp-fns: [ VEBG ] main function can not have arguments in it (fndefC 'main '(x) (numC 5))"))
 (lambda ()
   (interp-fns
    (list
     (fndefC 'main '(x) (numC 5))))))



;-------top-interp check-------
(check-equal?
 (top-interp
  '((named-fn f (x y) -> (+ x y))
    (named-fn main () -> (f 1 2))))
 3)

(check-equal?
 (top-interp
  '((named-fn f () -> 5)
    (named-fn main () -> (+ (f) (f)))))
 10)

(check-equal?
 (top-interp
  '((named-fn choose (x) -> (ifleq0? x 1 2))
    (named-fn main () -> (choose -4))))
 1)

(check-equal?
 (top-interp
  '((named-fn choose (x) -> (ifleq0? x 1 2))
    (named-fn main () -> (choose 7))))
 2)


(check-exn
 (regexp
  (regexp-quote
   "get-fndefn: [ VEBG ] no function named 'main"))
 (lambda ()
   (top-interp '((named-fn f () -> 5)))))