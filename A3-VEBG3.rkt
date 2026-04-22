#lang typed/racket
(require typed/rackunit)

#|
#TODO REMEMBER TO MAKE PROGRESS TO GOAL COMMENT HERE
|#

(define-type ExprC (U numC binopC identifierC fncallC))
(struct numC ([n : Real]) #:transparent)
(struct binopC ([op : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
(struct identifierC ([s : Symbol]) #:transparent)
(struct fncallC ([funcname : Symbol] [args : (Listof ExprC)]) #:transparent)
(struct fndefC ([funcname : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent)


#|
lookup accepts one argument, ex (Symbol), and returns the Racket operation
that the symbol represents (ie. '+ -> +).
|#
(: lookup (Symbol -> (Real Real -> Real)))

(define (lookup (ex : Symbol)) : (Real Real -> Real)
  (match ex
    ['+ +]
    ['* *]
    ['/ /]
    [other (error 'lookup
                  "[ VEBG ] lookup expected a valid symbol (+, *, /), got ~e"
                  ex)]))

#|
valid-symbol accepts on argument, s (Symbol), and returns #t or #f depending
on if the symbol is a valid binopC symbol
|#
(: valid-symbol (Symbol -> Boolean))

(define (valid-symbol (s : Symbol)) : Boolean
  (or (symbol=? s '+)
      (symbol=? s '*)
      (symbol=? s '/)))


#|
interp accepts one argument, arithexp (ArithC) and evaluates the expression
to return the answer. 
|#
(: interp (ExprC (Listof fndefC) -> Real))

(define (interp (arithexp : ExprC) (funcs : (Listof fndefC))) : Real
  (match arithexp
    [(numC n) n]
    [(binopC op l r) ((lookup op) (interp l funcs) (interp r funcs))]
    [(identifierC s) (error 'interp "[ VEBG ] received unknown identifier ~e" s)]
    [(fncallC name args)
     (define fd (get-fndefn name funcs))
     (define params (fndefC-args fd))
     (define body (fndefC-body fd))
     (define arg-values
       (map (lambda ([a : ExprC]) (numC (interp a funcs))) args)) ; eager eval
     (if (= (length arg-values) (length params))
         (interp (subst-full-body arg-values params body ) funcs)
         (error 'interp
                "[ VEBG ] wrong number of arguments in function call ~e"
                arithexp))]))


#|
parser accepts one argument, exp (Sexp) and converts the expression into
an expression of type ArithC to be evaluated by the Arith interpreter. 
|#
(: parser (Sexp -> ExprC))

(define (parser (exp : Sexp)) : ExprC
  (match exp
    [(? real? r) (numC r)]
    [(? symbol? s) (identifierC s)]
    [(list (? symbol? s) args ...)
     (define args-list (cast args (Listof Sexp)))
     (cond
       [(and (valid-symbol s) (= (length args-list) 2) )
        (binopC s
                (parser (list-ref args-list 0))
                (parser (list-ref args-list 1)))]
       [(valid-symbol s)
        (error 'parser
               "[ VEBG ] parser requires two arguments to use the operator in ~e" exp)]
       [else
        (fncallC s (map parser args-list))])]
    [other (error 'parser
                  "[ VEBG ] parser expected real number or expression like (<op> <expr> <expr>), got ~e"
                  exp)]))


#|
parse-fundef parses expressions (Sexp) and converts them into
actual fndefC expressions
|#
(: parse-fundef (Sexp -> fndefC))

(define (parse-fundef (s : Sexp)) : fndefC
  (match s
    [(list 'named-fn
           (? symbol? name)
           (list (? symbol? params) ...)
           '->
           fnbody)
     (define params-list
       (cast params (Listof Symbol)))
     (if (false? (check-duplicates params-list))
         (fndefC name params-list (parser fnbody))
         (error 'parse-fundef
                "[ VEBG ] duplicate parameter names in function: ~e"
                s))]
    [other (error 'parse-fundef "[ VEBG ] incorrect function definition ~e" s)]))


#|
get-fndefn gets the actual body of a provided function. It takes name (Symbol)
and fns (Listof fndefC) to get the provided function.
|#
(: get-fndefn (Symbol (Listof fndefC) -> fndefC))

(define (get-fndefn
         (name : Symbol)
         (fns : (Listof fndefC))) : fndefC
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

(define (subst (what : ExprC) (for : Symbol) (in : ExprC)) : ExprC
  (match in
    [(numC n) in]
    [(binopC op l r) (binopC op
                             (subst what for l)
                             (subst what for r))]
    [(identifierC x) (if (symbol=? x for)
                         what
                         in)]
    [(fncallC fname args) (fncallC fname
                                   (map (lambda ([a : ExprC])
                                          (subst what for a))
                                        args))]))


#|
subst-full-body takes in vals (Listof ExprC) params (Listof Symbol) and
body (ExprC), and substitutes in a parametes with its corresponding
value (using index)
|#
(: subst-full-body ((Listof ExprC) (Listof Symbol) ExprC -> ExprC))

(define (subst-full-body
         (vals : (Listof ExprC))
         (params : (Listof Symbol))
         (body : ExprC)) : ExprC
  (match* (vals params)
    [('() '()) body]
    [((cons val restvals) (cons param restparams))
     (subst-full-body restvals
                      restparams
                      (subst val param body))]
    [(_ _) (error 'subst-full-body
                  "[ VEBG ] substitution lists lengths are not the same in ~e"
                  body)]))

;-------CHECKS-------

;-------lookup check-------
(check-equal? (lookup '+) +)
(check-equal? (lookup '*) *)
(check-equal? (lookup '/) /)
(check-exn (regexp
            (regexp-quote
             "lookup: [ VEBG ] lookup expected a valid symbol (+, *, /), got '-"))
           (lambda () (lookup '-)))



;-------interp check-------
(define test-funs
  (list
   (fndefC 'five '() (numC 5))
   (fndefC 'area '(w h)
           (binopC '* (identifierC 'w) (identifierC 'h)))))

(check-equal? (interp (numC 4) '()) 4)
(check-equal? (interp (binopC '+ (numC 5) (numC 8)) '()) 13)
(check-equal? (interp (binopC '* (numC 5) (binopC '+ (numC 2) (numC 4))) '()) 30)
(check-equal? (interp (binopC '/ (binopC '* (numC 3) (numC 4)) (numC 6)) '()) 2)

(check-exn
 (regexp
  (regexp-quote
   "interp: [ VEBG ] received unknown identifier 'x"))
 (lambda ()
   (interp (identifierC 'x) '())))

(check-equal? (interp (fncallC 'five '()) test-funs) 5)
(check-equal? (interp (fncallC 'area (list (numC 3) (numC 4))) test-funs) 12)

(check-equal?
 (interp (fncallC 'area (list
                         (binopC '+ (numC 1) (numC 2))
                         (binopC '+ (numC 3) (numC 4))))
         test-funs)
 21)

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
(check-equal? (parser 5) (numC 5))
(check-equal? (parser '(+ 4 1)) (binopC '+ (numC 4) (numC 1)))
(check-equal? (parser '(* 5 2)) (binopC '* (numC 5) (numC 2)))
(check-equal? (parser '(/ 8 (+ 3 1))) (binopC '/ (numC 8) (binopC '+ (numC 3) (numC 1))))
(check-equal? (parser 'x) (identifierC 'x))
(check-equal? (parser '(five)) (fncallC 'five '()))
(check-equal? (parser '(f)) (fncallC 'f '()))
(check-equal? (parser '(area 3 4))
              (fncallC 'area (list (numC 3) (numC 4))))
(check-equal? (parser '(f (+ 1 2) (/ 8 4)))
              (fncallC 'f
                       (list (binopC '+ (numC 1) (numC 2))
                             (binopC '/ (numC 8) (numC 4)))))

(check-exn (regexp
            (regexp-quote
             "parser: [ VEBG ] parser expected real number or expression like (<op> <expr> <expr>), got '(4 2 bye)"))
           (lambda () (parser '(4 2 bye))))

(check-exn
 (regexp
  (regexp-quote
   "parser: [ VEBG ] parser requires two arguments to use the operator in '(+ 1)"))
 (lambda ()
   (parser '(+ 1))))

(check-exn
 (regexp
  (regexp-quote
   "parser: [ VEBG ] parser requires two arguments to use the operator in '(* 1 2 3)"))
 (lambda ()
   (parser '(* 1 2 3))))

(check-exn
 (regexp
  (regexp-quote
   "parser: [ VEBG ] parser requires two arguments to use the operator in '(+)"))
 (lambda ()
   (parser '(+))))




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




(check-equal? (subst (numC 3) 'x (identifierC 'x))
              (numC 3))
(check-equal? (subst (numC 3) 'x (identifierC 'y))
              (identifierC 'y))
(check-equal? (subst (numC 3) 'x (binopC '+ (identifierC 'x) (numC 2)))
              (binopC '+ (numC 3) (numC 2)))
(check-equal? (subst (numC 3)
                     'x
                     (fncallC 'f (list (identifierC 'x) (identifierC 'y))))
              (fncallC 'f (list (numC 3) (identifierC 'y))))




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
