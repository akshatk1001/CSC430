#lang typed/racket
(require typed/rackunit)

#|
#TODO REMEMBER TO MAKE PROGRESS TO GOAL COMMENT HERE
|#

;-------Question 3.1-------
(define-type ArithC (U numC binopC))
(struct numC ([n : Real]) #:transparent)
(struct binopC ([op : Symbol] [l : ArithC] [r : ArithC]) #:transparent)


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
interp accepts one argument, arithexp (ArithC) and evaluates the expression
to return the answer. 
|#
(: interp (ArithC -> Real))
(define (interp (arithexp : ArithC)) : Real
  (match arithexp
    [(numC n) n]
    [(binopC op l r) ((lookup op) (interp l) (interp r))]))


#|
parser accepts one argument, exp (Sexp) and converts the expression into
an expression of type ArithC to be evaluated by the Arith interpreter. 
|#
(: parser (Sexp -> ArithC))
(define (parser (exp : Sexp)) : ArithC
  (match exp
    [(? real? r) (numC r)]
    [(list (? symbol? s) l r) (binopC s (parser l) (parser r))]
    [other (error 'parser
                  "[ VEBG ] parser expected real number or expression like (<op> <expr> <expr>), got ~e"
                  exp)]))

;-------Question 3.2-------


#|
#TODO  #TODO  #TODO  #TODO  #TODO MOVE ALL CHECK-EQUALS TO BOTTOM HERE
|#
;-------Checks-------
(check-equal? (lookup '+) +)
(check-equal? (lookup '*) *)
(check-equal? (lookup '/) /)
(check-exn (regexp
            (regexp-quote
             "lookup: [ VEBG ] lookup expected a valid symbol (+, *, /), got '-"))
           (lambda () (lookup '-)))

(check-equal? (interp (numC 4)) 4)
(check-equal? (interp (binopC '+ (numC 5) (numC 8))) 13)
(check-equal? (interp (binopC '* (numC 5) (binopC '+ (numC 2) (numC 4)))) 30)
(check-equal? (interp (binopC '/ (binopC '* (numC 3) (numC 4)) (numC 6))) 2)

(check-equal? (parser 5) (numC 5))
(check-equal? (parser '(+ 4 1)) (binopC '+ (numC 4) (numC 1)))
(check-equal? (parser '(* 5 2)) (binopC '* (numC 5) (numC 2)))
(check-equal? (parser '(/ 8 (+ 3 1))) (binopC '/ (numC 8) (binopC '+ (numC 3) (numC 1))))
(check-exn (regexp
            (regexp-quote
             "parser: [ VEBG ] parser expected real number or expression like (<op> <expr> <expr>), got '(4 2 bye)"))
           (lambda () (parser '(4 2 bye))))
