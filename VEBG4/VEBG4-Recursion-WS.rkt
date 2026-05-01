#lang racket
   
   
;; this code defines the sim-VEBG4 language as a module
(module sim-VEBG4 racket
  (provide
   [rename-out (#%lam-app #%app)
               #;(#%lam-lam lambda)
               #;(my-if if)
               (my-let given)]
   else
   #%module-begin
   #%datum
   + - * / = equal? <= =>
   true false
   if
   do)
  (require (for-syntax syntax/parse))
   
  ;; this block is useful when `lambda` is the keyword for functions:
  #;(define-syntax (#%lam-lam stx)
      (syntax-case stx (:)
        [(_ (args ...) : body)
         #'(lambda (args ...) body)]
        [(_ z ...)
         (error 'zzz "ou44ch")]))
   
  ;; usually this is where lambda winds up, but not this year...
  ;; leaving this here for when we change to a different keyword again,
  ;; and lambdas get parsed as applications again:
  (define-syntax (#%lam-app stx)
    (syntax-case stx (fn)
      [(_ fn (args ...) -> e)
       #'(lambda (args ...) e)]
      [(_ e ...)
       #'(#%app e ...)]))
   
  #;(define-syntax (my-if stx)
      (syntax-case stx ()
        [(_ e1 e2 e3)
         #'(if e1 e2 e3)]))
   
  (define-syntax (my-let stx)
    (syntax-parse stx
      [(_
        ((var:id (~literal =) rhs:expr) ...)
        (~literal do)
        body:expr)
       #'(let ([var rhs] ...) body)])))
   
   
;; this module uses the sim-VEBG4 language. That is, its
;; contents are written *in* the sim-VEBG4 language.
;; the only edits you should make are inside this module:
(module my-mod1 (submod ".." sim-VEBG4)
   
  1234
   
  4567
   
  {+ 4 5}
   
  {if true 34 39}
    
  {{fn (x y) -> {+ x y}} 4 3}
   
  {given ([z = {+ 9 14}]
          [y =  98])
         do
         {+ z y}}
   
   
  ;; exercise 0: Using the binding form, give the name
  ;; `f` to the function that accepts an argument `x` and computes
  ;; x^2 + 4x + 4. Apply `f` to seven.
  {given {[f = {fn (x) -> {+ {* x x}
                             {+ {* 4 x}
                                4}}}]}
         do
         {f 7}}
   
  ;; exercise 1: Use the trick discussed in class to define
  ;; a `fact` function that computes the factorial of a given
  ;; number. Use it to compute the factorial of 12.
  {given {[fact = {fn (self n) ->
                      {if {<= n 0}
                          1
                          {* n {self self {- n 1}}}}}]}
         do
         {fact fact 12}}
    
  ;; exercise 2: Define a 'pow' function that accepts a base
  ;; and a (natural number) exponent, and computes the base to
  ;; the power of the exponent. Don't worry about non-natural
  ;; number exponents (6^1.5, 5^-4).
  {given {[pow = {fn (self base a) ->
                     {if {equal? a 0}
                         1
                         {* base {self self base {- a 1}}}}}]}
         do
         {pow pow 3 4}}

   
  ;; exercise 3: use `fact` and `pow` to build a "sin" function
  ;; that accepts a number x and a number of terms `n`, and computes
  ;; (sin x) using `n` terms of the taylor expansion. (Note that
  ;; this is a little ambigious; do zero-coefficient terms count?
  ;; You can go either way on this.) Use this function to compute
  ;; the sine of 1 radian to an error of no more than ten to the minus
  ;; 30th.


   
  )
   
;; this code actually invokes the 'my-mod1 module, so that the
;; code runs.
(require 'my-mod1)