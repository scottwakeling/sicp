; 1.3 Formulating Abstractions with Higher-Order Procedures

; 1.3.1

; implement the *concept* of summation..
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b)) ))

(define (inc n) (+ n 1))

(define (cube n) (* n n n))

; ..to sum any series you like
(define (sum-cubes a b)
  (sum cube a inc b))

(define (id x) x)

(define (sum-integers a b)
  (sum id a inc b))

; approx. pi/8
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

; definite integral of a function f between lim a->b, for small values of dx
; each term in the series of a definite integral is f(a+(dx/2))
; to go from one term to the *next*, add dx to x
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; 1.3.2 - Constructing Procedures Using Lambda

; (lambda (<params>) <body>)

; remove auxiliary procedure definitions by using lambda notation to create
; functions as objects
(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

; let is syntactic sugar for a lambda expression that passes the expression
; values as arguments to an anonymous procedure with the same body as the let
; statement. And since lambda functions can be nested (i.e. curried), the
; variables you define in the let (i.e. the args you pass to the generated
; lambda) have scope within that body (that lambda) only. They do work just
; like local variables, and they override, when bound, any free variables of
; the same name (i.e same-named variables in an outer scope)

; ex 1.34
(define (f g)
  (g 2))

;(f f)
;(f 2)
;(2 2)
; the object 2 is not applicable

; 1.3.3 - Procedures as General Methods
