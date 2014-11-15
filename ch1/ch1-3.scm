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

; It's Friday night; back on the wagon ;)

; 1.3.3 - Procedures as General Methods

(define tolerance 0.00001)

; e.g. (fixed-point cos 1.0)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	       next
         (try next))))
  (try first-guess))

(define (average x y) (/ (+ x y) 2))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
	       1.0))


; ex 1.35
;
; Show that the golden ratio ((1+sqrt(5))/2) is a fixed-point of the function
;  x->1+1/x, i.e. that ((1+sqrt(5))/2) = 1 + 1 / ((1+sqrt(5))/2) (..multiply
;  through by 2, and then (1+sqrt(5)), for 2(sqrt(5))+6 = 2(sqrt(5))+6)
;
; And, where x = 1 + 1/x, x is our fixed-point, and also the root(s) of the
;  function x(x) = x(1 + 1/x), or, x^2 - x - 1 = 0, which are.. ((1+sqrt(5))/2)

(define golden-ratio
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

;golden-ratio
;;Value: 1.6180327868852458


; ex 1.36
; Solve x^x = 1000 with and without average damping

(define (fixed-point-dbg f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
         next
         (try next))))
  (try first-guess))

; 28 steps
(define xx-nodamp
  (fixed-point-dbg (lambda (x) (/ (log 1000) (log x))) 5.0))

; 8 steps!
(define (xx-damp)
  (define (average-damp f)
    (lambda (x) (average x (f x))))
  (fixed-point-dbg (average-damp (lambda (x) (/ (log 1000) (log x)))) 5.0))

; x = 4.5555 when x^x = 1000


; 1.3.4 - Procedures as Returned Values

; a 'derivative' maps a function to a function, let's implement it on the basis
;  of g'(x) = (g(x + dx) - g(x)) / dx (rate of change of g(x) across dx..)
(define dx 0.00001)

(define (ddx f)
  (lambda (x)
    (/ (- (f (+ x dx)) (f x))
    dx)))

; derivative of x^3 should be 3(x^2)..
(define (cube x) (* x x x))
((ddx cube) 5)
; gives 3(x^2) = 75

; newton's transform
;  (http://en.wikipedia.org/wiki/Newton%27s_method#mediaviewer/File:NewtonIteration_Ani.gif)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((ddx g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; the zero of y->(y^2)-x would give us y such that (y^2)-x=0, or y=sqrt(x),
;  and since newtons-method can give us that zero, we can impl sqrt with it
(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))


; ex 1.40
; define a function that can be used with newtons-method to find zeroes (roots)
;  of the simple cubic x^3 + ax^2 + bx + c
(define (expt b n)
    (if (= n 0)
        1
        (* b (expt b (- n 1)))))

(define (cubic a b c)
  (lambda (x)
    (+ (expt x 3) (* a (* x x)) (* b x) c)))

(newtons-method (cubic 3 12 2) 1)
;1
;.1428595918409934
;-.14962646708045346
;-.17364709856819516
;-.17377912667924006
;;Value: -.1737791308867445
; checked here (http://www.wolframalpha.com/input/?i=x^3+%2B+3x^2+%2B+12x+%2B2)


; ex 1.41
; the double function is simply the church-encoding 'two'
(define (inc x) (+ x 1))
(define double
  (lambda (f)
    (lambda (x)
      (f (f x)))))
;(((double (double double)) inc) 5)
;;Value: 21


; ex 1.42
; implement compose, such that x->f(g(x))
(define (compose f g)
  (lambda (x)
    (f (g x))))
;((compose square inc) 6)
;;Value: 49


; ex 1.43
(define (repeated f n)
    (if (= n 0) f (compose f f)))
;((repeated square 2) 5)
;;Value: 625


; ex 1.44
; n-fold smoothed function
(define dx 0.1)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)
  )
)

(define (nfold-smooth f n)
  (repeated (smooth f) n)
)















;
