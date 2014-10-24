; Chapter 2 - Building Abstractions with Data

; The general technique of *isolating* the parts of a program that deal with
; how data objects are represented from the parts of a program that deal with
; how data objects are used is a powerful design methodology called
; 'data abstraction'

; As with compound procedures, the main issue to be addressed is that of
; abstraction as a technique for coping with complexity. We will see how data
; abstraction allows us to erect suitable abstraction barriers between
; different parts of a program

; 2.1

(define (make-rat n d)
    (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; ex 2.1

; My crappy version, where I failed to think about what I was trying to achieve
; *before* I started typing..

(define (make-rat-ex n d)
    (define (make-rat-pos n d)
        (let ((n2 (abs n))
              (d2 (abs d)))
                (let ((g (gcd n2 d2)))
                    (cons (/ n2 g) (/ d2 g)))))
    (define (make-rat-neg n d)
        (let ((n2 (abs n))
              (d2 (abs d)))
	             (let ((g (gcd n2 d2)))
                    (cons (/ (- n2) g) (/ d2 g)))))
    (cond ((negative? n)
            (cond ((negative? d) (make-rat-pos n d))
                  (else (make-rat-neg n d))))
          (else
            (cond ((negative? d) (make-rat-neg n d))
                  (else (make-rat-pos n d)))
          )
    )
)

; A much better version by Bill the Lizard (relies on gcd results being +ve)

(define (make-rat-ex n d)
    (let ((g (gcd n d)))
        (cond ((negative? d) (cons (/ (* n -1) g) (/ (* d -1) g)))
              (else (cons (/ n g) (/ d g)))
        )
    )
)

; ex 2.2

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (make-vector u v) (cons u v))
(define (start-vector v) (car v))
(define (end-vector v) (cdr v))
(define (mid-point v)
    (define (avg x y) (/ (+ x y) 2))
    (make-point (avg (x-point (start-vector v))
                     (x-point (end-vector v)))
                (avg (y-point (start-vector v))
                     (y-point (end-vector v)))))
(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")")))

; ex 2.3

; make-rect     - rect constructor
; builds a rectangle from an origin point in the plane and
;  dimensions relative to it
; @param origin - bottom-left coordinate in the plane
; @param size   - width and height in the plane (as a point)
; @e.g.         - (make-rect (make-point 0 0) (make-point 10 5))
;               -  constructs a rect with bl(0,0) and tr(10,5)
(define (make-rect origin size) (cons origin size))
; width and height selector impls provide the contract (i.e. the
;  abstraction barrier) that guarantees perimeter and area work
;  regardless of rect's internal representation
(define (get-rect-width r)  (x-point (cdr r)))
(define (get-rect-height r) (y-point (cdr r)))

; make-rect-ops - rect constructor
; builds a rectangle given any two opposite corner points
(define (make-rect-ops pt0 pt1) (cons pt0 pt1))
(define (get-rect-width r)
  (abs (- (x-point (cdr r)) (x-point (car r)))))
(define (get-rect-height r)
  (abs (- (y-point (cdr r)) (y-point (car r)))))

; perimeter and area work with either of the impls above

; return the perimater of the rect r, regardless of impl
(define (perimeter r)
  (+ (* 2 (get-rect-width r)) (* 2 (get-rect-height r)))
)
; returns the area of the rect r, regardless of impl
(define (area r)
  (* (get-rect-width r) (get-rect-height r))
)


; 2.1.4

; cons car cdr are primitives, but you can implement them..

; 'look - there's no such thing as 'data' ;-)'

; cons returns a function (a function is just an object)
(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
	  ((= m 1) y)
	  (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))

; and we can implement a triple
(define (triple x y z)
  (define (dispatch m)
    (cond ((= m 0) x)
	  ((= m 1) y)
	  ((= m 2) z)
	  (else (error "Oik! I bent my wookie! -- triple" m))))
  dispatch)
(define (get-x t) (t 0))
(define (get-y t) (t 1))
(define (get-z t) (t 2))

; todo: but how do we implement an n-tuple?

; ex 2.4

; cons - returns a procedure that takes a procedure as an argument and
; applies x y to it. e.g. ((cons 3 4) sum) => 7
(define (cons x y)
  (lambda (m) (m x y)))

; car - takes a proc (z) that when given a proc as its only arg applies the
; two parameters it was defined with to *that* proc. The proc z is given is 
; (lambda (p q) p), simply a proc that takes two args and returns the first.
; So z apples its two params to it (x is passed in as p, y as q), and it 
; returns p, or x.
(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

; substitution..
(car (cons x y))
(car ((lambda (m) (m x y))))
; car is being given a procedure that takes a proc as its only argument and gives it x y
; let's see what car does with it
(z (lambda (p q) p)))
; it's taking it in as z, and the arg/proc it's giving it is a proc that when applied to
; two params returns the first. When the proc returned by cons is given that proc, it applies
; x y to it, and it returns x.

; confused? it's a mind-bender; this might help..
;((lambda (p q) p) 1 3)
;=> 1
;((lambda (p q) q) 1 3)
;=> 3
; it's just the lambda notation you need to get used to, then it all unfolds

; ex 2.5

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

; so (car (cons 4 5)) should give 4 (i.e. (car 3888)

; The number of times you can divide 3888 by 2 will be a power of 3, since 2 and 3 have no factors in common. no. of times 3888 divides by 2 is not the same as dividing it by 2..

; Bill the Lizard to the rescue
; count the number of times n is evenly divisible by d
(define (num-divs n d)
  (define (iter x result)
    (if (= 0 (remainder x d))
        (iter (/ x d) (+ 1 result))
        result))
  (iter n 0))

(define (car a)
  (num-divs a 2))
(define (cdr a)
  (num-divs a 3))

; ex 2.6 - Church Numerals


