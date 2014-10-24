;------------------
; Dot Product
; f(V,V1) = V.x*V1.x + V.y*V1.y + V.z*V1.z
;------------------
(define (dot x y z x1 y1 z1)
  (+ (+ (* z z1) (* y y1)) (* x x1))
)


;------------------
; Newton Raphson
;------------------

; square root

(define (sqrt x)
  (define (average x y)
    (* (+ x y) 0.5))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
	guess
	(sqrt-iter (improve guess))))
(sqrt-iter 1.0))


; cube root

(define (cbrt x)
  (define (cube x)
    (* x x x))
  (define (average x y)
    (* (+ x y) 0.5))
  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) 0.001))
  (define (improve guess)
    (/
     (+ (/ x (square guess)) (* 2 guess))
     3))
  (define (cbrt-iter guess)
    (if (good-enough? guess)
	guess
	(cbrt-iter (improve guess))))
(cbrt-iter 1.0))


;-----------------------
; Pythagoras' Theorem
; f(x) = sqrt(x^2 + y^2)
;-----------------------

(define pythagoras
  (define (sum-sq x y)
    (+ (square x) (square y)))
  (lambda (x y)
    (sqrt (sum-sq x y))))

(pythagoras 1 2)

;syntactic sugar version

(define (pythagoras x y)
  (define (sum-sq x y)
    (+ (square x) (square y)))
  (sqrt (sum-sq x y)))

(pythagoras 3 4)


; ex 1.5
(define (p) (p))

(define (test x y)
    (if (= x 0)
        0
        y)
)

(test 0 (p))

; normal-order-evaluation (expand operators first, and then reduce)
(test 0 (p))
(if (= 0 0) 0 (p))
(if #t 0 (p))
0

; applicative-order-evaluation (eval args before applying ops)
(test 0 (p))
(test 0 (p))
(test 0 (p))
(test 0 (p))
; etc. ad-infinitum, (p) is infinitely expanded and never reaches a primitive
