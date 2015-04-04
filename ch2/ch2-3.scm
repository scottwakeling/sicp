; 2.3 - Symbolic Data

; Ex 2.53

(list 'a 'b 'c)
(list (list 'scott))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks))) ; #f
(memq 'red '(red green blue)) ; #t
; Note: '(r g b) is the 'same' as (list 'r 'g 'b) according to memq (but not eq)
(equal? '(red) (list 'red))

(define (memq? item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

; Ex 2.54

(define (equal? a b)
  (cond ((and (null? a) (null? b)) #t) ; both null, so equal
        ((or (null? a) (null? b)) #f) ; one or other null, but not both (as above)
        ((and (pair? a) (pair? b))
         ; both pairs, so equal lists if car and cdr are both equal
         (and (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))))
        ((or (pair? a) (pair? b)) #f) ; one or other pair, but not both (as above)
        (else (eq? a b)))) ; end of recursive descent, are a and b the same symbol
                           ; or do they have the same value?


; 2.3.2 - Symbolic Differentiation

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s)
  (if (null? (cdddr s))
    (caddr s)
    (cons '+ (cddr s))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p)
  (if (null? (cdddr p))
    (caddr p)
    (cons '* (cddr p))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (make-exponentiation b e)
  (cond ((=number? e 0) 1) ; anything to the zero power is 1
        ((=number? e 1) b) ; anything to the one power is the base itself
        ((=number? b 0) 0) ; zero to any power is zero
        (else (list '** b e))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-exponentiation (make-product (exponent exp) (base exp))
                              (make-sum (exponent exp) -1)))
        (else
          (error "Unknown expression type in deriv: " exp))))









