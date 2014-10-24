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
