
; Problem Set 2 - icampustutor

(define slow-mul (lambda (a b)
    (if (= b 0) 0 (+ a (slow-mul a (- b 1))))
))

;(slow-mul 2 3)
;(+ a (slow-mul a (- b 1)))
;(+ 2 (slow-mul 2 2))
;(+ 2 (+ 2 (slow-mul 2 1))
;(+ 2 (+ 2 (+ 2 (slow-mul 2 0))

(define dec (lambda (a)
          (- a 1)))
(define inc (lambda (a)
          (+ a 1)))
(define plus (lambda (a b)
    (if (zero? a) b (plus (dec a) (inc b)))))

(define simple-log (lambda (n)
    (if (= n 1) 0
                (+ 1 (simple-log (/ n 2)))
    )
))

;(simple-log 4)
;(+ 1 (simple-log 2))
;(+ 1 (+ 1 (simple-log 1))
;(+ 1 (+ 1 0))
;(+ 1 1)
;(2)

;(simple-log 8)
;(+ 1 (simple-log 4))
;(+ 1 (+ 1 (simple-log 2))
;(+ 1 (+ 1 (+ 1 simple-log 1)))
;(+ 1 (+ 1 (+ 1 (0))))
;(+ 1 (+ 1 (+ 1)))
;(+ 1 (+ 2))
;(3)

; PS.2.2.1
(define (odd? x)
    (cond ((= x 0) #f)
          ((= x 1) #t)
          (else (odd? (- x 2)))
    )
)

; PS.2.2.2
; write mul with only add sub and simple predicates
(define slow-mul-recurse (lambda (a b)
    (cond ((= b 1) a)
    (else (+ a (slow-mul-recurse a (- b 1)))))))

; PS.2.2.3
; Using only addition, subtraction and simple predicates, write an iterative
; procedure that takes a and b (assume positive)

(define (slow-mul-iter a b)
  (define (mul-iter a b sum)
    (cond ((= b 0) sum)
    (else (mul-iter a (- b 1) (+ sum a))))
  )
  (mul-iter a b 0)
)

; PS.2.2.4
; As 2.2.3, but you may use double and halve and even?, and the proc must run in
; logarithmic time
(define (halve x) (/ x 2))
(define (double x) (* x 2))
(define fast-mul
  (lambda (a b)
    (cond ((= b 1) a)
          (else (cond ((even? b) (double (fast-mul a (halve b))))
                      (else (+ a (double (fast-mul a (halve (- b 1)))))))))))

; PS.2.2.5
; Constant-time sum

(define quick-sum
  (lambda (n)
    (cond ((even? n) (* (/ n 2) (+ n 1)))
          (else (+ n (* (/ (- n 1) 2) n))))))

; PS.2.2.6
; Write a purely iterative version of the fast-exp procedure

(define (fast-expi a b)
  (define (exp-iter a b p)
    (cond ((= b 0) p)
          (else (exp-iter a (- b 1) (* a p)))))
  (exp-iter a b 1)
)
