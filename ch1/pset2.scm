(define slow-mul (lambda (a b)
    (if (= b 0) 0 (+ a (slow-mul a (- b 1))))
))




(slow-mul 2 3)
(+ a (slow-mul a (- b 1)))
(+ 2 (slow-mul 2 2))
(+ 2 (+ 2 (slow-mul 2 1))
(+ 2 (+ 2 (+ 2 (slow-mul 2 0))






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

(simple-log 4)
(+ 1 (simple-log 2))
(+ 1 (+ 1 (simple-log 1))
(+ 1 (+ 1 0))
(+ 1 1)
(2)

(simple-log 8)
(+ 1 (simple-log 4))
(+ 1 (+ 1 (simple-log 2))
(+ 1 (+ 1 (+ 1 simple-log 1)))
(+ 1 (+ 1 (+ 1 (0))))
(+ 1 (+ 1 (+ 1)))
(+ 1 (+ 2))
(3)




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
(define slow-mul-iter (lambda (a b)
    your_code_here
))
