; 2.2 - Hierarchical Data and Closure

; 2.2.1 - Representing Sequences

; The ability to create pairs whose elements themselves may be pairs gives cons
; its 'closure' property, i.e. a set of elements is closed under an operation
; that produces only more members of that set (e.g. * is closed on R)

(list 1 2 3 4)
; is syntactic sugar for
(cons 1 (cons 2 (cons 3 (cons 4 ()))))

; recursive length, at, append
(define (length items)
  (define (length-iter a count)
    (if (null? a)
      count
      (length-iter (cdr a) (+ 1 count))))
      (length-iter items 0)
)

(define (at items n)
  (if (= n 0)
    (car items)
    (at (cdr items) (- n 1))
))

(define (append l1 l2)
  (if (null? l1)
    l2
    (cons (car l1) (append (cdr l1) l2))))


; ex 2.17

; simple (though 'at' is iterative anyway, so might even be 'best'..)
(define (last-pair list)
  (at list (- (length list) 1)))

; recursive
(define (last-pair-r list)
  (if (= (length list) 1)
    list
    (last-pair-r (cdr list))
  )
)

; iterative
(define (last-pair-i list)
  (define (last-pair-iter a count)
    (if (= count (- (length list) 1))
      a
      (last-pair-iter (cdr a) (+ count 1))))
  (last-pair-iter list 0))


; ex 2.18
(define (reverse items)
  (if (null? items)
    items
    (append (reverse (cdr items)) (list (car items)))))


; ex 2.20 - dotted tail notation
(define (same-parity a . z)
  (define (parity-iter items bit)
    (if (null? items)
      items
      (if (= (remainder (car items) 2) bit)
        (append (list (car items)) (parity-iter (cdr items) bit))
        (parity-iter (cdr items) bit)
      )
    )
  )
  (append (list a) (parity-iter z (remainder a 2)))
)

; lambda notation for dotted tail would be:
;  (define var_args (lambda (x y . z) <body>))
; e.g.
;(define f (lambda (x y . z)
;	    (display x)
;	    (newline)
;	    (display y)
;	    (newline)
;	    (display z)))
;;Value: f
;(f 1 2 3 4 5)
;1
;2
;(3 4 5)


; ex 2.21
(define (square-list items)
  (if (null? items)
    items
    (cons (* (car items) (car items)) (square-list (cdr items)))
  )
)

(define (square-list items)
  (map (lambda (x) (* x x)) items)
)

; ex 2.22

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons (square (car things))
                  answer))))
  (iter items ())
)
;(iter (2 3) (cons 1 ()))
;(iter (3) (cons 4 (cons 1 ())))
;(cons 9 (cons 4 (cons 1 ())))
; builds in reverse order because the cons is always the cdr of each expansion

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer
                  (square (car things))))))
  (iter items ())
)
;this makes a new list at every expansion of iter, made up of the list before
;and the square, not pretty, we only want one list, not a list of lists

; Here's a fix for the iterative version
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (append answer (list (square (car things)))))))
  (iter items ())
)


; ex 2.23
(define (for-each f items)
  (cond ((null? items) #f)
    (else (f (car items))
          (for-each f (cdr items)))
  )
)

; done
