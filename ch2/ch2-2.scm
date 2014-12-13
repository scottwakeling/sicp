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


; 2.2.2 - Hierarchical Structures

; count-leaves of a tree is count-leaves of the car of three plus count-leaves
; of the cdr of three
; count-leaves of a leaf is 1

(define (count-leaves t)
  (cond ((null? t) 0)
        ((not (pair? t)) 1)
        (else (+ (count-leaves (car t))
                 (count-leaves (cdr t))))))

; ex 2.24
(list 1 (list 2 (list 3 4)))
;Value 39: (1 (2 (3 4)))


; ex 2.25

(cdr (car (cdr (cdr (list 1 3 (list 5 7) 9)))))
;Value 42: (7)

(car (car (list (list 7))))
;Value: 7

(car (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 5 (list 7))))))))))))))))))))
;Value: 7

; ex 2.26

(define x (list 1 2 3))
;Value: x

(define y (list 4 5 6))
;Value: y

(append x y)
;Value 53: (1 2 3 4 5 6)

(cons x y)
;Value 54: ((1 2 3) 4 5 6)

(list x y)
;Value 55: ((1 2 3) (4 5 6))

; ex 2.27
(define (deep-reverse items)
  (cond ((null? items) items)
        ((pair? (car items))
          (append (deep-reverse (cdr items)) (list (deep-reverse (car items)))))
        (else (append (deep-reverse (cdr items)) (list (car items))))))
;(define y (list 44 (list (list 1 2) (list 3 4 5) 55 (list 66 77 88))))
;;Value: y
;(deep-reverse y)
;;Value 20: (((88 77 66) 55 (5 4 3) (2 1)) 44)

; ex 2.28

; visit leaf nodes
(define (fringe t)
  (define visit display)
  (cond ((null? t) 0)
        ((not (pair? t)) (visit t))
        (else (fringe (car t))
              (fringe (cdr t)))))

; collate leaf nodes
(define (fringe t)
  (cond ((null? t) t)
        ((not (pair? t)) (list t))
        (else (append (fringe (car t)) (fringe (cdr t))))))

; ex 2.29

(define (make-mobile left right) (list left right))
(define (make-branch length payload) (list length payload))

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (car (cdr mobile)))

(define (branch-length branch) (car branch))
(define (branch-payload branch) (car (cdr branch)))

(define (branch-weight b)
  (cond ((pair? (branch-payload b))
   (total-weight (branch-payload b)))
  (else (branch-payload b))))

(define (branch-torque branch) (* (branch-length branch) (branch-weight branch)))

(define (total-weight mobile)
  (cond ((null? mobile) 0)
        (else (+ (branch-weight (left-branch mobile))
                  (branch-weight (right-branch mobile))))))

(define (branch-balanced? branch)
   (cond (pair? (branch-payload branch))
       (mobile-balanced? (branch-payload branch))
       (else (true))))

(define (mobile-balanced? m)
  (and (= (branch-torque (left-branch m))
          (branch-torque (right-branch m))
          (branch-balanced? (left-branch mobile))
          (branch-balanced? (right-branch mobile))
          )))

; ex 2.30

(define (square-tree tree)
  (cond ((null? tree) ())
    ((not (pair? tree)) (* tree tree))
    (else (cons (square-tree (car tree))
                (square-tree (cdr tree))))))

; or, regard the tree as a sequence of sub-trees and map a lambda over it..
(define (square-tree tree)
  (map (lambda (sub-tree)
        (if (pair? sub-tree)
          (square-tree sub-tree)  ; recursively scale this sub-tree
          (* sub-tree sub-tree))) ; leaf node, square it
        tree))


; ex 2.31

; map a lambda over a tree
(define (tree-map proc tree)
  (cond ((null? tree) ())
    ((not (pair? tree)) (proc tree))
    (else (cons (tree-map proc (car tree))
                (tree-map proc (cdr tree))))))

; e.g. square..
(define (square-tree tree)
  (tree-map square tree)
)


; ex 2.32

; Calculate the power-set of the set S, http://en.wikipedia.org/wiki/Power_set
; (power-set (list 1 (list 2 3)))
; Value 29: (() ((2 3)) (1) (1 (2 3)))
; (power-set (list 1 2 3))
; Value 22: (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
(define (power-set S)
  (if (null? S)
    (list ())
    (let ((T (power-set (cdr S))))
      (append T (map (lambda (x) (cons (car S) x)) T)))))


; 2.2.3 - Sequences as Conventional Interfaces

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
      (accumulate op initial (cdr sequence)))))


; ex 2.33

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) () sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (elem acc) (+ acc 1)) 0 sequence))


; ex 2.34

; Evaluate a polynomial in x using Horner's Rule

(define (horner-eval x coeff)
  (accumulate
    (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
    0
    coeff))


; ex 2.36

; note: (map car (list (list 1 2) (list 3 4))) - gives you first element of
;  each sub-sequence, 1 and 3, while (map cdr (list (list 1 2) (list 3 4)))
;  gives you the sequences of sub-sequences with the first element chopped off

(define (accumulate-n op init seqs)
  (if (null? (car seqs)) ()
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))


; ex 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (accumulate + 0 (map * v row))) m))

(define (transpose mat)
  (accumulate-n cons () mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))


; ex 2.38

(fold-left / 1 (list 1 2 3))
; 1/6 (1 divided by 1, divided by 2, divided by 3)

(fold-right / 1 (list 1 2 3))
; 3/2 (3 divided by 1, divided by 2, dividied by 1)

(fold-right / 2 (list 1 2 12))
; 3 (12 divided by 2, divided by 2, divided by 1)

(fold-right list () (list 1 2 3))
; (1 (2 (3 ())))

(fold-left list () (list 1 2 3))
; (((() 1) 2) 3)

; op should be commutative for fold-right to produce the same result as
;  fold-left, i.e. A op B = B op A, e.g. MULTIPLY


; ex 2.39

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) () sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) () sequence))


; ex 2.40

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
    sequence))

(define (permutations s)
  (if (null? s)
      (list ())
      (flatmap (lambda (x)
                  (map (lambda (p) (cons x p))
                        (permutations (remove x s))))
                s)))

(define (enumerate-interval low high)
  (if (> low high)
    ()
    (cons low (enumerate-interval (+ low 1) high))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
    (filter prime-sum?
      (flatmap
        (lambda (i)
          (map (lambda (j) (list i j))
               (enumerate-interval 1 (- i 1))))
        (enumerate-interval 1 n)))))

(define (unique-pairs n)
  ...
)










;
