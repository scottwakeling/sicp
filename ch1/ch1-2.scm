; 1.2 Procedures and the Processes They Generate

; 1.2.1 Linear Recursion and Iteration

; A linear recursive process in a recursive procedure
;  (factorial 6)
;  (* 6 (factorial 5))
;  (* 6 (* 5 (factorial 4)))
;  etc. - builds up a chain of *deferred operations*

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

; A linear iterative process in a recursive procedure
;  (factorial 6)
;  (fact-iter 1 1 6)
;  (fact-iter 1 2 6)
;  (fact-iter 2 3 6)
;  etc. - state is captured in state variables, no stack required

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
		 (+ counter 1)
		 max-count)))

; Don't confuse a recursive process with a recursive procedure

; 'Tail Recursive'
; Iterative processes CAN execute in constant space, with no stack,
;  language implementations with this property are called *tail recursive*

; ex 1.9

; substitution model shows this process is recursive
;  (as is the procedure, *indirectly*)

(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

; substitution model shows this process is iterative
;  (the procedure is stil indirectly recursive)

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9


; 1.2.2 Tree Recursion

(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))

; substitution shows this to be an exponential tree of recursive
;  calls - I'm not going to even try typing it out, even for (fib 4)

; fib calls itself *twice* each time it's invoked, not just once
;  like 'normal' recursive procedures - the steps it takes grow
;  exponentially with its input, but the space (stack) grows
;  only linearly. Steps is no. of NODES in the recursive tree
;  diagram, space (max. stack size at any point) is tree DEPTH

; ridiculous amounts of redundant computation with this approach
;  over half the recursive tree diagram of (fib 5) is redundant
;  computation of (fib 3) (memoization..)

; here's an iterative fibonacci process

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

; SO much faster, and can be done in constant space (no stack)
;  with a tail recursive interpreter

; BUT, the tree-recursive process was way easier to formulate
;  the linear iteration required noticing that fib could be
;  recast as a process with three state variables

; it essentially carries a running total along with it

(fib 4)
(fib-iter 1 0 4)
;(fib-iter (+ 1 0) 1 3)
(fib-iter 1 1 3)
;(fib-iter (+ 1 1) 1 2)
(fib-iter 2 1 2)
;(fib-iter (+ 2 1) 2 1)
(fib-iter 3 2 1)
;(fib-iter (+ 3 2) 3 0)
(fib-iter 5 3 0)
3

; linear process, recursive procedure, fast as fuck! ;-)


; EXAMPLE: count-change problem (tree recursive version)

(define (count-change amount)
  (cc amount 3))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 10)
	((= kinds-of-coins 2) 20)
	((= kinds-of-coins 3) 50)))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount
		          (- kinds-of-coins 1))
		  (cc (- amount
			 (first-denomination kinds-of-coins))
		           kinds-of-coins)))))

(count-change 100)
10 ; ways to change £1 with only 10s, 20s, and 50s

; Still, tree recursive processes can be made more efficient
;  by avoiding redundant computation. One such way is to
;  tabulate intermediate results (e.g. (fib 3)). This is
;  'tabulation' or 'memoization' - it can be
;  straightforward to implement and can transform processes
;  that require exp. no. of steps into processes whose
;  space and time requirements grow only linearly with input


; enum
(define (get-tran-class-name tran-class)
  (cond ((= tran-class 1) 'Sale')
	((= tran-class 2) 'Refund')
	((= tran-class 3) 'Chargeback')))

; ex 1.11

; recursive
(define (f n)
  (cond ((< n 3) n)
    (else
	(+ (f (- n 1))
	        (* 2 (f (- n 2)))
		     (* 3 (f (- n 3)))))))

; iterative
(define (f n)
  (f-iter 2 1 0 n))
(define (f-iter a b c n)
  (cond ((= n 0) c)
  (else (f-iter (+ a (* 2 b) (* 3 c))
                a
                b
                (- n 1)))))

; ex 1.12

; pascal's triangle (binomial coefficients of (x+y)^row)
(define (bi-coeff col row)
  (cond ((= row 0) 1)
        ((= col 0) 1)
        ((= col row) 1)
  (else (+ (bi-coeff (- col 1) (- row 1))
           (bi-coeff col (- row 1))))))


; 1.2.3 Orders of Growth

; linear recursive process
;   no. of steps grows in proportion to the input n -> O(n) ('theta of n')
;   space requirements also grow with n -> O(n)

; linear iterative process
;   time -> O(n)
;   space -> O(1) (i.e. constant space)

; tree recursive
;   time -> O(k^n) (i.e. nodes in tree, or, rows in expansion)
;   space -> O(n) (i.e. max. depth of tree (the stack))

; O(log n) is better than linear, it's like binary searching a phone book for
;  someone's name and number instead of having to check every page (O(n))
;   remember, log(n) decelerates as n increases.. for large values of n it
;   beats O(n) by a country mile

(define (cube x) (* x x x))

(define (p x)
  (display "*")
  (newline)
  (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

; ex 1.15
; a - (12.15, 5 times) (24.3, 5 times), (125, 7 times)
; b - R(n) is O(log n) (doubling n produces a constant increase in space req.)
;     time is better than linear; O(n/3) (?), as we can triple n before another call is made..)
sine (12.15)
(p (sine 4.5))
(p (p (sine 1.5)))
(p (p (p (sine 0.5))))
(p (p (p (p (sine 0.1667)))))
(p (p (p (p (p (0.056))))))


; 1.2.4 Exponentiation

; a linear recursive version (always O(n) in both space and time)
(define (expt b n)
    (if (= n 0)
        1
        (* b (expt b (- n 1)))
    )
)

; a linear iterative version (always O(n) in time and O(1) in space (constant space))
(define (expt b n)
    (expt-iter b n 1)
)
(define (expt-iter b counter product)
    (if (= counter 0)
        product
        (expt-iter b (- counter 1) (* b product)))
)

; Successive squaring saves a lot of steps:
;  b^n = (b^(n/2))^2 (if n is even)
;  b^n = b * b^(n-1) (if n is odd)
;  O(log n) in both space and time (b^2n is only one more step than b^n - see below)
(define (even? n)
    (= (remainder n 2) 0)
)
(define (fast-expt b n)
    (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))
    )
)

; n=3
(fast-expt 2 3)
(* 2 (square (fast-expt 2 1))
(* 2 (square (* 2 (fast-expt 2 0)))) ; three steps to turning point

; n=6
(fast-expt 2 6)
(* 2 (square (fast-expt 2 3))
(* 2 (square (* 2 (square (fast-expt 2 1)))
(* 2 (square (* 2 (square (* 2 (fast-expt 2 0))) ; four steps to turning point

; in short, O(log n) is awesome

; ex 1.17

; recursive multiply function using only addition (run time 'linear in b' = O(b))
(define (* a b)
    (if (= b 0)
        0
        (+ a (* a (- b 1)))
        )
)

; write double and halve (even integers only) procedures, and with additions,
;  implement a mul procedure analagous to fast-expt, with run time O(log n)

(define (double a)
    (+ a a)
)
(define (halve a)
    (/ a 2)
)
(define (* a b)
    (cond ((= b 1) a)
        ((even? b) (double (* a (halve b))))
        (else (+ a (* a (- b 1))))
    )
)

; 1.2.5 GCDs

; A simple, iterative Euclid's Algorithm
(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))
    )
)

; ex 1.20
;
; normal-order-evaluation = 'fully expand and reduce'
;
; applicative-order-evaluation = 'eval the args and then apply' (LISP uses this)
;   (also, 'arguments are evaluated before operators are applied')

; normal-order (gcd 206 40)
; 18 times

; applicative-order (gcd 206 40)
(gcd 206 40)
(gcd 40 6)
(gcd 6 4)
(gcd 4 2)
(gcd 2 0)
=2
; 4 times

; 1.2.6 Testing for Primality

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
  ((divides? test-divisor n) test-divisor)
  (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))




;
