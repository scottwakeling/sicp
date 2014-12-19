(define contains?
  (lambda (lst elt)
    (if (null? lst)
	#f
	(if (pair? (car lst))
	    (or (contains? (car lst) elt) (contains? (cdr lst) elt))
	    (or (= (car lst) elt) (contains? (cdr lst) elt))))))

(define remove-duplicates
  (lambda (lst)
    (if (null? lst)
	()
	(if (contains? (cdr lst) (car lst))
	    (remove-duplicates (cdr lst))
	    (cons (car lst) (remove-duplicates (cdr lst)))))))

(define union
  (lambda (lst1 lst2)
    (if (null? lst1)
	lst2
	(if (null? lst2)
	    lst1
	    (remove-duplicates
	     (cons (car lst1)
		   (cons (car lst2) (union (cdr lst1) (cdr lst2)))))))))

(define subset?
  (lambda (s1 s2)
    (if (null? s1)
	#t
	(if (null? s2)
	    #f
	    (and
	     (contains? s2 (car s1))
	     (subset? (cdr s1) s2))))))

(define intersection
  (lambda (s1 s2)
    (if (null? s1)
	()
	(if (null? s2)
	    ()
	    (remove-duplicates
	     (if (contains? s2 (car s1))
		 (cons (car s1) (intersection (cdr s1) s2))
		 (intersection (cdr s1) s2)))))))

(define every-other
  (lambda (lst1)
    (if (null? lst1)
	()
	(if (null? (cdr lst1))
	    lst1
	    (if (null? (cdr (cdr lst1)))
		(cons (car lst1) ())
		(cons (car lst1) (every-other (cdr (cdr lst1)))))))))
