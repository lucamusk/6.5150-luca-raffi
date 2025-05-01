(load "./sdf/manager/load.scm")
(manage 'new 'term)
(manage 'add 'design)
(manage 'add 'unification)


;;; loop fusion, loop tiling, loop hoisting, constant folding, dead code elimination,
;;; and instruction pipelining finished in this time

;;; Loop fusion
      #|
      (for i 0 10
...)
      (for i 0 10
      ...)
      ->
      (for i 0 10)
      ...)
|#

#|
(for k 0 10
(pp "wow"))

(for i 0 10
(for j 0 10
(for k 0 10
(pp "hi"))))
|#

(define test-blocking
  `(begin
     (for k 0 10
	  (pp "wow"))
     (for i 0 10
	  (for j 0 10
	       (for k 0 10
		    (pp "hi"))))
     (for j 0 10
	  (pp "cool"))))
(run-matcher
 (match:compile-pattern `((? b) 3))
 `((1 (b 3)))
 match:bindings)

(define (true-succ tree curr)
  #t)

(define (debug-succ tree curr)
  (pp curr)
  curr) 

(define (search-tree tree pattern succ)
  (let ((pattern-compiled (match:compile-pattern pattern)))
    (define (traverse-tree current)
      (case (run-matcher pattern-compiled current match:bindings)
	((#f)
	 (if (or (not (pair? current)) (= 0 (length current)))
	     #f
	     (let ((v (traverse-tree (car current)))) ;;; Dig down
	       (if (equal? v #f)
		   (traverse-tree (cdr current))
		   v)))) ;;; Dig across
	(else (succ tree current))))
    (traverse-tree tree)))
	  
(search-tree `((b 0 (1 2 0)) a (1 (b 3))) `((? b) 3) true-succ)

;;; Performs a deep copy of a given tree, with transformations
;;; done on the first sublist beginning with an element matching
;;; the given pattern
(define (transform-tree tree pattern transformer)	  
  (let ((pattern-compiled (match:compile-pattern pattern))
	(matched? #f))
    (define (traverse-tree current)
      (if matched?
	  current
	  (if (or (not (pair? current)) (= 0 (length current)))
	      current
	      (if (run-matcher pattern-compiled (car current) match:bindings)
		  (begin
		    (set! matched? #t)
		    (transformer current))
		  (let ((v (traverse-tree (car current)))) ;;; Dig down
		    (cons v (traverse-tree (cdr current)))))))) ;;; Dig across
      (traverse-tree tree)))

(transform-tree `((b 0 (1 2 0)) a (1 (b 3) 3)) `((? b) 3) (lambda (t) 'cool))
;;; -> ((b 0 (1 2 0)) a (1 1 3))

;;; Locations can either be across-wise (sequences) or depth-wise (blocks)
(define (nth-location pattern amount)
  `(nth ,pattern ,amount))
(define (nth-location? loc)
  (and (pair? loc)
       (= (length loc) 3)
       (equal? (car loc) 'nth)))

(define (nth-location-count loc)
  (caddr loc))
(define (nth-location-target loc)
  (cadr loc))

(define (block pattern)
  `(block pattern))
(define (block? loc)
  (and (pair? loc)
       (= (length loc) 2)
       (equal? (car loc) 'block)))

(define (block-pattern loc)
  (cadr loc))

;;; Dummy predicate
(define (ignore v)
  #t)

;;; Assumes no special handling for the matching code segment
(define (generic-optimize-at code location optimizer)
  (transform-tree code location optimizer))

(define (debug-opt code)
  (pp code)
  code)

(define optimize-at
  (simple-generic-procedure `optimize-at 3 generic-optimize-at))

(define-generic-procedure-handler optimize-at
  (match-args ignore nth-location? procedure?)
  (lambda (code location optimizer)
    (pp "opt at")
    (pp code)
    (let ((n (nth-location-count location))
	  (target (nth-location-target location)))
      (if (= n 1)
	  (transform-tree code target optimizer)
	  (transform-tree code target
			  (lambda (c)
			    (cons (car c)
				  (optimize-at (cdr c)
					       (nth-location target (- n 1))
					       optimizer))))))))

(define test-sequences
  `(
    (for i 0 10
	 (pp "wow"))
    (for i 0 10
	 (pp "hey"))
    (for i 0 10
	 (pp "nice!"))
    ))

(optimize-at test-sequences (nth-location `(for i 0 10 (? block)) 2) (lambda (c)
								       (pp "optimizing")
								       (pp c)
								       (cons 'cool (cdr c))))

(define top-level
  `(? code))

(define (loop-fuser loop-1 loop-2)
  (define optimizer (make-pattern-operator))
  (attach-rule! optimizer
		(rule `((
			(?? a)
			(for ,loop-1 (? bound-low) (? bound-high) (? body-1))
			(?? b)
			(for ,loop-2 (? bound-low) (? bound-high) (? body-2))
			(?? c)
			))
		      `(
			,@a
			(for ,loop-1 ,bound-low ,bound-high
			     (
			      ,body-1
			      ,body-2
			      )
			     )
			,@b
			,@c
			)))
  optimizer)

;;; Simple test for correctness
(define lf-test-1
  '(
    (for i 0 10
	 (pp "hi"))
    (for j 0 10
	 (pp "wow"))
    ))

(optimize-at lf-test-1 top-level (loop-fuser `i `j))
#|
((for i 0 10 ((pp "hi") (pp "wow"))))
|#

;;; Tests if the other code is preserved and if we only fuse first instances
(define lf-test-2
  '(
    (for i 0 10
	 (pp "hi"))
    (for i 0 10
	 (pp "cool"))
    (for j 0 10
	 (pp "wow"))
    (for j 0 10
	 (pp "neat"))
    ))

(optimize-at lf-test-2 top-level (loop-fuser `i `j))
#|
((for i 0 10 ((pp "hi") (pp "wow")))
 (for i 0 10 (pp "cool"))
 (for j 0 10 (pp "neat")))
|#

;;; Doesn't fuse across scopes
(define lf-test-3
  '(
    (for i 0 10
	 (pp "hi"))
    (for k 0 10
	 (for j 0 10
	      (pp "wow")))
    ))

(optimize-at lf-test-3 top-level (loop-fuser 'i 'j))
;;; -> No applicable operations: (((for i 0 10 (pp "hi")) (for k 0 10 (for j 0 10 (pp "wow")))))

;;; Doesn't fuse if the bounds are different
(define lf-test-4
  '(
    (for i 0 15
	 (pp "hi"))
    (for k 0 10
	 (pp "wow"))
    ))

(optimize-at lf-test-4 top-level (loop-fuser 'i 'k))
;;; -> No applicable operations: (((for i 0 15 (pp "hi")) (for k 0 10 (pp "wow"))))



(define optimizer (make-pattern-operator))

(attach-rule! optimizer
	      (rule `((begin (
				 (?? a)
				 (for i (? bound_low) (? bound_high) (? body_1))
				 (?? b)
				 (for j (? bound_low) (? bound_high) (? body_2))
				 (?? c)
				 )
				))
		    `(begin
		       (,@a
			(for i ,bound_low ,bound_high
			     (begin
			       ,body_1
			       ,body_2
			       )
			     )
			,@b
			,@c
			)
		       )
		      )
	      )


(define test1
  `(begin (
     (for i 0 x (pp "wow"))
     (for j 0 x (pp "cool!"))
     )))

(optimizer test1); -> (begin ((for i 0 x (begin (pp "wow") (pp "cool!")))))

(define test2
  `(begin (
	   (for i 0 10 (pp "wow"))
	   (for j 0 10 (pp "cool"))
	   )
	  ))

(optimizer test2); -> (begin ((for i 0 10 (begin (pp "wow") (pp "cool!")))))

(define test3
  `(begin
     (
      (for i 0 10 (pp "wow"))
      (for i 0 10 (pp "cool"))
      (for j 0 10 (pp "crazy"))
      )
     )
  )

(optimizer test3)

BB
