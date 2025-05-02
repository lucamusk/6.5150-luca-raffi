
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
(for i 0 10
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
 (match:compile-pattern `((? b) 2 3))
 `(1 2 3)
 match:bindings)

(define (true-succ tree curr)
  #t)

(define (id-succ tree curr)
  curr)

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
(search-tree `(for i 0 10 (for j 0 10 ((pp 'h)))) `(for j (? l) (? h) (? b)) true-succ)

;;; Performs a deep copy of a given tree, with transformations
;;; done on the first or all elements matching the given
;;; pattern
(define (transform-tree tree pattern transformer all?)
  (let ((pattern-compiled (match:compile-pattern pattern))
        (matched? #f))
    (define (traverse-tree current)
      (if (and matched? (not all?))
          current
          (if (run-matcher pattern-compiled current match:bindings)
              (begin
		(let ((transformed (transformer current)))
		  ;;; We match on the first pattern match that we
		  ;;; also transform. This enables transformers to
		  ;;; do their own pattern matching 
		  (set! matched? (not (equal? current transformed)))
                  (transformer current)))
	      (if (pair? current)
		  (let ((v (traverse-tree (car current)))) ;;; Dig down
                    (cons v (traverse-tree (cdr current)))) ;;; Dig across
		  current))))
  (traverse-tree tree)))

;;; Performs a deep copy of a given tree, with transformations
;;; done on the first subtree beginning with an element that
;;; matches the given pattern. Necessary for sequences to work
(define (transform-subtree tree pattern transformer)
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

(transform-tree `((b 0 (1 2 0)) a (1 (b 2) 3)) `((? b) 2) (lambda (t) 'cool) #f)
;;; -> ((b 0 (1 2 0)) a (1 cool 3))

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

(define (block block-pattern scope-index inner-pattern)
  `(block ,block-pattern ,inner-pattern ,scope-index))
(define (block? loc)
  (and (pair? loc)
       (= (length loc) 4)
       (equal? (car loc) 'block)))

(define (block-scope-index loc)
  (cadddr loc))

(define (block-pattern loc)
  (cadr loc))

(define (block-inner-pattern loc)
  (caddr loc))

;;; Dummy predicate
(define (ignore v)
  #t)

;;; Assumes no special handling for the matching code segment
(define (generic-optimize-at code location optimizer all?)
  (transform-tree code location optimizer all?))

(define (debug-opt code)
  (pp code)
  code)

(define optimize-at
  (simple-generic-procedure `optimize-at 4 generic-optimize-at))

(define-generic-procedure-handler optimize-at
  (match-args ignore nth-location? procedure? boolean?)
  (lambda (code location optimizer all?)
    (let ((n (nth-location-count location))
          (target (nth-location-target location)))
      (if (= n 1)
          (transform-tree code target optimizer all?)
          (transform-subtree code target
                          (lambda (c)
                            (cons (car c)
                                  (optimize-at (cdr c)
                                               (nth-location target (- n 1))
                                               optimizer
					       all?))))))))

(define test-sequences
  `((for i 0 10
      (pp "wow"))
    (for i 0 10
      (pp "hey"))
    (for i 0 10
      (pp "nice!"))))

(optimize-at test-sequences (nth-location `(for i 0 10 (? block)) 2) (lambda (c)
                                                                       'cool)
	     #f)
;;; -> ((for i 0 10 (pp "wow")) cool (for i 0 10 (pp "nice!")))

(define-generic-procedure-handler optimize-at
  (match-args ignore block? procedure? boolean?)
  (lambda (code location optimizer all?)
    (let ((target-block (block-pattern location))
	  (scope-index (block-scope-index location))
	  (inner-pattern (block-inner-pattern location)))
      (transform-tree code target-block
		      (lambda (b)
			(pp (list-ref b scope-index))
			(let ((list-left
			       (if (= scope-index 0)
				   '()
				   (sublist b 0 scope-index)))
			      (list-right 
			       (sublist b (+ scope-index 1) (length b))))
			  (append list-left
				  (cons 
				   (optimize-at (list-ref b scope-index) inner-pattern optimizer all?)
				   list-right))))
				  
		      #f))))

(define top-level
  `(? code))

;;; Simple block test
(define test-blocks-1
  `((for i 0 10 (
		 (pp i)
		 (pp "hey")))))

(optimize-at test-blocks-1 (block `(for i 0 10 (? block)) 4 `(pp i)) (lambda (c) 'cool) #f)

;;; Ensures if we match 1 block but not its contents, we still keep matching
(define test-blocks-2
  `((for i 0 10 (
		 (pp "wow")))
    (for i 0 10 (
		 (pp i)
		 (pp "hey")))))

(optimize-at test-blocks-2 (block `(for i 0 10 (? block)) 4 `(pp i)) (lambda (c) 'cool) #f) 
;;; -> ((for i 0 10 ((pp "wow"))) (for i 0 10 (cool (pp "hey"))))


(define (rename var new)
  (define optimizer (make-pattern-operator))
  (attach-rule! optimizer
		(rule `(,var)			 
		      `,new))
  optimizer)

(optimize-at test-sequences 'i (rename 'i 'j) #t)
;;; -> ((for j 0 10 (pp "wow")) (for j 0 10 (pp "hey")) (for j 0 10 (pp "nice!")))

(define (loop-fuser loop-1 loop-2)
  (define optimizer (make-pattern-operator))
  ;;; Where the bounds are equal
  (attach-rule! optimizer
                (rule `(((?? a)
                         (for ,loop-1 (? bound-low) (? bound-high) (? body-1))
                         (?? b)
                         (for ,loop-2 (? bound-low) (? bound-high) (? body-2))
                         (?? c)))
                      `(,@a
                        (for ,loop-1 ,bound-low ,bound-high
                          (,body-1
                           ,(optimize-at body-2 loop-2 (rename loop-2 loop-1) #t)))
                        ,@b
                        ,@c)))
  ;;; Where the bounds may not be equal (doesn't statically check if iterations are
  ;;; the same, that's the programmer's burden
  ;;; Numeric bounds
  (attach-rule! optimizer
		(rule `(((?? a)
			 (for ,loop-1 (? bound-low-1, number?) (? bound-high-1) (? body-1))
			 (?? b)
			 (for ,loop-2 (? bound-low-2, number?) (? bound-high-2) (? body-2))
			 (?? c)))
		      `(,@a
			(for ,loop-1 ,bound-low-1 ,bound-high-1
			     (,body-1
			      ,(optimize-at body-2 loop-2
					    (rename loop-2 `(+ ,loop-1 ,(- bound-low-2 bound-low-1)))
					    #t)))
			,@b
			,@c)))
  
  ;;; One or more symbolic bounds
  (attach-rule! optimizer
		(rule `(((?? a)
			 (for ,loop-1 (? bound-low-1) (? bound-high-1) (? body-1))
			 (?? b)
			 (for ,loop-2 (? bound-low-2) (? bound-high-2) (? body-2))
			 (?? c)))
		      `(,@a
			(for ,loop-1 ,bound-low-1 ,bound-high-1
			     (,body-1
			      ,(optimize-at body-2 loop-2
					    (rename loop-2 `(+ ,loop-1 (- ,bound-low-2 ,bound-low-1)))
					    #t)))
			,@b
			,@c)))
			     
  optimizer)


(optimize-at `(pp j j j) 
;;; Simple test for correctness
(define lf-test-1
  '((for i 0 10
      (pp "hi"))
    (for j 0 10
      (pp "wow"))))

(optimize-at lf-test-1 top-level (loop-fuser `i `j) #f)
#|
((for i 0 10 ((pp "hi") (pp "wow"))))
|#

;;; Tests if the other code is preserved and if we only fuse first instances
(define lf-test-2
  '((for i 0 10
      (pp "hi"))
    (for i 0 10
      (pp "cool"))
    (for j 0 10
      (pp "wow"))
    (for j 0 10
      (pp "neat"))))

(optimize-at lf-test-2 top-level (loop-fuser `i `j) #f)
#|
(
(for i 0 10 ((pp "hi") (pp "wow")))
(for i 0 10 (pp "cool"))
(for j 0 10 (pp "neat"))
)
|#

;;; Doesn't fuse across scopes
(define lf-test-3
  '((for i 0 10
      (pp "hi"))
    (for k 0 10
      (for j 0 10
        (pp "wow")))))

(optimize-at lf-test-3 top-level (loop-fuser 'i 'j) #f)
;;; -> No applicable operations: (((for i 0 10 (pp "hi")) (for k 0 10 (for j 0 10 (pp "wow")))))

;;; Fuses if different bounds
(define lf-test-4
  '((for i 0 15
      (pp i))
    (for k 15 30
      (pp k))))

(optimize-at lf-test-4 top-level (loop-fuser 'i 'k) #f)
;;; -> ((for i 0 15 ((pp i) (pp (+ i 15)))))

;;; Fuses if different variable bounds
(define lf-test-5
  '((for i 10 15
      (pp i))
    (for k x y
      (pp k))))

(optimize-at lf-test-5 top-level (loop-fuser 'i 'k) #f)
;;; -> ((for i 0 15 ((pp i) (pp (+ i (- x 10))))))

;;; Tests if we change the loop variables
(define lf-test-6
  `(
    (for i 0 10
	 (pp i))
    (for j 0 10
	 (pp j j j))))

(optimize-at lf-test-6 top-level (loop-fuser 'i 'j) #f)
;;; -> ((for i 0 10 ((pp i) (pp i i i))))

(define (loop-tile tile-factor)
  (define optimizer (make-pattern-operator))
  ;;; Where the bounds are equal
  (attach-rule! optimizer
                (rule `(((?? a)
                         (for (? loop-var) (? bound-low, number?) (? bound-high, number?) (? body))
                         (?? b)))
                      `(,@a
                        (for ,loop-var 0 ,(/ (- bound-high bound-low) tile-factor)
                             (for ,(symbol loop-var loop-var)
				  (* ,loop-var ,tile-factor)
				  (+ (* ,loop-var ,tile-factor) ,tile-factor)
				  ,(optimize-at body loop-var
					    (rename loop-var (symbol loop-var loop-var))
					    #t)))
                        ,@b)))
  (attach-rule! optimizer
                (rule `(((?? a)
                         (for (? loop-var) (? bound-low) (? bound-high) (? body))
                         (?? b)))
                      `(,@a
                        (for ,loop-var 0 (/ (- ,bound-high ,bound-low) ,tile-factor)
                             (for ,(symbol loop-var loop-var)
				  (* ,loop-var ,tile-factor)
				  (+ (* ,loop-var ,tile-factor) ,tile-factor)
				  ,(optimize-at body loop-var
					    (rename loop-var (symbol loop-var loop-var))
					    #t)))
                        ,@b)))			     
  optimizer)

(define lt-test-1
  `((for i 0 10
	 (pp i))))
(optimize-at lt-test-1 top-level (loop-tile 5) #f)
;;; -> ((for i 0 2 (for ii (* i 5) (+ (* i 5) 5) (pp ii))))


(define lt-test-2
  `((for i x y
	 (pp i))))
(optimize-at lt-test-2 top-level (loop-tile 5) #f)
;;; -> ((for i 0 (/ (- y x) 5) (for ii (* i 5) (+ (* i 5) 5) (pp ii))))

(define (loop-reorder outer-loop-var inner-loop-var)
  ;;; More complicated since we need to bring data
  ;;; into shallower scopes
  (lambda (c)
    (define (at-loop outer-loop)
      (let (
	    (bound-low-o (caddr outer-loop))
	    (bound-high-o (cadddr outer-loop))
	    (inner-loop (search-tree outer-loop
				     `(for ,inner-loop-var (? bound-low-i) (? bound-high-i) (? body-i))
				     id-succ)))
	(if (equal? inner-loop #f)
	    outer-loop ;;; Not a match, keep searching
	    (let ((bound-low-i (caddr inner-loop))
		  (bound-high-i (cadddr inner-loop)))
	      (define optimizer-inner (make-pattern-operator))
	      (attach-rule! optimizer-inner
			    (rule `((for ,inner-loop-var (? bound-low-i) (? bound-high-i)
					  (? body-i)))
				  `(for ,outer-loop-var ,bound-low-o ,bound-high-o ,body-i)))
				    
	      (define optimizer-outer (make-pattern-operator))
	      (attach-rule! optimizer-outer
			    (rule `((?? a)
				     (for ,outer-loop-var (? bound-low-o) (? bound-high-o)
					  (? body-o))
				     (?? b))
				  `(,@a
				    (for ,inner-loop-var ,bound-low-i ,bound-high-i
					 ,(transform-tree body-o
							  `(for ,inner-loop-var
								,bound-low-i
								,bound-high-i
								(? body-i))
							  optimizer-inner
							  #f))	
				    ,@b)))
	      (optimizer-outer c)))))
	
    (transform-tree c
		    `(for ,outer-loop-var (? bound-low-o) (? bound-high-o) (? body-o))
		    at-loop
		    #f)))

(define lr-test-1
  `(for i 0 10
	((for j 0 10
	     ((for k 0 10
		  (
		   (pp i)
		   (pp j)
		   (pp k))))))))

(optimize-at lr-test-1 top-level (loop-reorder 'i 'j) #f)
;;; -> ((for j 0 10 ((for i 0 10 ((for k 0 10 ((pp i) (pp j) (pp k))))))))

(define lr-test-2
  `(for i 0 10
	((for z 0 10
	      (pp z))
	 (for j 0 10
	     ((for k 0 10
		  (
		   (pp i)
		   (pp j)
		   (pp k))))))))

(optimize-at lr-test-2 top-level (loop-reorder 'i 'j) #f)
;;; -> ((for j 0 10 ((for z 0 10 (pp z)) (for i 0 10 ((for k 0 10 ((pp i) (pp j) (pp k))))))))
