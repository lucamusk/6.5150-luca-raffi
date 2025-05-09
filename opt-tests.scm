(load "./opt-interface.scm")
(load "./loop-fusion.scm")
(load "./loop-tiling.scm")
(load "./loop-reordering.scm")
(load "./constant-propagation.scm")

;; TRANSFORM TREE
(transform-tree `((b 0 (1 2 0)) a (1 (b 2) 3)) `((? b) 2) 1 (lambda (t) 'cool) #f)
;;; -> ((b 0 (1 2 0)) a (1 cool 3))

;; BLOCK LOCATIONS
;;; Simple block test
(define test-blocks-1
  `((for i 0 10
	 (pp i)
         (pp "hey"))))

(optimize-at test-blocks-1 (block `(for i 0 10 (?? block)) 4 `(pp i)) (lambda (c) 'cool) #f)
;;; -> ((for i 0 10 (cool (pp "hey"))))

;;; Ensures if we match 1 block but not its contents, we still keep matching
(define test-blocks-2
  `((for i 0 10 (pp "wow"))
    (for i 0 10 (pp i)
                (pp "hey"))))

(optimize-at test-blocks-2 (block `(for i 0 10 (?? block)) 4 `(pp i)) (lambda (c) 'cool) #f)
;;; -> ((for i 0 10 ((pp "wow"))) (for i 0 10 (cool (pp "hey"))))

(define test-blocks-3
  `((for i 0 10
	 (for j 0 10
	      (pp i)))))
(optimize-at test-blocks-3 (block `(for i 0 10 (?? block)) 4
				  (block `(for j 0 10 (?? block)) 4
					 `(pp i)))
	     (lambda (c) 'cool) #f)
;;; -> ((for i 0 10 (for j 0 10 cool)))
	     
;; NTH LOCATIONS
(define test-sequences
  `((for i 0 10
      (pp "wow"))
    (for i 0 10
      (pp "hey"))
    (for i 0 10
      (pp "nice!"))))

(optimize-at test-sequences (nth-location `(for i 0 10 (? block)) 2) (lambda (c) 'cool)
             #f)
;;; -> ((for i 0 10 (pp "wow")) cool (for i 0 10 (pp "nice!")))

;; COMBINED LOCATIONS
(define test-combo-1
  `((for i 0 10
	(for j 0 10
	     (set! x (i j) 0)))
   (for i 0 10
	(for j 0 10
	     (set! y (i j) 0)))
   (for i 0 10
	(for j 0 10
	     (set! z (i j) 0)))))

(optimize-at test-combo-1
	     (nth-location
	      (block `(for i 0 10 (?? block)) 4
		     (block `(for j 0 10 (?? block)) 4
			    `(? anything)))
	      2)
	     (lambda (c)
	       '(cool))
	     #f)

#|
((for i 0 10
      (for j 0 10
	   (set! x (i j) 0)))
 (for i 0 10
      (for j 0 10 cool))
 (for i 0 10
      (for j 0 10
	   (set! z (i j) 0))))
|#
x
(define test-combo-2
  `((for i 0 10
	 (for j 0 10
	      (pp j))
	 (for j 0 10
	      (pp j)))))

(optimize-at test-combo-2
	     (block `(for i 0 10 (?? block)) 4
		    (nth-location
		     (block `(for j 0 10 (?? block)) 4
			    `(?? anything))
		     2))
	     (lambda (c)
	       '(cool))
	     #f)
;;; -> ((for i 0 10 ((for j 0 10 (pp j)) (for j 0 10 cool))))


(optimize-at test-sequences 'i (rename 'i 'j) #t)
;;; -> ((for j 0 10 (pp "wow")) (for j 0 10 (pp "hey")) (for j 0 10 (pp "nice!")))

;; LOOP FUSION
;;; Simple test for correctness
(define lf-test-1
  '((for i 0 10
      ((pp "hi")))
    (for j 0 10
      ((pp "wow")))))

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
;;; -> ((for i 10 15 ((pp i) (pp (+ i (- x 10))))))

;;; Tests if we change the loop variables
(define lf-test-6
  `((for i 0 10
     (pp i))
    (for j 0 10
     (pp j j j))))

(optimize-at lf-test-6 top-level (loop-fuser 'i 'j) #f)
;;; -> ((for i 0 10 ((pp i) (pp i i i))))


;; LOOP TILING
(define lt-test-1
  `(for i 0 10
     (pp i)))
(optimize-at lt-test-1 top-level (loop-tile 5) #f)
;;; -> ((for i 0 2 (for ii (* i 5) (+ (* i 5) 5) (pp ii))))

(define lt-test-2
  `(for i x y
      (pp i)))
(optimize-at lt-test-2 top-level (loop-tile 5) #f)
;;; -> ((for i 0 (/ (- y x) 5) (for ii (* i 5) (+ (* i 5) 5) (pp ii))))


;; LOOP REORDERING
(define lr-test-1
  `(for i 0 10
     ((for j 0 10
        ((for k 0 10
           ((pp i)
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
           ((pp i)
            (pp j)
            (pp k))))))))

(optimize-at lr-test-2 top-level (loop-reorder 'i 'j) #f)
;;; -> ((for j 0 10 ((for z 0 10 (pp z)) (for i 0 10 ((for k 0 10 ((pp i) (pp j) (pp k))))))))

;; CONSTANT PROPAGATION
(join-assignments `((x 5) (y 6) (z 7)) `())
;;; -> ((x 5) (y 6) (z 7))
(join-assignments `() `())
;;; -> ()
(join-assignments `((x 5) (y 6) (z 7)) `((y 6)))
;;; -> ((x 5) (y 6) (z 7))
(join-assignments `((x 5) (y 6) (z 7)) `((y 3) (x 6) (z 7)))
;;; -> ((z 7))

(intersection-assignments `((x 5) (y 6) (z 7)) `())
;;; -> ()
(intersection-assignments `() `())
;;; -> ()
(intersection-assignments `((x 5) (y 6) (z 7)) `((y 6)))
;;; -> ((y 6))
(intersection-assignments `((x 5) (y 6) (z 7)) `((y 6) (x 5) (z 7)))
;;; -> ((x 5) (y 6) (z 7))

(add-entry `() `(x 5))
;;; -> ((x 5))
(add-entry `((x 5) (y 6)) `(z 6))
;;; -> ((z 6) (x 5) (y 6))
(add-entry `((x 5) (y 6)) `(y 7))
;;; -> ((y 7) (x 5))

(delete-key `(((x 1 2) 4) ((x 4 3) 1) ((y 5 4 2) 2)) '(x 1 2))
;;; -> (((x 4 3) 1) ((y 5 4 2) 2))
(delete-key `(((x 1 2) 4) ((x 4 3) 1) ((y 5 4 2) 2)) '(x 1 3))
;;; -> (((x 1 2) 4) ((x 4 3) 1) ((y 5 4 2) 2))


(delete-variable `(((x 1 2) 4) ((x 4 3) 1) ((y 5 4 2) 2)) 'x)
;;; -> (((y 5 4 2) 2))

(delete-variable `(((x 1 2) 4) ((x 4 3) 1) ((y 5 4 2) 2)) 'y)
;;; -> (((x 1 2) 4) ((x 4 3) 1))


(define atr-test-1
  `(ref x 8 0 1))

(optimize-at atr-test-1 `(ref (? var) (?? indices, number?)) (assignments-to-replacer `(((x 8 0 1) 5))) #t)
;;; -> 5


(define atr-test-2
  `(ref x (ref y (ref z 1))))

(optimize-at atr-test-2 `(ref (? var) (?? indices, number?))
             (assignments-to-replacer `(((x 8) 5) ((y 4) 8) ((z 1) 4))) #t)
;;; -> 5

(define atr-test-3
  `(ref x (ref y (ref z 1))))
(optimize-at atr-test-3 `(ref (? var) (?? indices, number?))
             (assignments-to-replacer `(((y 4) 8) ((z 1) 4))) #t)
;;; -> (ref x 8)

(constant-propagation-generic `(ref x) `(((x) 1)))
;;; -> 1
;;; -> (((x) 1)
(constant-propagation-generic `(ref x 1 2 3) `(((x 1 2 3) 1)))
;;; -> 1
;;; -> (((x 1 2 3) 1))


(constant-propagation-generic `(set! x (1 2 3) 4) `())
;;; -> (set! x (1 2 3) 4)
;;; -> (((x 1 2 3) 4))

(constant-propagation-generic `(set! x (1 2 3) 4) `(((x 1 2 3) 1)))
;;; -> (set! x (1 2 3) 4)
;;; -> (((x 1 2 3) 4))

(constant-propagation-generic `(set! x (1 2 3) y) `(((x 1 2 3) 1) ((x 1 2 4) 4)))
;;; -> (set! x (1 2 3) y)
;;; -> (((x 1 2 4) 4))

(constant-propagation-generic `(set! x (1 2 z) 4) `(((x 1 2 3) 1) ((x 3 1 2) 3) ((y 7 2) 4)))
;;; -> (set! x (1 2 z) 4)
;;; -> (((y 7 2) 4))

(constant-propagation-generic `(set! x (1 2 3) y) `(((x 1 2 3) 1)))
;;; -> (set! x (1 2 3) 4)
;;; -> ()

(constant-propagation-generic `(set! x ((ref x 1 2 3) 2 3) 5) `(((x 1 2 3) 1)))
;;; -> (set! x (1 2 3) 5)
;;; -> (((x 1 2 3) 5))

(constant-propagation-generic
 `(if (ref x 1) (set! x (2) 5) (set! x (2) 5))
 `(((x 1) 0) ((x 2) 1)))
;;; -> (if 0 (set! x (2) 5) (set! x (2) 5))
;;; -> (((x 1) 0) ((x 2) 5))

(constant-propagation-generic
 `(for i 0 10 (set! x (i) 0))
 `(((x 3) 5) ((y 5 10) 3)))
;;; -> (for i 0 10 (set! x (i) 0))
;;; -> (((y 5 10) 3))

(constant-propagation-generic
 `(for i 0 10 (set! x (3) 0))
 `(((x 3) 5)))
;;; -> (for i 0 10 (set! x (3) 0))
;;; -> (((x 3) 0))

(constant-propagation-generic
 `(for i 0 5 (set! r () (+ (ref r) (* (ref x (ref i)) (ref y (ref i))))))
 `(((r) 0)))
;;; -> (for i 0 5 (set! r () (+ (ref r) (* (ref x (ref i)) (ref y (ref i))))))
;;; -> ()

(constant-propagation-generic
 `(
   (set! x (1 2 3) 5)
   (set! y () 0)
   )
 `(((z 1) 6) ((x 1 2 3) 4) ((x 3 3 3) 2)))
;;; -> ((set! x (1 2 3) 5) (set! y () 0))
;;; -> (((y) 0) ((x 1 2 3) 5) ((z 1) 6) ((x 3 3 3) 2))

(constant-propagation-generic
 `(
   (for i 0 10
	(set! x (i) 0)
	(set! y () 3))
   (ref y)
   )
 `())
;;; -> ((for i 0 10 (set! x (i) 0) (set! y () 3)) 3)
;;; -> (((y) 3))


(constant-propagation-generic
 `(declare x (ref x 0 1 2) 3 (ref y))
 `(((x 0 1 2) 3)))
;;; -> (declare x 3 3 (ref y))
;;; -> (((x 0 1 2) 3))


(constant-propagation-generic
 `(+ (ref x) (ref y))
 `(((x) 2)))
;;; -> (+ 2 (ref y))
;;; -> (((x) 2))


(pp (optimize-at
 `((declare len-v)
   (set! len-v () 5)
   (declare x (ref len-v))
   (declare y (ref len-v))
   (declare r)
   (set! r () 0)
   (for i 0 (ref len-v)
	(set! r () (+ (ref r)
		      (* (ref x (ref i))
			 (ref y (ref i))))))
   (return r))
 top-level
 constant-propagation-optimizer
 #f))
#|
((declare len-v)
 (set! len-v () 5)
 (declare x 5)
 (declare y 5)
 (declare r)
 (set! r () 0)
 (for i 0 5
      (set! r ()
	    (+ (ref r)
	       (*
		(ref x (ref i))
		(ref y (ref i))))))
(return r))
|#


;; EXAMPLE REAL WORLD TEST
(define demo-test
  `(
    (declare a 5)
    (set! a (0) 1)
    (set! a (1) 2)
    (set! a (2) 3)
    (set! a (3) 4)
    (set! a (4) 5)
    
    (declare b 5)
    (set! b (0) 1)
    (set! b (1) 0)
    (set! b (2) 1)
    (set! b (3) 0)
    (set! b (4) 1)
    
    (declare x 4)
    (set! x (0) 5)
    (set! x (1) 8)
    (set! x (2) 4)
    (set! x (3) 1)
    
    (declare y 4)
    (set! y (0) 4)
    (set! y (1) -2)
    (set! y (2) 1)
    (set! y (3) 4)
    
    (declare result1)

    (set! result1 () 0)
    (for i 0 (max (length a) (length b))
	 (set! result1 () (+ result1 (* (ref a i) (ref b i)))))
    
    (declare result2)
    
    (set! result2 () 0)
    (for i 0 (max (length x) (length y))
	 (set! result2 () (+ result2 (* (ref x i) (ref y i)))))
    
    (return (* result1 result2))))

(pp (optimize-at demo-test `(for i 0 (? max) (?? body)) (loop-tile 2) #f))

(define after-tile  
  (optimize-at demo-test
	     (nth-location
	      `(for i 0 (? max) (?? body))
	      2)
	     (loop-tile 2)
	     #f))

(define after-rename
  (optimize-at after-tile
	       'b
	       (rename 'b 'include-vec)
	       #t))

(pp after-rename)
