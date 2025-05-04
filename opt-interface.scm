A
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

(define (default-value v d)
  (if v
      v
      d))

(run-matcher
 (match:compile-pattern `(for i 0 10 (? block)))
 `((for i 0 10 ((pp "hey"))) (for i 0 10 (pp "nice!")))
 match:bindings)

(run-matcher
 (match:compile-pattern `((?? a, number?) b))
 `(1 2 3 4 b)
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
(search-tree `((for i 0 10 (pp "hey")) (for i 0 10 (pp "nice!"))) `(for i 0 10 (? block)) true-succ)

;;; Performs a deep copy of a given tree, with transformations
;;; done on the first or all elements matching the given
;;; pattern
(define (transform-tree tree pattern transformer all?)
  ;; (pp "Transform-tree")
  ;; (pp tree)
  ;; (pp pattern)
  (let ((pattern-compiled (match:compile-pattern pattern))
        (matched? #f))
    (define (traverse-tree current)
      ;;      (pp "Traversing")
      ;;      (pp current)
      (if (and matched? (not all?))
          current
          (if (run-matcher pattern-compiled current match:bindings)
              (begin
                ;;      (pp "Transforming")
                (let ((transformed (transformer current)))
                  ;;        (pp "Transformed")
                  ;;        (pp transformed)
                  (if transformed
                      (set! matched? #t))
                  transformed))
              (if (pair? current)
                  (let ((b (traverse-tree (car current)))) ;;; Dig down
             ;;; Need two lets since the traversal of r
             ;;; depends on b (via matched?)
                    (let ((r (traverse-tree (cdr current)))) ;;; Dig across
               ;;; Final check to allow recursive transformation, i.e. transform
               ;;; a transformed section if it now matches. Useful for constant
               ;;; propogation
                      ;;            (pp "attempted to transform tree")
                      (let ((final (cons (if b b (car current)) (if r r (cdr current)))))
                        ;;          (pp "Transformed tree")
                        ;;          (pp final)
                        ;;          (pp all?)
                        ;;          (pp (run-matcher pattern-compiled final match:bindings))
                        (if (and all? (run-matcher pattern-compiled final match:bindings))
                            (begin
                              ;;(pp "Transforming final tree")
                              ;;                (pp (transformer final))
                              (transformer final))
                            final)))))


              current))
      (and matched? (traverse-tree tree)))))

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
(define (any v)
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

(optimize-at test-sequences (nth-location `(for i 0 10 (? block)) 2) (lambda (c) 'cool)
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
                               (sublist b (+ scope-index 1) (length b)))
                              (optimized
                               (optimize-at (list-ref b scope-index) inner-pattern optimizer all?)))
                          (if optimized
                              (append list-left
                                      (cons
                                       optimized
                                       list-right))
                              #f)))
                      #f))))

(define top-level
  `(? code))

;;; Simple block test
(define test-blocks-1
  `((for i 0 10 ((pp i)
                 (pp "hey")))))

(optimize-at test-blocks-1 (block `(for i 0 10 (? block)) 4 `(pp i)) (lambda (c) 'cool) #f)

;;; Ensures if we match 1 block but not its contents, we still keep matching
(define test-blocks-2
  `((for i 0 10 ((pp "wow")))
    (for i 0 10 ((pp i)
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
                           ,(default-value (optimize-at body-2 loop-2 (rename loop-2 loop-1) #t) body-2)))
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
                           ,(default-value
                              (optimize-at body-2 loop-2
                                           (rename loop-2 `(+ ,loop-1 ,(- bound-low-2 bound-low-1)))
                                           #t)
                              body-2)))
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
                           ,(default-value
                              (optimize-at body-2 loop-2
                                           (rename loop-2 `(+ ,loop-1 (- ,bound-low-2 ,bound-low-1)))
                                           #t)
                              body-2)))
                        ,@b
                        ,@c)))

  optimizer)


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

(define (loop-tile tile-factor)
  (define optimizer (make-pattern-operator))
  ;;; Numeric bounds
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

  ;;; Symbolic bounds
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
      (let ((bound-low-o (caddr outer-loop))
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

(define (join-assignments assi-1 assi-2)
  (pp "join assignments")
  (pp assi-1)
  (pp assi-2)
  (if (= (length assi-1) 0)
      assi-2
      (if (= (length assi-2) 0)
          assi-1
          (let* ((entry (car assi-1))
                 (entry-key (car entry))
                 (entry-value (cadr entry))
                 (2-entry (
                           (member-procedure
                            (lambda (v o)
                              (equal? (car o) v))) entry-key assi-2)))
            (pp 2-entry)
            (if 2-entry
                (begin
                  (pp "Checking remainder")
                  (let ((remainder
                         (join-assignments (cdr assi-1)
                                           (delete (car 2-entry) assi-2))))
                    (if (= (cadar 2-entry) entry-value)
                        (cons entry remainder)
                        remainder)))
                (cons entry (join-assignments (cdr assi-1) assi-2)))))))

(join-assignments `((x 5) (y 6) (z 7)) `())
;;; -> ((x 5) (y 6) (z 7))
(join-assignments `() `())
;;; -> ()
(join-assignments `((x 5) (y 6) (z 7)) `((y 6)))
;;; -> ((x 5) (y 6) (z 7))
(join-assignments `((x 5) (y 6) (z 7)) `((y 3) (x 6) (z 7)))
;;; -> ((z 7))

(define (intersection-assignments assi-1 assi-2)
  (if (= (length assi-1) 0)
      `()
      (if (= (length assi-2) 0)
          `()
          (let* ((entry (car assi-1))
                 (entry-key (car entry))
                 (entry-value (cadr entry))
                 (2-entry ((member-procedure
                            (lambda (v o)
                              (equal? (car o) v))) entry-key assi-2)))
            (if 2-entry
                (begin
                  (pp "Checking remainder")
                  (let ((remainder
                         (intersection-assignments
                          (cdr assi-1)
                          (delete (car 2-entry) assi-2))))
                    (if (= (cadar 2-entry) entry-value)
                        (cons entry remainder)
                        remainder)))
                (intersection-assignments (cdr assi-1) assi-2))))))

(intersection-assignments `((x 5) (y 6) (z 7)) `())
;;; -> ()
(intersection-assignments `() `())
;;; -> ()
(intersection-assignments `((x 5) (y 6) (z 7)) `((y 6)))
;;; -> ((y 6))
(intersection-assignments `((x 5) (y 6) (z 7)) `((y 6) (x 5) (z 7)))
;;; -> ((x 5) (y 6) (z 7))

(define (add-entry assignments entry)
  (let* ((entry-key (car entry))
         (entry-value (cadr entry))
         (assignment-entry (
                            (member-procedure (lambda (v o)
                                                (equal? (car o) v))) entry-key assignments)))
    (if assignment-entry
        (cons entry (delete (car assignment-entry) assignments))
        (cons entry assignments))))

(add-entry `() `(x 5))
;;; -> ((x 5))
(add-entry `((x 5) (y 6)) `(z 6))
;;; -> ((z 6) (x 5) (y 6))
(add-entry `((x 5) (y 6)) `(y 7))
;;; -> ((y 7) (x 5))

(define (delete-key assignments key)
  ((list-deletor
    (lambda (e)
      (equal? (car e) key)))
   assignments))
(delete-key `(((x 1 2) 4) ((x 4 3) 1) ((y 5 4 2) 2)) '(x 1 2))
;;; -> (((x 4 3) 1) ((y 5 4 2) 2))
(delete-key `(((x 1 2) 4) ((x 4 3) 1) ((y 5 4 2) 2)) '(x 1 3))
;;; -> (((x 1 2) 4) ((x 4 3) 1) ((y 5 4 2) 2))

(define (delete-variable assignments var)
  ((list-deletor
    (lambda (e)
      (equal? (caar e) var)))
   assignments))

(delete-variable `(((x 1 2) 4) ((x 4 3) 1) ((y 5 4 2) 2)) 'x)
;;; -> (((y 5 4 2) 2))

(delete-variable `(((x 1 2) 4) ((x 4 3) 1) ((y 5 4 2) 2)) 'y)
;;; -> (((x 1 2) 4) ((x 4 3) 1))


(define (assignments-to-replacer assignments)
  (define replacer (make-pattern-operator))
  (for-each
   (lambda (entry)
     (let ((key (car entry))
           (value (cadr entry)))
       (attach-rule! replacer
                     (rule `((ref ,@key))
                           value))))
   assignments)

  (attach-rule! replacer
        (rule `((? val))
              val)) ;; Catch all

  replacer)

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

(optimize-at `(a a a a a b a a b) `((?? a) b) (lambda (c) 'cool) #f)

#|
Expr         := Literal | Variable | Call | Index
Call     := (Op Expr Expr) | (length Expr)
Index        := (ref Variable Expr ...)
Init         := (declare Variable Expr ...)
Assignment  := (set! Variable (Expr ...) Expr)
Statement   := Read | Write | Assignment | Init | Conditional | Loop
Read         := (read Variable Number)
Write        := (write Expr)
Conditional := (if Expr (Statement ...) (Statement ...))
Loop         := (for Variable Expr_min Expr_max Statement ...)
|#


(define (ref-statement? s)
  (and (pair? s)
       (equal? (car s) 'ref)))

(define (init-statement? s)
  (and (pair? s)
       (equal? (car s) 'declare)))

(define (set-statement? s)
  (and (pair? s)
       (equal? (car s) `set!)))

(define (read-statement? s)
  (and (pair? s)
       (equal? (car s) `read)))

(define (write-statement? s)
  (and (pair? s)
       (equal? (car s) 'write)))

(define (cond-statement s)
  (and (pair? s)
       (equal? (car s) 'if)))

(define (loop-statement s)
  (and (pair? s)
       (equal? (car s) 'for)))

(define (generic-cp c a)
  (values c a))

;;; Takes in a code segment and our assignment state
(define constant-propagation-generic
  (simple-generic-procedure 'constant-propagation 2 generic-cp))

(define-generic-procedure-handler constant-propagation-generic
  (match-args ref-statement? any)
  (lambda (code assignments)
    (values (optimize-at code
                         `(ref (? var) (?? indices, number?))
                         (assignments-to-replacer assignments) #t)
            assignments)))

(constant-propagation-generic `(ref x) `(((x) 1)))
(constant-propagation-generic `(ref x 1 2 3) `(((x 1 2 3) 1)))

(define-generic-procedure-handler constant-propagation-generic
  (match-args set-statement? any)
  (lambda (code assignments)
    (pp "CP for set ops")
    (define cp-optimizer (make-pattern-operator))
    (attach-rule! cp-optimizer
                  (rule `((set! (? var) ((?? index-exprs)) (? value-expr)))
                        (let ((indices-processed
                               (map
                                (lambda (e)
                                  (let-values (((c a) (constant-propagation-generic e assignments)))
                                    c))
                                index-exprs))
                              (value-processed
                               (let-values (((c a) (constant-propagation-generic value-expr assignments)))
                                 c)))
                          (pp "Returning from cp opt")
                          `(set! ,var (,@indices-processed) ,value-processed))))

    (define assignment-optimizer (make-pattern-operator))
    ;;; We know what the set is doing and can adjust our assignments accordingly
    (attach-rule!
     assignment-optimizer
     (rule `((set! (? var) ((?? indicies, number?)) (? value, number?)))
           (values `(set! ,var (,@indicies) ,value)
                   (add-entry assignments (list (cons var indicies) value)))))
    ;;; We can't resolve the set's value, but know its index
    (attach-rule!
     assignment-optimizer
     (rule `((set! (? var) ((?? indicies, number?)) (? value)))
           (values `(set! ,var (,@indicies) ,value)
                   (delete-key assignments (cons var indicies)))))
    ;;; We know nothing, and have to throw out all the variables assignments
    (attach-rule!
     assignment-optimizer
     (rule `((set! (? var) ((?? indicies)) (? value)))
           (values `(set! ,var (,@indicies) ,value)
                   (delete-variable assignments var))))

    (assignment-optimizer (cp-optimizer code))))

(constant-propagation-generic `(set! x (1 2 3) 4) `())
;;; -> (set! x (1 2 3) 4)
;;; -> (((x 1 2 3) 4))

(constant-propagation-generic `(set! x (1 2 3) 4) `(((x 1 2 3) 1)))
;;; -> (set! x (1 2 3) 4)
;;; -> (((x 1 2 3) 4))

(constant-propagation-generic `(set! x (1 2 3) y) `(((x 1 2 3) 1)))
;;; -> (set! x (1 2 3) y)
;;; -> ()

(constant-propagation-generic `(set! x (1 2 z) 4) `(((x 1 2 3) 1) ((x 3 1 2) 3) ((y 7 2) 4)))
;;; -> (set! x (1 2 z) 4)
;;; -> (((y 7 2) 4))

(constant-propagation-generic `(set! x (1 2 3) y) `(((x 1 2 3) 1)))
;;; -> (set! x (1 2 3) 4)
;;; -> ()

(constant-propagation-generic `(set! x ((ref x 1 2 3) 2 3) 5) `(((x 1 2 3) 1)))
;;; -> (set! x (1 2 3) 5)
;;; -> (((x 1 2 3) 5))

;;; (write Expr)
(define (write-statement? s)
  (and (pair? s)
       (equal? (car s) 'write)))

(define-generic-procedure-handler constant-propagation-generic
  (match-args write-statement? any)
  (lambda (c a)
    (let-values (((c-new a-new) (constant-propagation-generic (cadr c) a)))
      (values `(write ,c-new) a-new))))

(constant-propagation-generic `(write (ref x 1 2 3)) `(((x 1 2 3) 5)))
;;; -> (write 5)
;;; -> (((x 1 2 3) 5))

;;; (if Expr (statement ...) (statement ...))
(define (cond-statement? s)
  (and (pair? s)
       (equal? (car s) 'if)))

(define-generic-procedure-handler constant-propagation-generic
  (match-args cond-statement? any)
  (lambda (code assignments)
    (define cp-optimizer (make-pattern-operator))
    (attach-rule!
     cp-optimizer
     (rule `((if (? cond-expr) (? t-s) (? f-s)))
           (let-values
               (((cond-c cond-a) (constant-propagation-generic cond-expr assignments)))
             (let-values
                 (((t-c t-a) (constant-propagation-generic t-s cond-a))
                  ((f-c f-a) (constant-propagation-generic f-s cond-a)))
               (values `(if ,cond-c ,t-c ,f-c)
                       (intersection-assignments t-a f-a))))))

    (cp-optimizer code)))

(constant-propagation-generic
 `(if (ref x 1) (write (ref x 2)) (write (ref x 3)))
 `(((x 1) 0) ((x 2) 1)))
;;; -> (if 0 (write 1) (write (ref x 3)))
;;; -> (((x 1) 0) ((x 2) 1))

(constant-propagation-generic
 `(if (ref x 1) (set! x (2) 5) (write (ref x 3)))
 `(((x 1) 0) ((x 2) 1)))
;;; -> (if 0 (set! x (2) 5) (write (ref x 3)))
;;; -> (((x 1) 0))

(constant-propagation-generic
 `(if (ref x 1) (set! x (2) 5) (set! x (2) 5))
 `(((x 1) 0) ((x 2) 1)))
;;; -> (if 0 (set! x (2) 5) (set! x (2) 5))
;;; -> (((x 1) 0) ((x 2) 5))

;;; (for Variable Expr_min Expr_max Statement ...)
(define (loop-statement? s)
  (and (pair? s)
       (equal? (car s) 'for)))

(define-generic-procedure-handler constant-propagation-generic
  (match-args loop-statement? any)
  (lambda (code assignments)
    (define cp-optimizer (make-pattern-operator))
    (attach-rule!
     cp-optimizer
     (rule `((for (? loop-var) (? min) (? max) (? loop-body)))
           (let-values
               (((min-c min-a) (constant-propagation-generic min assignments)
                 (let-values)
                 (((max-c max-a) (constant-propagation-generic max min-a)
                   (let-values)
                   (((loop-c loop-a) (constant-propagation-generic loop-body max-a)))))
                 (values `(for ,loop-var ,min-c ,max-c ,loop-c)
                              loop-a))))))

    (let loop ((c code) (a assignments))
      (let-values
          (((code-new assi-new) (cp-optimizer c)))
        (if (equal? a assi-new)
            (values code-new assi-new)
            (loop code-new assi-new))))))

(constant-propagation-generic
 `(for i 0 (ref x) (write (ref y 5 10)))
 `(((x) 5) ((y 5 10) 3)))
;;; -> (for i 0 5 (write 3))
;;; -> (((x) 5) ((y 5 10) 3))

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
