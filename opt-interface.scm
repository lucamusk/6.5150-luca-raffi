(manage 'add 'term)
(manage 'add 'design)
(manage 'add 'unification)
(load "./utils.scm")

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
      (if (run-matcher pattern-compiled current match:bindings)
	  (succ tree current)
          (if (or (not (pair? current)) (= 0 (length current)))
              #f
              (let ((v (traverse-tree (car current)))) ;;; Dig down
		(if (equal? v #f)
                    (traverse-tree (cdr current))
                    v))))) ;;; Dig across
    (traverse-tree tree)))

;;; Performs a deep copy of a given tree, with transformations
;;; done on the first or all elements matching the given
;;; pattern
(define (transform-tree tree pattern count transformer all?)
  (let ((pattern-compiled (match:compile-pattern pattern))
        (matched? #f))
    (define (handle-no-match current)
      (if (pair? current)
          (let ((b (traverse-tree (car current)))) ;;; Dig down
            ;;; Need two lets since the traversal of r
            ;;; depends on b (via matched?)
            (let ((r (traverse-tree (cdr current)))) ;;; Dig across
              ;;; Final check to allow recursive transformation, i.e. transform
              ;;; a transformed section if it now matches. Useful for constant
              ;;; propogation
              (let ((final (cons (if b b (car current)) (if r r (cdr current)))))
                (if (and all? (run-matcher pattern-compiled final match:bindings))
                    (begin
                      (transformer final))
                    final))))
	  current))
    (define (traverse-tree current)
      (if (and matched? (not all?))
          current
          (if (run-matcher pattern-compiled current match:bindings)
	      (if (<= count 1)
		  (begin
                    (let ((transformed (transformer current)))
                      (if transformed
			  (set! matched? #t))
                      transformed))
		  (begin
		    (set! count (- count 1))
		    (handle-no-match current)))
	      (handle-no-match current))))
    ;;; Need the let statement, otherwise we see matched? is false and short circuit.
    (let ((result (traverse-tree tree)))
      (and matched? result))))

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

;;; Assumes no special handling for the matching code segment
(define (generic-optimize-at code location count optimizer all?)
  (transform-tree code location count optimizer all?))

(define (debug-opt code)
  (pp code)
  code)

(define optimize-at-generic
  (simple-generic-procedure `optimize-at 5 generic-optimize-at))

(define (optimize-at code location optimizer all?)
  (optimize-at-generic code location 1 optimizer all?))

;;; Block locations
(define-generic-procedure-handler optimize-at-generic
  (match-args any-object? block? number? procedure? boolean?)
  (lambda (code location count optimizer all?)
    (pp "Optimizing a block")
    (let ((target-block (block-pattern location))
          (scope-index (block-scope-index location))
          (inner-pattern (block-inner-pattern location)))
      (optimize-at-generic code target-block count
                   (lambda (b)
		     (pp "block found")
                     (pp (list-tail b scope-index))
                     (let ((list-left
                            (if (= scope-index 0)
                                '()
                                (sublist b 0 scope-index)))
                           (list-right
                            (sublist b (+ scope-index 1) (length b)))
                           (optimized
                            (optimize-at (list-tail b scope-index) inner-pattern optimizer all?)))
		       (pp "Optiization done")
		       (pp optimized)
                       (if optimized
                           (append list-left
                                    optimized)
                           #f)))
                   #f))))
o
(define top-level
  `(? code))

;;; Now we do sequences. This is made far easier since
;;; transform tree can be given a count to match on.
(define-generic-procedure-handler optimize-at-generic
  (match-args any-object? nth-location? number? procedure? boolean?)
  (lambda (code location count optimizer all?)
    (let ((n (nth-location-count location))
          (target (nth-location-target location)))
      (pp "Optimizing an nth location")
      (pp code)
      (pp location)
      (optimize-at-generic code target n optimizer all?))))

(define (rename var new)
  (define optimizer (make-pattern-operator))
  (attach-rule! optimizer
        (rule `(,var)
              `,new))
  optimizer)

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
                           ,(or (optimize-at body-2 loop-2 (rename loop-2 loop-1) #t) body-2)))
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
                           ,(or
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
                           ,(or
                              (optimize-at body-2 loop-2
                                           (rename loop-2 `(+ ,loop-1 (- ,bound-low-2 ,bound-low-1)))
                                           #t)
                              body-2)))
                        ,@b
                        ,@c)))

  optimizer)

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

(define (add-entry assignments entry)
  (let* ((entry-key (car entry))
         (entry-value (cadr entry))
         (assignment-entry (
                            (member-procedure (lambda (v o)
                                                (equal? (car o) v))) entry-key assignments)))
    (if assignment-entry
        (cons entry (delete (car assignment-entry) assignments))
        (cons entry assignments))))

(define (delete-key assignments key)
  ((list-deletor
    (lambda (e)
      (equal? (car e) key)))
   assignments))

(define (delete-variable assignments var)
  ((list-deletor
    (lambda (e)
      (equal? (caar e) var)))
   assignments))

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

#|
Call     := (Op Expr Expr) | (length Expr)
Index        := (ref Variable Expr ...)
Init         := (declare Variable Expr ...)
Assignment  := (set! Variable (Expr ...) Expr)
Read         := (read Variable Number)
Write        := (write Expr)
Conditional := (if Expr (Statement ...) (Statement ...))
Loop         := (for Variable Expr_min Expr_max Statement ...)
|#

(define (generic-cp code assignments)
  (values code assignments))

;;; Takes in a code segment and our assignment state
(define constant-propagation-generic
  (simple-generic-procedure 'constant-propagation 2 generic-cp))

(define-generic-procedure-handler constant-propagation-generic
  (match-args (form 'ref) any-object?)
  (lambda (code assignments)
    (values (optimize-at code
                         `(ref (? var) (?? indices, number?))
                         (assignments-to-replacer assignments) #t)
            assignments)))

(define-generic-procedure-handler constant-propagation-generic
  (match-args (form 'set!) any-object?)
  (lambda (code assignments)
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

;;; (write Expr)
(define-generic-procedure-handler constant-propagation-generic
  (match-args (form 'write) any-object?)
  (lambda (c a)
    (let-values (((c-new a-new) (constant-propagation-generic (cadr c) a)))
      (values `(write ,c-new) a-new))))

;;; (if Expr (statement ...) (statement ...))
(define-generic-procedure-handler constant-propagation-generic
  (match-args (form 'if) any-object?)
e  (lambda (code assignments)
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

;;; (for Variable Expr_min Expr_max Statement ...)
(define-generic-procedure-handler constant-propagation-generic
  (match-args (form 'for) any-object?)
  (lambda (code assignments)
    (pp "Loop called")
    (pp assignments)
    (let loop ((a assignments))
      (pp "Loop pass")
      (pp a)
      (define cp-optimizer (make-pattern-operator))
      (attach-rule!
       cp-optimizer
       (rule `((for (? loop-var) (? min) (? max) (?? loop-body)))
             (let-values
		 (((min-c min-a) (constant-propagation-generic min a)))
               (let-values
                   (((max-c max-a) (constant-propagation-generic max min-a)))
		 (let-values
                     (((loop-c loop-a) (constant-propagation-generic loop-body max-a)))
                   (values `(for ,loop-var ,min-c ,max-c ,@loop-c)
                           loop-a))))))

      (let-values
	  ;;; Only optimize our old code each pass
          (((code-new assi-new) (cp-optimizer code)))
	(pp "Loop optimized")
	(pp code-new)
	(pp assi-new)
        (if (equal? a assi-new)
            (values code-new assi-new)
            (loop assi-new))))))

(define (statement-list? l)
  (and
   (pair? l)
   (not (symbol? (car l)))))

;;; Catches lists of statements
(define-generic-procedure-handler constant-propagation-generic
  (match-args statement-list? any-object?)
  (lambda (code assignments)
    (pp "Statement list")
    (pp code)
    (pp assignments)
    ;;; There's probably a nice way to do this with folds but
    ;;; handling the multiple values seemed to complicate it
    (let-values
	(((c a) (constant-propagation-generic (car code) assignments)))
      (if (= (length code) 1)
	;;; Just 1 statement, compile it
	  (values `(,c) a)
	;;; Otherwise, pipeline to next statement
	  (let-values
	      (((c-remaining a-remaining) (constant-propagation-generic (cdr code) a)))
	    (values (cons c c-remaining) a-remaining))))))

;;; (declare Variable Expr ...)
(define-generic-procedure-handler constant-propagation-generic
  (match-args (form 'declare) any-object?)
  (lambda (c a)
    (pp (cddr c))
    (let-values (((c-new a-new) (constant-propagation-generic (cddr c) a)))
      (values `(declare ,(cadr c) ,@c-new) a-new))))

(define reserved-keywords
  `(read write if for set! declare ref))

(define (call? l)
  (and
   (pair? l)
   (not (pair? (car l)))
   (not (there-exists? reserved-keywords (lambda (k) (equal? k (car l)))))))

;;; (call Expr ...)
(define-generic-procedure-handler constant-propagation-generic
  (match-args call? any-object?)
  (lambda (c a)
    (pp "call expr")
    (pp c)
    (let-values (((c-new a-new) (constant-propagation-generic (cdr c) a)))
      (values `(,(car c) ,@c-new) a-new))))

(define constant-propagation-optimizer
  (lambda (code)
    (let-values
	(((c a) (constant-propagation-generic code '())))
      c)))
o
