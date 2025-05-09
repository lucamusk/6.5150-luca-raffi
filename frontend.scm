(load "./utils")
(load "./test")

#|
This file implements the compilation of the high-level SCARY language
into its IR. I try to keep things generic where I can.

I use the term "term" to represent any object which represents code
and the term "expression" to represent any object which is an actual `Expr`
as defined by the grammar.
|#

;;; Syntactic classes

;; Primitive numeric operation
(define-generic (primop? op)
  (memq op '(+ * - / min max < > = %)) #t)

(register-predicate! primop? 'primop)

;; Uniform operators are operators which have no non-Expr arguments
(define-generic (uniform-operator? op)
  (or (primop? op) (memq '(length ref array if))))

(register-predicate! uniform-operator? 'uniform-operator)

;; Operators whose first argument is a list of Variable/Expr pairs,
;; then a subexpression. Should mean something along the lines of "bind these variables
;; using these subexpressions within this body"
(define-generic (binder? op)
  (memq op '(let* compute)))

(register-predicate! binder? 'binder)

;;; Generic traversals

;; The immediate subexpressions of an expression
(define-generic (subexpressions expr))

(define-method (subexpressions (expr atom?))
  '())

(define-method (subexpressions (expr (form uniform-operator?)))
  (cdr expr))

(define-method (subexpressions (expr (form binder?)))
  (append (map second (second expr)) (list (third expr))))

(define-method (subexpressions (expr (form 'fold)))
  (cddr expr))

(define-method (subexpressions (expr (form 'empty)))
  (list))

;; Replaces an expressions's subexpressions with the ones provided
(define-generic (replace-subexpressions expr new-subexprs))

(define-method (replace-subexpressions (expr atom?) (_ list?))
  expr)

(define-method (replace-subexpressions (expr (form uniform-operator?)) (subexprs list?))
  (cons (first expr) subexprs))

(define-method (replace-subexpressions (expr (form binder?)) (subexprs list?))
  (define-values (bindings body) (rdc+rac subexprs))
  (assert (length=? bindings (second expr)))
  (list (first expr)
        (map (lambda (bnd sexpr) (list (first bnd) sexpr))
             (second expr)
             bindings)
        body))

(define-method (replace-subexpressions (expr (form 'fold)) (subexprs list?))
  (append (take expr 2) subexprs))

(define-method (replace-subexpressions (expr (form 'empty)) (subexprs list?))
  (assert (null? subexprs))
  expr)

;;; Transforms over the syntax tree

;; Expands binders into single bindings
(define expand-binders
  (make-tree-walker 'expand-binders
                    (list subexpressions)
                    (lambda (rs as) (replace-subexpressions as rs))))

(define-method (expand-binders (expr (form binder?)))
  (pmatch expr
    (((? op) (? bindings) (? body))
     (if (null? bindings)
         body
         `(,op (,(car bindings))
               ,(expand-binders
                 `(,op ,(cdr bindings)
                       ,body)))))))

;; Helper: folds an operation over a list of values
(define (many-op op lst)
  (fold-left (cut list op <> <>) (car lst) (cdr lst)))

(define (simple-gensym)
  (define counter 0)
  (lambda (base)
    (set! counter (+ 1 counter))
    (string->symbol (format #f "~a~a" base counter))))

(define *gensym-impl* (make-settable-parameter generate-uninterned-symbol))
(define (gensym name) ((*gensym-impl*) name))

;; Helper: If SUBTERMS/RANKS of OP have expected ranks more than EXPECTED,
;; then wrap them in COMPUTE blocks until they don't.
;; Returns a wrapped expression and how many layers
;; of wrapping were performed.
(define ((lift-all expr expected) subexprs/ranks)
  (assert (length=? expected subexprs/ranks))
  (if (every (lambda (e s/r) (or (not e) (= e (second s/r)))) expected subexprs/ranks)
      (list (replace-subexpressions expr (map first subexprs/ranks))
            0)
      (let ((index-var (gensym 'index)))
        (define new-subexprs/ranks
          (map (lambda (ex s/r)
                 (define-list (s r) s/r)
                 (if (or (not ex) (= ex r))
                     s/r
                     `((ref ,s ,index-var) ,(- r 1))))
               expected
               subexprs/ranks))
        (define lifting-over
          (filter-map (lambda (ex s/r)
                        (and ex
                             (not (= ex (second s/r)))
                             (first s/r)))
                      expected
                      subexprs/ranks))
        (define-list (body rank) ((lift-all expr expected) new-subexprs/ranks))
        (list `(compute ((,index-var ,(many-op 'max (map (cut list 'length <>) lifting-over))))
                 ,body)
              (+ 1 rank)))))

#;
(define-test lift-all-smoke-test
  (define-list (new-expr rank) ((lift-all '(+ 1 1) '(0 0)) '((1 0) (1 0))))
  (check (equal? new-expr '(+ 1 1)))
  (check (equal? rank 0)))

#;
(define-test lift-all-ops
  (parameterize ((*gensym-impl* (simple-gensym)))
    (define lifter (lift-all '(+ xs ys) '(0 0)))
    (define-list (new-expr rank) (lifter '((xs 1) (ys 2))))
    (equal? rank 2)
    (equal? new-expr '(compute ((index1 (max (length xs) (length ys))))
                        (compute ((index2 (length (ref ys index1))))
                          (+ (ref xs index1) (ref (ref ys index1) index2)))))))

(define-generic (implicitly-lift recursive-results expr assoc)
  (define op (first expr))
  (define expected (make-list (length recursive-results) 0))
  (define lifted-code/added-ranks ((lift-all expr expected) recursive-results))
  (list (first lifted-code/added-ranks) (second lifted-code/added-ranks)))

(define explicitly-lift
  (make-tree-walker 'explicitly-lift
                    (list subexpressions repeat)
                    implicitly-lift))

(define-method (explicitly-lift (expr number?) (_ list?))
  (list expr 0))

(define-method (explicitly-lift (expr symbol?) (ranks list?))
  (cond
   ((assq expr ranks) => (lambda (a.b) (list (car a.b) (cdr a.b))))
   (else (error "Use of unbound variable:" expr))))

(define-method (explicitly-lift (expr (form 'empty)) (_ list?))
  (list expr (second expr)))

(define-method (implicitly-lift (results list?) (expr (form 'length)) (ranks list?))
  (define-list (subexpr rank) (first results))
  (unless (positive? rank)
    (error "Cannot take the rank of a scalar" expr))
  (list `(length ,subexpr) 0))

(define-method (implicitly-lift (subexprs/ranks list?) (expr (form 'array)) (ranks list?))
  (unless (every (lambda (x) (= (second x) (second (first subexprs/ranks))))
                 (cdr subexprs/ranks))
    (error "Cannot encode a jagged array" expr))
  (list `(array ,@(map first subexprs/ranks)) (+ 1 (second (first subexprs/ranks)))))

(define-method (implicitly-lift (subexprs/ranks list?) (expr (form 'ref)) (ranks list?))
  ((lift-all expr '(1 0)) subexprs/ranks))

(define-method (implicitly-lift (subexprs/ranks list?) (expr (form 'fold)) (ranks list?))
  ((lift-all expr '(0 1)) subexprs/ranks))

(define-method (explicitly-lift (expr (form 'compute)) (ranks list?))
  (pmatch expr
    ((compute ((?? bindings)) (? body))
     (define new-ranks (append (map (lambda (bd) (cons (first bd) 0))
                                    bindings)
                               ranks))
     (define-list (body-lifted body-rank) (explicitly-lift body new-ranks))
     (define values-lifted/values-ranks (map (compose (cut explicitly-lift <> ranks) second)
                                             bindings))
     (unless (every (compose zero? second) values-lifted/values-ranks)
       (error "Every bound on a COMPUTE block must be a scalar:" expr))
     (list (replace-subexpressions expr (append (map first values-lifted/values-ranks) (list body-lifted)))
           (+ 1 body-rank)))))

(define-method (explicitly-lift (expr (form 'let*)) (ranks list?))
  (pmatch expr
    ((let* (((? var) (? val))) (? body))
     (define-list (val-lifted val-rank) (explicitly-lift val ranks))
     (define new-ranks (cons (cons var val-rank) ranks))
     (define-list (body-lifted body-rank) (explicitly-lift body new-ranks))
     `((let* ((,var ,val-lifted)) ,body-lifted) ,body-rank))
    ((let* ((?? binding)) (? body))
     (explicitly-lift (expand-binders expr) ranks))))

(define-test lift-trivial
  (check (equal?
          (explicitly-lift 1 '())
          '(1 0)))
  (check (equal?
          (explicitly-lift '(array 1 2 3) '())
          '((array 1 2 3) 1)))
  (check (equal?
          (explicitly-lift '(+ 1 2) '())
          '((+ 1 2) 0))))

(define-test lift-basic
  (parameterize ((*gensym-impl* (simple-gensym)))
    (check (equal?
            (explicitly-lift '(+ (array 1 2 3) (array 4 5 6)) '())
            '((compute ((index1 (max (length (array 1 2 3)) (length (array 4 5 6)))))
                (+ (ref (array 1 2 3) index1)
                   (ref (array 4 5 6) index1)))
              1)))

    (check (equal?
            (explicitly-lift '(+ 9 (array 4 5 6)) '())
            '((compute ((index2 (length (array 4 5 6))))
                (+ 9 (ref (array 4 5 6) index2)))
              1)))))

(define-test lift-advanced
  (parameterize ((*gensym-impl* (simple-gensym)))
    (check (equal?
            (explicitly-lift '(compute ((x (array 1 2 3))
                                        (y (array 1 3 9)))
                                (+ y y))
                             '())
            '((compute ((index1 (max (length (array 1 2 3)) (length (array 1 3 9)))))
                (compute ((x (ref (array 1 2 3) index1))
                          (y (ref (array 1 3 9) index1)))
                  (+ y y)))
              2)))

    (check (equal?
            (explicitly-lift '(let* ((x (array 1 2 3))
                                     (y (array x x x)))
                                (+ y y))
                             '())
            '((let* ((x (array 1 2 3)))
                (let* ((y (array x x x)))
                  (compute ((index2 (max (length y) (length y))))
                    (compute ((index3 (max (length (ref y index2)) (length (ref y index2)))))
                      (+ (ref (ref y index2) index3)
                         (ref (ref y index2) index3))))))
              2)))))

(define (rank-of expr bindings)
  (cond
   ((number? expr) 0)
   ((symbol? expr) (cdr (assq expr bindings)))
   (else (case (car expr)
           ((array)   (+ 1 (rank-of (second expr) bindings)))
           ((compute) (+ (length (second expr)) (rank-of (third expr) bindings)))
           ((let*)    (rank-of (third expr)
                               (cons (cons (first (first (second expr)))
                                           (rank-of (second (first (second expr))) bindings))
                                     bindings)))
           ((ref)     (- (rank-of (second expr) bindings) 1))
           (else      0)))))

(define (scary:ref src indicies)
  (if (null? indicies)
      src
      `(ref ,src . ,indicies)))

(define *statements*  (make-settable-parameter '(())))
(define *declarations* (make-settable-parameter '(())))

(define (push-to-scope! obj scope-param)
  (define-values (f b) (car+cdr (scope-param)))
  (scope-param (cons (cons obj f) b)))

(define (emit! obj)
  (when ((form 'declare) obj)
    (push-to-scope! (cons (second obj) (cddr obj)) *declarations*))
  (push-to-scope! obj *statements*))

(define (with-new-scope thunk)
  (parameterize ((*statements*  (cons '() (*statements*)))
                 (*declarations* (cons '() (*declarations*))))
    (thunk)))

(define (emit-for! low high cont)
  (cond
   ((not (and (number? low)
              (number? high)
              (<= (- high low) 1)))
    (let ((sym (gensym 'for-index)))
      (let ((resulting-statements
             (with-new-scope
              (lambda ()
                (push-to-scope! `(,sym . ()) *declarations*)
                (cont sym)
                (car (*statements*))))))
        (emit! `(for ,sym ,low ,high ,@(reverse resulting-statements))))))
   ((>= low high)      #t)
   ((= low (+ 1 high)) (with-new-scope (lambda () (cont low))))))

(define (copy-values! dimensions destination-indicies indicies source destination)
  (if (null? dimensions)
      (emit! `(set! ,destination (,@destination-indicies ,@indicies) ,(scary:ref source indicies)))
      (emit-for! 0
                 (car dimensions)
                 (lambda (var)
                   (copy-values! (cdr dimensions)
                                 destination-indicies
                                 (cons var indicies)
                                 source destination)))))

(define (lookup var)
  (let loop ((decls (*declarations*)))
    (cond
     ((null? decls) (error "Use of unbound variable:" var))
     ((assq var (car decls)) => cdr)
     (else (loop (cdr decls))))))

(define ((malloc-exactly size index var prefix) request)
  (check (equal? size request))
  (values var (append prefix index)))

(define ((malloc-whatever where) amount)
  (define amount-vars (map compile! amount))
  (emit! `(declare ,where . ,amount-vars))
  (values where '()))

(define (compile! expr)
  (define result-var (gensym 'result))
  (compile-ir! expr (malloc-whatever result-var))
  result-var)

;; This should be rewritten as generic procedures
(define (compile-ir! expr malloc)
  (cond
    ((number? expr)
     (let-values (((var prefix) (malloc '())))
       (emit! `(set! ,var ,prefix ,expr))))
    ((symbol? expr)
     (let*-values (((size) (lookup expr))
                   ((var prefix) (malloc size)))
       (copy-values! size prefix '() expr var)))
    (else
     (pmatch expr
       ((array (? arg1) (?? arg-rest))
        (let*-values (((rank) (+ 1 (length arg-rest)))
                      ((subterm-size-exprs)
                       (call/cc
                         (lambda (return)
                           (compile-ir! arg1 return))))
                      ((subterm-size-vars) (map compile! subterm-size-exprs))
                      ((var prefix)        (malloc (cons rank subterm-size-vars))))
          (for-each
           (lambda (subterm i)
            (compile-ir! subterm (malloc-exactly subterm-size-vars (list i) var prefix)))
           (cons arg1 arg-rest)
           (iota rank))
          (values var (append prefix (list 0)))))
      ((compute (((? var) (? val))) (? body))
       (let*-values (((subterm-size-expr)
                      (call/cc
                       (lambda (return)
                         (with-new-scope
                          (lambda ()
                            (emit! `(declare ,var))
                            (emit! `(set! ,var () 0))
                            (compile-ir! body return))))))
                     ((subterm-size-vars) (map compile! subterm-size-expr))
                     ((dest prefix) (malloc (cons val subterm-size-vars)))
                     ((new-block)
                      (with-new-scope
                        (lambda ()
                          (push-to-scope! (cons var '()) *declarations*)
                          (compile-ir! body (malloc-exactly subterm-size-vars (list var) dest prefix))
                          (car (*statements*))))))
         (emit! `(for ,var 0 ,(compile! val) ,@(reverse new-block)))))
      ((let* (((? var) (? val))) (? body))
       (compile-ir! val
                    (lambda (size)
                      (emit! `(declare ,var ,@size))
                      (values var '())))
       (compile-ir! body malloc))
      ((ref (? arr) (? idx))
       (let ((idx-tmp   (gensym 'idx-tmp))
             (array-tmp (gensym 'array-tmp)))
         (compile-ir! idx (malloc-whatever idx-tmp))
         (compile-ir! arr (malloc-whatever array-tmp))
         (let*-values (((size)       (cdr (lookup array-tmp)))
                       ((var prefix) (malloc size)))
           (copy-values! size prefix '() (scary:ref array-tmp (list idx-tmp)) var))))
      ((if (? test) (? on-true) (? on-false))
       (let-values (((test-tmp)                 (gensym 'test-tmp))
                    ((result-var result-prefix) (malloc '())))
         (compile-ir! test (malloc-exactly '() '() '() test-tmp '()))
         (let ((true-code (with-new-scope
                           (lambda ()
                             (compile-ir! on-true (malloc-exactly '() '() result-var result-prefix))
                             (car (*statements*)))))
               (false-code (with-new-scope
                            (lambda ()
                              (compile-ir! on-false (malloc-exactly '() '() result-var reusult-prefix))
                              (car (*statements*))))))
           (emit! `(if ,test-tmp ,true-code ,false-code)))))
      ((length (array (?? xs)))
       (compile-ir! (length xs) malloc))
      ((length (? obj))
       (let-values (((obj-tmp)            (gensym 'obj-tmp))
                    ((res-var res-prefix) (malloc '())))
         (compile-ir! obj (malloc-whatever obj-tmp))
         (emit! `(set! ,res-var ,res-prefix (length ,obj-tmp)))))
      ((fold (? op) (? zero) (? arr))
       (let-values (((arr-tmp)            (gensym 'arr-tmp))
                    ((res-var res-prefix) (malloc '())))
         (compile-ir! arr (malloc-whatever arr-tmp))
         (compile-ir! zero (malloc-exactly '() '() res-var res-prefix))
         (emit-for! 0 `(length ,arr-tmp)
                    (lambda (counter)
                      (compile-ir! `(,op ,(scary:ref res-var res-prefix)
                                         ,(scary:ref arr-tmp (list counter)))
                                   (malloc-exactly '() '() res-var res-prefix))))))
      (((? op) (? left) (? right))
       (let-values (((left-tmp)           (gensym 'left-tmp))
                    ((right-tmp)          (gensym 'right-tmp))
                    ((res-var res-prefix) (malloc '())))
         (compile-ir! left  (malloc-whatever left-tmp))
         (compile-ir! right (malloc-whatever right-tmp))
         (emit! `(set! ,res-var ,res-prefix (,op ,left-tmp ,right-tmp)))))))))

(define (compile-ir expr)
  (define final-result (gensym 'final-result))
  (*statements*  '(()))
  (*declarations* '(()))
  (compile-ir! expr (malloc-whatever final-result))
  (emit! `(return ,final-result))
  `(begin . ,(reverse (apply append (*statements*)))))

(define (run-frontend code)
  (define-list (stx rank) (explicitly-lift code '()))
  (compile-ir stx))
