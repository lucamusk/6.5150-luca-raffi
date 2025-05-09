(manage 'add 'term)
(manage 'add 'design)
(manage 'add 'unification)
(load "./utils.scm")

;;; Some dummy transformations
(define (true-succ tree curr)
  #t)

(define (id-succ tree curr)
  curr)

(define (debug-succ tree curr)
  (pp curr)
  curr)

;;; Performs a deep copy of a given tree, with transformations
;;; done on the first or all elements matching the given
;;; pattern
(define (transform-tree tree pattern count transformer all?)
  (let ((pattern-compiled (match:compile-pattern pattern))
        (matched? #f))
    ;;(pp "Transform tree")
    ;;(pp tree)
    ;;(pp pattern)
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

;;; Useful for some optimizations
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

;; Location interface
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
    (let ((target-block (block-pattern location))
          (scope-index (block-scope-index location))
          (inner-pattern (block-inner-pattern location)))
      (optimize-at-generic code target-block count
                   (lambda (b)
                     (let ((list-left
                            (if (= scope-index 0)
                                '()
                                (sublist b 0 scope-index)))
                           (list-right
                            (sublist b (+ scope-index 1) (length b)))
                           (optimized
                            (optimize-at (list-tail b scope-index) inner-pattern optimizer all?)))
                       (and optimized
                           (append list-left
                                    optimized))))
                   #f))))

(define top-level
  `(? code))

;;; Sequence Locations
(define-generic-procedure-handler optimize-at-generic
  (match-args any-object? nth-location? number? procedure? boolean?)
  (lambda (code location count optimizer all?)
    (let ((n (nth-location-count location))
          (target (nth-location-target location)))
      ;;(pp "Optimizing an nth location")
      ;;(pp code)
      ;;(pp location)
      (optimize-at-generic code target n optimizer all?))))


;;; Simple Optimization Example
(define (rename var new)
  (define optimizer (make-pattern-operator))
  (attach-rule! optimizer
        (rule `(,var)
              `,new))
  optimizer)
