(load "./opt-interface.scm")

;;; Our tools for working with assignments
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
Our constant propagation setup, assuming this IR:

Call     := (Op Expr Expr) | (length Expr)
Index        := (ref Variable Expr ...)
Init         := (declare Variable Expr ...)
Assignment  := (set! Variable (Expr ...) Expr)
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

;;; (if Expr (statement ...) (statement ...))
(define-generic-procedure-handler constant-propagation-generic
  (match-args (form 'if) any-object?)
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

