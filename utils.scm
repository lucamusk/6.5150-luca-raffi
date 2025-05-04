#|
Shorthand for a few common patterns
|#

(load "./cut.scm")

(define-syntax define-syntax-rule
  (syntax-rules ()
    ((define-syntax-rule (name arg ...)
       body ...)
     (define-syntax name
       (syntax-rules ()
         ((name arg ...)
          (begin body ...)))))))

(define-syntax define-generic
  (syntax-rules ()
    ((define-generic (name arg ...))
     (define-generic (name arg ...)
       (error (string "No default for generic procedure " 'name ", called with arguments")
              arg ...)))
    ((define-generic (name arg ...)
       body ...)
     (define name
       (simple-generic-procedure 'name (length '(arg ...))
                                 (lambda (arg ...)
                                   body ...))))))


(define-syntax-rule (define-method (name (arg arg-type) ...)
                      body ...)
  (define-generic-procedure-handler name
    (match-args arg-type ...)
    (lambda (arg ...)
      body ...)))

(define lit
  (memoize-multi-arg-eqv
   (lambda (sym #!optional =)
     (define pred (if (default-object? =) eq? =))
     (define (is-that-sym? obj) (pred obj sym))
     (register-predicate! is-that-sym? `(marker ,sym))
     is-that-sym?)))

(register-predicate! list? 'list)
(register-predicate! procedure? 'procedure)

(define (form obj)
  (is-pair-of
   (if (predicate? obj) obj (lit obj))
   list?))

(define-syntax pmatch
  (syntax-rules ()
    ((pmatch obj)
     (error 'pmatch "No pattern matched" obj))
    ((pmatch obj (pat res ...) more ...)
     ((rule `pat (begin res ...))
      obj
      (lambda (x y) x)
      (lambda ()
        (pmatch obj
          more ...))))))

;; Common Lisp's ATOM
(define atom? (complement pair?))

;; Returns (all but the last element) and (the last element) of a list
(define (rdc+rac lst)
  (split-at lst (- (length lst) 1)))

#|
Defines a paramorphism, which is an abstract way of walking over
aribtrary trees.

NAME is the name of the procedure.
SPLITTERS is a list of procedures.
COMBINER is a procedure.

This returns a generic procedure of as many arguments as there are
SPLITTERS. By default, when the generic procedure is called with
some arguments, each is sent to its corresponding splitter procedure.
These should split the relevant arguments into a list of smaller
sub-objects to be recurred over. Each splitter should return a list
with the same number of arguments. Recursion is then performed over
each split value. The combiner then rebuilds the value and is given
both the initial argument list as its first argument and the results
of each recurrance as its second.

Users can also define methods on the resulting generic function,
and use the arguments as they see fit (including recursively).
|#
(define (make-tree-walker name splitters combiner)
  (define (default-impl . args)
    (combiner
     args
     (apply map
            proc
            (map (cut <> <>)
                 splitters
                 args))))

  (define proc (simple-generic-procedure name
                                         (length splitters)
                                         default-impl))
  proc)

;; Given an object, returns an infinite list of that object
(define (repeat obj)
  (define pair (cons obj '()))
  (set-cdr! pair pair)
  pair)
