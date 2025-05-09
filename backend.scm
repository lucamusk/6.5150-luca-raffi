(load "./utils.scm")

;;; Specification of a compiler backend

;; A BACKEND is an object which responds to the following methods:

;; Creates a literal number term from NUM
;; Returns a new rvalue
(define-generic (literal! backend num))

;; Performs each operation on binary operands LEFT and RIGHT
;; Returns a new rvalue object
(define-generic (add!       backend left right))
(define-generic (subtract!  backend left right))
(define-generic (multiply!  backend left right))
(define-generic (divide!    backend left right))
(define-generic (minimize!  backend left right))
(define-generic (maximize!  backend left right))

;; Emits a length expression
;;   VAL is an rvalue which evaluates to a tensor
(define-generic (length! backend val))

;; Emits an array reference expression
;;   VAR is an lvalue which is to be read from
;;   DIMS are a list of rvalues, representing the indicies
;; Returns an rvalue
(define-generic (reference! backend var dims))

;; Emits a declaration
;;   VAR is an lvalue which is to be declared
;;   EXPRESSIONS are a list of rvalues
;; Returns nothing of importance
(define-generic (declare! var backend expressions))

;; Emits an assignment statment
;;  VAR is an lvalue which is to be written to
;;  DIMS are a list of rvalues, representing the indicies
;;  VALUE is an rvalue representing the value to be written
;; Returns nothing of importance
(define-generic (assign! backend var dims value))

;; Emits a for statement
;;   MIN and MAX are rvalues which represent the bounds
;;   VAR is the index variable
;;   BODY-CONT is a procedure which will get called with no arguments,
;;     Any code emitted within this dynamic extent will be in the loop body
;; Returns nothing of importance
(define-generic (for! backend var min max body-cont))

;; Emits an if statement
;;   CONDITION is an rvalue on which the conditional will dispatch
;;   CONSEQUENCE-CONT is a procedure which will get called with no arguments,
;;     Any code emitted within this dynamic extent will be in the consequence of the conditional
;;   ALTERNATIVE-CONT is a procedure which will get called with no arguments,
;;     Any code emitted within this dynamic extent will be in the alternative of the conditional
(define-generic (if! backend condition consequence-cont alternative-cont))

;; Emits a return clause, returning RESULT
;;   RESULT is an rvalue
(define-generic (return! backend result))

;;; Derived functions for the compiler

(define-generic (compile-statement! backend statement))
(define-generic (compile-expr! backend expr))

(define (compile-block! backend block)
  (for-each (cut compile-statement! backend <>) block))

(define-method (compile-expr! (backend any-object?) (num integer?))
  (literal! backend num))

(define-method (compile-expr! (backend any-object?) (sym symbol?))
  (reference! backend sym '()))

(define-method (compile-expr! (backend any-object?) (obj (form 'length)))
  (length! backend (compile-expr! backend (second obj))))

(define-method (compile-expr! (backend any-object?) (obj (form 'ref)))
  (pmatch obj
    ((ref (? var) (?? dims))
     (reference! backend var (map (cut compile-expr! backend <>) dims)))))

(define (define-binop name op)
  (define-method (compile-expr! (backend any-object?) (app (form name)))
    (pmatch app
      ((,name (? left) (? right))
       (op backend
           (compile-expr! backend left)
           (compile-expr! backend right))))))

(define-binop '+   add!)
(define-binop '*   multiply!)
(define-binop '-   subtract!)
(define-binop '/   divide!)
(define-binop 'min minimize!)
(define-binop 'max maximize!)

(define-method (compile-statement! (backend any-object?) (obj (form 'begin)))
  (compile-block! backend (cdr obj)))

(define-method (compile-statement! (backend any-object?) (obj (form 'declare)))
  (pmatch obj
    ((declare (? var) (?? dims))
     (declare! backend var (map (cut compile-expr! backend <>) dims)))))

(define-method (compile-statement! (backend any-object?) (obj (form 'set!)))
  (pmatch obj
    ((set! (? var) (? indicies) (? value))
     (assign! backend
              var
              (map (cut compile-expr! backend <>) indicies)
              (compile-expr! backend (fourth obj))))))

(define-method (compile-statement! (backend any-object?) (obj (form 'if)))
  (pmatch obj
    ((if (? condition) (? consequences) (? alternatives))
     (if! backend
          (compile-expr! backend condition)
          (lambda () (compile-block! backend consequences))
          (lambda () (compile-block! backend alternatives))))))

(define-method (compile-statement! (backend any-object?) (obj (form 'for)))
  (pmatch obj
    ((for (? var) (? mn) (? mx) (?? st))
     (for! backend
           var
           (compile-expr! backend mn)
           (compile-expr! backend mx)
           (lambda () (compile-block! backend st))))))

(define-method (compile-statement! (backend any-object?) (obj (form 'return)))
  (pmatch obj
    ((return (? val))
     (return! backend (compile-expr! backend val)))))
