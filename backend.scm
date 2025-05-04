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

;; Emits a reference expression
;;   VAR is an lvalue which is to be read from
;;   DIMS are a list of rvalues, representing the indicies
;; Returns an rvalue
(define-generic (reference! backend var dims))

;; Emits a declaration
;;  EXPRESSIONS are a list of rvalues
;; Returns an lvalue object
(define-generic (declare! backend expressions))

;; Emits an assignment statment
;;  VAR is an lvalue which is to be written to
;;  DIMS are a list of rvalues, representing the indicies
;;  VALUE is an rvalue representing the value to be written
;; Returns nothing of importance
(define-generic (assign! backend var dims value))


;; Emits a loop statement
;;   MIN and MAX are rvalues which represent the loop bounds
;;   BODY-CONT is a procedure which will get called with the index variable, which is a new lvalue
;;     Any code emitted within this dynamic extent will be in the loop body
;; Returns nothing of importance
(define-generic (loop! backend min max body-cont))

;; Emits an if statement
;;   CONDITION is an rvalue on which the conditional will dispatch
;;   CONSEQUENCE-CONT is a procedure which will get called with no arguments,
;;     Any code emitted within this dynamic extent will be in the consequence of the conditional
;;   ALTERNATIVE-CONT is a procedure which will get called with no arguments,
;;     Any code emitted within this dynamic extent will be in the alternative of the conditional
(define-generic (if! backend condition consequence-cont alternative-cont))

;;; Derived functions for the compiler

(define-generic (compile-stmt! backend stmt))
(define-generic (compile-expr! backend expr))

(define (compile-block! backend block)
  (for-each (lambda (st) (compile-stmt! backend st)) block))

(define-method (compile-expr! (backend any-object?) (num integer?))
  (literal! backend num))

;; TODO: Define the rest of the traversal
