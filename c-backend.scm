(load "./backend.scm")

(define-record-type %c-backend
  (make-c-backend)
  c-backend?)

(define-method (literal! (backend c-backend?) (obj any-object?))
  (write-to-string (->flonum obj)))

(define (define-infix-binop symbol proc)
  (define-method (proc (_ c-backend?) (l any-object?) (r any-object?))
    (format #f "(~a) ~a (~a)" l symbol r)))

(define-infix-binop '+ add!)
(define-infix-binop '* multiply!)
(define-infix-binop '- subtract!)
(define-infix-binop '/ divide!)

(define-method (minimize! (backend c-backend?) (l any-object?) (r any-object?))
  (format #f "MIN((~a), (~a))" l r))

(define-method (maximize! (backend c-backend?) (l any-object?) (r any-object?))
  (format #f "MAX((~a), (~a))" l r))
