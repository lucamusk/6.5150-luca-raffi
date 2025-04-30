#|
Shorthand for a few common patterns
|#

(load "./cut.scm")

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

(define-syntax define-method
  (syntax-rules ()
    ((define-method (name (arg arg-type) ...)
       body ...)
     (define-generic-procedure-handler name
       (match-args arg-type ...)
       (lambda (arg ...)
         body ...)))))

(define lit
  (memoize-multi-arg-eqv
   (lambda (sym #!optional =)
     (define pred (if (default-object? =) eq? =))
     (define (is-that-sym? obj) (pred obj sym))
     (register-predicate! is-that-sym? `(marker ,sym))
     is-that-sym?)))
