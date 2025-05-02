(load "./backend.scm")
(load "./zipper.scm")

#|
A scheme backend for SCARY
- Tensor objects pass around a rank value, but we should know that at compile-time
- No/minimal type checking
|#

(define-record-type scheme-backend
  (%make-scheme-backend statements)
  scheme-backend?
  ;; A zipper over a begin statement, pointing to the most recent statement
  (statements scheme-backend-statements set-scheme-backend-statements!))

(register-predicate! scheme-backend? 'scheme-backend)

(define (make-scheme-backend)
  (%make-scheme-backend (zip-child (zip-up '(begin)))))

(define scheme-backend-code
  (compose zipper-point zip-top scheme-backend-statements))

(define (swap-statements! backend fn)
  (define zip (scheme-backend-statements backend))
  (set-scheme-backend-statements! backend (fn zip)))

(define (emit! backend stmt)
  (swap-statements! backend
                    (compose zip-right (cut zipper-insert-right <> stmt))))

(define-method (literal! (backend scheme-backend?) (obj any-object?))
  obj)

(define (define-binop symbol proc)
  (define-method (proc (_ scheme-backend?) (l any-object?) (r any-object?))
    (list symbol l r)))

(define-binop '+   add!)
(define-binop '*   multiply!)
(define-binop '-   subtract!)
(define-binop '/   divide!)
(define-binop 'min minimize!)
(define-binop 'max maximize!)

(define-method (length! (backend scheme-backend?) (obj any-object?))
  `(tensor-length ,obj))

(define-method (reference! (backend scheme-backend?) (var symbol?))
  var)

(define-method (declare! (backend scheme-backend?) (var symbol?) (exprs list?))
  (emit! backend `(define ,var (allocate-tensor . ,exprs)))
  (swap-statements! backend (cut zip-child <> 1)))

(define-method (assign! (backend scheme-backend?) (var symbol?) (dims list?) (val any-object?))
  (emit! backend `(tensor-write! ,var (list . ,dims) ,val)))

(define-method (for! (backend scheme-backend?)
                     (var symbol?)
                     (min any-object?)
                     (max any-object?)
                     (body-cont procedure?))
  (emit! backend `(for-loop ,min ,max (lambda (,var))))
  (swap-statements! backend (cut zip-child <> 3))
  (swap-statements! backend (cut zip-child <> 1))
  (body-cont)
  (swap-statements! backend (compose zip-parent zip-parent)))

(define-method (if! (backend scheme-backend?)
                    (condition any-object?)
                    (consequences-cont procedure?)
                    (alternatives-cont procedure?))
  (emit! backend `(if ,condition
                      (begin)
                      (begin)))
  (swap-statements! backend (compose zip-child (cut zip-child <> 2)))
  (consequences-cont)
  (swap-statements! backend (compose zip-child zip-right zip-parent))
  (alternatives-cont)
  (swap-statements! backend (compose zip-parent zip-parent)))

(define-method (write! (backend scheme-backend?) (obj any-object?))
  (emit! backend `(display ,obj))
  (emit! backend `(newline)))
