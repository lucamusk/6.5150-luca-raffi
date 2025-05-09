(load "./backend.scm")
(load "./zipper.scm")
(load "./utils.scm")
(load "./test.scm")
(load "./runtime.scm")

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

(define (scheme:emit! backend stmt)
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
  `(array-length ,obj))

(define-method (reference! (backend scheme-backend?) (var symbol?) (dims list?))
  `(array-ref ,var (list ,@dims)))

(define-method (return! (backend scheme-backend?) (obj any-object?))
  (scheme:emit! backend obj))

(define-method (declare! (backend scheme-backend?) (var symbol?) (exprs list?))
  (scheme:emit! backend `(define ,var (allocate-array . ,exprs))))

(define-method (assign! (backend scheme-backend?) (var symbol?) (dims list?) (val any-object?))
  (scheme:emit! backend `(array-set! ,var (list . ,dims) ,val)))

(define-method (for! (backend scheme-backend?)
                     (var symbol?)
                     (min any-object?)
                     (max any-object?)
                     (body-cont procedure?))
  (scheme:emit! backend `(for-loop ,min ,max (lambda (,var))))
  (swap-statements! backend (compose (cut zip-child <> 1) (cut zip-child <> 3)))
  (body-cont)
  (swap-statements! backend (compose zip-parent zip-parent)))

(define-method (if! (backend scheme-backend?)
                    (condition any-object?)
                    (consequences-cont procedure?)
                    (alternatives-cont procedure?))
  (scheme:emit! backend `(if ,condition
                             (begin)
                             (begin)))
  (swap-statements! backend (compose zip-child (cut zip-child <> 2)))
  (consequences-cont)
  (swap-statements! backend (compose zip-child zip-right zip-parent))
  (alternatives-cont)
  (swap-statements! backend (compose zip-parent zip-parent)))

(define (ir->scheme obj)
  (define backend (make-scheme-backend))
  (if (symbol? (car obj))
      (compile-statement! backend obj)
      (compile-block! backend obj))
  (scheme-backend-code backend))

(define-test simple-indexing
  (define program
    '((declare a 3 7)
      (for i 0 (min (length (ref a 0)) (length a))
        (set! a (i i) i))
      (return a)))
  (equal? (eval (ir->scheme program) (nearest-repl/environment)
           #(#(0. 0. 0. 0. 0. 0. 0.) #(0. 1. 0. 0. 0. 0. 0.) #(0. 0. 2. 0. 0. 0. 0.)))))

(define-test lucas-example
  (define program
     '((declare a 5)
       (set! a (0) 1)
       (set! a (1) 2)
       (set! a (2) 3)
       (set! a (3) 4)
       (set! a (4) 5)
       (declare b 5)
       (set! b (0) 1)
       (set! b (1) 0)
       (set! b (2) 1)
       (set! b (3) 0)
       (set! b (4) 1)
       (declare x 4)
       (set! x (0) 5)
       (set! x (1) 8)
       (set! x (2) 4)
       (set! x (3) 1)
       (declare z 4)
       (set! z (0) 4)
       (set! z (1) -2)
       (set! z (2) 1)
       (set! z (3) 4)
       (declare result1)
       (set! result1 () 0)
       (for i 0 (max (length a) (length b))
        (set! result1 () (+ result1 (* (ref a i) (ref b i)))))
       (declare result2)
       (set! result2 () 0)
       (for i 0 (/ (- (max (length x) (length z)) 0) 2)
        (for ii (* i 2) (+ (* i 2) 2)
         (set! result2 () (+ result2 (* (ref x ii) (ref z ii))))))
       (return (* result1 result2))))
  (check (= 108.0 (eval (ir->scheme program) (nearest-repl/environment)))))

(define-test dot-product-test
  (load "./frontend.scm")
  (define example-program '(fold + 0 (* (array 0 1 2) (array 10 9 8))))
  (check (= 25.0
            (eval (ir->scheme (run-frontend example-program))
                  (nearest-repl/environment)))))
