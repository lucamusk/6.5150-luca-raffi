(load "./backend.scm")
(load-option 'synchronous-subprocess)

(define-record-type c-backend
  (make-c-backend port)
  c-backend?
  (port c-backend-port))

(define-method (literal! (backend c-backend?) (obj any-object?))
  (format #f "array_singleton(~a)" obj))

(define (define-infix-binop symbol proc)
  (define-method (proc (_ c-backend?) (l any-object?) (r any-object?))
    (format #f "~a((~a), (~a))" symbol l r)))

(define-infix-binop 'add add!)
(define-infix-binop 'mul multiply!)
(define-infix-binop 'sub subtract!)
(define-infix-binop 'div divide!)
(define-infix-binop 'min minimize!)
(define-infix-binop 'max maximize!)

(define-method (minimize! (backend c-backend?) (l any-object?) (r any-object?))
  (format #f "min((~a), (~a))" l r))

(define-method (maximize! (backend c-backend?) (l any-object?) (r any-object?))
  (format #f "max((~a), (~a))" l r))

(define-method (length! (backend c-backend?) (l any-object?))
  (format #f "array_length(~a)" l))

(define dims->shape
  (lambda (xs)
    ((cut format #f "SHAPE(~a, ~a)" (length xs) <>)
     ((string-joiner* 'prefix "array_singleton_ref(" 'suffix ")" 'infix ", ") xs))))

(define-method (reference! (backend c-backend?) (var any-object?) (dims any-object?))
  (format #f "array_ref(~a, ~a)" var (dims->shape dims)))

(define c:emit!
  (case-lambda
    ((backend)
     (newline (c-backend-port backend)))
    ((backend str . more)
     (apply format (c-backend-port backend) str more)
     (newline (c-backend-port backend)))))

(define-method (return! (backend c-backend?) (obj any-object?))
  (c:emit! backend "return ~a;" obj))

(define-method (declare! (backend c-backend?) (var symbol?) (exprs list?))
  (c:emit! backend
           "Array *~a = array_singleton(0.0); array_init(~a, ~a);" var var (dims->shape exprs)))

(define-method (assign! (backend c-backend?) (var symbol?) (dims list?) (val any-object?))
  (c:emit! backend "array_set(~a, ~a, ~a);" var (dims->shape dims) val))

(define-method (for! (backend c-backend?)
                     (var symbol?)
                     (min any-object?)
                     (max any-object?)
                     (body-cont procedure?))
  (c:emit! backend "for_each (idx, array_singleton_ref(~a), array_singleton_ref(~a)) {
Array *~a = array_singleton(idx);" min max var)
  (body-cont)
  (c:emit! backend "}"))

(define-method (if! (backend c-backend?)
                    (test any-object?)
                    (yes-cont procedure?)
                    (no-cont procedure?))
  (c:emit! backend "if (~a) {" test)
  (yes-cont)
  (c:emit! backend "} else {")
  (no-cont)
  (c:emit! backend "}"))

(define (ir->c obj #!optional in-port)
  (define port (if (default-object? in-port)
                   (open-output-string)
                   in-port))
  (define backend (make-c-backend port))
  (c:emit! backend "#include \"runtime.c\"")
  (c:emit! backend "Array *scary_program() {")
  (if (symbol? (car obj))
      (compile-statement! backend obj)
      (compile-block! backend obj))
  (c:emit! backend "}")
  (when (default-object? in-port)
    (get-output-string port)))

(define example-program
  '(begin
     (declare a 3 5)
     (for i 0 3
       (set! a (i) 9))
     (return a)))

(define *c-source-file* (make-parameter "test.c"))
(define *executable-file* (make-parameter "test.exe"))

(define (eval-c src #!optional c-source-file executable-file)
  (when (default-object? c-source-file) (set! c-source-file "test.c"))
  (when (default-object? executable-file) (set! executable-file "test.exe"))
  (call-with-output-file c-source-file
    (cut write-string src <>))
  (assert (zero? (run-shell-command (format #f
                                            "cc ~a -lgc -g -O0 -o ~a"
                                            c-source-file
                                            executable-file))))
  (define sexpr
    (read-from-string
     (call-with-output-string
      (lambda (ostr)
        (assert (zero? (run-shell-command (format #f "./~a" executable-file)
                                          'output ostr)))))))


  (define shape    (second sexpr))
  (define arr (apply allocate-array shape))
  (for-each
   (lambda (i)
     (flo:vector-set! (array-elements arr) i (list-ref (third sexpr) i)))
   (iota (apply * shape)))

  arr)
