(load "./utils.scm")

;; Parameter which contains all the global tests
(define *tests* (make-settable-parameter '()))

;; Runs all the global tests
(define (run-all-tests!)
  (for-each (cut <>) (*tests*)))

;; Signal that an application of TOPLEVEL-FORM to ARGUMENTS has failed
(define (check-failure toplevel-form arguments)
  (apply error
         "The call" `(,toplevel-form ,@(map car arguments))
         (error-irritant/noise " has failed:\n")
         (append-map (lambda (form.value)
                       (list (car form.value)
                             (error-irritant/noise " =>")
                             (cdr form.value)
                             (error-irritant/noise "\n")))
                     arguments)))

;; Semantically equivalent to ASSERT, but parses the
;; subterms of the passed application
(define-syntax check
  (syntax-rules ()
    ((check (form arg ...))
     (let* ((arg-vals (list arg ...))
            (res (apply form arg-vals)))
       (unless res
         (check-failure 'form (map cons '(arg ...) arg-vals)))))
    ((check val)
     (assert val))))

;; Define a test, but don't make it global
(define-syntax-rule (define-local-test name
                      body ...)
  (define (name)
    (call/cc
     (lambda (return)
       (bind-condition-handler
        (list condition-type:error)
        (lambda (issue)
          (format (current-error-port) "Test ~a failed:~%" 'name)
          (write-condition-report issue (current-error-port))
          (return 'test-failed))
        (lambda ()
          body
          ...
          (format (current-error-port) "Test ~a passed~%" 'name)
          'test-passed))))))

;; Define a test and make it global
(define-syntax-rule (define-test name
                      body ...)
  (define-local-test name
    body ...)
  (*tests* (cons name (*tests*))))

(define-test test-testing
  (define-local-test failing-test
    (check (= 3 9)))

  (define resulting-call
    (call-with-output-string
     (lambda (out)
       (parameterize ((current-error-port out))
         (failing-test)))))

  (check (string=? resulting-call
                   "Test failing-test failed:
The call (= 3 9) has failed:
 3 => 3
 9 => 9
")))
