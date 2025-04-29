#| -*-Scheme-*-

Copyright (C) 2019, 2020, 2021 Chris Hanson and Gerald Jay Sussman

This file is part of SDF.  SDF is software supporting the book
"Software Design for Flexibility", by Chris Hanson and Gerald Jay
Sussman.

SDF is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SDF is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with SDF.  If not, see <https://www.gnu.org/licenses/>.

|#

;;;; Simple in-line tests

;;; The idea behind this tester is that it executes a file of
;;; expressions, in order, except that some of the expressions
;;; will be annotated with "expectations" that must be satisfied
;;; by the evaluation of the corresponding expression.

;;; For example,
;;;
;;; (fib 20)
;;; 'expect-value: 6765
;;;
;;; is a trivial example.  There are also expectations involving
;;; printed output, and perhaps others as we develop the idea.

(define (load-inline-test filename #!optional test-eval)
  (summarize-test-results
   (with-notification
       (lambda (port)
         (display "Loading test: " port)
         (write (->namestring filename) port))
     (lambda ()
       (load-inline-test-1 filename
                           (nearest-repl/environment)
                           (show-test-expressions?)
                           (debug-test-errors?))))))

(define (load-inline-test-1 filename env show-tests?
                            debug-errors? #!optional test-eval)
  (parameterize ((*test-eval*
                  (if (default-object? test-eval)
                      eval
                      test-eval))
                 (*test-env* env)
                 (show-test-expressions? show-tests?)
                 (debug-test-errors? debug-errors?))
    (execute-grouped-expressions
     (group-expressions
      (read-test-expressions filename)))))

(define *test-eval*
  (make-parameter
   (lambda (expr env)
     (eval expr env))))

(define *test-env*
  (make-parameter (default-object)))

(define show-test-expressions?
  (make-settable-parameter #f))

(define debug-test-errors?
  (make-settable-parameter #f))

(define (read-test-expressions filename)
  (read-file (pathname-default-type filename "scm")))

(define (group-expressions exprs)
  (if (pair? exprs)
      (let ((to-eval (car exprs)))
        (let ((r (parse-expectations (cdr exprs))))
          (let ((expectations (car r))
                (rest (cadr r)))
            (cons (cons to-eval expectations)
                  (group-expressions rest)))))
      '()))

(define (parse-expectations exprs)
  (let ((r (parse-expectation exprs)))
    (if r
        (let ((expectation (car r))
              (rest (cadr r)))
          (let ((r* (parse-expectations rest)))
            (list (cons expectation (car r*))
                  (cadr r*))))
        (list (list) exprs))))

(define (parse-expectation exprs)
  (let loop ((rules expectation-rules))
    (and (pair? rules)
         (or (match-head (car rules) exprs)
             (loop (cdr rules))))))

(define (match-head rule exprs)
  (and (pair? exprs)
       (is-quotation? (car exprs))
       (eq? (quotation-text (car exprs))
            (expectation-rule-keyword rule))
       (n:>= (length (cdr exprs))
             (expectation-rule-n-args rule))
       (let ((tail (cdr exprs))
             (n-args (expectation-rule-n-args rule)))
         (let ((args (list-head tail n-args))
               (rest (list-tail tail n-args)))
           (list (cons rule
                       (map (lambda (expr)
                              (if (is-quotation? expr)
                                  (quotation-text expr)
                                  expr))
                            args))
                 rest)))))

(define (is-quotation? object)
  (and (pair? object)
       (eq? (car object) 'quote)
       (pair? (cdr object))
       (null? (cddr object))))

(define (quotation-text expr)
  (cadr expr))

(define (define-expectation-rule keyword n-args handler)
  (let ((rule (make-expectation-rule keyword n-args handler))
        (tail
         (find-tail (lambda (rule)
                      (eq? keyword
                           (expectation-rule-keyword rule)))
                    expectation-rules)))
    (if tail
        (set-car! tail rule)
        (set! expectation-rules
              (cons rule
                    expectation-rules)))))

(define expectation-rules
  '())

(define-record-type <expectation-rule>
    (make-expectation-rule keyword n-args handler)
    expectation-rule?
  (keyword expectation-rule-keyword)
  (n-args expectation-rule-n-args)
  (handler expectation-rule-handler))

(define (rule-expects-error? rule)
  (eq? 'expect-error: (expectation-rule-keyword rule)))

(define (expecting-error? expectations)
  (any (lambda (expectation)
         (rule-expects-error? (car expectation)))
       expectations))

;;; Lots or hair here to let the test driver deal with
;;; "interesting" uses of continuations.  In particular, the
;;; state of the driver is moved outside of the control
;;; structure, so that if there are multiple returns from
;;; evaluating an expression, the "current" expectations are used
;;; for each.

(define *groups-to-test*)
(define *current-group*)
(define *test-results*)

(define (execute-grouped-expressions groups)
  (fluid-let ((*groups-to-test* groups)
              (*current-group*)
              (*test-results* '())
              (cpp pp))
    (let loop ()
      (if (pair? *groups-to-test*)
          (begin
            (set! *current-group* (car *groups-to-test*))
            (set! *groups-to-test* (cdr *groups-to-test*))
            (set! *test-results*
                  (cons (execute-grouped-expression)
                        *test-results*))
            (loop))))
    (reverse *test-results*)))

(define (execute-grouped-expression)
  (maybe-show-test-expression (car *current-group*)
    (lambda ()
      (let ((output-port (open-output-string)))
        (let ((value
               (handle-errors
                (lambda ()
                  (with-output-to-port output-port
                    (lambda ()
                      ((*test-eval*)
                       (car *current-group*)
                       (*test-env*))))))))
          (let ((context
                 (make-expectation-context output-port value)))
            (cons (car *current-group*)
                  (filter-map (lambda (expectation)
                                (apply-expectation expectation
                                                   context))
                              (cdr *current-group*)))))))))

(define (make-expectation-context output-port value)
  (let* ((output-string (get-output-string! output-port))
         (port (open-input-string output-string)))
    (define (get-output-string) output-string)
    (define (get-port) port)
    (define (get-value) value)
    (bundle expectation-context?
            get-output-string get-port get-value)))

(define expectation-context?
  (make-bundle-predicate 'expectation-context))

(define (apply-expectation expectation context)
  (apply (expectation-rule-handler (car expectation))
         context
         (cdr expectation)))

(define (maybe-show-test-expression expr thunk)
  (if (show-test-expressions?)
      (with-notification
       (lambda (port)
         (display "evaluate: " port)
         (display
          (cdr
           (call-with-truncated-output-string 50
             (lambda (port)
               (write expr port))))
          port))
       thunk)
      (thunk)))

(define (handle-errors thunk)
  (call-with-current-continuation
    (lambda (k)
      (bind-condition-handler
          (list condition-type:error)
          (lambda (condition)
            (if (not (expecting-error? (cdr *current-group*)))
                (if (debug-test-errors?)
                    (standard-error-handler condition)
                    (show-error condition)))
            (k condition))
        thunk))))

(define (show-error condition)
  (with-notification
      (lambda (port)
        (write-string "While evaluating\n" port)
        (write (car *current-group*) port)
        (write-string "\ngot error: " port)
        (write-condition-report condition port))))

(define (skeletal-test-results results)
  (values (count failing-test-result? results)
          (length results)))

(define (summarize-test-results results)
  (let-values (((failures all) (skeletal-test-results results)))
    (fresh-line)
    (display "Ran ")
    (write all)
    (display " test")
    (if (not (n:= 1 all))
        (display "s"))
    (display "; ")
    (write failures)
    (display " failure")
    (if (not (n:= 1 failures))
        (display "s")))
  (summarize-failing-results results))

(define (summarize-failing-results results)
  (for-each summarize-failing-result
            (filter failing-test-result? results)))

(define (failing-test-result? result)
  (pair? (cdr result)))

(define (summarize-failing-result failure)
  (newline)
  (newline)
  (display "evaluating ")
  (newline)
  (pp (car failure))
  (display "failed the following expectations:")
  (newline)
  (for-each (lambda (error)
              (display error)
              (newline))
            (cdr failure)))

(define-expectation-rule 'expect-write: 1
  (lambda (context written-value)
    (read-written-value context written-value)))

(define-expectation-rule 'expect-description: 1
  (lambda (context description)
    (read-line (context 'get-port))     ;discard printed object
    (let loop ((description description))
      (if (pair? description)
          (or (read-written-value context (car description))
              (loop (cdr description)))
          #f))))

(define (read-written-value context written-value)
  (let ((x
         (ignore-errors
          (lambda ()
            (read (context 'get-port))))))
    (cond ((condition? x)
           (string-append "expected to see output\n"
                          (write-to-string written-value)
                          "\nbut it did not appear"))
          ((not (equal? x written-value))
           (string-append "expected to see output\n"
                          (write-to-string written-value)
                          "\nbut instead saw\n"
                          (write-to-string x)))
          (else #f))))

(define-expectation-rule 'expect-output: 1
  (lambda (context output-string)
    (let ((actual (string-trim (context 'get-output-string))))
      (if (string=? actual output-string)
          #f
          (string-append "expected to see output\n"
                         (write-to-string output-string)
                         "\nbut instead saw\n"
                         (write-to-string actual))))))

(define-expectation-rule 'expect-value: 1
  (lambda (context value)
    (if (equal? (context 'get-value) value)
        #f
        (string-append
         "expected value\n"
         (write-to-string value)
         "\nbut instead got "
         (actual-value-string (context 'get-value))))))

(define-expectation-rule 'expect-value-in: 1
  (lambda (context vals)
    (if (member (context 'get-value) vals)
        #f
        (string-append
         "expected one of the following\n"
         (write-to-string vals)
         "\nbut instead got "
         (actual-value-string (context 'get-value))))))

(define (actual-value-string value)
  (if (condition? value)
      (string-append "an error\n"
                     (condition/report-string value))
      (string-append "value\n"
                     (write-to-string value))))

(define-expectation-rule 'expect-error: 0
  (lambda (context)
    (let ((value (context 'get-value)))
      (if (condition? value)
          #f
          (string-append
           "expected an error but instead got a value\n"
           (write-to-string value))))))

;;; General written output expectation.
(define-expectation-rule 'expect-writes: 2
  (lambda (context pred expected)
    (let loop ((writes '()))
      (let ((x
             (ignore-errors
              (lambda ()
                (read (context 'get-port))))))
        (cond ((condition? x)
               "Error while reading output")
              ((eof-object? x)
               (let ((v
                      (((*test-eval*) pred (*test-env*))
                       expected
                       (reverse writes))))
                 (cond ((string? v) v)
                       ((eq? #t v) #f)
                       ((eq? #f v)
                        (string-append
                         "Output\n"
                         (pp-to-string writes)
                         "doesn't satisfy predicate\n"
                         (pp-to-string pred)
                         "with expected value\n"
                         (pp-to-string expected)))
                       (else
                        (error "illegal predicate value:"
                               v)))))
              (else
               (loop (cons x writes))))))))

;;; Check that only whitespace was written.
(define-expectation-rule 'expect-no-output 0
  (lambda (context)
    (let ((port (context 'get-port)))
      (let loop ()
        (let ((char (read-char port)))
          (cond ((eof-object? char) #f)
                ((char-whitespace? char) (loop))
                (else
                 (string-append "expected no output but found "
                                (write-to-string char)))))))))