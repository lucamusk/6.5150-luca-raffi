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

;;;

;;; Global environment for REPL.
(define the-global-environment
  'not-initialized)

(define (initialize-repl!)
  (set! the-global-environment (make-global-environment))
  (for-each
   (lambda (e)
     (let ((operator (vector-ref e 0))
           (layer-name (vector-ref e 1))
           (handler (vector-ref e 2)))
       (let ((proc
              (lookup-variable-value operator
                                     the-global-environment)))
         (global-environment-define operator
            (with-layer! proc layer-name (handler operator))))))
   installed-layer-handlers))

(define (check-repl-initialized)
  (if (eq? the-global-environment 'not-initialized)
      (error "Interpreter not initialized. Run (init) first.")))

(define (global-environment-define var val)
  (define-variable! var val the-global-environment))

(define installed-layer-handlers '())

(define (install-layer-handler! operator layer-name handler)
  (let ((e
         (find (lambda (e)
                 (and (eq? (vector-ref e 0) operator)
                      (eq? (vector-ref e 1) layer-name)))
               installed-layer-handlers)))
    (if e
        (vector-set! e 2 handler)
        (set! installed-layer-handlers
              (cons (vector operator layer-name handler)
                    installed-layer-handlers)))))

;;(initialize-repl!)