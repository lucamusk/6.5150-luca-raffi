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

;;;; This is the applicator for layered procedures.

(define-record-type <applyer>
    (make-applyer procedure extra)
    applyer?
  (procedure applyer-procedure)
  (extra applyer-extra))

(define (layered-procedure-dispatch procedure args continue)
  (layers-processor procedure
                    args
                    (apply lset-union eq?
                           (available-layers procedure)
                           (map available-layers args))
                    (process-layers continue))
  (end-of-layers continue))

(define (layers-processor procedure args relevant-layers
                          process-a-layer)
  (for-each (lambda (layer-name)
              (process-a-layer layer-name
                (layer-applicator procedure args layer-name)))
            relevant-layers))

(define (layer-applicator procedure args layer-name)

  (define (base-layer-applicator procedure args continue)
    (let ((base-proc (base-value procedure)))
      (l:apply-strict base-proc
                      (if (strict-primitive-procedure? base-proc)
                          (map base-value args)
                          args)
                      continue)))

  (lambda (cont-layer-value continue)
    (cond ((eq? layer-name 'base)
           (base-layer-applicator procedure
                                  args
                                  continue))
          ((any (lambda (arg)      ; A changeable policy.
                  (memq layer-name
                        (available-layers arg)))
                args)
           (l:apply-strict (get-layer-value-or-default
                            layer-name procedure)
                           (cons cont-layer-value args)
                           continue))
          (else 'OK))))

(define (process-layers cont)
  (cont-increment-nested-apps cont)

  (define (process-a-layer layer-name applicator)
    (applicator (((cont-dict cont) 'get-value) layer-name)
                (extend-continuation
                 (lambda (new-value)
                   (if (and (eq? layer-name 'base)
                            (layered-thing? new-value))
                       (accept-values apply-merge new-value cont)
                       (value-receiver layer-name new-value
                                       cont))))))
  process-a-layer)

(define (end-of-layers cont)
  (cont-decrement-nested-apps cont)
  (return-if-complete cont))

;;;; Wrappers for layer handlers

(define (layer-args procedure layer)
  (lambda (current . args)
    (apply procedure
           current
           (map (lambda (arg)
                  (get-layer-value-or-default layer arg))
                args))))

(define (base-and-layer-args procedure layer)
  (lambda (current . args)
    (apply procedure
           current
           (map (lambda (arg)
                  (cons (get-layer-value 'base arg)
                        (get-layer-value-or-default layer arg)))
                args))))