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

;;;;  Trivial support set layer test.

;;; The provenance layer of a layered thing is a
;;; provenance entry in the layer map.  The
;;; provenance layer value is a support set,
;;; represented as a list.

;;; To make things easier to type:

(define (signed x signature)
  (with-layer! x
               'provenance
               (if (general-procedure? x)
                   (add-signature-to-procedure signature)
                   (let ((pvalue (layer-value 'provenance x)))
                     (if (novalue? pvalue)
                         (list signature)
                         (lset-adjoin equal?
                                      pvalue
                                      signature))))))

(define (add-signature-to-procedure signer)
  (lambda (value . args)
    (lset-adjoin equal?
                 (apply lset-union
                        equal?
                        value
                        (map (lambda (arg)
                               (get-layer-value-or-default
                                'provenance arg))
                             args))
                 signer)))

;;; Setting up a layer for tracking provenance of data:

(define (union-contributions value . args)
  (union-contributions* value args))

(define (union-contributions* value args)
  (apply lset-union equal? value args))

;;; Merger for provenance in IF conditionals

(define (provenance-merge value new-value)
  (union-contributions value new-value))

;;; Now for primitive procedures

(define (only-provenance-layer-args operator)
  (layer-args union-contributions 'provenance))

(define-layer 'provenance
  ;; default value
  '()
  ;; default procedure value
  ;; **code smell** here
  (only-provenance-layer-args 'unknown-procedure)
  ;; apply-merge
  provenance-merge
  ;; p-merge
  provenance-merge
  ;; c/a-merge
  provenance-merge)

(install-layer-handler! '+ 'provenance
                        only-provenance-layer-args)
(install-layer-handler! '- 'provenance
                        only-provenance-layer-args)

#|
(define (provenance:* operator)
  ;; This needs to be more sophisticated, see below
  (base-and-layer-args
   (lambda (value . blargs)
     (let ((a (assv 0 blargs)))
       (if a
           (cdr a)
           (union-contributions* value
                                 (map cdr blargs)))))
   'provenance))
|#

(define (provenance:* operator)
  (base-and-layer-args
   ;; blarg = (base-arg . support-set-arg)
   (lambda (value . blargs)
     (let ((a ; zero arg with smallest support set
            (sort (filter (lambda (blarg)
                            (eqv? (car blarg) 0))
                          blargs)
                  (lambda (bl1 bl2)
                    (< (length (cdr bl1))
                       (length (cdr bl2)))))))

       (if (not (null? a))
           (cdar a)
           (union-contributions* value
                                 (map cdr blargs)))))
   'provenance))

(install-layer-handler! '* 'provenance provenance:*)

(define (provenance:/ operator)
  (base-and-layer-args
   (lambda (value . blargs)
     (if (zero? (caar blargs))
         (cdar blargs)
         (union-contributions* value (map cdr blargs))))
   'provenance))

(install-layer-handler! '/ 'provenance provenance:/)


(install-layer-handler! '= 'provenance
                        only-provenance-layer-args)
(install-layer-handler! '< 'provenance
                        only-provenance-layer-args)
(install-layer-handler! '> 'provenance
                        only-provenance-layer-args)

(install-layer-handler! 'exp 'provenance
                        only-provenance-layer-args)
(install-layer-handler! 'sin 'provenance
                        only-provenance-layer-args)
(install-layer-handler! 'cos 'provenance
                        only-provenance-layer-args)
(install-layer-handler! 'sqrt 'provenance
                        only-provenance-layer-args)

(define (provenance:atan operator)
  (base-and-layer-args
   (lambda (value . blargs)
     (let ((a (assv 0 blargs)))
       (if a
           (cdr a)
           (union-contributions* value (map cdr blargs)))))
   'provenance))

(install-layer-handler! 'atan 'provenance provenance:atan)

(install-layer-handler! 'car 'provenance
                        only-provenance-layer-args)
(install-layer-handler! 'cdr 'provenance
                        only-provenance-layer-args)
(install-layer-handler! 'cons 'provenance
                        only-provenance-layer-args)
(install-layer-handler! 'pair? 'provenance
                        only-provenance-layer-args)
(install-layer-handler! 'eq? 'provenance
                        only-provenance-layer-args)
(install-layer-handler! 'null? 'provenance
                        only-provenance-layer-args)
(install-layer-handler! 'list 'provenance
                        only-provenance-layer-args)
