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

;;;; Layered objects

(define (make-layered-thing layer-map)

  (define (layers-available)
    (layer-map:layers-available layer-map))

  (define (layer-value layer-name fail)
    (let ((entry (layer-map:entry layer-name layer-map)))
      (if entry
          (layer-map:entry-value entry)
          (fail))))

  (define (set-layer-value! layer-name value)
    (set! layer-map
          (layer-map:new-value layer-map layer-name value))
    unspecific)

  (let ((internal
         (bundle internal-layered-thing?
                 layers-available layer-value set-layer-value!)))
    (if (general-procedure? internal)
        (make-applyer layered-procedure-dispatch
                      internal)
        internal)))

(define internal-layered-thing?
  (make-bundle-predicate 'layered-thing))

(define (layered-thing? object)
  (or (internal-layered-thing? object)
      (and (applyer? object)
           (internal-layered-thing? (applyer-extra object)))))

(define (layered-thing-internal thing)
  (if (applyer? thing)
      (applyer-extra thing)
      thing))

(define (layered-procedure? thing)
  (and (layered-thing? thing)
       (general-procedure? thing)))

(define (general-procedure? object)
  (interpreter-procedure? (get-layer-value 'base object)))

(define (available-layers thing)
  (if (layered-thing? thing)
      ((layered-thing-internal thing) 'layers-available)
      '(base)))

(define (with-layer! thing layer-name value)
  (if (layered-thing? thing)
      (begin
        ((layered-thing-internal thing)
         'set-layer-value! layer-name value)
        thing)
      (make-layered-thing
       (layer-map:make
        (layer-map:make-entry 'base thing)
        (layer-map:make-entry layer-name value)))))

(define (layer-value layer-name thing)
  (if (layered-thing? thing)
      ((layered-thing-internal thing) 'layer-value layer-name novalue)
      (if (eq? layer-name 'base)
          thing
          (novalue))))

(define (get-layer-value layer-name thing)
  (let ((value (layer-value layer-name thing)))
    (if (novalue? value)
        (error "No value for layer" layer-name thing))
    value))

(define (get-layer-value-or-default layer-name thing)
  (let ((value (layer-value layer-name thing)))
    (if (novalue? value)
        (get-default-for-layer layer-name thing)
        value)))

(define (novalue)
  *novalue*)

(define (novalue? entry)
  (eq? entry *novalue*))

(define *novalue*
  (list '*novalue*))

;;;; Layer-map abstraction

;;; For this elementary implementation we will
;;; implement layer maps as association lists.

(define (layer-map:layers-available layer-map)
  (map layer-map:entry-name layer-map))

(define (layer-map:make . entries)
  (if (not (assq 'base entries))
      (error "Must have a base entry:" 'layer-map:make))
  entries)

(define (layer-map:make-entry layer value)
  (list layer value))

(define (layer-map:entry-name entry)
  (car entry))

(define (layer-map:entry-value entry)
  (cadr entry))

(define (layer-map:entry layer-name layer-map)
  (assq layer-name layer-map))

(define (layer-map:new-value layer-map layer-name new-value)
  (let ((entry (layer-map:entry layer-name layer-map)))
    (if entry
        (begin
          (set-car! (cdr entry) new-value)
          layer-map)
        (cons (list layer-name new-value)
              layer-map))))

;;;; Layer abstraction

(define-record-type <layer>
    (make-layer name default procedure-default apply-merge
                p-merge c/a-merge)
    layer?
  (name layer-name)
  (default layer-default)
  (procedure-default layer-procedure-default)
  (apply-merge layer-apply-merge)
  (p-merge layer-p-merge)
  (c/a-merge layer-c/a-merge))

(define (define-layer name default procedure-default apply-merge
          if-p-handler if-c/a-handler)
  (let ((layer
         (make-layer name default procedure-default apply-merge
                     if-p-handler if-c/a-handler))
        (old
         (find-tail (lambda (layer)
                      (eq? (layer-name layer) name))
                    *defined-layers*)))
    (if old
        (set-car! old layer)
        (set! *defined-layers*
              (cons layer *defined-layers*))))
  name)

(define (get-layer name)
  (let ((layer
         (find (lambda (layer)
                 (eq? (layer-name layer) name))
               *defined-layers*)))
    (if (not layer)
        (error "Unknown layer:" name))
    layer))

(define *defined-layers* '())

(define (default-if-handler value-in-dict value)
  (declare (ignore value-in-dict value))
  *ignore-value*)

(define *ignore-value*
  (list 'ignore-value))

(define (ignore-value? x)
  (eq? x *ignore-value*))

(define (get-default-for-layer layer-name thing)
  (let ((layer (get-layer layer-name)))
    (if (general-procedure? thing)
        (layer-procedure-default layer)
        (layer-default layer))))

(define (apply-merge layer-name)
  (layer-apply-merge (get-layer layer-name)))

(define (p-merge layer-name)
  (layer-p-merge (get-layer layer-name)))

(define (c/a-merge layer-name)
  (layer-c/a-merge (get-layer layer-name)))

(define-generic-procedure-handler l:if-hook
  (match-args layered-thing? extended-continuation? any-object?)
  (lambda (p-value continue receiver)
    (receiver (base-value p-value)
              (merge-predicate-layer-values p-value continue))))

(define (merge-predicate-layer-values p-value cont)
  (accept-values p-merge p-value cont)
  (extend-continuation
    (lambda (c/a-value)
      (accept-values c/a-merge c/a-value cont)
      (return-if-complete cont))))

(define (accept-values get-merger value cont)
  (for-each
   (lambda (layer-name)
     (value-receiver layer-name
                     ((get-merger layer-name)
                      (((cont-dict cont) 'get-value) layer-name)
                      (get-layer-value-or-default layer-name
                                                  value))
                     cont))
   (available-layers value)))

(define (value-receiver layer-name new-value cont)
  (cond ((ignore-value? new-value)
         'nothing-to-do)
        (else
         (((cont-dict cont) 'update!) layer-name new-value)
         'updated-layer-with-new-value)))

(define (return-if-complete cont)
  (if (= (cont-nested-apps cont) 0)
      (cont
       (make-layered-thing (((cont-dict cont) 'filled-entries))))
      'multiple-nested-applications))

(define (make-dictionary)
  (let ((dict '()))

    (define (get-entry layer-name)
      (let ((entry (assq layer-name dict)))
        (if (not entry)
            (let ((entry (list layer-name (novalue))))
              (set! dict (cons entry dict))
              entry)
            entry)))

    (define (update! layer-name new-value)
      (let ((entry (get-entry layer-name)))
        (set-car! (cdr entry) new-value)))

    (define (get-value layer-name)
      (let ((v (cadr (get-entry layer-name))))
        (if (novalue? v)
            (layer-default (get-layer layer-name))
            v)))

    (define (filled-entries)
      (filter (lambda (entry)
                  (not (novalue? (cadr entry))))
                dict))

    (define (the-dict m)
      (case m
        ((get-value) get-value)
        ((update!) update!)
        ((filled-entries) filled-entries)
        ((get-entry) get-entry)
        ((all-entries) dict)
        (else (error "unknown message -- dictionary" m))))
    the-dict))

;;;; Layering marks

(define (cont-nested-apps cont)
  (continuation-mark cont
                     mark-key:nested-applications
                     (lambda () 0)))

(define (cont-increment-nested-apps cont)
  (update-continuation-mark! cont
                             mark-key:nested-applications
                             (lambda (n) (+ n 1))
                             (lambda () 0)))

(define (cont-decrement-nested-apps cont)
  (update-continuation-mark! cont
                             mark-key:nested-applications
                             (lambda (n) (- n 1))))

(define mark-key:nested-applications
  (make-continuation-mark-key 'nested-applications))

(define (cont-dict cont)
  (continuation-mark cont mark-key:dict make-dictionary))

(define mark-key:dict
  (make-continuation-mark-key 'dict))

;;;; Base layer

(define (base-merge value new-value)
  (cond ((novalue? value) new-value)
        ((novalue? new-value) value)
        (else
         (if (not (equal? value new-value))
             (error "Multiple base values:" value new-value))
         new-value)))

(define-layer 'base (novalue) (novalue) base-merge
  default-if-handler base-merge)

(define (make-base-value value)
  (make-layered-thing
   (layer-map:make (layer-map:make-entry 'base value))))

(define (base-value thing)
  (get-layer-value 'base thing))