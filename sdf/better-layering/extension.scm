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

;;; This takes an ordinary continuation and extends it to support
;;; key-value metadata.  This is a simplification of the more
;;; elaborate system of continuation marks from SRFI-226.

(define (empty-marks) (list '()))

(define (extend-continuation continuation)
  (set-cont-marks! continuation (empty-marks))
  continuation)

(define (set-cont-marks! cont marks)
  ((marks-of-continuation 'put!) cont marks))

(define (cont-marks cont)
  ((marks-of-continuation 'get) cont))

(define (extended-continuation? object)
  ((marks-of-continuation 'has?) object))

(define marks-of-continuation
  (make-metadata-association))

(define (continuation-mark cont key #!optional fail)
  (let ((marks (cont-marks cont)))
    (let ((p (assv key (car marks))))
      (if p
          (cdr p)
          (let ((value (call-fail key fail)))
            (set-car! marks
                      (cons (cons key value)
                            (car marks)))
            value)))))

(define (update-continuation-mark! cont key updater
                                   #!optional fail)
  (let ((marks (cont-marks cont)))
    (let ((p (assv key (car marks))))
      (if p
          (let ((new-value (updater (cdr p))))
            (set-cdr! p new-value)
            new-value)
          (let ((value (updater (call-fail key fail))))
            (set-car! marks
                      (cons (cons key value)
                            (car marks)))
            value)))))

(define (call-fail key fail)
  (if (default-object? fail)
      (error "Expected to find value:" key))
  (fail))

(define (remove-continuation-mark! cont key)
  (let ((marks (cont-marks cont)))
    (let ((p (assv key (car marks))))
      (if p
          (set-car! marks (delq! p (car marks)))))))

(define (make-continuation-mark-key #!optional name)
  (list (if (default-object? name) 'continuation-mark-key name)))