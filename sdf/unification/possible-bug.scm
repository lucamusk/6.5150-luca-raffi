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

;;; A student in 6.5150
;;;  Andres D Buritica Monroy <andresb@mit.edu>
;;; discovered this bug in 2024.  

#|
(unify-constraints '((= (? type:208) (? x:206))
                     (= (? type:208) (? x:206))
                     (= (? type:209) (numeric-type))
                     (= (numeric-type) (numeric-type))
                     (= (? type:208) (numeric-type))))
;;; Infinite loop

;;; But
(unify-constraints '((= (? type:208) (? x:206))
                     (= (? type:209) (numeric-type))
                     (= (numeric-type) (numeric-type))
                     (= (? type:208) (numeric-type))))
;Value:
;(dict (type:208 (numeric-type) ?)
;      (type:209 (numeric-type) ?)
;      (x:206 (numeric-type) ?))
|#

;;; We wrote:

(define (do-substitute var term dict)
  (let ((term* ((match:dict-substitution dict) term)))
    (and (match:satisfies-restriction? var term*)
         (or (and (match:var? term*)
                  (match:vars-equal? var term*))
             (not (match:occurs-in? var term*)))
         (match:extend-dict var term*
           (match:map-dict-values
            (match:single-substitution var term*)
            dict)))))


;;; Andres suggests

(define (do-substitute var term dict)
  (let ((term* ((match:dict-substitution dict) term)))
    (and (match:satisfies-restriction? var term*)
         (or (and (match:var? term*)
                  (match:vars-equal? var term*)
                  dict)
             (and
               (not (match:occurs-in? var term*))
               (match:extend-dict var term*
                 (match:map-dict-values
                   (match:single-substitution var term*)
                   dict)))))))


;;; The following is nicer and cleaner!

(define (do-substitute var term dict)
  (let ((term* ((match:dict-substitution dict) term)))
    (and (match:satisfies-restriction? var term*)
         (if (and (match:var? term*)
                  (match:vars-equal? var term*))
             dict
             (and (not (match:occurs-in? var term*))
                  (match:extend-dict var term*
                    (match:map-dict-values
                     (match:single-substitution var term*)
                     dict)))))))
