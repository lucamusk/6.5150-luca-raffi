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

;;; Example provenance run

;(repl)

'#!no-fold-case

;;; A simple function

(define (Id V)
  (* Is (- (exp (/ (* :q V) (* :k T))) 1)))

(define Is 1e-13)

(define :q 1.602176487e-19)

(define :k 1.3806505e-23)

(define T 300)

(Id 0.6)
;base;=> 1.2010041136964896e-3


;;; Starting to add provenance.

(set! Is (signed Is "IRC"))

(set! :q (signed :q "NIST:CODATA-2006"))

(set! :k (signed :k "NIST:CODATA-2006"))

(set! T (signed T "GJS-T"))

(Id (signed 0.6 "GJS-V"))
;provenance;=> ("NIST:CODATA-2006" "GJS-V" "GJS-T" "IRC")
;base;=> 1.2010041136964896e-3
'expect-layered:
'((base 1.2010041136964896e-3)
  (provenance ("NIST:CODATA-2006" "GJS-V" "GJS-T" "IRC")))

;;; Someone needs to sign the formula!

(define (Id V)
  (signed (* Is (- (exp (/ (* :q V) (* :k T))) 1))
          "Searle&Gray"))

(Id (signed 0.6 "GJS-V"))
;provenance;=> ("Searle&Gray" "NIST:CODATA-2006"
;               "GJS-V" "GJS-T" "IRC")
;base;=> 1.2010041136964896e-3
'expect-layered:
'((provenance ("Searle&Gray" "NIST:CODATA-2006"
               "GJS-V" "GJS-T" "IRC"))
  (base 1.2010041136964896e-3))


(set! Id (signed Id  "GJS-code"))

(Id (signed 0.6 "GJS-V"))
;provenance;=> ("IRC" "GJS-T" "NIST:CODATA-2006"
;               "Searle&Gray" "GJS-code" "GJS-V")
;base;=> 1.2010041136964896e-3
'expect-layered:
'((provenance ("IRC" "GJS-T" "NIST:CODATA-2006"
              "Searle&Gray" "GJS-code" "GJS-V"))
  (base 1.2010041136964896e-3))

(Id 0.6)
;provenance;=> ("Searle&Gray" "NIST:CODATA-2006"
;               "GJS-T" "IRC")
;base;=> 1.2010041136964896e-3
'expect-layered:
'((provenance ("Searle&Gray" "NIST:CODATA-2006" "GJS-T" "IRC"))
  (base 1.2010041136964896e-3))

;;; Simple procedures tests

((signed (lambda () (+ (signed 2 'gjs))) 'me))
;provenance;=> (gjs)
;base;=> 2
'expect-layered:
'((provenance (gjs))
  (base 2))

((signed (lambda ()
           (+ (signed 2 'gjs) (signed 3 'gjs)))
         'me))
;provenance;=> (gjs)
;base;=> 5
'expect-layered:
'((provenance (gjs))
  (base 5))

((signed (lambda ()
           (signed (+ (signed 2 'gjs) (signed 3 'gjs))
                   'test))
         'me))
;provenance;=> (test gjs)
;base;=> 5
'expect-layered:
'((provenance (test gjs))
  (base 5))

((signed (lambda (x)
           (+ (signed x 'jems) (signed 3 'gjs)))
         'me)
 (signed 5 'bilbo))
;provenance;=> (gjs jems me bilbo)
;base;=> 8
'expect-layered:
'((provenance (gjs jems me bilbo))
  (base 8))

(if (signed #f 'p)
    (signed 'c 'wrong)
    (signed 'a 'correct))
;provenance;=> (correct p)
;base;=> a
'expect-layered:
'((provenance (correct p))
  (base a))

(if (if (signed #f 'p)
        (signed 'c0 'wrong0)
        (signed #f 'correct0))
    (signed 'c 'wrong)
    (signed 'a 'correct))
;provenance;=> (correct correct0 p)
;base;=> a
'((provenance (correct correct0 p))
  (base a))

(if (signed (if (signed #f 'p)
                (signed 'c0 'wrong0)
                (signed #f 'correct0))
            'p1)
    (signed 'c 'wrong)
    (signed 'a 'correct))
;provenance;=> (correct p1 correct0 p)
;base;=> a
'expect-layered:
'((provenance (correct p1 correct0 p))
  (base a))

(if (signed #t 'p)
    (signed 'c 'correct)
    (signed 'a 'wrong))
;provenance;=> (correct p)
;base;=> c
'expect-layered:
'((provenance (correct p))
  (base c))

(if (if (signed #t 'p)
        (signed #t 'correct0)
        (signed #f 'wrong0))
    (signed 'c 'correct)
    (signed 'a 'wrong))
;provenance;=> (correct correct0 p)
;base;=> c
'expect-layered:
'((provenance (correct correct0 p))
  (base c))

(if (signed (if (signed #t 'p)
                (signed #t 'correct0)
                (signed #f 'wrong0))
            'p1)
    (signed 'c 'correct)
    (signed 'a 'wrong))
;provenance;=> (correct p1 correct0 p)
;base;=> c
'expect-layered:
'((provenance (correct p1 correct0 p))
  (base c))

(if (signed (if (signed #t 'p)
                (signed #t 'correct0)
                (signed #f 'wrong0))
            'p1)
    (if (signed #f 'p2)
        (signed 'c 'wrong1)
        (signed 'a 'correct1))
    (if (signed #t 'p2)
        (signed 'c1 'wrong2)
        (signed 'a1 'wrong2)))
;provenance;=> (correct1 p2 p1 correct0 p)
;base;=> a
'expect-layered:
'((provenance (correct1 p2 p1 correct0 p))
  (base a))

;;; Conditionals work in procedures

(define (count n)
  (if (= n (signed 0 'frodo))
      (signed 'done 'gjs)
      (count (- n (signed 1 'bilbo)))))

(count 0)
;provenance;=> (gjs frodo)
;base;=> done
'expect-layered:
'((provenance (gjs frodo))
  (base done))

(count 1)
;provenance;=> (gjs bilbo frodo)
;base;=> done
'expect-layered:
'((provenance (gjs bilbo frodo))
  (base done))

(count 2)
;provenance;=> (gjs bilbo frodo)
;base;=> done
'expect-layered:
'((provenance (gjs bilbo frodo))
  (base done))

(count (signed 0 'sam))
;provenance;=> (gjs frodo sam)
;base;=> done
'expect-layered:
'((provenance (gjs frodo sam))
  (base done))

(count (signed 1 'sam))
;provenance;=> (gjs bilbo frodo sam)
;base;=> done
'expect-layered:
'((provenance (gjs bilbo frodo sam))
  (base done))

(count (signed 2 'sam))
;provenance;=> (gjs bilbo frodo sam)
;base;=> done
'expect-layered:
'((provenance (gjs bilbo frodo sam))
  (base done))

(define (count1 n)
  (if (= n (signed 0 'frodo))
      (signed 'done 'gjs)
      (count1 (- n
                 (signed 1
                         (symbol 'bilbo
                                 (layer-value 'base n)))))))

(count1 5)
;provenance;=> (gjs bilbo1 bilbo2 bilbo3 bilbo4 bilbo5 frodo)
;base;=> done
'expect-layered:
'((provenance (gjs bilbo1 bilbo2 bilbo3 bilbo4 bilbo5 frodo))
  (base done))

(define (count2 n)
  (if (= n (signed 0 (symbol 'frodo (layer-value 'base n))))
      (signed 'done 'gjs)
      (count2 (- n (signed 1 'bilbo)))))

(count2 5)
;provenance;=> (gjs frodo0 frodo1 frodo2
;               frodo3 bilbo frodo4 frodo5)
;base;=> done
'expect-layered:
'((provenance (gjs frodo0 frodo1 frodo2
              frodo3 bilbo frodo4 frodo5))
  (base done))

(define (count3 n)
  (if (= n (signed 0 (symbol 'frodo (layer-value 'base n))))
      (signed 'done 'gjs)
      (count3 (- n
                 (signed 1
                         (symbol 'bilbo
                                 (layer-value 'base n)))))))

(count3 2)
;provenance;=> (gjs bilbo1 frodo0 bilbo2 frodo1 frodo2)
;base;=> done
'expect-layered:
'((provenance (gjs bilbo1 frodo0 bilbo2 frodo1 frodo2))
  (base done))

(define (count4 n)
  (if (= n (signed 0 'frodo))
      (signed 'done 'gjs)
      (count4 (- n (signed 1 (layer-value 'base n))))))

(count4 3)
;provenance;=> (gjs 1 2 3 frodo)
;base;=> done
'expect-layered:
'((provenance (gjs 1 2 3 frodo))
  (base done))

(define (count5 n)
  (if (= n (signed 0 (symbol 'frodo (layer-value 'base n))))
      (signed 'done 'gjs)
      (count5 (- n
                 (signed 1
                         (symbol 'bilbo
                                 (layer-value 'base n)))))))
(set! count5 (signed count5 'gjs1))

(count5 3)
;provenance;=> (gjs frodo0 bilbo1 frodo1
;               bilbo2 frodo2 gjs1 bilbo3 frodo3)
;base;=> done
'expect-layered:
'((provenance (gjs frodo0 bilbo1 frodo1
              bilbo2 frodo2 gjs1 bilbo3 frodo3))
  (base done))

;;; complex stuff with conditionals work!

(define ff-test
  (lambda (select)
    (let ((fact:1 (signed 1 'fact1))
          (fact:2 (signed 2 'fact2))
          (fib:1 (signed 1 'fib1))
          (fib:2 (signed 2 'fib2)))
      (let ((f1
             (lambda (f1 f2)
               (lambda (n)

                 (if (< n fact:2)
                     fact:1
                     (* n ((f1 f1 f2) (- n fact:1)))))))
            (f2
             (lambda (f1 f2)
               (lambda (n)
                 (if (< n fib:2)
                     n
                     (+ ((f2 f1 f2) (- n fib:1))
                        ((f2 f1 f2) (- n fib:2))))))))
        (let ((fact (f1 f1 f2))
              (fib (f2 f1 f2)))
          (let ((fact5 (fact (signed 5 'gjs5)))
                (fib10 (fib (signed 10 'gjs10))))
            (select fact5 fib10 (+ fact5 fib10))))))))

(ff-test (lambda (a b c) a))
;provenance;=> (fact1 fact2 gjs5)
;base;=> 120
'expect-layered:
'((provenance (fact1 fact2 gjs5))
  (base 120))

(ff-test (lambda (a b c) b))
;provenance;=> (fib1 fib2 gjs10)
;base;=> 55
'expect-layered:
'((provenance (fib1 fib2 gjs10))
  (base 55))

(ff-test (lambda (a b c) c))
;provenance;=> (gjs10 fib2 fib1 fact1 fact2 gjs5)
;base;=> 175
'expect-layered:
'((provenance (gjs10 fib2 fib1 fact1 fact2 gjs5))
  (base 175))

;;; Layered Procedure with no explicit provenance layer
;;; acts as base.

(define a
  (make-base-value (lambda (x)
                     (if (< x (signed 0 'g1))
                         (- (signed 0 'g2) x)
                         (signed x 'g3)))))

(a 3)
;provenance;=> (g3 g1)
;base;=> 3
'expect-layered:
'((provenance (g3 g1))
  (base 3))

(a (signed 3 'foo))
;provenance;=> (g3 g1 foo)
;base;=> 3
'expect-layered:
'((provenance (g3 g1 foo))
  (base 3))

(a (signed -3 'foo))
;provenance;=> (g2 g1 foo)
;base;=> 3
'expect-layered:
'((provenance (g2 g1 foo))
  (base 3))

;;; Since a is a layered procedure with no provenance,
;;; its signed argument loses its provenance as the
;;; value of the formal parameter x, so this does not
;;; propagate internal provenance computations.

;;; Raw scheme just passes through arguments with
;;; provenance Subtlety with **IF** here, see
;;; analyze.scm.

(define b
  (lambda (x)
    (if (< x (signed 0 'g1))
        (- (signed 0 'g2) x)
        (signed x 'g3))))

(b (signed -3 'foo))
;provenance;=> (g2 g1 foo)
;base;=> 3
'expect-layered:
'((provenance (g2 g1 foo))
  (base 3))

(b (signed 3 'foo))
;provenance;=> (g3 g1 foo)
;base;=> 3
'expect-layered:
'((provenance (g3 g1 foo))
  (base 3))

(define b (signed b "gjs"))

(b (signed -3 'foo))
;provenance;=> (g2 g1 "gjs" foo)
;base;=> 3
'expect-layered:
'((provenance (g2 g1 "gjs" foo))
  (base 3))

(b (signed 3 'foo))
;provenance;=> (g3 g1 "gjs" foo)
;base;=> 3
'expect-layered:
'((provenance (g3 g1 "gjs" foo))
  (base 3))

;;; Here is unsigned count.

(define count
        (lambda (n)
          (if (= n (signed 0 'g3))
              (signed 'done 'g4)
              (count (- n (signed 1 'g5))))))

(count 4)
;provenance;=> (g4 g5 g3)
;base;=> done
'expect-layered:
'((provenance (g4 g5 g3))
  (base done))

(count (signed 0 'g6))
;provenance;=> (g4 g3 g6)
;base;=> done
'expect-layered:
'((provenance (g4 g3 g6))
  (base done))

(count (signed 4 'g6))
;provenance;=> (g4 g5 g3 g6)
;base;=> done
'expect-layered:
'((provenance (g4 g5 g3 g6))
  (base done))

;;; Now we sign count.

(define count
  (signed count "gjs"))

(count (signed 4 'g6))
;provenance;=> (g4 g5 g3 "gjs" g6)
;base;=> done
'expect-layered:
'((provenance (g4 g5 g3 "gjs" g6))
  (base done))
