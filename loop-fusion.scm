(load "./opt-interface.scm")
(define (loop-fuser loop-1 loop-2)
  (define optimizer (make-pattern-operator))
  ;;; Where the bounds are equal
  (attach-rule! optimizer
                (rule `(((?? a)
                         (for ,loop-1 (? bound-low) (? bound-high) (? body-1))
                         (?? b)
                         (for ,loop-2 (? bound-low) (? bound-high) (? body-2))
                         (?? c)))
                      `(,@a
                        (for ,loop-1 ,bound-low ,bound-high
                          (,body-1
                           ,(or (optimize-at body-2 loop-2 (rename loop-2 loop-1) #t) body-2)))
                        ,@b
                        ,@c)))
  ;;; Where the bounds may not be equal (doesn't statically check if iterations are
  ;;; the same, that's the programmer's burden
  ;;; Numeric bounds
  (attach-rule! optimizer
                (rule `(((?? a)
                         (for ,loop-1 (? bound-low-1, number?) (? bound-high-1) (? body-1))
                         (?? b)
                         (for ,loop-2 (? bound-low-2, number?) (? bound-high-2) (? body-2))
                         (?? c)))
                      `(,@a
                        (for ,loop-1 ,bound-low-1 ,bound-high-1
                          (,body-1
                           ,(or
                              (optimize-at body-2 loop-2
                                           (rename loop-2 `(+ ,loop-1 ,(- bound-low-2 bound-low-1)))
                                           #t)
                              body-2)))
                        ,@b
                        ,@c)))

  ;;; One or more symbolic bounds
  (attach-rule! optimizer
                (rule `(((?? a)
                         (for ,loop-1 (? bound-low-1) (? bound-high-1) (? body-1))
                         (?? b)
                         (for ,loop-2 (? bound-low-2) (? bound-high-2) (? body-2))
                         (?? c)))
                      `(,@a
                        (for ,loop-1 ,bound-low-1 ,bound-high-1
                          (,body-1
                           ,(or
                              (optimize-at body-2 loop-2
                                           (rename loop-2 `(+ ,loop-1 (- ,bound-low-2 ,bound-low-1)))
                                           #t)
                              body-2)))
                        ,@b
                        ,@c)))

  optimizer)
