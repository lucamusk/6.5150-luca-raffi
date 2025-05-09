(load "./opt-interface.scm")
(define (loop-reorder outer-loop-var inner-loop-var)
  ;;; More complicated since we need to bring data
  ;;; into shallower scopes
  (lambda (c)
    (define (at-loop outer-loop)
      (let ((bound-low-o (caddr outer-loop))
            (bound-high-o (cadddr outer-loop))
            (inner-loop (search-tree outer-loop
                                     `(for ,inner-loop-var (? bound-low-i) (? bound-high-i) (? body-i))
                                     id-succ)))
        (if (not inner-loop)
            outer-loop ;;; Not a match, keep searching
            (let ((bound-low-i (caddr inner-loop))
                  (bound-high-i (cadddr inner-loop)))
              (define optimizer-inner (make-pattern-operator))
              (attach-rule! optimizer-inner
                            (rule `((for ,inner-loop-var (? bound-low-i) (? bound-high-i)
                                      (? body-i)))
                                  `(for ,outer-loop-var ,bound-low-o ,bound-high-o ,body-i)))

              (define optimizer-outer (make-pattern-operator))
              (attach-rule! optimizer-outer
                            (rule `((?? a)
                                    (for ,outer-loop-var (? bound-low-o) (? bound-high-o)
                                      (? body-o))
                                    (?? b))
                                  `(,@a
                                    (for ,inner-loop-var ,bound-low-i ,bound-high-i
                                      ,(transform-tree body-o
                                                       `(for ,inner-loop-var
                                                            ,bound-low-i
                                                            ,bound-high-i
                                                          (? body-i))
                                                       optimizer-inner
                                                       #f))
                                    ,@b)))
              (optimizer-outer c)))))

    (transform-tree c
                    `(for ,outer-loop-var (? bound-low-o) (? bound-high-o) (? body-o))
                    at-loop
                    #f)))
