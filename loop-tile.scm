(load "./opt-interface.scm")
(define (loop-tile tile-factor)
  (define optimizer (make-pattern-operator))
  ;;; Numeric bounds
  (attach-rule! optimizer
                (rule `((for (? loop-var) (? bound-low, number?) (? bound-high, number?) (? body)))
                      `(for ,loop-var 0 ,(/ (- bound-high bound-low) tile-factor)
                          (for ,(symbol loop-var loop-var)
                              (* ,loop-var ,tile-factor)
                              (+ (* ,loop-var ,tile-factor) ,tile-factor)
                            ,(optimize-at body loop-var
                                          (rename loop-var (symbol loop-var loop-var))
                                          #t)))))

  ;;; Symbolic bounds
  (attach-rule! optimizer
                (rule `((for (? loop-var) (? bound-low) (? bound-high) (? body)))
                      `(for ,loop-var 0 (/ (- ,bound-high ,bound-low) ,tile-factor)
                               (for ,(symbol loop-var loop-var)
				    (* ,loop-var ,tile-factor)
				    (+ (* ,loop-var ,tile-factor) ,tile-factor)
				    ,(optimize-at body loop-var
						  (rename loop-var (symbol loop-var loop-var))
						  #t)))))
  optimizer)
