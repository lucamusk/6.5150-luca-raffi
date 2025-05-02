#|
Zippers into SExprs
|#

(define-record-type zipper
  (%make-zipper point left right trail)
  zipper?
  (point zipper-point)
  (left  zipper-left)
  (right zipper-right)
  (trail zipper-trail))

(define (zip-up value)
  (%make-zipper value '() '() '()))

(define (zip-parent zip)
  (define left  (zipper-left zip))
  (define right (zipper-right zip))
  (define trail (zipper-trail zip))
  (define point (zipper-point zip))
  (and (pair? trail)
       (let ((point^ (append-reverse left (cons point right)))
             (left^  (caar trail))
             (right^ (cdar trail))
             (trail^ (cdr trail)))
         (%make-zipper point^ left^ right^ trail^))))

(define (zip-left zip)
  (define left (zipper-left zip))
  (and (pair? left)
       (%make-zipper (car left)
                     (cdr left)
                     (cons (zipper-point zip) (zipper-right zip))
                     (zipper-trail zip))))

(define (zip-right zip)
  (define right (zipper-right zip))
  (and (pair? right)
       (%make-zipper (car right)
                     (cons (zipper-point zip) (zipper-left zip))
                     (cdr right)
                     (zipper-trail zip))))

(define (zip-child zip #!optional index)
  (define left  (zipper-left  zip))
  (define right (zipper-right zip))
  (define trail (zipper-trail zip))
  (define point (zipper-point zip))
  (set! index (if (default-object? index) 0 index))
  (let*-values (((left^ point^.right^) (split-at point index))
                ((point^ right^)       (car+cdr point^.right^))
                ((trail^)              `((,left . ,right) . ,trail)))
    (%make-zipper point^ (reverse left^) right^ trail^)))

(define (zip-top zip)
  (cond ((zip-parent zip) => zip-top)
        (else zip)))

(define (zipper-insert-right zip obj)
  (%make-zipper (zipper-point zip)
                (zipper-left zip)
                (cons obj (zipper-right zip))
                (zipper-trail zip)))

(define (zipper-insert-left zip obj)
  (%make-zipper (zipper-point zip)
                (cons obj (zipper-left zip))
                (zipper-right zip)
                (zipper-trail zip)))

(define (zipper-update zip fn)
  (%make-zipper (fn (zipper-point zip))
                (zipper-left zip)
                (zipper-right zip)
                (zipper-trail zip)))

(define (zipper-swap zip obj) (zip-update zip (constant-procedure obj)))

(define (zipper-delete-left zip)
  (and (pair? (zipper-left zip))
       (%make-zipper (zipper-point zip)
                     (cdr (zipper-left zip))
                     (zipper-right zip)
                     (zipper-trail zip))))

(define (zipper-delete-right zip)
  (and (pair? (zipper-right zip))
       (%make-zipper (zipper-point zip)
                     (zipper-left zip)
                     (cdr (zipper-right zip))
                     (zipper-trail zip))))
