(define-record-type array
  (make-array shape start elements)
  array?
  (shape     array-shape)
  (start    array-start)
  (elements array-elements))

(define array-length (compose car array-shape))

(define (array-slice arr indices)
  (let loop ((indices  indices)
             (shape    (array-shape arr))
             (start    (array-start arr)))
    (if (pair? indices)
        (let* ((element-size (apply * (cdr shape)))
               (start        (+ start (* (car indices) element-size))))
          (loop (cdr indices) (cdr shape) start))
        (values shape start))))

(define (array-ref arr indicies)
  (define-values (shape start) (array-slice arr indicies))
  (if (null? shape)
      (flo:vector-ref (array-elements arr) (inexact->exact start))
      (make-array shape start (array-elements arr))))

(define (allocate-array . indicies)
  (define size (apply * indicies))
  (make-array indicies 0 (flo:vector-cons (inexact->exact size))))

(define (constant-array obj)
  (define fv (flo:vector-cons 1))
  (flo:vector-set! fv 0 (->flonum obj))
  (make-array '() 0 fv))

(define (for-loop min max body)
  (when (< min max)
    (body (constant-array min))
    (for-loop (+ 1 min) max body)))

(define (array-set! arr indicies val)
  (define-values (shape start) (array-slice arr indicies))
  (let loop ((shape  shape)
             (start start))
    (if (null? shape)
        (flo:vector-set! (array-elements arr) (inexact->exact start) (->flonum val))
        (for-each (lambda (i) (loop (cdr shape) (+ start (* i (car shape)))))
                  (iota (car shape))))))

(define (array->vector-of-vectors arr)
  (cond
   ((not (array? arr))
    arr)
   ((null? (array-shape arr))
    (array-ref arr (list)))
   (else
    (make-initialized-vector
     (inexact->exact (array-length arr))
     (lambda (i)
       (array->vector-of-vectors (array-ref arr (list i))))))))
