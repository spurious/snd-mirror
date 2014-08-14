(provide 'mockery.scm)

(define (outlet-member obj e)
  (or (eq? obj e)
      (and (not (eq? obj (rootlet)))
	   (outlet-member (outlet obj) e))))


;;; --------------------------------------------------------------------------------

(define *mock-vector*
  (let ((mock-vector-class
	 (openlet  ; this holds the mock-vector methods
	  (inlet 'local-set!         (lambda (obj i val) (#_vector-set! (obj 'v) i val))
		 'vector?            (lambda (obj) #t)
		 'vector-ref         (lambda (obj i) (#_vector-ref (obj 'v) i))
		 'vector-set!        (lambda (obj i val) ((obj 'local-set!) obj i val) val)
		 'let-ref            (lambda (obj i) (#_vector-ref (obj 'v) i))           ; these are the implicit cases
		 'let-set!           (lambda (obj i val) ((obj 'local-set!) obj i val) val)
		 'length             (lambda (obj) (#_vector-length (obj 'v)))
		 'vector-length      (lambda (obj) (#_vector-length (obj 'v)))
		 'map                (lambda (f obj) (map f (obj 'v)))
		 'for-each           (lambda (f obj) (for-each f (obj 'v)))
		 'vector->list       (lambda (obj) (#_vector->list (obj 'v)))
		 'make-shared-vector (lambda* (obj dim (off 0)) (#_make-shared-vector (obj 'v) dim off))
		 'vector-append      (lambda (obj . vs) (apply #_vector-append (obj 'v) vs))
		 'vector-fill!       (lambda* (obj val (start 0) end) (#_vector-fill! (obj 'v) val start (or end (#_vector-length (obj 'v)))))
		 'fill!              (lambda (obj val) (#_fill! (obj 'v) val))
		 'reverse            (lambda (obj) (#_reverse (obj 'v)))
		 'sort!              (lambda (obj f) (#_sort! (obj 'v) f))
		 'object->string     (lambda* (obj (w #t)) "#<mock-vector-class>")
		 'vector-dimensions  (lambda (obj) (#_vector-dimensions (obj 'v)))
		 'copy               (lambda* (src dest . args)
				       ;; copy by itself does not make a new vector, but if no dest we
				       ;;   need a copy of src, so use coverlet/openlet to make sure
				       ;;   we aren't caught in an infinite recursion.
				       (if (and dest (not (let? dest)))
					   (apply copy (obj 'v) dest args)
					   (let ((nobj (or dest (openlet (copy (coverlet src))))))
					     (openlet src)
					     (set! (nobj 'v) (copy (src 'v)))
					     nobj)))))))

    (define* (make-mock-vector len (init #<unspecified>))
      (openlet (sublet mock-vector-class 
		 'v (make-vector len init)
		 'object->string (lambda* (obj (w #t)) (#_object->string (obj 'v) w)))))

    (define (mock-vector . args)
      (let ((v (make-mock-vector 0)))
	(set! (v 'v) (apply vector args))
	v))

    (define (mock-vector? obj)
      (and (openlet? obj)
	   (outlet-member obj mock-vector-class)))

    (curlet)))

#|
(define stretchable-vector-class
  (let ((local-ref (lambda (obj index)
		     (if (>= index (length (obj 'v)))
			 (obj 'initial-element)
			 (#_vector-ref (obj 'v) index))))
	(local-set! (lambda (obj index val)
		      (if (>= index (length (obj 'v)))
			  (set! (obj 'v) (copy (obj 'v) (make-vector (+ index 8) (obj 'initial-element)))))
		      (#_vector-set! (obj 'v) index val))))
    (openlet
     (sublet (*mock-vector* 'mock-vector-class)
       'v (vector)
       'object->string (lambda* (obj (w #t)) (format #f "#<stretchable-vector: ~S>" (obj 'v)))
       'initial-element #f
       'vector-ref local-ref
       'let-ref local-ref
       'vector-set! local-set!
       'let-set! local-set!))))
|#


;;; --------------------------------------------------------------------------------

(define *mock-hash-table*
  (let ((mock-hash-table-class
	 (openlet
	  (inlet 'hash-table?     (lambda (obj) #t)
		 'hash-table-ref  (lambda (obj key)      (#_hash-table-ref (obj 'ht) key))
		 'hash-table-set! (lambda (obj key val)  (#_hash-table-set! (obj 'ht) key val))
		 'hash-table-size (lambda (obj)          (#_hash-table-size (obj 'ht)))
		 'hash-table-entries (lambda (obj)       (#_hash-table-entries (obj 'ht)))
		 'make-hash-table-iterator (lambda (obj) (#_make-hash-table-iterator (obj 'ht)))
		 'let-ref         (lambda (obj i)        (#_hash-table-ref (obj 'ht) key))
		 'let-set!        (lambda (obj i val)    (#_hash-table-set! (obj 'ht) key val))
		 'length          (lambda (obj)          (#_hash-table-size (obj 'ht)))
		 'map             (lambda (f obj)        (map f (obj 'ht)))
		 'for-each        (lambda (f obj)        (for-each f (obj 'ht)))
		 'fill!           (lambda (obj val)      (#_fill! (obj 'ht) val))
		 'reverse         (lambda (obj)          (#_reverse (obj 'ht)))
		 'object->string  (lambda* (obj (w #t))  "#<mock-hash-table-class>")
		 'copy            (lambda* (src dest . args)
				    (if (and dest (not (let? dest)))
					(apply copy (obj 'ht) dest args)
					(let ((nobj (or dest (openlet (copy (coverlet src))))))
					  (openlet src)
					  (set! (nobj 'ht) (copy (src 'ht)))
					  nobj)))))))


    (define* (make-mock-hash-table (len 511))
      (openlet (sublet mock-hash-table-class 
		 'ht (make-hash-table len)
		 'object->string (lambda* (obj (w #t)) (#_object->string (obj 'ht) w)))))

    (define (mock-hash-table . args)
      (let ((v (make-mock-hash-table)))
	(set! (v 'ht) (apply hash-table args))
	v))

    (define (mock-hash-table? obj)
      (and (openlet? obj)
	   (outlet-member obj mock-hash-table-class)))

    (curlet)))


