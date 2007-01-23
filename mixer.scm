;;; mixer and frame stuff, mostly oriented toward linear algebra (see also snd-test)
;;;
;;; make-zero-mixer, mixer-diagonal?, mixer-transpose, mixer-determinant,
;;; mixer-solve, mixer-inverse, invert-matrix, mixer-trace, mixer-poly, mixer-copy

(provide 'snd-mixer.scm)

(define (mixer-copy umx)
  "(mixer-copy umx) returns a copy of its argument (a mixer)"
  (let* ((size (mus-length umx))
	 (mx (make-mixer size)))
    (do ((i 0 (1+ i)))
	((= i size))
      (do ((j 0 (1+ j)))
	  ((= j size))
	(mixer-set! mx i j (mixer-ref umx i j))))
    mx))

(define (make-zero-mixer n) 
  "(make-zero-mixer n) returns an empty mixer"
  (make-mixer n))

(define mat
  (make-procedure-with-setter
   (lambda (m i j)
     "(mat m i j) accesses an element of the matrix (mixer) 'm'"
     (mixer-ref m i j))
   (lambda (m i j x)
     (mixer-set! m i j x))))

(define (mixer-diagonal? m)
  "(mixer-diagonal? m) returns #t if 'm' is a diagonal mixer"
  (let ((n (mus-length m)))
    (or (= n 1)
	(call-with-current-continuation
	 (lambda (return)
	   (do ((i 0 (1+ i)))
	       ((= i n) #t)
	     (do ((j 0 (1+ j)))
		 ((= j n))
	       (if (and (not (= i j))
			(not (= (mat m i j) 0.0)))
		   (return #f)))))))))
	   
(define (mixer-transpose mx)
  "(mixer-transpose mx) returns a new mixer of 'mx' transposed"
  (let* ((n (mus-length mx))
	 (nmx (make-zero-mixer n)))
    (do ((i 0 (1+ i)))
	((= i n))
      (do ((j 0 (1+ j)))
	  ((= j n))
	(set! (mat nmx j i) (mat mx i j))))
    nmx))

(define (sub-matrix mx row col)
  "(sub-matrix mx row col) returns a portion of the matrix 'mx'"
  (let* ((old-n (mus-length mx))
	 (new-n (1- old-n))
	 (nmx (make-zero-mixer new-n)))
    (do ((i 0 (1+ i))
	 (ni 0))
	((= i old-n))
      (if (not (= i row))
	  (begin
	    (do ((j 0 (1+ j))
		 (nj 0))
		((= j old-n))
	      (if (not (= j col))
		  (begin
		    (set! (mat nmx ni nj) (mat mx i j))
		    (set! nj (+ nj 1)))))
	    (set! ni (1+ ni)))))
    nmx))

(define (mixer-determinant mx)
  "(mixer-determinant mx) returns the determinant of 'mx'"
  (let ((n (mus-length mx)))
    (if (= n 1) 
	(mat mx 0 0)
	(if (= n 2)
	    (- (* (mat mx 0 0) (mat mx 1 1))
	       (* (mat mx 0 1) (mat mx 1 0)))
	    (if (= n 3)
		(- (+ (* (mat mx 0 0) (mat mx 1 1) (mat mx 2 2))
		      (* (mat mx 0 1) (mat mx 1 2) (mat mx 2 0))
		      (* (mat mx 0 2) (mat mx 1 0) (mat mx 2 1)))
		   (+ (* (mat mx 0 0) (mat mx 1 2) (mat mx 2 1))
		      (* (mat mx 0 1) (mat mx 1 0) (mat mx 2 2))
		      (* (mat mx 0 2) (mat mx 1 1) (mat mx 2 0))))
		(let ((sum 0.0)
		      (sign 1))
		  (do ((i 0 (1+ i)))
		      ((= i n))
		    (let ((mult (mat mx 0 i)))
		      (if (not (= mult 0.0))
			  (set! sum (+ sum (* sign mult (mixer-determinant (sub-matrix mx 0 i))))))
		      (set! sign (- sign))))
		  sum))))))

(define* (mixer-poly mx :rest coeffs)
  "(mixer-poly mx :rest coeffs) returns a new mixer, the result of treating 'mx' as the argument to the polynomial defined by the 'coeffs' list"
  (let* ((n (length coeffs))
	 (nmx (make-scalar-mixer (mus-length mx) (list-ref coeffs (1- n))))
	 (x (mixer* mx 1.0)))
    (do ((i (- n 2) (1- i)))
	((< i 0))
      (set! nmx (mixer+ nmx (mixer* x (list-ref coeffs i))))
      (set! x (mixer* mx x)))
    nmx))

;;; (define (vct-norm v1) (sqrt (dot-product v1 v1)))

(define (mixer-trace mx)
  "(mixer-trace mx) returns the trace of 'mx'"
  (let ((sum 0.0)
	(n (mus-length mx)))
    (do ((i 0 (1+ i)))
	((= i n) sum)
      (set! sum (+ sum (mat mx i i))))))


(define* (invert-matrix matrix :optional b (zero 1.0e-7))
  "(invert-matrix matrix :optional b (zero 1.0e-7)) inverts 'matrix'"
  ;; translated from Numerical Recipes (gaussj)
  (call-with-current-continuation
   (lambda (return)
     (let* ((n (mus-length matrix))
	    (cols (make-vector n 0))
	    (rows (make-vector n 0))
	    (pivots (make-vector n 0)))
       (do ((i 0 (1+ i)))
	   ((= i n))
	 (let ((biggest 0.0)
	       (col 0)
	       (row 0))
	   (do ((j 0 (1+ j)))
	       ((= j n))
	     (if (not (= (vector-ref pivots j) 1))
		 (begin
		   (do ((k 0 (1+ k)))
		       ((= k n))
		     (if (= (vector-ref pivots k) 0)
			 (let ((val (abs (mat matrix j k))))
			   (if (> val biggest)
			       (begin
				 (set! col k)
				 (set! row j)
				 (set! biggest val))))
			 (if (> (vector-ref pivots k) 1)
			     (return #f)))))))
	   (if (< biggest zero) (return #f)) ; this can be fooled (floats...): (invert-matrix (make-mixer 3 1 2 3 3 2 1 4 5 6))
	   (vector-set! pivots col (+ (vector-ref pivots col) 1))
	   (if (not (= row col))
	       (let ((temp (if b (frame-ref b row) 0.0)))
		 (if b
		     (begin
		       (frame-set! b row (frame-ref b col))
		       (frame-set! b col temp)))
		 (do ((k 0 (1+ k)))
		     ((= k n))
		   (set! temp (mat matrix row k))
		   (set! (mat matrix row k) (mat matrix col k))
		   (set! (mat matrix col k) temp))))
	   (vector-set! cols i col)
	   (vector-set! rows i row)
	   ;; round-off troubles here
	   (if (< (abs (mat matrix col col)) zero)
	       (return #f))
	   (let ((inverse-pivot (/ 1.0 (mat matrix col col))))
	     (set! (mat matrix col col) 1.0)
	     (do ((k 0 (1+ k)))
		 ((= k n))
	       (set! (mat matrix col k) (* inverse-pivot (mat matrix col k))))
	     (if b (frame-set! b col (* inverse-pivot (frame-ref b col)))))
	   (do ((k 0 (1+ k)))
	       ((= k n))
	     (if (not (= k col))
		 (let ((scl (mat matrix k col)))
		   (set! (mat matrix k col) 0.0)
		   (do ((m 0 (1+ m)))
		       ((= m n))
		     (set! (mat matrix k m) (- (mat matrix k m) (* scl (mat matrix col m)))))
		   (if b (frame-set! b k (- (frame-ref b k) (* scl (frame-ref b col))))))))))
       (do ((i (1- n) (1- i)))
	   ((< i 0))
	 (if (not (= (vector-ref rows i) (vector-ref cols i)))
	     (do ((k 0 (1+ k)))
		 ((= k n))
	       (let ((temp (mat matrix k (vector-ref rows i))))
		 (set! (mat matrix k (vector-ref rows i)) (mat matrix k (vector-ref cols i)))
		 (set! (mat matrix k (vector-ref cols i)) temp)))))
       (list matrix b)))))

;;; it would be faster to use invert-matrix to calculate the determinant, but that
;;;   really forces us to use doubles throughout -- probably should anyway...

(define (mixer-solve A b)
  "(mixer-solve A b) returns the solution of Ax=b"
  (let ((val (invert-matrix A b)))
    (and val (cadr val))))

(define (mixer-inverse A)
  "(mixer-inverse A) returns the inverse of 'A'"
  (let ((val (invert-matrix A)))
    (and val (car val))))

#|
(define (plane p1 p2 p3) ; each p a list of 3 coords, returns list (a b c d) of ax + by + cz = 1 (d = -1)
  (let ((m (make-mixer 3))
	(f (make-frame 3 1 1 1)))
    (do ((i 0 (1+ i)))
	((= i 3))
      (mixer-set! m 0 i (list-ref p1 i))
      (mixer-set! m 1 i (list-ref p2 i))
      (mixer-set! m 2 i (list-ref p3 i)))
    (let ((b (mixer-solve m f)))
      (list (frame-ref b 0) (frame-ref b 1) (frame-ref b 2) -1))))

;;; (plane '(0 0 1) '(1 0 0) '(0 1 0))
;;; (1.0 1.0 1.0 -1)
|#
