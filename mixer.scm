;;; mixer and frame stuff, mostly oriented toward linear algebra (see also snd-test)
;;;
;;; make-zero-mixer, mixer-diagonal?, mixer-transpose, mixer-determinant,
;;; mixer-solve, mixer-inverse, invert-matrix, mixer-trace, mixer-poly, mixer-copy

(provide 'snd-mixer.scm)

(define make-zero-mixer make-mixer)

(define (mixer-copy umx)
  "(mixer-copy umx) returns a copy of its argument (a mixer)"
  (let* ((size (length umx))
	 (mx (make-mixer size)))
    (do ((i 0 (+ i 1)))
	((= i size))
      (do ((j 0 (+ j 1)))
	  ((= j size))
	(set! (mx i j) (umx i j))))
    mx))


(define (mixer-diagonal? m)
  "(mixer-diagonal? m) returns #t if 'm' is a diagonal mixer"
  (let ((n (length m)))
    (or (= n 1)
	(call-with-exit
	 (lambda (return)
	   (do ((i 0 (+ i 1)))
	       ((= i n) #t)
	     (do ((j 0 (+ j 1)))
		 ((= j n))
	       (if (and (not (= i j))
			(not (= (m i j) 0.0)))
		   (return #f)))))))))
	   
(define (mixer-transpose mx)
  "(mixer-transpose mx) returns a new mixer of 'mx' transposed"
  (let* ((n (length mx))
	 (nmx (make-zero-mixer n)))
    (do ((i 0 (+ i 1)))
	((= i n))
      (do ((j 0 (+ j 1)))
	  ((= j n))
	(set! (nmx j i) (mx i j))))
    nmx))

(define (sub-matrix mx row col)
  "(sub-matrix mx row col) returns a portion of the matrix 'mx'"
  (let* ((old-n (length mx))
	 (new-n (- old-n 1))
	 (nmx (make-zero-mixer new-n)))
    (do ((i 0 (+ i 1))
	 (ni 0))
	((= i old-n))
      (if (not (= i row))
	  (begin
	    (do ((j 0 (+ j 1))
		 (nj 0))
		((= j old-n))
	      (if (not (= j col))
		  (begin
		    (set! (nmx ni nj) (mx i j))
		    (set! nj (+ nj 1)))))
	    (set! ni (+ 1 ni)))))
    nmx))

(define (mixer-determinant mx)
  "(mixer-determinant mx) returns the determinant of 'mx'"
  (if (not (mixer? mx))
      (error 'wrong-type-arg "mixer-determinant argument should be a mixer")
      (let ((n (length mx)))
	(if (= n 1) 
	    (mx 0 0)
	    (if (= n 2)
		(- (* (mx 0 0) (mx 1 1))
		   (* (mx 0 1) (mx 1 0)))
		(if (= n 3)
		    (- (+ (* (mx 0 0) (mx 1 1) (mx 2 2))
			  (* (mx 0 1) (mx 1 2) (mx 2 0))
			  (* (mx 0 2) (mx 1 0) (mx 2 1)))
		       (+ (* (mx 0 0) (mx 1 2) (mx 2 1))
			  (* (mx 0 1) (mx 1 0) (mx 2 2))
			  (* (mx 0 2) (mx 1 1) (mx 2 0))))
		    (let ((sum 0.0)
			  (sign 1))
		      (do ((i 0 (+ i 1)))
			  ((= i n))
			(let ((mult (mx 0 i)))
			  (if (not (= mult 0.0))
			      (set! sum (+ sum (* sign mult (mixer-determinant (sub-matrix mx 0 i))))))
			  (set! sign (- sign))))
		      sum)))))))

(define* (mixer-poly mx :rest coeffs)
  "(mixer-poly mx :rest coeffs) returns a new mixer, the result of treating 'mx' as the argument to the polynomial defined by the 'coeffs' list"
  (let* ((n (length coeffs))
	 (nmx (make-scalar-mixer (length mx) (coeffs (- n 1))))
	 (x (mixer* mx 1.0)))
    (do ((i (- n 2) (- i 1)))
	((< i 0))
      (set! nmx (mixer+ nmx (mixer* x (coeffs i))))
      (set! x (mixer* mx x)))
    nmx))

;;; (define (float-vector-norm v1) (sqrt (dot-product v1 v1)))

(define (mixer-trace mx)
  "(mixer-trace mx) returns the trace of 'mx'"
  (let ((sum 0.0)
	(n (length mx)))
    (do ((i 0 (+ i 1)))
	((= i n) sum)
      (set! sum (+ sum (mx i i))))))


(define* (invert-matrix matrix b (zero 1.0e-7))
  "(invert-matrix matrix b (zero 1.0e-7)) inverts 'matrix'"
  ;; translated from Numerical Recipes (gaussj)

  ;(format #t "~%~%invert-matrix n: ~D, ~S, b: ~A, ~S~%" (length matrix) matrix (and b (length b)) b)
  (call-with-exit
   (lambda (return)
     (let* ((n (length matrix))
	    (cols (make-vector n 0))
	    (rows (make-vector n 0))
	    (pivots (make-vector n 0)))
       (do ((i 0 (+ i 1)))
	   ((= i n))
	 (let ((biggest 0.0)
	       (col 0)
	       (row 0))
	   (do ((j 0 (+ j 1)))
	       ((= j n))
	     ;(format #t "j: ~A, n: ~A~%" j n)
	     (if (not (= (pivots j) 1))
		 (begin
		   (do ((k 0 (+ k 1)))
		       ((= k n))
		     (if (= (pivots k) 0)
			 (let ((val (abs (matrix j k))))
			   (if (> val biggest)
			       (begin
				 (set! col k)
				 (set! row j)
				 ;(format #t "k: ~A, row: ~D, col: ~A~%" k row col)
				 (set! biggest val))))
			 (if (> (pivots k) 1)
			     (return #f)))))))
	   (if (< biggest zero) (return #f)) ; this can be fooled (floats...): (invert-matrix (make-mixer 3 1 2 3 3 2 1 4 5 6))
	   (set! (pivots col) (+ (pivots col) 1))
	   ;(format #t "i: ~D, row: ~D, col: ~A~%" i row col)
	   (if (not (= row col))
	       (let ((temp (if b (b row) 0.0)))
		 (if b
		     (begin
		       (set! (b row) (b col))
		       (set! (b col) temp)))
		 (do ((k 0 (+ k 1)))
		     ((= k n))
		   (set! temp (matrix row k))
		   (set! (matrix row k) (matrix col k))
		   (set! (matrix col k) temp))))
	   (set! (cols i) col)
	   (set! (rows i) row)
	   ;; round-off troubles here
	   (if (< (abs (matrix col col)) zero)
	       (return #f))
	   (let ((inverse-pivot (/ 1.0 (matrix col col))))
	     (set! (matrix col col) 1.0)
	     (do ((k 0 (+ k 1)))
		 ((= k n))
	       (set! (matrix col k) (* inverse-pivot (matrix col k))))
	     (if b (set! (b col) (* inverse-pivot (b col)))))
	   (do ((k 0 (+ k 1)))
	       ((= k n))
	     (if (not (= k col))
		 (let ((scl (matrix k col)))
		   (set! (matrix k col) 0.0)
		   (do ((m 0 (+ 1 m)))
		       ((= m n))
		     (set! (matrix k m) (- (matrix k m) (* scl (matrix col m)))))
		   (if b (set! (b k) (- (b k) (* scl (b col))))))))))
       (do ((i (- n 1) (- i 1)))
	   ((< i 0))
	 (if (not (= (rows i) (cols i)))
	     (do ((k 0 (+ k 1)))
		 ((= k n))
	       (let ((temp (matrix k (rows i))))
		 (set! (matrix k (rows i)) (matrix k (cols i)))
		 (set! (matrix k (cols i)) temp)))))
       (list matrix b)))))

;;; it would be faster to use invert-matrix to calculate the determinant

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
    (do ((i 0 (+ i 1)))
	((= i 3))
      (set! (m 0 i) (p1 i))
      (set! (m 1 i) (p2 i))
      (set! (m 2 i) (p3 i)))
    (let ((b (mixer-solve m f)))
      (list (b 0) (b 1) (b 2) -1))))

;;; (plane '(0 0 1) '(1 0 0) '(0 1 0))
;;; (1.0 1.0 1.0 -1)
|#
