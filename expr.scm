(define* (linear-expr data (initial-range 1.0) (generations 100))
  
  (let ((size 100)
	(range initial-range)
	(data-size (length data)))

    (let ((population (make-vector size)))
      
      (define (initial-generation)
	(do ((i 0 (+ i 1)))
	    ((= i size))
	  (set! (population i) (vector (- (random range) (/ range 2)) (- (random range) (/ range 2))))))


      (define (distance coeff offset)
	(let ((dist 0.0))
	  (do ((k 0 (+ k 1)))
	      ((= k data-size) dist)
	    (let ((err (- (data k) (+ (* coeff k) offset))))
	      (set! dist (+ dist (* err err)))))))
	

      (define (next-generation)
	(let ((distances (make-vector size 0.0)))
	  (do ((i 0 (+ i 1)))
	      ((= i size))
	    (set! (distances i) (cons (distance ((population i) 0) ((population i) 1)) i)))
	  (sort! distances (lambda (a b) (< (car a) (car b))))
	  (do ((i (/ size 2) (+ i 1))
	       (j 0 (+ j 1)))
	      ((= i size))
	    (let ((coeff ((population j) 0))
		  (offset ((population j) 1))
		  (loc (cdr (distances i))))
	      (set! (population loc) (vector (+ coeff (- (random range) (/ range 2)))
					     (+ offset (- (random range) (/ range 2)))))))
	  (distances 0)))


      (initial-generation)
      (let ((last-dist size)
	    (best #f))
	(do ((i 0 (+ i 1)))
	    ((= i generations))
	  (set! best (next-generation))
	  (let ((new-dist (car best)))
	    (if (< (abs (- new-dist last-dist)) range)
		(set! range (* range 0.9)))
	    (set! last-dist new-dist)))

	(let ((simpler-coeff (rationalize ((population (cdr best)) 0) .1))
	      (simpler-offset (rationalize ((population (cdr best)) 1) .1)))
	  (let ((dist (distance simpler-coeff simpler-offset)))
	    (if (< dist (* 2 (car best)))
		(format #f "~A * k + ~A, err: ~A~%" simpler-coeff simpler-offset dist)
		(format #f "~A * k + ~A, err: ~A~%" ((population (cdr best)) 0) ((population (cdr best)) 1) (car best)))))))))



#|
(linear-expr (let ((v (make-vector 100)))
	(do ((i 0 (+ i 1))) 
	    ((= i 100)) 
	  (set! (v i) (+ 1 (* i 2))) )
	v))
"2 * k + 1, err: 0.0
"


|#


