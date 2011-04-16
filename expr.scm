(define* (linear-expr-1D data (generations 100))
  
  (let ((size 100)
	(range 1.0)
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
	    (let* ((loc (cdr (distances j)))      ; location (in population) of a good one
		   (coeff ((population loc) 0))   ;   its coeff
		   (offset ((population loc) 1))  ;   and offset
		   (new-loc (cdr (distances i)))) ; location of the bad one
	      ;(format #t "~D: replace ~A using ~A~%" j (distances i) (distances j))
	      (set! (population new-loc) (vector (+ coeff (- (random range) (/ range 2)))
						 (+ offset (- (random range) (/ range 2)))))))
	  (distances 0)))


      (initial-generation)
      (let ((last-dist size)
	    (best #f))
	(call-with-exit
	 (lambda (done)
	   (do ((i 0 (+ i 1)))
	       ((= i generations))
	     (set! best (next-generation))
	     ;(format #t "~D: ~A ~A ~A~%" i range (car best) (population (cdr best)))
	     (if (< (car best) 1e-9)
		 (done))
	     (let* ((new-dist (car best))
		    (improvement (abs (- new-dist last-dist))))
	       (if (< improvement range)
		   (set! range (max 0.001 (* range 0.5)))
		   (if (> improvement range)
		       (set! range (min 1024.0 (* 2 range)))))
	       (set! last-dist new-dist)))))

	(let ((simpler-coeff (rationalize ((population (cdr best)) 0) .1))
	      (simpler-offset (rationalize ((population (cdr best)) 1) .1)))
	  (let ((dist (distance simpler-coeff simpler-offset)))
	    (if (< dist (* 2 (car best)))
		(format #f "~A * k + ~A, err: ~A~%" simpler-coeff simpler-offset dist)
		(format #f "~A * k + ~A, err: ~A~%" ((population (cdr best)) 0) ((population (cdr best)) 1) (car best)))))))))



#|
(linear-expr-1D (let ((v (make-vector 100)))
	(do ((i 0 (+ i 1))) 
	    ((= i 100)) 
	  (set! (v i) (+ 1 (* i 2))) )
	v))
"2 * k + 1, err: 0.0"

(linear-expr-1D (let ((v (make-vector 100)))
	(do ((i 0 (+ i 1))) 
	    ((= i 100)) 
	  (set! (v i) (+ 1 (- (random .01) .005) (* i 2))))
	v))
"2 * k + 1, err: 0.00079150685524436"
;; so noise is not a problem

|#

(define *show-progress* #f)

(define* (poly-expr data (generations 100) (top-power 10) (xscale 1.0))
  
  (let ((size 100)
	(range 1.0)
	(data-size (length data)))

    (let ((population (make-vector size)))

      (define (new-coeffs good-coeffs bad-coeffs power)
	(let* ((len (length good-coeffs))
	       (coeffs (make-vector len 0.0)))
	  (do ((k 0 (+ k 1)))
	      ((> k power) coeffs)
	    (let ((local-diff (/ (- (good-coeffs k) (bad-coeffs k)) 2.0)))
	      (set! (coeffs k) (+ (good-coeffs k) local-diff (- (random range) (/ range 2))))))))
      
      (define (initial-generation power)
	(let ((len (+ 1 power)))
	  (do ((i 0 (+ i 1)))
	      ((= i size))
	    (set! (population i) (new-coeffs (make-vector len 0.0) (make-vector len 0.0) power)))))

      (define (poly coeffs x power)
	;; 0 => offset, 1 => x, 2 => x^2 etc
	(if (= power 1)
	    (+ (coeffs 0) (* (coeffs 1) x))
	    (let ((sum (coeffs power)))
	      (do ((k (- power 1) (- k 1)))
		  ((< k 0) sum)
		(set! sum (+ (coeffs k) (* x sum)))))))

      (define (distance coeffs power)
	(let ((dist 0.0))
	  (do ((k 0 (+ k 1)))
	      ((= k data-size) dist)
	    (let ((err (- (data k) (poly coeffs (* xscale k) power))))
	      (set! dist (+ dist (* err err)))))))

      (define (iterate-distance coeffs power)
	;; distance if using iterated function, rather than just polynomial
	(call-with-exit
	 (lambda (return)
	   (let* ((dist 0.0)
		  (y 0.0))
	     (do ((k 0 (+ k 1)))
		 ((= k data-size) dist)
	       (let ((err (modulo (- (data k) y) 2.0)))
		 (if (nan? err) ; modulo returns NaN if either arg is inf
		     (return 1.0e50))
		 (if (> err 1.0)
		     (set! err (- 2.0 err)))
		 (set! dist (+ dist (* err err)))
		 (set! y (poly coeffs y power))))))))

      (define (display-expr coeffs power)
	(do ((k power (- k 1)))
	    ((= k 1))
	  (format #t "~,3g k^~D + " (coeffs k) k))
	(format #t "~,3g k + ~,3g" (coeffs 1) (coeffs 0)))
	
      (define (next-generation power)
	(let ((distances (make-vector size 0.0)))
	  (do ((i 0 (+ i 1)))
	      ((= i size))
	    (set! (distances i) (cons (distance (population i) power) i)))
	  (sort! distances (lambda (a b) (< (car a) (car b))))
	  (do ((i (/ size 2) (+ i 1))
	       (j 0 (+ j 1)))
	      ((= i size))
	    (let* ((good-loc (cdr (distances j)))      ; location (in population) of a good one
		   (good-coeffs (population good-loc)) ;   its coeffs
		   (bad-loc (cdr (distances i)))       ; location of the bad one
		   (bad-coeffs (population bad-loc)))  ;    its coeffs
	      (set! (population bad-loc) (new-coeffs good-coeffs bad-coeffs power))))
	  (distances 0)))

      (do ((power 1 (+ power 1)))
	  ((> power top-power))

	(initial-generation power)
	(let ((last-dist size)
	      (best #f))
	  (call-with-exit
	   (lambda (done)
	     (do ((i 0 (+ i 1)))
		 ((= i generations))
	       (let ((new-best (next-generation power)))
		 (if (and *show-progress* 
			  best
			  (< (car new-best) (car best)))
		     (begin
		       (format #t "    ")
		       (display-expr (population (cdr new-best)) power)
		       (format #t ", err: ~,3g~%" (car new-best))))
		 (set! best new-best))
	       (if (< (car best) 1e-9)
		   (done))
	       (let* ((new-dist (car best))
		      (improvement (abs (- new-dist last-dist))))
		 (if (< improvement range)
		     (set! range (max 0.001 (* range 0.5)))
		     (if (> improvement range)
			 (set! range (min 1024.0 (* 2 range)))))
		 (set! last-dist new-dist)))))

	  (let ((simpler-coeffs (apply vector (map (lambda (x) (rationalize x .1)) (population (cdr best))))))
	    (let ((dist (distance simpler-coeffs power)))
	      (if (< dist (* 2 (car best)))
		  (begin
		    (display-expr simpler-coeffs power)
		    (format #t ", err: ~,3g~%~%" dist))
		  (begin
		    (display-expr (population (cdr best)) power)
		    (format #t ", err: ~,3g~%~%" (car best)))))))))))

#|
:(poly-expr (let ((v (make-vector 100)))
	(do ((i 0 (+ i 1))) 
	    ((= i 100)) 
	  (set! (v i) (+ 1.5 (* i 2))))
	v)
	   100 2)
()
2 * k + 3/2, err: 0.0

0 * k^2 + 2 * k + 3/2, err: 0.0


:(poly-expr (let ((v (make-vector 100)))
	(do ((i 0 (+ i 1))) 
	    ((= i 100)) 
	  (set! (v i) (+ 1.5 (* i 2) (* i i))))
	v)
	   100 2)
()
101 * k + -3231/2, err: 55527780.0

1 * k^2 + 2 * k + 3/2, err: 0.0


(poly-expr (let ((v (make-vector 100)))
	(do ((i 0 (+ i 1))) 
	    ((= i 100)) 
	  (set! (v i) (+ 10 (* i 3) (* i i) (* 2 i i i))))
	v)
	   500 3)

71211/4 * k + -775493/2, err: 5073746188744.4

298 * k^2 + -46797/4 * k + 282358/3, err: 142657213706.32

1.9999646468097 * k^3 + 1.0060760943148 * k^2 + 2.7343239562578 * k + 10.395190849075, err: 341.7343511583


(poly-expr (let ((v (make-vector 100)))
	(do ((i 0 (+ i 1))) 
	    ((= i 100)) 
	  (set! (v i) (+ 10 (* i 3) (* i i) (* .02 i i i))))
	v)
	   500 3)

279 * k + -16405/3, err: 889433525.53111

4 * k^2 + -114 * k + 4756/5, err: 16055636.02

0.01993019520734 * k^3 + 1.0122316382516 * k^2 + 2.423061574187 * k + 16.287894409628, err: 451.01103769494

if scaled by .01:

(poly-expr (let ((v (make-vector 100)))
	(do ((i 0 (+ i 1))) 
	    ((= i 100)) 
	  (set! (v i) (+ 1.0 (* i 32 .01) (* i i 3 .01 .01) (* 1 i i i .01 .01 .01))))
	v)
	   500 3 .01)

179/5 k + 1/3, err: 11.3

4.48 k^2 + 31.4 k + 1.05, err: 0.0357

1 k^3 + 3 k^2 + 32 k + 1, err: 4.76e-28

|#
