;;; polynomial-related stuff
;;;
;;; poly+ poly* poly/ poly-gcd poly-reduce poly-roots poly-derivative

(provide 'snd-poly.scm)

;;; using lists and vectors internally for complex intermediates

(define (vector-add! p1 p2)
  (let ((len (min (vector-length p1) (vector-length p2)))) 
    (do ((i 0 (1+ i)))
	((= i len))
      (vector-set! p1 i (+ (vector-ref p1 i) (vector-ref p2 i))))
    p1))

(define (vector-scale! p1 scl)
  (let ((len (vector-length p1)))
    (do ((i 0 (1+ i)))
	((= i len))
      (vector-set! p1 i (* scl (vector-ref p1 i))))
    p1))

(define (vector-copy p1)
  (let* ((len (vector-length p1))
	 (v (make-vector len)))
    (do ((i 0 (1+ i)))
	((= i len))
      (vector-set! v i (vector-ref p1 i)))
    v))

(define (poly-as-vector-eval v x)
  (let ((sum (vector-ref v (1- (vector-length v)))))
    (do ((i (- (vector-length v) 2) (1- i)))
	((< i 0) sum)
      (set! sum (+ (* sum x) (vector-ref v i))))))


(define (poly-as-vector-reduce p1)
  ;; remove trailing (high-degree) zeros
  ;; always return at least a 0 coeff (rather than return #f=0 polynomial)
  (let ((new-len (do ((i (1- (vector-length p1)) (1- i)))
		     ((or (= i 0)
			  (not (= (vector-ref p1 i) 0.0)))
		      (1+ i)))))
    (if (= new-len (vector-length p1))
	p1
	(let ((np (make-vector new-len)))
	  (do ((i 0 (1+ i)))
	      ((= i new-len))
	    (vector-set! np i (vector-ref p1 i)))
	  np))))

(define (poly-reduce p1)
  (if (= (vct-ref p1 (1- (vct-length p1))) 0.0)
      (vector->vct (poly-as-vector-reduce (vct->vector p1)))
      p1))

;;; (poly-reduce (vct 1 2 3)) -> #<vct[len=3]: 1.000 2.000 3.000>
;;; (poly-reduce (vct 1 2 3 0 0 0)) -> #<vct[len=3]: 1.000 2.000 3.000>
;;; (poly-reduce (vct 0 0 0 0 1 0)) -> #<vct[len=5]: 0.000 0.000 0.000 0.000 1.000>

      
(define (poly-as-vector+ p1 p2)
  (if (vector? p1)
      (if (vector? p2)
	  (if (> (vector-length p1) (vector-length p2))
	      (vector-add! (vector-copy p1) p2)
	      (vector-add! (vector-copy p2) p1))
	  (let ((v (vector-copy p1)))
	    (vector-set! v 0 (+ (vector-ref v 0) p2))
	    v))
      (let ((v (vector-copy p2)))
	(vector-set! v 0 (+ (vector-ref v 0) p1))
	v)))

(define (poly+ p1 p2) (vector->vct (poly-as-vector+ (if (vct? p1) (vct->vector p1) p1) (if (vct? p2) (vct->vector p2) p2))))

;;; (poly+ (vct .1 .2 .3) (vct 0.0 1.0 2.0 3.0 4.0)) -> #<vct[len=5]: 0.100 1.200 2.300 3.000 4.000>
;;; (poly+ (vct .1 .2 .3) .5) -> #<vct[len=3]: 0.600 0.200 0.300>
;;; (poly+ .5 (vct .1 .2 .3)) -> #<vct[len=3]: 0.600 0.200 0.300>


(define (poly-as-vector* p1 p2)
  (if (vector? p1)
      (if (vector? p2)
	  (let* ((p1len (vector-length p1))
		 (p2len (vector-length p2))
		 (len (+ p1len p2len))
		 (m (make-vector len 0)))
	    (do ((i 0 (1+ i)))
		((= i p1len))
	      (do ((j 0 (1+ j)))
		  ((= j p2len))
		(vector-set! m (+ i j) (+ (vector-ref m (+ i j)) (* (vector-ref p1 i) (vector-ref p2 j))))))
	    m)
	  (vector-scale! (vector-copy p1) p2))
      (vector-scale! (vector-copy p2) p1)))

(define (poly* p1 p2) (vector->vct (poly-as-vector* (if (vct? p1) (vct->vector p1) p1) (if (vct? p2) (vct->vector p2) p2))))
    
;;; (poly* (vct 1 1) (vct -1 1)) -> #<vct[len=4]: -1.000 0.000 1.000 0.000>
;;; (poly* (vct -5 1) (vct 3 7 2)) -> #<vct[len=5]: -15.000 -32.000 -3.000 2.000 0.000>
;;; (poly* (vct -30 -4 2) (vct 0.5 1)) -> #<vct[len=5]: -15.000 -32.000 -3.000 2.000 0.000>
;;; (poly* (vct -30 -4 2) 0.5) -> #<vct[len=3]: -15.000 -2.000 1.000>
;;; (poly* 2.0 (vct -30 -4 2)) -> #<vct[len=3]: -60.000 -8.000 4.000>


(define (poly-as-vector/ p1 p2)
  (if (vector? p1)
      (if (vector? p2)
	  ;; Numerical Recipes poldiv
	  (let ((p1len (vector-length p1))
		 (p2len (vector-length p2)))
	    (if (> p2len p1len)
		(list (vector 0) p2)
		(let* ((len (max p1len p2len))
		       (r (make-vector len 0))
		       (q (make-vector len 0)))
		  (do ((i 0 (1+ i)))
		      ((= i len))
		    (vector-set! r i (vector-ref p1 i)))
		  (let ((n (1- p1len))
			(nv (1- p2len)))
		    (do ((k (- n nv) (1- k)))
			((< k 0))
		      (vector-set! q k (/ (vector-ref r (+ nv k)) (vector-ref p2 nv)))
		      (do ((j (+ nv k -1) (1- j)))
			  ((< j k))
			(vector-set! r j (- (vector-ref r j) (* (vector-ref q k) (vector-ref p2 (- j k)))))))
		    (do ((j nv (1+ j)))
			((> j n))
		      (vector-set! r j 0))
		    (list q r)))))
	  (list (poly-as-vector* p1 (/ 1 p2)) (vector 0)))
      (list (vector 0) p2)))

(define (poly/ p1 p2)
  (map vector->vct (poly-as-vector/ (if (vct? p1) (vct->vector p1) p1) (if (vct? p2) (vct->vector p2) p2))))

;;; (poly/ (vct -1.0 -0.0 1.0) (vector 1.0 1.0)) -> (#<vct[len=3]: -1.000 1.000 0.000> #<vct[len=3]: 0.000 0.000 0.000>)
;;; (poly/ (vct -15 -32 -3 2) (vector -5 1)) -> (#<vct[len=4]: 3.000 7.000 2.000 0.000> #<vct[len=4]: 0.000 0.000 0.000 0.000>)
;;; (poly/ (vct -15 -32 -3 2) (vector 3 1)) -> (#<vct[len=4]: -5.000 -9.000 2.000 0.000> #<vct[len=4]: 0.000 0.000 0.000 0.000>)
;;; (poly/ (vct -15 -32 -3 2) (vector .5 1)) -> (#<vct[len=4]: -30.000 -4.000 2.000 0.000> #<vct[len=4]: 0.000 0.000 0.000 0.000>)
;;; (poly/ (vct -15 -32 -3 2) (vector 3 7 2)) -> (#<vct[len=4]: -5.000 1.000 0.000 0.000> #<vct[len=4]: 0.000 0.000 0.000 0.000>)
;;; (poly/ (vct -15 -32 -3 2) 2.0) -> (#<vct[len=4]: -7.500 -16.000 -1.500 1.000> #<vct[len=1]: 0.0>)


(define (poly-as-vector-derivative p1)
  (let* ((len (1- (vector-length p1)))
	 (v (make-vector len)))
    (do ((i (1- len) (1- i))
	 (j len (1- j)))
	((< i 0) v)
      (vector-set! v i (* j (vector-ref p1 j))))))

(define (poly-derivative p1) (vector->vct (poly-as-vector-derivative (vct->vector p1))))

;;; (poly-derivative (vct 0.5 1.0 2.0 4.0)) -> #<vct[len=3]: 1.000 4.000 12.000>



(if (not (defined? 'pi))
    (define pi 3.141592653589793))

(define poly-roots-epsilon 1.0e-7)

(define (simplify-complex a)
  (if (< (abs (imag-part a)) poly-roots-epsilon)
      (if (< (abs (real-part a)) poly-roots-epsilon)
	  0.0
	  (real-part a))
      (if (< (abs (real-part a)) poly-roots-epsilon)
	  (make-rectangular 0.0 (imag-part a))
	  a)))


(define (poly-gcd p1 p2)
  (if (< (vct-length p1) (vct-length p2))
      (vct 0.0)
      (let ((qr (map poly-reduce (poly/ p1 p2))))
	;(display (format #f ";poly-gcd ~A ~A -> ~A~%" p1 p2 qr))
	(if (= (vct-length (cadr qr)) 1)
	    (if (= (vct-ref (cadr qr) 0) 0.0)
		p2
		(vct 0.0))
	    (apply poly-gcd qr)))))

(define (poly-as-vector-gcd p1 p2)
  (if (< (vector-length p1) (vector-length p2))
      (vector 0)
      (let ((qr (map poly-as-vector-reduce (poly-as-vector/ p1 p2))))
	;(display (format #f ";poly-as-vector-gcd ~A ~A ->~A ~%" p1 p2 qr))
	(if (= (vector-length (cadr qr)) 1)
	    (if (= (vector-ref (cadr qr) 0) 0.0)
		p2
		(vector 0))
	    (apply poly-as-vector-gcd qr)))))

;;; (poly-gcd (poly-reduce (poly* (vct 2 1) (vct -3 1))) (vct 2 1)) -> #<vct[len=2]: 2.000 1.000>
;;; (poly-gcd (poly-reduce (poly* (vct 2 1) (vct -3 1))) (vct 3 1)) -> #<vct[len=1]: 6.000>
;;; (poly-gcd (poly-reduce (poly* (vct 2 1) (vct -3 1))) (vct -3 1)) -> #<vct[len=2]: -3.000 1.000>
;;; (poly-gcd (poly-reduce (poly* (vct 8 1) (poly* (vct 2 1) (vct -3 1)))) (vct -3 1)) -> #<vct[len=2]: -3.000 1.000>
;;; (poly-gcd (poly-reduce (poly* (vct 8 1) (poly* (vct 2 1) (vct -3 1)))) (poly-reduce (poly* (vct 8 1) (vct -3 1)))) -> #<vct[len=3]: -24.000 5.000 1.000>
;;; (poly-gcd (vct -1 0 1) (vct 2 -2 -1 1)) -> #<vct[len=1]: 0.000>
;;; (poly-gcd (vct 2 -2 -1 1) (vct -1 0 1)) -> #<vct[len=2]: 1.000 -1.000>
;;; (poly-gcd (vct 2 -2 -1 1) (vct -2.5 1)) -> #<vct[len=1]: 0.000>


(define (poly-as-vector-roots p1)
  
  (define (linear-root a b) ; ax + b
    (list (/ (- b) a)))

  (define (quadratic-roots a b c) ; ax^2 + bx + c
    (let ((d (sqrt (- (* b b) (* 4 a c)))))
      (list (/ (+ (- b) d) (* 2 a))
	    (/ (- (- b) d) (* 2 a)))))

  (define (cubic-roots a b c d) ; ax^3 + bx^2 + cx + d

    ;; Abramowitz & Stegun 3.8.2
    (let* ((a0 (/ d a))
	   (a1 (/ c a))
	   (a2 (/ b a))
	   (q (- (/ a1 3.0) (/ (* a2 a2) 9.0)))
	   (r (- (/ (- (* a1 a2) (* 3.0 a0)) 6.0) (/ (* a2 a2 a2) 27.0)))
	   (q3r2 (+ (* q q q) (* r r)))
	   (sq3r2 (sqrt q3r2))
	   (r1 (expt (+ r sq3r2) (/ 1 3)))
	   (r2 (expt (- r sq3r2) (/ 1 3)))
	   (incr (/ (* 2 pi 0+i) 3))
	   (vals (call-with-current-continuation
		  (lambda (return)
		    (do ((i 0 (1+ i)))   ; brute force! this can almost certainly be optimized
			((= i 3))
		      (do ((j 0 (1+ j)))
			  ((= j 3))
			(let* ((s1 (* r1 (exp (* i incr))))
			       (s2 (* r2 (exp (* j incr))))
			       (z1 (simplify-complex (- (+ s1 s2) (/ a2 3.0)))))
			  (if (< (magnitude (poly-as-vector-eval (vector a0 a1 a2 1.0) z1)) poly-roots-epsilon)
			      (let ((z2 (simplify-complex (+ (* -0.5 (+ s1 s2))
							     (/ a2 -3.0) 
							     (* (- s1 s2) 0.5 (sqrt -3.0))))))
				(if (< (magnitude (poly-as-vector-eval (vector a0 a1 a2 1.0) z2)) poly-roots-epsilon)
				    (let ((z3 (simplify-complex (+ (* -0.5 (+ s1 s2)) 
								   (/ a2 -3.0) 
								   (* (- s1 s2) -0.5 (sqrt -3.0))))))
				      (if (< (magnitude (poly-as-vector-eval (vector a0 a1 a2 1.0) z3)) poly-roots-epsilon)
					  (return (list z1 z2 z3))))))))))
		    'error))))
      vals))

  ;; (poly-roots (vct 1 -1 -1 1)) ; q=0.0 -> (1.0 -1.0 1.0)
  ;; (poly-roots (vct 2 -1 -2 1)) ; q<0.0 -> (2.0 -1.0 1.0)
  ;; (poly-roots (vct -1 1 1 1))  ; q>0.0 -> (0.543689012692076 -0.771844506346038+1.11514250803994i -0.771844506346038-1.11514250803994i)
  ;; (poly-roots (vct -1 3 -3 1)) ; q=0.0 -> (1.0 1.0 1.0)

  (define (quartic-roots a b c d e) ; ax^4 + bx^3 + cx^2 + dx + e
    ;; Weisstein, "Encyclopedia of Mathematics"
    (let* ((a0 (/ e a))
	   (a1 (/ d a))
	   (a2 (/ c a))
	   (a3 (/ b a))
	   (yroot (poly-as-vector-roots (vector (+ (* 4 a2 a0) (- (* a1 a1)) (- (* a3 a3 a0)))
						(- (* a1 a3) (* 4 a0))
						(- a2)
						1.0)))
	   (y1 (if (real? (car yroot)) (car yroot)
		   (if (real? (cadr yroot)) (cadr yroot)
		       (caddr yroot))))
	   (R (sqrt (+ (* 0.25 a3 a3) (- a2) y1)))
	   (D (if (= R 0)
		  (sqrt (+ (* 0.75 a3 a3) (* -2 a2) (* 2 (sqrt (- (* y1 y1) (* 4 a0))))))
		  (sqrt (+ (* 0.75 a3 a3) (* -2 a2) (- (* R R))
			   (/ (* 0.25 (+ (* 4 a3 a2) (* -8 a1) (- (* a3 a3 a3)))) R)))))
	   (E (if (= R 0)
		  (sqrt (+ (* 0.75 a3 a3) (* -2 a2) (* -2 (sqrt (- (* y1 y1) (* 4 a0))))))
		  (sqrt (+ (* 0.75 a3 a3) (* -2 a2) (- (* R R)) 
			   (/ (* -0.25 (+ (* 4 a3 a2) (* -8 a1) (- (* a3 a3 a3)))) R)))))
	   (z1 (+ (* -0.25 a3) (* 0.5 R) (* 0.5 D)))
	   (z2 (+ (* -0.25 a3) (* 0.5 R) (* -0.5 D)))
	   (z3 (+ (* -0.25 a3) (* -0.5 R) (* 0.5 E)))
	   (z4 (+ (* -0.25 a3) (* -0.5 R) (* -0.5 E))))
      (list z1 z2 z3 z4)))

  ;; (poly-roots (vct 1 -4 6 -4 1)) -> (1.0 1.0 1.0 1.0)

  (define (nth-roots a b deg) ; ax^n + b
    (let* ((n (expt (/ (- b) a) (/ 1.0 deg)))
	   (incr (/ (* 2 pi 0+i) deg))
	   (roots '()))
      (do ((i 0 (1+ i)))
	  ((= i deg))
	(set! roots (cons (simplify-complex (* n (exp (* i incr)))) roots)))
      roots))

  ;; (poly-roots (vct 0.5 0 0 1.0)) -> (0.396850262992049-0.687364818499302i -0.7937005259841 0.39685026299205+0.687364818499301i)

  (let ((deg (1- (vector-length p1))))

    (if (= deg 0)                                     ; just constant
	'()

	(if (= (vector-ref p1 0) 0.0)                 ; constant=0.0, divide through by x, recurse on new
	    (if (= deg 1)
		(list 0.0)
		(let ((pnew (make-vector deg)))
		  (do ((i 1 (1+ i)))
		      ((> i deg))
		    (vector-set! pnew (1- i) (vector-ref p1 i)))
		  (append (list 0.0) (poly-as-vector-roots pnew))))

	    (if (= deg 1)                             ; ax + b -> -b/a
		(linear-root (vector-ref p1 1) (vector-ref p1 0))

		(if (= deg 2)                         ; ax^2 + bx + c -> -b +/- sqrt(b^2 - 4ac) / 2a
		    (quadratic-roots (vector-ref p1 2) (vector-ref p1 1) (vector-ref p1 0))

		    (let ((ones 0))
		      (do ((i 1 (1+ i)))
			  ((> i deg))
			(if (not (= (vector-ref p1 i) 0.0))
			    (set! ones (1+ ones))))
		      (if (= ones 1)                  ; x^n + b -- "linear" in x^n
			  (nth-roots (vector-ref p1 deg) (vector-ref p1 0) deg)

			  (if (and (= ones 2)
				   (even? deg)
				   (not (= (vector-ref p1 (/ deg 2)) 0.0)))
			      (let ((roots '())       ; quadratic in x^(n/2)
				    (n (/ deg 2)))
				(for-each
				 (lambda (qr)
				   (set! roots (append roots (nth-roots 1.0 (- qr) n))))
				 (poly-as-vector-roots (vector (vector-ref p1 0) (vector-ref p1 (/ deg 2)) (vector-ref p1 deg))))
				roots)

			  (if (= deg 3)
			      ;; it may be better to fall into Newton's method here
			      (cubic-roots (vector-ref p1 3) (vector-ref p1 2) (vector-ref p1 1) (vector-ref p1 0))

			      (if (= deg 4)
				  (quartic-roots (vector-ref p1 4) (vector-ref p1 3) (vector-ref p1 2) (vector-ref p1 1) (vector-ref p1 0))

				  ;; degree>4, use Newton's method
				  (begin
				    ;; perhaps get derivative roots, plug in main -- need to get nth derivative to be safe in this
				    ;; from Cohen, "Computational Algebraic Number Theory"
				    (let* ((roots '())
					   (q (vector-copy p1))
					   (pp (poly-as-vector-derivative p1))
					   (qp (vector-copy pp))
					   (n deg)
					   (f #t) ; has real coeffs for now
					   (x 1.3+0.314159i)
					   (v (poly-as-vector-eval q x))
					   (m (* (magnitude v) (magnitude v)))
					   (c 0)
					   (dx 0.0)
					   (happy #f))
				      (do ()
					  ((or happy (c-g?)))
					(set! dx (/ v (poly-as-vector-eval qp x)))
					;(display (format #f "dx: ~A~%" (poly-as-vector-eval q x)))
					(if (<= (magnitude dx) poly-roots-epsilon)
					    (set! happy #t)
					    (do ((c 0 (1+ c))
						 (step3 #f))
						((or (>= c 20)
						     (c-g?)
						     step3
						     (<= (magnitude dx) poly-roots-epsilon)))
					      (let* ((y (- x dx))
						     (v1 (poly-as-vector-eval q y))
						     (m1 (* (magnitude v1) (magnitude v1))))
						(if (< m1 m)
						    (begin
						      (set! x y)
						      (set! v v1)
						      (set! m m1)
						      (set! step3 #t))
						    (set! dx (/ dx 4.0)))))))
				      (set! x (- x (/ (poly-as-vector-eval p1 x) (poly-as-vector-eval pp x))))
				      (set! x (- x (/ (poly-as-vector-eval p1 x) (poly-as-vector-eval pp x))))
				      ;(display (format #f "root: ~A -> ~A~%" x (magnitude (poly-as-vector-eval q x))))
				      (if (< (imag-part x) poly-roots-epsilon)
					  (begin
					    (set! x (real-part x))
					    (set! q (poly-as-vector/ q (vector (- x) 1.0)))
					    (set! n (1- n)))
					  (begin
					    (set! q (poly-as-vector/ q (vector (magnitude x) 0.0 1.0)))
					    (set! n (- n 2))))
				      (set! roots (cons x roots))
				      (if (> n 0) 
					  (set! roots (append (poly-as-vector-roots (poly-as-vector-reduce (car q))) roots)))
				      roots)))))))))))))

(define (poly-roots p1) 
  (let* ((v1 (vct->vector (poly-reduce p1)))
	 (roots (poly-as-vector-roots v1)))
    (for-each
     (lambda (q)
       (let ((dx (magnitude (poly-as-vector-eval v1 q))))
	 (if (> dx poly-roots-epsilon) (snd-display ";~A at ~A: ~A" v1 q dx))))
     roots)
    roots))

#!
(do ((i 0 (1+ i))) ((= i 10)) 
  (poly-as-vector-roots (vector (make-rectangular (mus-random 1.0) (mus-random 1.0)) 
				(make-rectangular (mus-random 1.0) (mus-random 1.0)))))
(do ((i 0 (1+ i))) ((= i 10)) 
  (poly-as-vector-roots (vector (make-rectangular (mus-random 1.0) (mus-random 1.0)) 
				(make-rectangular (mus-random 1.0) (mus-random 1.0))
				(make-rectangular (mus-random 1.0) (mus-random 1.0)))))

(do ((i 0 (1+ i))) ((= i 10)) 
  (poly-roots (vct (mus-random 1.0) (mus-random 1.0) (mus-random 1.0) (mus-random 1.0))))

(do ((i 0 (1+ i))) ((= i 10)) 
  (poly-as-vector-roots (vector (make-rectangular (mus-random 1.0) (mus-random 1.0)) 
				(make-rectangular (mus-random 1.0) (mus-random 1.0))
				(make-rectangular (mus-random 1.0) (mus-random 1.0))
				(make-rectangular (mus-random 1.0) (mus-random 1.0)))))

(do ((i 0 (1+ i))) ((= i 10)) 
  (poly-roots (vct (mus-random 1.0) (mus-random 1.0) (mus-random 1.0) (mus-random 1.0) (mus-random 1.0))))

(do ((i 0 (1+ i))) ((= i 10)) 
  (poly-as-vector-roots (vector (make-rectangular (mus-random 1.0) (mus-random 1.0)) 
				(make-rectangular (mus-random 1.0) (mus-random 1.0))
				(make-rectangular (mus-random 1.0) (mus-random 1.0))
				(make-rectangular (mus-random 1.0) (mus-random 1.0))
				(make-rectangular (mus-random 1.0) (mus-random 1.0)))))

(do ((i 3 (1+ i))) ((= i 20)) 
  (let ((v (make-vct i 0.0)))
    (vct-set! v 0 (mus-random 1.0))
    (vct-set! v (1- i) 1.0)
    (poly-roots v)))

(do ((i 3 (+ i 2))) ((= i 21)) 
  (let ((v (make-vct i 0.0)))
    (vct-set! v 0 (mus-random 1.0))
    (vct-set! v (1- i) 1.0)
    (vct-set! v (/ (1- i) 2) 1.0)
    (poly-roots v)))


;;; these can be off by a lot!
(do ((i 0 (1+ i))) ((= i 10)) 
  (poly-roots (vct (mus-random 1.0) (mus-random 1.0) (mus-random 1.0) (mus-random 1.0) (mus-random 1.0) (mus-random 1.0))))


;;; TODO: put all these tests in snd-test.scm
!#
