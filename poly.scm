;;; polynomial-related stuff

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
		 (m (make-vector len 0.0)))
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
		(list (vector 0.0) p2)
		(let* ((len (max p1len p2len))
		       (r (make-vector len 0.0))
		       (q (make-vector len 0.0)))
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
		      (vector-set! r j 0.0))
		    (list q r)))))
	  (list (poly-as-vector* p1 (/ 1.0 p2)) (vector 0.0)))
      (list (vector 0.0) p2)))

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

(define (simplify-complex a)
  (if (< (abs (imag-part a)) 1.0e-7)
      (if (< (abs (real-part a)) 1.0e-7)
	  0.0
	  (real-part a))
      (if (< (abs (real-part a)) 1.0e-7)
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
      (vector 0.0)
      (let ((qr (map poly-as-vector-reduce (poly-as-vector/ p1 p2))))
	;(display (format #f ";poly-as-vector-gcd ~A ~A ->~A ~%" p1 p2 qr))
	(if (= (vector-length (cadr qr)) 1)
	    (if (= (vector-ref (cadr qr) 0) 0.0)
		p2
		(vector 0.0))
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
  ;(display (format #f "poly-roots: ~A~%" p1))
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
		(list (/ (- (vector-ref p1 0)) (vector-ref p1 1)))
		(if (= deg 2)                         ; ax^2 + bx + c -> -b +/- sqrt(b^2 - 4ac) / 2a
		    (let* ((a (vector-ref p1 2))
			   (b (vector-ref p1 1))
			   (c (vector-ref p1 0))
			   (d (sqrt (- (* b b) (* 4 a c)))))
		      (list (/ (+ (- b) d) (* 2 a))
			    (/ (- (- b) d) (* 2 a))))
		    (let ((ones 0))
		      (do ((i 1 (1+ i)))
			  ((> i deg))
			(if (not (= (vector-ref p1 i) 0.0))
			    (set! ones (1+ ones))))
		      (if (= ones 1)             ; x^n - b -- "linear" in x^n -- do the roots-of-unity shuffle
			  (let* ((n (expt (/ (- (vector-ref p1 0)) (vector-ref p1 deg)) (/ 1.0 deg)))
				 (mag (magnitude n))
				 (roots (list n))
				 (incr (/ (* pi 2.0) deg)))
			    (do ((i 1 (1+ i))
				 (ang incr (+ ang incr)))
				((= i deg))
			      (set! roots (cons (simplify-complex (make-polar mag ang)) roots)))
			    roots)
			  (if (and (= ones 2)
				   (even? deg)
				   (not (= (vector-ref p1 (/ deg 2)) 0.0)))
			                         ; quadratic in x^(n/2)
			      (let ((quad-roots (poly-as-vector-roots (vector (vector-ref p1 0)
									      (vector-ref p1 (/ deg 2))
									      (vector-ref p1 deg))))
				    (roots '())
				    (n (/ deg 2)))
				(for-each
				 (lambda (qr)
				   (if (= n 2)
				       (let ((sq (sqrt qr)))
					 (set! roots (append (list sq (- sq)) roots)))
				       (let* ((xqr (expt qr (/ 1.0 n)))
					      (mag (magnitude xqr))
					      (incr (/ (* pi 2.0) n)))
					 (do ((i 0 (1+ i))
					      (ang 0.0 (+ ang incr)))
					     ((= i n))
					   (set! roots (cons (simplify-complex (make-polar mag ang)) roots))))))
				 quad-roots)
				roots)
			  (if (= deg 3)
			      ;; it may be better to just fall into Newton's method here
			      ;; Abramowitz & Stegun 3.8.2
			      (let* ((a0 (/ (vector-ref p1 0) (vector-ref p1 3)))
				     (a1 (/ (vector-ref p1 1) (vector-ref p1 3)))
				     (a2 (/ (vector-ref p1 2) (vector-ref p1 3)))
				     (q (- (/ a1 3.0) (/ (* a2 a2) 9.0)))
				     (r (- (/ (- (* a1 a2) (* 3.0 a0)) 6.0) (/ (* a2 a2 a2) 27.0)))
				     (q3r2 (+ (* q q q) (* r r)))
				     (sq3r2 (sqrt q3r2))
				     (s1 (if (= q3r2 0.0) (- (expt (abs r) (/ 1.0 3.0))) (expt (+ r sq3r2) (/ 1.0 3.0))))
				     ;; the problem here is that we're not being specific about which cube root we want
				     ;; if only real roots, we need to specify the real root if r is negative
				     (s2 (if (= q3r2 0.0) s1 (expt (- r sq3r2) (/ 1.0 3.0))))
				     (z1 (- (+ s1 s2) (/ a2 3.0)))
				     (z2 (+ (* -0.5 (+ s1 s2))
					    (/ a2 -3.0) 
					    (* (- s1 s2) 0.5 (sqrt 3.0) (make-rectangular 0.0 1.0))))
				     (z3 (+ (* -0.5 (+ s1 s2)) 
					    (/ a2 -3.0) 
					    (* (- s1 s2) -0.5 (sqrt 3.0) (make-rectangular 0.0 1.0)))))
				      (list z1 z2 z3))
			      (if (= deg 4)
				  ;; Weisstein, "Encyclopedia of Mathematics"
				  (let* ((a0 (/ (vector-ref p1 0) (vector-ref p1 4)))
					 (a1 (/ (vector-ref p1 1) (vector-ref p1 4)))
					 (a2 (/ (vector-ref p1 2) (vector-ref p1 4)))
					 (a3 (/ (vector-ref p1 3) (vector-ref p1 4)))
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
				    (list z1 z2 z3 z4))

				  ;; degree>4
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
					   (accuracy 1.0e-7)
					   (happy #f))
				      (do ()
					  ((or happy (c-g?)))
					(set! dx (/ v (poly-as-vector-eval qp x)))
					;(display (format #f "dx: ~A~%" dx))
					(if (<= (magnitude dx) accuracy)
					    (set! happy #t)
					    (do ((c 0 (1+ c))
						 (step3 #f))
						((or (>= c 20)
						     (c-g?)
						     step3
						     (<= (magnitude dx) accuracy)))
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
				      (if (< (imag-part x) accuracy)
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

(define (poly-roots p1) (poly-as-vector-roots (vct->vector p1)))



(define (poly-roots-tests)
  (letrec ((ceql (lambda (a b)
		   (if (null? a)
		       (null? b)
		       (if (null? b)
			   #f
			   (if (or (fneq (real-part (car a)) (real-part (car b)))
				   (fneq (imag-part (car a)) (imag-part (car b))))
			       #f
			       (ceql (cdr a) (cdr b))))))))

    ;; degree=0
    (let ((val (poly-roots (vct 0.0))))
      (if (not (null? val)) (snd-display ";poly-roots 0.0: ~A" val)))
    (let ((val (poly-roots (vct 12.3))))
      (if (not (null? val)) (snd-display ";poly-roots 12.3: ~A" val)))
    
    ;; degree 0 + x=0
    (let ((val (poly-roots (vct 0.0 1.0))))
      (if (not (feql val (list 0.0))) (snd-display ";poly-roots 0.0 1.0: ~A" val)))
    (let ((val (poly-roots (vct 0.0 0.0 0.0 121.0))))
      (if (not (feql val (list 0.0 0.0 0.0))) (snd-display ";poly-roots 0.0 0.0 0.0 121.0: ~A" val)))
    
    ;; degree=1
    (let ((val (poly-roots (vct -1.0 1.0))))
      (if (not (feql val (list 1.0))) (snd-display ";poly-roots -1.0 1.0: ~A" val)))
    (let ((val (poly-roots (vct -2.0 4.0))))
      (if (not (feql val (list 0.5))) (snd-display ";poly-roots -2.0 4.0: ~A" val)))
    (let ((val (poly-as-vector-roots (vector 0.0-i 1))))
      (if (not (ceql val (list -0.0+1.0i))) (snd-display ";poly-roots: -i 1: ~A" val)))
    
    ;; linear x^n
    (let ((val (poly-roots (vct -1.0 0.0 0.0 0.0 1.0))))
      (if (not (feql val (list 0.0-1.0i -1.0 0.0+1.0i 1.0))) (snd-display ";poly-roots -1.0 0.0 0.0 0.0 1.0: ~A" val)))
    (let ((val (poly-roots (vct -16.0 0.0 0.0 0.0 1.0))))
      (if (not (feql val (list 0.0-2.0i -2.0 0.0+2.0i 2.0))) (snd-display ";poly-roots -16.0 0.0 0.0 0.0 1.0: ~A" val)))
    (let ((val (poly-roots (vct 1.0 0.0 0.0 0.0 1.0))))
      (if (not (ceql val (list 0.0-1.0i -1.0 0.0+1.0i 0.70710+0.70710i))) (snd-display ";poly-roots 1 0 0 0 1: ~A" val)))
    (let ((val (poly-roots (vct -32.0 0 0 0 0 0 0.5))))
      (if (not (ceql val (list 1.0-1.7320i -1.0-1.7320i -2.0 -1.0+1.7320i 1.0+1.7320i 2.0))) (snd-display ";poly-roots 32 0 0 0 0 0 0.5: ~A" val)))
    
    ;; linear + x=0
    (let ((val (poly-roots (vct 0.0 -2.0 4.0))))
      (if (not (feql val (list 0.0 0.5))) (snd-display ";poly-roots 0.0 -2.0 4.0: ~A" val)))
    
    ;; degree=2
    (let ((val (poly-roots (vct -1.0 0.0 1.0))))
      (if (not (feql val (list 1.0 -1.0))) (snd-display ";poly-roots -1.0 0.0 1.0: ~A" val)))
    (let ((val (poly-roots (vct 15.0 -8.0 1.0))))
      (if (not (feql val (list 5.0 3.0))) (snd-display ";poly-roots 15.0 -8.0 1.0: ~A" val)))
    (let ((val (poly-roots (vct 1 -2 1))))
      (if (not (feql val (list 1.0 1.0))) (snd-display ";poly-roots 1 -2 1: ~A" val)))
    (let ((val (poly-as-vector-roots (vector -1 0.0+2i 1))))
      (if (not (ceql val (list 0.0-1.0i 0.0-1.0i))) (snd-display ";poly-roots -1 2i 1: ~A" val)))
    (let ((val (poly-roots (vct 1 1 5))))
      (if (not (ceql val (list -0.1+0.43589i -0.1-0.43589i))) (snd-display ";poly-roots 1 1 5: ~A" val)))
    
    ;; 2 + x=0
    (let ((val (poly-roots (vct 0.0 0.0 -1.0 0.0 1.0))))
      (if (not (feql val (list 0.0 0.0 1.0 -1.0))) (snd-display ";poly-roots 0.0 0.0 -1.0 0.0 1.0: ~A" val)))
    
    ;; quadratic in x^(n/2)
    (let ((vals (poly-roots (vct 1.0 0.0 -2.0 0.0 1.0))))
      (if (not (feql vals (list 1.0 -1.0 1.0 -1.0))) (snd-display ";poly-roots 1 0 -2 0 1: ~A" vals)))
    (let ((vals (poly-roots (vct 64.0 0.0 0.0 -16.0 0.0 0.0 1.0))))
      (if (not (ceql vals (list -1.0-1.73205i -1.0+1.73205i 2.0 -1.0-1.73205i -1.0+1.73205i 2.0)))
	  (snd-display ";poly-roots 64 0 0 -16 0 0 1: ~A" vals)))
    (let ((vals (poly-roots (vct 8.0 0.0 0.0 -9.0 0.0 0.0 1.0))))
      (if (not (ceql vals (list -0.5-0.86602i -0.5+0.86602i 1.0 -1.0-1.73205i -1.0+1.73205i 2.0)))
	  (snd-display ";poly-roots 8 9 9 -9 0 0 1: ~A" vals)))
    
    ;; degree=3
    (let ((val (poly-roots (vct -15.0 23.0 -9.0 1.0))))
      (if (not (feql val (list 5.0 1.0 3.0))) (snd-display ";poly-roots 5 1 3: ~A" val)))
    (let ((val (poly-roots (vct -126 -15 0 1))))
      (if (not (ceql val (list 6.0 -3.0+3.46410i -3.0-3.46410i))) (snd-display ";poly-roots -126 -15 0 1: ~A" val)))
    (let ((val (poly-roots (vct -1 3 -3 1))))
      (if (not (feql val (list 1.0 1.0 1.0))) (snd-display ";poly-roots -1 3 -3 1: ~A" val))) 
    (let ((val (poly-roots (vct 1 -1 -1 1))))
      (if (not (feql val (list -1.0 1.0 1.0))) (snd-display ";poly-roots 1 -1 -1 1: ~A" val)))
    (let ((val (poly-roots (vct 2 -2 -2 2))))
      (if (not (feql val (list -1.0 1.0 1.0))) (snd-display ";poly-roots 2 -2 -2 2: ~A" val)))
    
    ;; degree=4
    (let ((vals (poly-roots (vct -15 8 14 -8 1))))
      (if (not (feql vals (list 5.0 3.0 1.0 -1.0))) (snd-display ";poly-roots -15 8 14 -8 1: ~A" vals)))
    (let ((vals (poly-roots (poly-reduce (poly* (poly* (vct 2 1) (vct -3 1)) (poly* (vct 8 1) (vct -9 1)))))))
      (if (not (feql vals (list 9.0 3.0 -2.0 -8.0))) (snd-display ";poly-roots 4(1): ~A" vals)))
    (let ((vals (poly-roots (poly-reduce (poly* (poly* (vct .2 1) (vct -3 1)) (poly* (vct .8 1) (vct -9 1)))))))
      (if (not (feql vals (list 9.0 3.0 -0.2 -0.8))) (snd-display ";poly-roots 4(2): ~A" vals)))
    (let ((vals (poly-roots (poly-reduce (poly* (poly* (vct .02 1) (vct -32 1)) (poly* (vct .8 1) (vct -9 1)))))))
      (if (not (feql vals (list 32.0 9.0 -0.02 -0.8))) (snd-display ";poly-roots 4(3): ~A" vals)))
    
    ;; degree>4
    (let ((vals (poly-roots (poly-reduce (poly* (vct 1 1) (poly* (poly* (vct 2 1) (vct -3 1)) (poly* (vct -1 1) (vct -2 1))))))))
      (if (not (feql vals (list 3.0 2.0 -1.0 -2.0 1.0))) (snd-display ";poly-roots n(1): ~A" vals)))
    (let ((vals (poly-roots (poly-reduce (poly* (vct 1 1) (poly* (poly* (vct 2 1) (vct -3 1)) (poly* (vct 8 1) (vct -9 1))))))))
      (if (not (feql vals (list 9.0 3.0 -2.0 -8.0 -1.0))) (snd-display ";poly-roots n(2): ~A" vals)))
    (let ((vals (poly-roots (poly-reduce (poly* (vct -1 0 1) (poly* (poly* (vct 9 1) (vct -3 1)) (poly* (vct -10 1) (vct -2 1))))))))
      (if (not (feql vals (list 10.0 3.0 -1.0 -9.0 2.0 1.0))) (snd-display ";poly-roots n(3): ~A" vals)))
    (let ((vals (poly-roots (poly-reduce (poly* (vct -1 0 1) (poly* (poly* (vct -4 0 1) (vct -3 1)) (poly* (vct -10 1) (vct -9 0 1))))))))
      (if (not (feql vals (list 10.0 3.0 -2.0 -3.0 -1.0 3.0 2.0 1.0))) (snd-display ";poly-roots n(4): ~A" vals)))
    (let ((vals (poly-roots (poly-reduce (poly* (vct -1 0 1) (poly* (poly* (vct -4 0 1) (vct -16 0 1)) (poly* (vct -25 0 1) (vct -9 0 1))))))))
      (if (not (feql vals (list 5.0 -3.0 -4.0 -5.0 4.0 -2.0 3.0 -1.0 2.0 1.0))) (snd-display ";poly-roots n(5): ~A" vals)))
    (let ((vals (poly-roots (poly-reduce (poly* (vct 1 1) (poly* (poly* (vct 2 1) (vct -3 1)) (poly* (vct 1 1) (vct -2 1))))))))
      (if (not (feql vals (list 3.0 -1.0 -1.0 -2.0 2.0))) (snd-display ";poly-roots n(6): ~A" vals)))
    
    ))
  
