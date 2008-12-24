(use-modules (ice-9 optargs) (ice-9 format))

(provide 'snd-numerics.scm)

;;; random stuff I needed at one time or another while goofing around...
;;;   there are a lot more in snd-test.scm

(define factorial
  (let* ((num-factorials 128)
	 (factorials (let ((temp (make-vector num-factorials 0)))
		       (vector-set! temp 0 1) ; is this correct?
		       (vector-set! temp 1 1)
		       temp)))
    (lambda (n)
      (if (> n num-factorials)
	  (let ((old-num num-factorials)
		(old-facts factorials))
	    (set! num-factorials n)
	    (set! factorials (make-vector num-factorials 0))
	    (do ((i 0 (+ 1 i)))
		((= i old-num))
	      (vector-set! factorials i (vector-ref old-facts i)))))
      (if (zero? (vector-ref factorials n))
	  (vector-set! factorials n (* n (factorial (- n 1)))))
      (vector-ref factorials n))))

(define (binomial-direct n m) ; "n-choose-m" might be a better name (there are much better ways to compute this -- see below)
  (/ (factorial n)
     (* (factorial m) (factorial (- n m)))))

(define (n-choose-k n k)
  "(n-choose-k n k) computes the binomial coefficient C(N,K)"
  (let ((mn (min k (- n k))))
    (if (< mn 0)
	0
	(if (= mn 0)
	    1
	    (let* ((mx (max k (- n k)))
		   (cnk (+ 1 mx)))
	      (do ((i 2 (+ 1 i)))
		  ((> i mn) cnk)
		(set! cnk (/ (* cnk (+ mx i)) i))))))))

(define binomial n-choose-k)



;;; from Numerical Recipes
(define (plgndr l m x)			;Legendre polynomial P m/l (x), m and l integer
					;0 <= m <= l and -1<= x <= 1 (x real)
  (if (or (< m 0) 
	  (> m l) 
	  (> (abs x) 1.0))
      (snd-error "invalid arguments to plgndr")
      (let ((pmm 1.0)
	    (fact 0.0) 
	    (somx2 0.0))
	(if (> m 0)
	    (begin
	      (set! somx2 (sqrt (* (- 1.0 x) (+ 1.0 x))))
	      (set! fact 1.0)
	      (do ((i 1 (+ 1 i)))
		  ((> i m))
		(set! pmm (* (- pmm) fact somx2))
		(set! fact (+ fact 2.0)))))
	(if (= l m) 
	    pmm
	    (let ((pmmp1 (* x pmm (+ (* 2 m) 1))))
	      (if (= l (+ m 1)) 
		  pmmp1
		  (let ((pk 0.0)) ; NR used "ll" which is unreadable
		    (do ((k (+ m 2) (+ 1 k)))
			((> k l))
		      (set! pk (/ (- (* x (- (* 2 k) 1) pmmp1) 
				      (* (+ k m -1) pmm)) 
				   (- k m)))
		      (set! pmm pmmp1)
		      (set! pmmp1 pk))
		    pk)))))))


;;; A&S (bessel.lisp)
(define (legendre-polynomial a x) ; sum of weighted polynomials (m=0)
  (let ((n (- (vector-length a) 1)))
    (if (= n 0) 
	(vector-ref a 0)
	(let* ((r x)
	       (s 1.0)
	       (h 0.0)
	       (sum (vector-ref a 0)))
	  (do ((k 1 (+ 1 k)))
	      ((= k n))
	    (set! h r)
	    (set! sum (+ sum (* r (vector-ref a k))))
	    (set! r (/ (- (* r x (+ (* 2 k) 1))  (* s k)) (+ k 1)))
	    (set! s h))
	  (+ sum (* r (vector-ref a n)))))))

(define (legendre n x)
  (legendre-polynomial (let ((v (make-vector (+ 1 n) 0.0)))
			 (vector-set! v n 1.0)
			 v)
		       x))

;;; (with-sound (:scaled-to 0.5) (do ((i 0 (+ 1 i)) (x 0.0 (+ x .1))) ((= i 10000)) (outa i (legendre 20 (cos x)))))

#|
;; if l odd, there seems to be sign confusion:
(with-sound (:channels 2 :scaled-to 1.0)
  (do ((i 0 (+ 1 i))
       (theta 0.0 (+ theta 0.01)))
      ((= i 10000))
    (outa i (plgndr 1 1 (cos theta)))
    (let ((x (sin theta)))
      (outb i (- x)))))

;; this works:
(with-sound (:channels 2 :scaled-to 1.0)
  (do ((i 0 (+ 1 i))
       (theta 0.0 (+ theta 0.01)))
      ((= i 10000))
    (let ((x (cos theta)))
      (outa i (plgndr 3 0 x))
      (outb i (* 0.5 x (- (* 5 x x) 3))))))
|#


(define* (gegenbauer n x :optional (alpha 0.0))
  (if (< alpha -0.5) (set! alpha -0.5))
  (if (= n 0)
      1.0
      (if (= alpha 0.0) ; maxima and A&S 22.3.14 (gsl has bogus values here)
	  (* (/ 2.0 n) 
	     (cos (* n x)))
	  (if (= n 1)   ; gsl splits out special cases 
	      (* 2 alpha x)                             ; G&R 8.93(2)
	      (if (= n 2)
		  (- (* 2 alpha (+ alpha 1) x x) alpha) ; G&R 8.93(3)
		  (let ((fn1 (* 2 x alpha))
			(fn 0.0)
			(fn2 1.0))
		    (if (= n 1)
			fn1
			(do ((k 2 (+ 1 k))
			     (k0 2.0 (+ k0 1.0)))
			    ((> k n) fn)
			  (set! fn (/ (- (* 2 x fn1 (+ k alpha -1.0))
					 (* fn2 (+ k (* 2 alpha) -2.0)))
				      k0))
			  (set! fn2 fn1)
			  (set! fn1 fn)))))))))


;;; (with-sound (:scaled-to 0.5) (do ((i 0 (+ 1 i)) (x 0.0 (+ x .1))) ((= i 10000)) (outa i (gegenbauer 15 (cos x) 1.0))))

#|
(with-sound (:scaled-to 0.5)
  (do ((i 0 (+ 1 i))
       (theta 0.0 (+ theta 0.05)))
      ((= i 10000))
    (let ((x (cos theta)))
      (outa i (gegenbauer 20 x)))))
|#


(define* (chebyshev-polynomial a x :optional (kind 1))
  (let ((n (- (vector-length a) 1)))
    (if (= n 0) 
	(vector-ref a 0)
	(let* ((r (* kind x))
	       (s 1.0)
	       (h 0.0)
	       (sum (vector-ref a 0)))
	  (do ((k 1 (+ 1 k)))
	      ((= k n))
	    (set! h r)
	    (set! sum (+ sum (* r (vector-ref a k))))
	    (set! r (- (* 2 r x) s))
	    (set! s h))
	  (+ sum (* r (vector-ref a n)))))))

(define* (chebyshev n x :optional (kind 1))
  (let ((a (make-vector (+ 1 n) 0.0)))
    (vector-set! a n 1.0)
    (chebyshev-polynomial a x kind)))


(define (hermite-polynomial a x)
  (let ((n (- (vector-length a) 1)))
    (if (= n 0) 
	(vector-ref a 0)
	(let* ((r (* 2 x))
	       (s 1.0)
	       (h 0.0)
	       (sum (vector-ref a 0)))
	  (do ((k 1 (+ 1 k))
	       (k2 2 (+ k2 2)))
	      ((= k n))
	    (set! h r)
	    (set! sum (+ sum (* r (vector-ref a k))))
	    (set! r (- (* 2 r x) (* k2 s)))
	    (set! s h))
	  (+ sum (* r (vector-ref a n)))))))

(define* (hermite n x)
  (let ((a (make-vector (+ 1 n) 0.0)))
    (vector-set! a n 1.0)
    (hermite-polynomial a x)))


(define* (laguerre-polynomial a x :optional (alpha 0.0))
  (let ((n (- (vector-length a) 1)))
    (if (= n 0) 
	(vector-ref a 0)
	(let* ((r (- (+ alpha 1.0) x))
	       (s 1.0)
	       (h 0.0)
	       (sum (vector-ref a 0)))
	  (do ((k 1 (+ 1 k)))
	      ((= k n))
	    (set! h r)
	    (set! sum (+ sum (* r (vector-ref a k))))
	    (set! r (/ (- (* r (- (+ (* 2 k) 1 alpha) x)) 
			  (* s (+ k alpha)))
		       (+ k 1)))
	    (set! s h))
	  (+ sum (* r (vector-ref a n)))))))

(define* (laguerre n x :optional (alpha 0.0))
  (let ((a (make-vector (+ 1 n) 0.0)))
    (vector-set! a n 1.0)
    (laguerre-polynomial a x alpha)))


#|
;;; ----------------
;;; 
;;; just for my amusement -- apply a linear-fractional or Mobius transformation to the fft data (treated as complex)
;;; 
;;; (automorph 1 0 0 1) is the identity
;;; (automorph 2 0 0 1) scales by 2
;;; (automorph 0.0+1.0i 0 0 1) rotates 90 degrees (so 4 times = identity)
;;; most cases won't work right because we're assuming real output and so on

(define* (automorph a b c d :optional snd chn)
  (let* ((len (frames snd chn))
	 (pow2 (inexact->exact (ceiling (/ (log len) (log 2)))))
	 (fftlen (inexact->exact (expt 2 pow2)))
	 (fftscale (/ 1.0 fftlen))
	 (rl (channel->vct 0 fftlen snd chn))
	 (im (make-vct fftlen)))
    (fft rl im 1)
    (vct-scale! rl fftscale)
    (vct-scale! im fftscale)
    ;; handle 0 case by itself
    (let* ((c1 (make-rectangular (vct-ref rl 0) (vct-ref im 0)))
	   (val (/ (+ (* a c1) b)
		   (+ (* c c1) d)))
	   (rval (real-part val))
	   (ival (imag-part val)))
      (vct-set! rl 0 rval)
      (vct-set! im 0 ival))
    (do ((i 1 (+ i 1))
	 (k (- fftlen 1) (- k 1)))
	((= i (/ fftlen 2)))
      (let* ((c1 (make-rectangular (vct-ref rl i) (vct-ref im i)))
	     (val (/ (+ (* a c1) b)      ; (az + b) / (cz + d)
		     (+ (* c c1) d)))
	     (rval (real-part val))
	     (ival (imag-part val)))
	(vct-set! rl i rval)
	(vct-set! im i ival)
	(vct-set! rl k rval)
	(vct-set! im k (- ival))))
    (fft rl im -1)
    (vct->channel rl 0 len snd chn #f (format #f "automorph ~A ~A ~A ~A" a b c d))))
|#


(define (bes-i1 x)				;I1(x)
  (if (< (abs x) 3.75)
      (let* ((y (expt (/ x 3.75) 2)))
	(* x (+ 0.5
		(* y (+ 0.87890594
			(* y (+ 0.51498869
				(* y (+ 0.15084934
					(* y (+ 0.2658733e-1
						(* y (+ 0.301532e-2
							(* y 0.32411e-3))))))))))))))
      (let* ((ax (abs x))
	     (y (/ 3.75 ax))
	     (ans1 (+ 0.2282967e-1
		      (* y (+ -0.2895312e-1
			      (* y (+ 0.1787654e-1 
				      (* y -0.420059e-2)))))))
	     (ans2 (+ 0.39894228
		      (* y (+ -0.3988024e-1
			      (* y (+ -0.362018e-2
				      (* y (+ 0.163801e-2
					      (* y (+ -0.1031555e-1 (* y ans1)))))))))))
	     (sign (if (< x 0.0) -1.0 1.0)))
	(* (/ (exp ax) (sqrt ax)) ans2 sign))))

(define (bes-in n x)			;return In(x) for any integer n, real x
  (if (= n 0) 
      (bes-i0 x)
      (if (= n 1) 
	  (bes-i1 x)
	  (if (= x 0.0) 
	      0.0
	      (let* ((iacc 40)
		     (bigno 1.0e10)
		     (bigni 1.0e-10)
		     (ans 0.0)
		     (tox (/ 2.0 (abs x)))
		     (bip 0.0)
		     (bi 1.0)
		     (m (* 2 (+ n (inexact->exact (truncate (sqrt (* iacc n)))))))
		     (bim 0.0))
		(do ((j m (- j 1)))
		    ((= j 0))
		  (set! bim (+ bip (* j tox bi)))
		  (set! bip bi)
		  (set! bi bim)
		  (if (> (abs bi) bigno)
		      (begin
			(set! ans (* ans bigni))
			(set! bi (* bi bigni))
			(set! bip (* bip bigni))))
		  (if (= j n) (set! ans bip)))
		(if (and (< x 0.0) (odd? n)) (set! ans (- ans)))
		(* ans (/ (bes-i0 x) bi)))))))



(define (aux-f x)			;1<=x<inf
  (let ((x2 (* x x)))
    (/ (+ 38.102495 (* x2 (+ 335.677320 (* x2 (+ 265.187033 (* x2 (+ 38.027264 x2)))))))
       (* x (+ 157.105423 (* x2 (+ 570.236280 (* x2 (+ 322.624911 (* x2 (+ 40.021433 x2)))))))))))

(define (aux-g x)
  (let ((x2 (* x x)))
    (/ (+ 21.821899 (* x2 (+ 352.018498 (* x2 (+ 302.757865 (* x2 (+ 42.242855 x2)))))))
       (* x2 (+ 449.690326 (* x2 (+ 1114.978885 (* x2 (+ 482.485984 (* x2 (+ 48.196927 x2)))))))))))

(define (Si x) 
  (if (>= x 1.0)
      (- (/ pi 2) (* (cos x) (aux-f x)) (* (sin x) (aux-g x)))
    (let* ((sum x)
	   (fact 2.0)
	   (one -1.0)
	   (xs x)
	   (x2 (* x x))
	   (err .000001)
	   (unhappy #t))
      (do ((i 3.0 (+ i 2.0)))
	  ((not unhappy))
	(set! xs (/ (* one x2 xs) (* i fact)))
	(set! one (- one))
	(set! fact (+ 1 fact))
	(set! xs (/ xs fact))
	(set! unhappy (> (abs xs) err))
	(set! sum (+ sum xs)))
      sum)))

(define (Ci x) 
  (if (>= x 1.0)
      (- (* (sin x) (aux-f x)) (* (cos x) (aux-g x)))
    (let* ((g .5772156649)
	   (sum 0.0)
	   (fact 1.0)
	   (one -1.0)
	   (xs 1.0)
	   (x2 (* x x))
	   (err .000001)
	   (unhappy #t))
      (do ((i 2.0 (+ i 2.0)))
	  ((not unhappy))
	(set! xs (/ (* one x2 xs) (* i fact)))
	(set! one (- one))
	(set! fact (+ 1 fact))
	(set! xs (/ xs fact))
	(set! unhappy (> (abs xs) err))
	(set! sum (+ sum xs)))
      (+ g (log x) sum))))



(define bernoulli3
  (let ((saved-values (let ((v (make-vector 100 #f))
			    (vals (vector 1 -1/2 1/6 0 -1/30 0 1/42 0 -1/30 0 5/66 0 -691/2730
					  0 7/6 0 -3617/510 0 43867/798 0 -174611/330 0
					  854513/138 0 -236364091/2730 0 8553103/6 0
					  -23749461029/870 0 8615841276005/14322 0)))
			(do ((i 0 (+ i 1)))
			    ((= i 30))
			  (vector-set! v i (vector-ref vals i)))
			v)))
    (lambda (n)
      (if (number? (vector-ref saved-values n))
	  (vector-ref saved-values n)
	  (let ((value (if (odd? n) 
			   0.0
			   (let ((sum2 0.0)
				 (itmax 1000)
				 (tol 5.0e-7)
				 (close-enough #f))
			     (do ((i 1 (+ i 1)))
				 ((or close-enough
				      (> i itmax)))
			       (let ((term (/ 1.0 (expt i n))))
				 (set! sum2 (+ sum2 term))
				 (set! close-enough (or (< (abs term) tol)
							(< (abs term) (* tol (abs sum2)))))))
			     (/ (* 2.0 sum2 (factorial n)
				   (if (= (modulo n 4) 0) -1 1))
				(expt (* 2.0 pi) n))))))
	    (vector-set! saved-values n value)
	    value)))))

(define (bernoulli-poly n x)
  (let ((fact 1.0)
	(value (bernoulli3 0)))
    (do ((i 1 (+ i 1)))
	((> i n) value)
      (set! fact (* fact (/ (- (+ n 1) i) i)))
      (set! value (+ (* value x)
		     (* fact (bernoulli3 i)))))))

#|
(with-sound (:clipped #f :channels 2)
  (let ((x 0.0)
	(incr (hz->radians 100.0))
	(N 2))
    (do ((i 0 (+ i 1)))
	((= i 44100))
      (outa i (* (expt -1 (- N 1)) 
		 (/ 0.5 (factorial N))
		 (expt (* 2 pi) (+ (* 2 N) 1))
		 (bernoulli-poly (+ (* 2 N) 1) (/ x (* 2 pi)))))
      (outb i (* (expt -1 (- N 1)) 
		 (/ 0.5 (factorial N))
		 (expt (* 2 pi) (+ (* 2 N) 1))
		 (bernoulli-poly (+ (* 2 N) 0) (/ x (* 2 pi)))))
      (set! x (+ x incr))
      (if (> x (* 2 pi)) (set! x (- x (* 2 pi)))))))
|#


;;; --------------------------------------------------------------------------------

(define (sin-m*pi/n m1 n1)

  ;; this returns an expression giving the exact value of sin(m*pi/n), m and n integer
  ;;   if we can handle n -- currently it can be anything of the form 2^a 3^b [5 | 7 | 17 | 35 | 85 | 119]
  ;;   so (sin-m*pi/n 1 60) returns an exact expression for sin(pi/60)

  (define (decrement-m m n)
    (let ((m1 (sin-m*pi/n (- m 1) n))
	  (n1 (sin-m*pi/n 1 n))
	  (m2 (sin-m*pi/n (- m 2) n)))
      (and m1 m2 n1
	   (if (number? n1)
	       `(- (* 2 ,m1 (sqrt ,(- 1 (* n1 n1)))) ,m2)
	       (if (eq? (car n1) 'sqrt)
		   (if (number? (cadr n1))
		       `(- (* 2 ,m1 (sqrt ,(- 1 (cadr n1)))) ,m2)
		       `(- (* 2 ,m1 (sqrt (- 1 ,(cadr n1)))) ,m2))
		   `(- (* 2 ,m1 (sqrt (- 1 (* ,n1 ,n1)))) ,m2))))))

  (let ((m (numerator (/ m1 n1)))
	(n (denominator (/ m1 n1))))

    (set! m (modulo m (* 2 n)))
    ;; now it's in lowest terms without extra factors of 2*pi

    (cond ((zero? m) 0)

	  ((zero? n) (error "divide by zero (sin-m*pi/n n = 0)"))

	  ((negative? n) 
	   (let ((val (sin-m*pi/n m (- n))))
	     (if (number? val)
		 (- val)
		 `(- ,val))))

	  ((= n 1) 0)

	  ((> m n) 
	   (let ((val (sin-m*pi/n (- m n) n)))
	     (and val
		  (if (number? val)
		      (- val)
		      `(- ,val)))))

	  ((= n 2) (if (= m 0) 0 1))

	  ((= (modulo n 2) 0)
	   (cond ((= m 1)
		  (let ((val (sin-m*pi/n 1 (/ n 2))))
		    (and val
			 (if (number? val)
			     (if (= (* val val) 1)
				 `(sqrt 1/2)
				 `(sqrt (/ (- 1 (sqrt ,(- 1 (* val val)))) 2)))
			     (if (eq? (car val) 'sqrt)
				 (if (number? (cadr val))
				     `(sqrt (/ (- 1 (sqrt ,(- 1 (cadr val)))) 2))
				     `(sqrt (/ (- 1 (sqrt (- 1 ,(cadr val)))) 2)))
				 `(sqrt (/ (- 1 (sqrt (- 1 (* ,val ,val)))) 2)))))))
		 (else (decrement-m m n))))

	  ((= n 3) `(sqrt 3/4))

	  ((= (modulo n 3) 0)
	   (cond ((= m 1)
		  (let ((val (sin-m*pi/n 1 (/ n 3))))
		    (and val
			 (let ((x (if (number? val)
				      (- 1 (* x x))
				      (if (eq? (car val) 'sqrt)
					  (if (number? (cadr val))
					      (- 1 (cadr val))
					      `(- 1 ,(cadr val)))
					  `(- 1 (* ,val ,val))))))
			   `(/ (- (expt (+ (sqrt ,x) (* 0+i ,val)) 1/3) (expt (- (sqrt ,x) (* 0+i ,val)) 1/3)) 0+2i)))))
		 (else (decrement-m m n))))

	  ((= n 5) 
	   (cond ((= m 1) `(/ (sqrt (- 10 (* 2 (sqrt 5)))) 4))
		 ((= m 2) `(/ (sqrt (+ 10 (* 2 (sqrt 5)))) 4))
		 (else (decrement-m m n))))

	  ((= n 7)
	   (cond ((or (= m 1)
		     (= m 6)) `(sqrt (+ 7/12 
					(* -1/2 (+ (expt (+ -7/3456 (sqrt -49/442368)) 1/3) 
						   (expt (- -7/3456 (sqrt -49/442368)) 1/3))) 
					(* 1/2 0+i (sqrt 3) (-  (expt (+ -7/3456 (sqrt -49/442368)) 1/3) 
								(expt (- -7/3456 (sqrt -49/442368)) 1/3))))))
		 ((or (= m 2)
		      (= m 5)) `(sqrt (+ 7/12 
					 (* -1/2 (+ (expt (+ -7/3456 (sqrt -49/442368)) 1/3) 
						    (expt (- -7/3456 (sqrt -49/442368)) 1/3))) 
					 (* -1/2 0+i (sqrt 3) (- (expt (+ -7/3456 (sqrt -49/442368)) 1/3)
								 (expt (- -7/3456 (sqrt -49/442368)) 1/3))))))
		 ((or (= m 3)
		      (= m 4)) `(sqrt (+ 7/12 
					 (expt (+ -7/3456 (sqrt -49/442368)) 1/3) 
					 (expt (- -7/3456 (sqrt -49/442368)) 1/3))))))

	  ((= n 17)
	   (cond ((= m 1) 
		  `(* 1/8 (sqrt 2) 
		      (sqrt (- 17 
			       (sqrt 17)
			       (* (sqrt 2)
				  (+ (sqrt (- 17 (sqrt 17))) 
				     (sqrt (+ 34 
					      (* 6 (sqrt 17)) 
					      (* (sqrt 2) (- (sqrt 17) 1) (sqrt (- 17 (sqrt 17)))) 
					      (* -8 (sqrt 2) (sqrt (+ 17 (sqrt 17))))))))))))
		 (else (decrement-m m n))))

	  ((= n 35)
	   (cond ((= m 1)
		  ;; use 1/10 - 1/14 = 1/35
		  ;;  sin 1/10 cos 1/14 - cos 1/10 sin 1/14
		  (let* ((sin10 (sin-m*pi/n 1 10))
			 (sin14 (sin-m*pi/n 1 14)))
		    `(- (* ,sin10 (sqrt (- 1 ,(cadr sin14)))) (* ,sin14 (sqrt (- 1 ,(cadr sin10)))))))
		 (else (decrement-m m n))))

	  ((= n 85)
	   (cond ((= m 1)
		  ;; use 7/17 - 2/5 = 1/85
		  (let* ((sin17 (sin-m*pi/n 7 17))
			 (sin5 (sin-m*pi/n 2 5)))
		    `(- (* ,sin17 (sqrt (- 1 (* ,sin5 ,sin5)))) (* ,sin5 (sqrt (- 1 (* ,sin17 ,sin17)))))))
		 (else (decrement-m m n))))

	  ((= n 119)
	   (cond ((= m 1)
		  ;; use 5/17 - 2/7 = 1/119
		  (let* ((sin17 (sin-m*pi/n 5 17))
			 (sin7 (sin-m*pi/n 2 7)))
		    `(- (* ,sin17 (sqrt (- 1 (* ,sin7 ,sin7)))) (* ,sin7 (sqrt (- 1 (* ,sin17 ,sin17)))))))
		 (else (decrement-m m n))))

	  (else #f))))


