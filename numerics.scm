(use-modules (ice-9 optargs) (ice-9 format))

(provide 'snd-numerics.scm)

;;; random stuff I needed at one time or another while goofing around...


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
	    (do ((i 0 (1+ i)))
		((= i old-num))
	      (vector-set! factorials i (vector-ref old-facts i)))))
      (if (zero? (vector-ref factorials n))
	  (vector-set! factorials n (* n (factorial (1- n)))))
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
		   (cnk (1+ mx)))
	      (do ((i 2 (1+ i)))
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
	      (do ((i 1 (1+ i)))
		  ((> i m))
		(set! pmm (* (- pmm) fact somx2))
		(set! fact (+ fact 2.0)))))
	(if (= l m) 
	    pmm
	    (let ((pmmp1 (* x pmm (+ (* 2 m) 1))))
	      (if (= l (+ m 1)) 
		  pmmp1
		  (let ((pk 0.0)) ; NR used "ll" which is unreadable
		    (do ((k (+ m 2) (1+ k)))
			((> k l))
		      (set! pk (/ (- (* x (- (* 2 k) 1) pmmp1) 
				      (* (+ k m -1) pmm)) 
				   (- k m)))
		      (set! pmm pmmp1)
		      (set! pmmp1 pk))
		    pk)))))))


;;; A&S (bessel.lisp)
(define (legendre-polynomial a x) ; sum of weighted polynomials (m=0)
  (let ((n (1- (vector-length a))))
    (if (= n 0) 
	(vector-ref a 0)
	(let* ((r x)
	       (s 1.0)
	       (h 0.0)
	       (sum (vector-ref a 0)))
	  (do ((k 1 (1+ k)))
	      ((= k n))
	    (set! h r)
	    (set! sum (+ sum (* r (vector-ref a k))))
	    (set! r (/ (- (* r x (+ (* 2 k) 1))  (* s k)) (+ k 1)))
	    (set! s h))
	  (+ sum (* r (vector-ref a n)))))))

(define (legendre n x)
  (legendre-polynomial (let ((v (make-vector (1+ n) 0.0)))
			 (vector-set! v n 1.0)
			 v)
		       x))


#|
;; if l odd, there seems to be sign confusion:
(with-sound (:channels 2 :scaled-to 1.0)
  (do ((i 0 (1+ i))
       (theta 0.0 (+ theta 0.01)))
      ((= i 10000))
    (outa i (plgndr 1 1 (cos theta)) *output*)
    (let ((x (sin theta)))
      (outb i (- x) *output*))))

;; this works:
(with-sound (:channels 2 :scaled-to 1.0)
  (do ((i 0 (1+ i))
       (theta 0.0 (+ theta 0.01)))
      ((= i 10000))
    (let ((x (cos theta)))
      (outa i (plgndr 3 0 x) *output*)
      (outb i (* 0.5 x (- (* 5 x x) 3)) *output*))))
|#


;;; TODO: add the div close to 0 check to dsp.scm sum-of-sines stuff
;;; TODO: break this into gegen-poly case+straight 

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
			(do ((k 2 (1+ k))
			     (k0 2.0 (+ k0 1.0)))
			    ((> k n) fn)
			  (set! fn (/ (- (* 2 x fn1 (+ k alpha -1.0))
					 (* fn2 (+ k (* 2 alpha) -2.0)))
				      k0))
			  (set! fn2 fn1)
			  (set! fn1 fn)))))))))




(define* (chebyshev-polynomial a x :optional (kind 1))
  (let ((n (1- (vector-length a))))
    (if (= n 0) 
	(vector-ref a 0)
	(let* ((r (* kind x))
	       (s 1.0)
	       (h 0.0)
	       (sum (vector-ref a 0)))
	  (do ((k 1 (1+ k)))
	      ((= k n))
	    (set! h r)
	    (set! sum (+ sum (* r (vector-ref a k))))
	    (set! r (- (* 2 r x) s))
	    (set! s h))
	  (+ sum (* r (vector-ref a n)))))))

(define* (chebyshev n x :optional (kind 1))
  (let ((a (make-vector (1+ n) 0.0)))
    (vector-set! a n 1.0)
    (chebyshev-polynomial a x kind)))


(define (hermite-polynomial a x)
  (let ((n (1- (vector-length a))))
    (if (= n 0) 
	(vector-ref a 0)
	(let* ((r (* 2 x))
	       (s 1.0)
	       (h 0.0)
	       (sum (vector-ref a 0)))
	  (do ((k 1 (1+ k))
	       (k2 2 (+ k2 2)))
	      ((= k n))
	    (set! h r)
	    (set! sum (+ sum (* r (vector-ref a k))))
	    (set! r (- (* 2 r x) (* k2 s)))
	    (set! s h))
	  (+ sum (* r (vector-ref a n)))))))

(define* (hermite n x)
  (let ((a (make-vector (1+ n) 0.0)))
    (vector-set! a n 1.0)
    (hermite-polynomial a x)))


(define* (laguerre-polynomial a x :optional (alpha 0.0))
  (let ((n (1- (vector-length a))))
    (if (= n 0) 
	(vector-ref a 0)
	(let* ((r (- (+ alpha 1.0) x))
	       (s 1.0)
	       (h 0.0)
	       (sum (vector-ref a 0)))
	  (do ((k 1 (1+ k)))
	      ((= k n))
	    (set! h r)
	    (set! sum (+ sum (* r (vector-ref a k))))
	    (set! r (/ (- (* r (- (+ (* 2 k) 1 alpha) x)) 
			  (* s (+ k alpha)))
		       (+ k 1)))
	    (set! s h))
	  (+ sum (* r (vector-ref a n)))))))

(define* (laguerre n x :optional (alpha 0.0))
  (let ((a (make-vector (1+ n) 0.0)))
    (vector-set! a n 1.0)
    (laguerre-polynomial a x alpha)))
