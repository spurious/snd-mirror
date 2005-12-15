;;; various analog filters, based primarily on Anders Johansson's (GPL'd) code
;;;
;;; butterworth-lowpass|highpass|bandstop|bandpass, any order (but numerical trouble if order > ca 16)
;;; chebyshev-lowpass|highpass|bandstop|bandpass, any order (needs scaling fixups and balance)
;;; inverse-chebyshev-lowpass|highpass|bandstop|bandpass, any order (needs scaling fixups, balance, and ripple is wrong)
;;;
;;; if GSL included in Snd:
;;; elliptic-lowpass|highpass|bandstop|bandpass, any order (needs scaling fixups, massive debugging... and balance)
;;; bessel-lowpass|highpass|bandstop|bandpass, any order (needs scaling fixups, massive debugging... and balance)


(load "t.scm") 

;;; TODO: help strings for specfun etc (also arg type checks)
;;; TODO: use gsl-roots to test poly-roots (poly coeffs are stored in same order)


(if (not (defined? 'cascade->canonical)) ; dsp.scm normally
(define (cascade->canonical A)
  ;; convert cascade coeffs to canonical form
  ;; from Orfanidis "Introduction to Signal Processing"

  (define (conv M h L x y)
    ;; x * h -> y
    (do ((n 0 (1+ n)))
	((= n (+ L M)))
      (vct-set! y n 0.0)
      (do ((m (max 0 (- (+ n 1 L))) (1+ m)))
	  ((> m (min n M)))
	(vct-set! y n (+ (vct-ref y n) (* (vct-ref h m) (vct-ref x (- n m))))))))

  (let* ((K (length A))
	 (d (make-vct (1+ (* 2 K))))
	 (a1 (make-vct (1+ (* 2 K)))))
    (vct-set! a1 0 1.0)
    (do ((i 0 (1+ i)))
	((= i K))
      (conv 2 (list-ref A i) (1+ (* 2 i)) a1 d)
      (do ((j 0 (1+ j)))
	  ((= j (+ 3 (* 2 i))))
	(vct-set! a1 j (vct-ref d j))))
    a1)))
    

(define* (analog->digital n num den fz)
  (let* ((g 1.0)
	 (Q 1.0)
	 (wc (tan (* pi fz)))
	 (c (make-vct (* 2 n))))
    (if (odd? n)
	(begin
	  (vct-set! c 0 (/ (- (/ (vct-ref den 0) wc) (vct-ref den 1))
			   (+ (/ (vct-ref den 0) wc) (vct-ref den 1))))
	  (vct-set! c 1 (/ (- (/ (vct-ref num 0) wc) (vct-ref num 1))
			   (+ (/ (vct-ref num 0) wc) (vct-ref num 1))))
	  (set! g (* g (/ (+ (/ (vct-ref num 0) wc) (vct-ref num 1))
			  (+ (/ (vct-ref den 0) wc) (vct-ref den 1)))))))
    (do ((i (if (odd? n) 1 0) (+ i 2))
	 (j (if (odd? n) 2 0) (+ j 3))
	 (k (if (odd? n) 2 0) (+ k 4)))
	((>= i n))
      (let ((nt (make-vct 3))
	    (dt (make-vct 3))
	    (kn 0.0)
	    (kd 0.0))
	(do ((m 0 (1+ m)))
	    ((= m 3))
	  (vct-set! nt m (vct-ref num (+ j m)))
	  (vct-set! dt m (vct-ref den (+ j m))))
	(vct-set! dt 1 (/ (vct-ref dt 1) Q))
	(vct-set! nt 0 (/ (vct-ref nt 0) (* wc wc)))
	(vct-set! nt 1 (/ (vct-ref nt 1) wc))
	(vct-set! dt 0 (/ (vct-ref dt 0) (* wc wc)))
	(vct-set! dt 1 (/ (vct-ref dt 1) wc))
	(set! kd (+ (vct-ref dt 0) (vct-ref dt 1) (vct-ref dt 2)))
	(set! kn (+ (vct-ref nt 0) (vct-ref nt 1) (vct-ref nt 2)))
	(vct-set! c k (/ (- (* 2.0 (vct-ref dt 2)) (* 2.0 (vct-ref dt 0))) kd))
	(vct-set! c (+ k 1) (/ (+ (vct-ref dt 0) (- (vct-ref dt 1)) (vct-ref dt 2)) kd))
	(vct-set! c (+ k 2) (/ (- (* 2.0 (vct-ref nt 2)) (* 2.0 (vct-ref nt 0))) kn))
	(vct-set! c (+ k 3) (/ (+ (vct-ref nt 0) (- (vct-ref nt 1)) (vct-ref nt 2)) kn))
	(set! g (* g (/ kn kd)))))

    (let ((a '())
	  (b '()))
      (if (odd? n)
	  (begin
	    (set! a (cons (vct 1 1 0) a))
	    (set! b (cons (vct 1.0 (vct-ref c 0) 0) b))))

      (do ((i (if (odd? n) 1 0) (+ i 2))
	   (k (if (odd? n) 2 0) (+ k 4))) ; c
	  ((>= i n))
	(set! a (cons (vct (vct-ref c (+ k 3)) (vct-ref c (+ k 2)) (vct-ref c (+ k 3))) a))
	(set! b (cons (vct 1.0 (vct-ref c k) (vct-ref c (+ k 1))) b)))

      (list (vct-scale! (cascade->canonical a) g)
	    (cascade->canonical b)))))

(define (prototype->highpass n num den fc)
  (let* ((g 1.0)
	 (numt (make-vct (vct-length num)))
	 (dent (make-vct (vct-length den))))
    (if (odd? n)
	(begin
	  (set! g (* g (/ (vct-ref num 1) (vct-ref den 1))))
	  (vct-set! numt 0 1.0)
	  (vct-set! numt 1 (* fc (/ (vct-ref num 0) (vct-ref num 1))))
	  (vct-set! dent 0 1.0)
	  (vct-set! dent 1 (* fc (/ (vct-ref den 0) (vct-ref den 1))))))

      (do ((k (if (odd? n) 1 0) (+ k 2))
	   (i (if (odd? n) 2 0) (+ i 3)))
	  ((>= k n)) 
	(set! g (* g (/ (vct-ref num (+ i 2)) (vct-ref den (+ i 2)))))
	(vct-set! numt (+ i 0) 1.0)
	(vct-set! numt (+ i 1) (* fc (/ (vct-ref num (+ i 1)) (vct-ref num (+ i 2)))))
	(vct-set! numt (+ i 2) (* fc fc (/ (vct-ref num i) (vct-ref num (+ i 2)))))
	(vct-set! dent (+ i 0) 1.0)
	(vct-set! dent (+ i 1) (* fc (/ (vct-ref den (+ i 1)) (vct-ref den (+ i 2)))))
	(vct-set! dent (+ i 2) (* fc fc (/ (vct-ref den i) (vct-ref den (+ i 2))))))

    (list g numt dent)))



;;; ---------------- Butterworth ----------------

(define (butterworth-prototype n)
  (let* ((len (if (even? n) (/ (* n 3) 2) (+ 1 (inexact->exact (floor (/ (* n 3) 2))))))
	 (num (make-vct len))
	 (den (make-vct len)))
    (if (odd? n)
	(begin
	  (vct-set! num 0 0.0)
	  (vct-set! num 1 1.0)
	  (vct-set! den 0 1.0)
	  (vct-set! den 1 1.0)))
    (do ((w (if (odd? n) 2 1) (+ w 2))
	 (j (if (odd? n) 2 0) (+ j 3)))
	((>= w n))
      (vct-set! num j 0.0)
      (vct-set! num (+ j 1) 0.0)
      (vct-set! num (+ j 2) 1.0)
      (vct-set! den j 1.0)
      (vct-set! den (+ j 1) (* 2.0 (cos (/ (* w pi) (* 2.0 n)))))
      (vct-set! den (+ j 2) 1.0))
    (list num den)))

(define (butterworth-lowpass n fc) ; n = order, fc = cutoff freq (srate = 1.0)
  ;; identical to make-butter-lp except for fc (freq->1.0) fixup (and odd order)
  (let ((proto (butterworth-prototype n)))
    (analog->digital n (car proto) (cadr proto) fc)))

(define (butterworth-highpass n fc) ; n = order, fc = cutoff freq (srate = 1.0)
  ;; make-butter-hp except for fc (freq->1.0) fixup and odd order
  (let* ((proto (butterworth-prototype n))
	 (hproto (prototype->highpass n (car proto) (cadr proto) 1.0)))
    (analog->digital n (cadr hproto) (caddr hproto) fc)))

(define (butterworth-bandpass n fl fh)
  (let* ((lp (butterworth-lowpass n fh))
	 (hp (butterworth-highpass n fl)))
    (list lp hp)))

#!
(let ((f1 (make-filter :xcoeffs (car (car vals)) :ycoeffs (cadr (car vals))))
 (f2 (make-filter :xcoeffs (car (cadr vals)) :ycoeffs (cadr (cadr vals)))))
   (map-channel (lambda (y) (filter f1 (filter f2 y)))))
!#

(define (butterworth-bandstop n fl fh)
  (let* ((lp (butterworth-lowpass n fl))
	 (hp (butterworth-highpass n fh)))
    (list lp hp)))

#!
(let ((f1 (make-filter :xcoeffs (car (car vals)) :ycoeffs (cadr (car vals))))
 (f2 (make-filter :xcoeffs (car (cadr vals)) :ycoeffs (cadr (cadr vals)))))
   (map-channel (lambda (y) (+ (filter f1 y) (filter f2 y)))))
!#


;;; ---------------- Bessel ---------------- 

;;; TODO: scaling is wrong, and even-order screws up imag=0 check

(define (bessel-prototype n)
  (let* ((len (if (even? n) (/ (* n 3) 2) (+ 1 (inexact->exact (floor (/ (* n 3) 2))))))
	 (num (make-vct len))
	 (den (make-vct len))
	 (mn 0.0)
	 (eps 0.0000001))
    (if (= n 1)
	(begin
	  (vct-set! num 0 0.0)
	  (vct-set! num 1 1.0)
	  (vct-set! den 0 1.0)
	  (vct-set! den 1 1.0))
	(let* ((b1 (make-vct (+ n 1)))
	       (b2 (make-vct (+ n 1))))
	  (vct-set! b1 0 1.0)
	  (vct-set! b2 0 1.0)
	  (vct-set! b2 1 1.0)
	  
	  (do ((i 2 (1+ i)))
	      ((>= i (1+ n)))
	    (let ((b b1))
	      (do ((l (- i 2) (1- l)))
		  ((< l 0))
		(vct-set! b (+ l 2) (vct-ref b1 l)))
	      (vct-set! b1 0 0.0)
	      (vct-set! b 0 0.0)
	      (do ((l 0 (1+ l)))
		  ((>= l i))
		(vct-set! b l (+ (vct-ref b l) (* (- (* 2.0 i) 1.0) (vct-ref b2 l)))))
	      (set! b1 b2)
	      (set! b2 b)))

	  (set! mn (expt (vct-ref b2 0) (/ 1.0 n)))
	  (let* ((p (gsl-roots (vct->vector b2))))

	    (display (format #f ";roots: ~A~%" p))
	    
	    (do ((i 0 (1+ i)))
		((>= i n))
	      (do ((j 0 (1+ j)))
		  ((>= j (1- n)))
		(if (> (magnitude (vector-ref p j))
		       (magnitude (vector-ref p (+ j 1))))
		    (let ((tmp (vector-ref p (+ j 1))))
		      (vector-set! p (1+ j) (vector-ref p j))
		      (vector-set! p j tmp)))))

	    (do ((i 0 (1+ i)))
		((>= i n))
	      (vector-set! p i (/ (vector-ref p i) mn)))

	  (if (odd? n)
	      (begin
		(vct-set! num 0 0.0)
		(vct-set! num 1 1.0)
		(vct-set! den 0 1.0)
		(vct-set! den 1 1.0)))

	    (do ((j (if (odd? n) 2 0))
		 (i 0))
		((>= i n))
	      (if (> (imag-part (vector-ref p i)) eps)
		  (begin
		    (vct-set! num (+ j 0) 0.0)
		    (vct-set! num (+ j 1) 0.0)
		    (vct-set! num (+ j 2) 1.0)
		    (vct-set! den (+ j 0) 1.0)
		    (vct-set! den (+ j 1) (* -2.0 (real-part (vector-ref p i))))
		    (vct-set! den (+ j 2) (real-part (* (vector-ref p i) (vector-ref p (+ i 1)))))
		    (set! j (+ j 3))
		    (set! i (+ i 2)))
		  (begin

		    (display (format #f ";zero imag: ~A ~A~%" (vector-ref p i) i))

		    (vct-set! den 1 (- (real-part (vector-ref p i))))
		    (set! i (+ i 1))))))))
	  
    (list num den)))

(define (bessel-lowpass n fc) ; n = order, fc = cutoff freq (srate = 1.0)
  (let ((proto (bessel-prototype n)))
    (analog->digital n (car proto) (cadr proto) fc)))

(define* (bessel-highpass n fc)
  (let* ((proto (bessel-prototype n))
	 (hproto (prototype->highpass n (cadr proto) (caddr proto) 1.0)))
    (analog->digital n (cadr hproto) (caddr hproto) fc)))

(define* (bessel-bandpass n fl fh)
  (let* ((lp (bessel-lowpass n fh))
	 (hp (bessel-highpass n fl)))
    (list lp hp)))

(define* (bessel-bandstop n fl fh)
  (let* ((lp (bessel-lowpass n fl))
	 (hp (bessel-highpass n fh)))
    (list lp hp)))



;;; ---------------- Chebyshev ---------------- 

(define* (chebyshev-prototype n #:optional (ripple 1.0)) ; ripple in dB (positive)
  (let* ((e (sqrt (- (expt 10.0 (* 0.1 ripple)) 1.0)))
	 (v0 (/ (asinh (/ 1.0 e)) (exact->inexact n)))
	 (len (if (even? n) (/ (* n 3) 2) (+ 1 (inexact->exact (floor (/ (* n 3) 2))))))
	 (num (make-vct len))
	 (den (make-vct len))
	 (g 1.0))
    (if (odd? n)
	(begin
	  (vct-set! num 0 0.0)
	  (vct-set! num 1 1.0)
	  (vct-set! den 0 1.0)
	  (vct-set! den 1 (sinh v0))
	  (set! g (vct-ref den 1)))
	(set! g (/ 1.0 (sqrt (+ 1.0 (* e e))))))
    (do ((l 1.0 (+ l 2.0))
	 (j (if (odd? n) 2 0) (+ j 3)))
	((>= l n))
      (let* ((u (- (* (sinh v0) (sin (/ (* l pi) (* 2.0 n))))))
	     (w (* (cosh v0) (cos (/ (* l pi) (* 2.0 n))))))
      (vct-set! num (+ j 0) 0.0)
      (vct-set! num (+ j 1) 0.0)
      (vct-set! num (+ j 2) 1.0)
      (vct-set! den (+ j 0) 1.0)
      (vct-set! den (+ j 1) (* -2.0 u))
      (vct-set! den (+ j 2) (+ (* u u) (* w w)))
      (set! g (* g (vct-ref den 2)))))
    (list g num den)))

;;; TODO: figure out scaling (will need 2 scalings in bp/bs)

(define* (chebyshev-lowpass n fc #:optional (ripple 1.0)) ; n = order, fc = cutoff freq (srate = 1.0)
  (let ((proto (chebyshev-prototype n ripple)))
    (analog->digital n (cadr proto) (caddr proto) fc)))

(define* (chebyshev-highpass n fc #:optional (ripple 1.0)) ; n = order, fc = cutoff freq (srate = 1.0)
  (let* ((proto (chebyshev-prototype n ripple))
	 (hproto (prototype->highpass n (cadr proto) (caddr proto) 1.0)))
    (analog->digital n (cadr hproto) (caddr hproto) fc)))

(define* (chebyshev-bandpass n fl fh #:optional (ripple 1.0))
  (let* ((lp (chebyshev-lowpass n fh ripple))
	 (hp (chebyshev-highpass n fl ripple)))
    (list lp hp)))

(define* (chebyshev-bandstop n fl fh #:optional (ripple 1.0))
  (let* ((lp (chebyshev-lowpass n fl ripple))
	 (hp (chebyshev-highpass n fh ripple)))
    (list lp hp)))


;;; ---------------- inverse Chebyshev ---------------- 

;;; TODO: i-c needs scaling/balancing
(define* (inverse-chebyshev-prototype n #:optional (loss-dB 60.0)) ; stopband loss
  (let* ((e (sqrt (/ 1.0 (- (expt 10.0 (* 0.1 loss-dB)) 1.0))))
	 (v0 (/ (asinh (/ 1.0 e)) (exact->inexact n)))
	 (len (if (even? n) (/ (* n 3) 2) (+ 1 (inexact->exact (floor (/ (* n 3) 2))))))
	 (num (make-vct len))
	 (den (make-vct len))
	 (g 1.0))
    (if (odd? n)
	(begin
	  (vct-set! num 0 0.0)
	  (vct-set! num 1 1.0)
	  (vct-set! den 0 1.0)
	  (vct-set! den 1 (/ 1.0 (sinh v0)))
	  (set! g (vct-ref den 1))))

    (let ((pl (if (odd? n) 1.0 0.0)))
    (do ((l 1.0 (+ l 2.0))
	 (j (if (odd? n) 2 0) (+ j 3)))
	((>= l n))
      (let* ((u (- (* (sinh v0) (sin (/ (* l pi) (* 2.0 n))))))
	     (w (* (cosh v0) (cos (/ (* l pi) (* 2.0 n)))))
	     (t (/ 1.0 (sin (/ (* (+ l pl) pi) (* 2.0 n))))))
      (vct-set! num (+ j 0) 1.0)
      (vct-set! num (+ j 1) 0.0)
      (vct-set! num (+ j 2) (* t t))
      (vct-set! den (+ j 0) 1.0)
      (vct-set! den (+ j 1) (/ (* -2.0 u) (+ (* u u) (* w w))))
      (vct-set! den (+ j 2) (/ 1.0 (+ (* u u) (* w w))))
      (set! g (* g (/ (vct-ref den 2) (vct-ref num 2)))))))
    (list g num den)))

(define* (inverse-chebyshev-lowpass n fc #:optional (loss-dB 60.0)) ; n = order, fc = cutoff freq (srate = 1.0)
  (let ((proto (inverse-chebyshev-prototype n loss-dB)))
    (analog->digital n (cadr proto) (caddr proto) fc)))

(define* (inverse-chebyshev-highpass n fc #:optional (loss-dB 60.0)) ; n = order, fc = cutoff freq (srate = 1.0)
  (let* ((proto (inverse-chebyshev-prototype n loss-dB))
	 (hproto (prototype->highpass n (cadr proto) (caddr proto) 60.0)))
    (analog->digital n (cadr hproto) (caddr hproto) fc)))

(define* (inverse-chebyshev-bandpass n fl fh #:optional (loss-dB 60.0))
  (let* ((lp (inverse-chebyshev-lowpass n fh loss-dB))
	 (hp (inverse-chebyshev-highpass n fl loss-dB)))
    (list lp hp)))

(define* (inverse-chebyshev-bandstop n fl fh #:optional (loss-dB 60.0))
  (let* ((lp (inverse-chebyshev-lowpass n fl loss-dB))
	 (hp (inverse-chebyshev-highpass n fh loss-dB)))
    (list lp hp)))



;;; ---------------- Elliptic ---------------- 

;;; TODO: debug elliptic filter

(define* (minimize-function f xmin xmax #:optional arg1 arg2)
  (let* ((n 20)
	 (x (make-vct n))
	 (fx (f xmin arg1 arg2)))
    (do ((i 0 (1+ i)))
	((= i n))

      (let ((step (/ (- xmax xmin) (- n 1.0))))
	(do ((j 0 (1+ j))
	     (s xmin (+ s step)))
	    ((= j (1- n)))
	  (vct-set! x j s))
	(vct-set! x (1- n) xmax))

      (do ((j 0 (1+ j)))
	  ((= j n))
	(let ((ft (f (vct-ref x j) arg1 arg2)))
	  (if (< ft fx)
	      (begin
		(set! fx ft)
		(set! xmax (if (< j (1- n)) (vct-ref x (1+ j)) (vct-ref x (1- n))))
		(set! xmin (if (> j 0) (vct-ref x (1- j)) (vct-ref x 0))))))))

    (/ (+ xmax xmin) 2.0)))

(define (findm m arg1 arg2)
  (abs (- (/ (gsl-ellipk m) (gsl-ellipk (- 1.0 m))) arg1)))

(define (findv u arg1 arg2)
  (let ((vals (gsl-ellipj u arg1)))
    (abs (- arg2 (/ (car vals) (cadr vals))))))
  
(define* (elliptic-prototype n #:optional (ripple 1.0) (loss-dB 60.0))
  (let* ((e (sqrt (- (expt 10.0 (* 0.1 ripple)) 1.0)))
	 (k1 (/ e (sqrt (- (expt 10.0 (* 0.1 loss-dB)) 1.0))))
	 (k1p (sqrt (- 1.0 (* k1 k1))))
	 (kr 0.0)
	 (m 0.0)
	 (k 0.0)
	 (len (if (even? n) (/ (* n 3) 2) (+ 1 (inexact->exact (floor (/ (* n 3) 2))))))
	 (num (make-vct len))
	 (den (make-vct len))
	 (g 1.0)
	 (eps 0.0000001))
    (if (> (abs (- 1.0 (* k1p k1p))) eps)
	(set! kr (* (exact->inexact n) (/ (gsl-ellipk (* k1 k1)) (gsl-ellipk (* k1p k1p))))))
    (set! m (minimize-function findm 0.001 0.999 kr))
    (set! k (gsl-ellipk m))

    (if (odd? n)
	(begin
	  (vct-set! num 0 0.0)
	  (vct-set! num 1 1.0)
	  (vct-set! den 0 1.0)
	  (vct-set! den 1 0.0)))

    (let* ((nc (if (odd? n) (1- n) n))
	   (cv (make-vct (inexact->exact (floor (* 0.5 (* 3 (1+ n))))))))
	   
      (do ((l (if (odd? n) 0.0 1.0) (+ l 2.0))
	   (i 0)
	   (j 0 (+ j 3))
	   (ctr (if (odd? n) 2 0)))
	  ((>= i nc))
	(let* ((vals (gsl-ellipj (/ (* l k) (exact->inexact n)) m))
	       (sn (car vals))
	       (cn (cadr vals))
	       (dn (caddr vals)))
	  (vct-set! cv (+ j 0) sn)
	  (vct-set! cv (+ j 1) cn)
	  (vct-set! cv (+ j 2) dn)
	  (if (> sn eps)
	      (let* ((z (/ 0.0-i (* (sqrt m) sn)))
		     (pz (real-part (* z (make-rectangular (real-part z) (- (imag-part z)))))))
		(set! g (/ g pz))
		(vct-set! num (+ ctr 0) 1.0)
		(vct-set! num (+ ctr 1) (* -2.0 (real-part z)))
		(vct-set! num (+ ctr 2) pz)
		(set! ctr (+ ctr 3))
		(set! i (+ i 2))))))

      (let* ((optarg0 (* k1p k1p))
	     (optarg1 (/ 1.0 e))
	     (v0 (/ (* k (minimize-function findv 0.0 (/ 1.0 e) optarg0 optarg1)
		       (* n (gsl-ellipk (* k k1))))))
	     (vals (gsl-ellipj v0 (- 1.0 m)))
	     (sn (car vals))
	     (cn (cadr vals))
	     (dn (caddr vals)))

	(do ((i 0)
	     (j 0 (+ j 3))
	     (ctr (if (odd? n) 2 0)))
	    ((>= i nc))
	  (let* ((p (/ (- (+ (* (vct-ref cv (+ j 1)) (vct-ref cv (+ j 2)) sn cn)
			     (* 0.0+i (vct-ref cv (+ j 0)) dn)))
		       (- 1.0 (* (vct-ref cv (+ j 2)) sn
				 (vct-ref cv (+ j 2)) sn)))))
	    (if (> (abs (imag-part p)) eps)
		(let ((pp (* p (make-rectangular (real-part p) (- (imag-part p))))))
		  (set! g (* g pp))
		  (vct-set! den (+ ctr 0) 1.0)
		  (vct-set! den (+ ctr 1) (* -2.0 (real-part p)))
		  (vct-set! den (+ ctr 2) pp)
		  (set! i (+ i 2))
		  (set! ctr (+ ctr 3)))
		(begin
		  (set! g (* g (real-part p)))
		  (vct-set! den 1 (- (real-part p)))
		  (set! i (+ i 1)))))))) ; it starts at 0 above even in odd case

    (if (even? n)
	(set! g (/ g (sqrt (+ 1.0 (* e e))))))
    (set! g (abs g))
    (list g num den)))

(define* (elliptic-lowpass n fc #:optional (ripple 1.0) (loss-dB 60.0)) ; n = order, fc = cutoff freq (srate = 1.0)
  (let ((proto (elliptic-prototype n ripple loss-dB)))
    (analog->digital n (cadr proto) (caddr proto) fc)))

(define* (elliptic-highpass n fc #:optional (ripple 1.0) (loss-dB 60.0)) ; n = order, fc = cutoff freq (srate = 1.0)
  (let* ((proto (elliptic-prototype n ripple loss-dB))
	 (hproto (prototype->highpass n (cadr proto) (caddr proto) 60.0)))
    (analog->digital n (cadr hproto) (caddr hproto) fc)))

(define* (elliptic-bandpass n fl fh #:optional (ripple 1.0) (loss-dB 60.0))
  (let* ((lp (elliptic-lowpass n fh ripple loss-dB))
	 (hp (elliptic-highpass n fl ripple loss-dB)))
    (list lp hp)))

(define* (elliptic-bandstop n fl fh #:optional (ripple 1.0) (loss-dB 60.0))
  (let* ((lp (elliptic-lowpass n fl ripple loss-dB))
	 (hp (elliptic-highpass n fh ripple loss-dB)))
    (list lp hp)))
