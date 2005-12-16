;;; various analog filters, based primarily on Anders Johansson's (GPL'd) code
;;;   I removed the odd order cases because they didn't work right and I didn't immediately see why not
;;;   (and they make the code basically unreadable, for no apparent gain -- who cares about odd orders here?)
;;;
;;; butterworth-lowpass|highpass|bandstop|bandpass, even order (but numerical trouble if order > ca 16)
;;; chebyshev-lowpass|highpass|bandstop|bandpass, even order
;;; inverse-chebyshev-lowpass|highpass|bandstop|bandpass, even order
;;;
;;; if GSL included in Snd:
;;; elliptic-lowpass|highpass|bandstop|bandpass, even order (needs scaling fixups, massive debugging... and balance)
;;; bessel-lowpass|highpass|bandstop|bandpass, even order (needs scaling fixups, massive debugging... and balance)


(load "t.scm") 


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

    (do ((i 0 (+ i 2))
	 (j 0 (+ j 3))
	 (k 0 (+ k 4)))
	((>= i n))
      (let* ((nt0 (/ (vct-ref num (+ j 0)) (* wc wc)))
	    (nt1 (/ (vct-ref num (+ j 1)) wc))
	    (nt2 (vct-ref num (+ j 2)))
	    (dt0 (/ (vct-ref den (+ j 0)) (* wc wc)))
	    (dt1 (/ (vct-ref den (+ j 1)) (* wc Q)))
	    (dt2 (vct-ref den (+ j 2)))
	    (kd (+ dt0 dt1 dt2))
	    (kn (+ nt0 nt1 nt2)))
	(vct-set! c (+ k 0) (/ (- (* 2.0 dt2) (* 2.0 dt0)) kd))
	(vct-set! c (+ k 1) (/ (+ dt0 (- dt1) dt2) kd))
	(vct-set! c (+ k 2) (/ (- (* 2.0 nt2) (* 2.0 nt0)) kn))
	(vct-set! c (+ k 3) (/ (+ nt0 (- nt1) nt2) kn))
	(set! g (* g (/ kn kd)))))

    (let ((a '())
	  (b '()))
      (do ((i 0 (+ i 2))
	   (k 0 (+ k 4))) ; c
	  ((>= i n))
	(set! a (cons (vct (vct-ref c (+ k 3)) (vct-ref c (+ k 2)) (vct-ref c (+ k 3))) a))
	(set! b (cons (vct 1.0 (vct-ref c k) (vct-ref c (+ k 1))) b)))

      (list (vct-scale! (cascade->canonical a) g) ; scale entire numerator because this is the convolved form
	    (cascade->canonical b)))))

(define (prototype->highpass n num den)
  (let* ((g 1.0)
	 (numt (make-vct (vct-length num)))
	 (dent (make-vct (vct-length den))))
    (do ((k 0 (+ k 2))
	 (i 0 (+ i 3)))
	((>= k n)) 
      (set! g (* g (/ (vct-ref num (+ i 2)) (vct-ref den (+ i 2)))))
      (vct-set! numt (+ i 0) 1.0)
      (vct-set! numt (+ i 1) (/ (vct-ref num (+ i 1)) (vct-ref num (+ i 2))))
      (vct-set! numt (+ i 2) (/ (vct-ref num i) (vct-ref num (+ i 2))))
      (vct-set! dent (+ i 0) 1.0)
      (vct-set! dent (+ i 1) (/ (vct-ref den (+ i 1)) (vct-ref den (+ i 2))))
      (vct-set! dent (+ i 2) (/ (vct-ref den i) (vct-ref den (+ i 2)))))

    (if (fneq g 1.0) (snd-display ";high g: ~A" g))
    (list numt dent)))



;;; ---------------- Butterworth ----------------

(define (butterworth-prototype n)
  (let* ((len (/ (* n 3) 2))
	 (num (make-vct len))
	 (den (make-vct len)))
    (do ((w 1 (+ w 2))
	 (j 0 (+ j 3)))
	((>= w n))
      (vct-set! num j 0.0)
      (vct-set! num (+ j 1) 0.0)
      (vct-set! num (+ j 2) 1.0)
      (vct-set! den j 1.0)
      (vct-set! den (+ j 1) (* 2.0 (cos (/ (* w pi) (* 2.0 n)))))
      (vct-set! den (+ j 2) 1.0))
    (list num den)))

(define (make-butterworth-lowpass n fc) ; n = order, fc = cutoff freq (srate = 1.0)
  ;; identical to make-butter-lp except for fc (freq->1.0) fixup
  (if (odd? n) (set! n (1+ n)))
  (let* ((proto (butterworth-prototype n))
	 (coeffs (analog->digital n (car proto) (cadr proto) fc)))
    (make-filter :xcoeffs (car coeffs) :ycoeffs (cadr coeffs))))

(define (make-butterworth-highpass n fc) ; n = order, fc = cutoff freq (srate = 1.0)
  (if (odd? n) (set! n (1+ n)))
  (let* ((proto (butterworth-prototype n))
	 (hproto (prototype->highpass n (car proto) (cadr proto)))
	 (coeffs (analog->digital n (car hproto) (cadr hproto) fc)))
    (make-filter :xcoeffs (car coeffs) :ycoeffs (cadr coeffs))))

(define (make-butterworth-bandpass n fl fh)
  (if (odd? n) (set! n (1+ n)))
  (let* ((lp (make-butterworth-lowpass n fh))
	 (hp (make-butterworth-highpass n fl)))
    (lambda (y) (filter lp (filter hp y)))))

(define (make-butterworth-bandstop n fl fh)
  (if (odd? n) (set! n (1+ n)))
  (let* ((lp (make-butterworth-lowpass n fl))
	 (hp (make-butterworth-highpass n fh)))
    (lambda (y) (+ (filter lp y) (filter hp y)))))



;;; ---------------- Bessel ---------------- 

;;; TODO: scaling is wrong, and even-order screws up imag=0 check

(define (bessel-prototype n)
  (let* ((len (/ (* n 3) 2))
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

	    (do ((j 0)
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

(define (make-bessel-lowpass n fc) ; n = order, fc = cutoff freq (srate = 1.0)
  (if (odd? n) (set! n (1+ n)))
  (let ((proto (bessel-prototype n)))
    (analog->digital n (car proto) (cadr proto) fc)))

(define* (make-bessel-highpass n fc)
  (if (odd? n) (set! n (1+ n)))
  (let* ((proto (bessel-prototype n))
	 (hproto (prototype->highpass n (cadr proto) (caddr proto))))
    (analog->digital n (car hproto) (cadr hproto) fc)))

(define* (make-bessel-bandpass n fl fh)
  (if (odd? n) (set! n (1+ n)))
  (let* ((lp (bessel-lowpass n fh))
	 (hp (bessel-highpass n fl)))
    (list lp hp)))

(define* (make-bessel-bandstop n fl fh)
  (if (odd? n) (set! n (1+ n)))
  (let* ((lp (bessel-lowpass n fl))
	 (hp (bessel-highpass n fh)))
    (list lp hp)))



;;; ---------------- Chebyshev ---------------- 

(define* (chebyshev-prototype n #:optional (ripple 1.0)) ; ripple in dB (positive)
  (let* ((e (sqrt (- (expt 10.0 (* 0.1 ripple)) 1.0)))
	 (v0 (/ (asinh (/ 1.0 e)) (exact->inexact n)))
	 (len (/ (* n 3) 2))
	 (num (make-vct len))
	 (den (make-vct len))
	 (g 1.0))
    (set! g (/ 1.0 (sqrt (+ 1.0 (* e e)))))
    (do ((l 1.0 (+ l 2.0))
	 (j 0 (+ j 3)))
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
    (vct-set! num 2 (expt 2.0 (- 2 n)))
    (list num den)))

(define* (make-chebyshev-lowpass n fc #:optional (ripple 1.0)) ; n = order, fc = cutoff freq (srate = 1.0)
  (if (odd? n) (set! n (1+ n)))
  (let* ((proto (chebyshev-prototype n ripple))
	 (coeffs (analog->digital n (car proto) (cadr proto) fc)))
    (make-filter :xcoeffs (car coeffs) :ycoeffs (cadr coeffs))))

(define* (make-chebyshev-highpass n fc #:optional (ripple 1.0)) ; n = order, fc = cutoff freq (srate = 1.0)
  (if (odd? n) (set! n (1+ n)))
  (let* ((proto (chebyshev-prototype n ripple))
	 (hproto (prototype->highpass n (car proto) (cadr proto)))
	 (coeffs (analog->digital n (car hproto) (cadr hproto) fc)))
    (make-filter :xcoeffs (car coeffs) :ycoeffs (cadr coeffs))))

(define* (make-chebyshev-bandpass n fl fh #:optional (ripple 1.0))
  (if (odd? n) (set! n (1+ n)))
  (let* ((lp (chebyshev-lowpass n fh ripple))
	 (hp (chebyshev-highpass n fl ripple)))
    (lambda (y) (filter lp (filter hp y)))))

(define* (make-chebyshev-bandstop n fl fh #:optional (ripple 1.0))
  (if (odd? n) (set! n (1+ n)))
  (let* ((lp (chebyshev-lowpass n fl ripple))
	 (hp (chebyshev-highpass n fh ripple)))
    (lambda (y) (+ (filter lp y) (filter hp y)))))



;;; ---------------- inverse Chebyshev ---------------- 

;;; TODO: i-c needs scaling/balancing
(define* (inverse-chebyshev-prototype n #:optional (loss-dB 60.0)) ; stopband loss
  (let* ((e (sqrt (/ 1.0 (- (expt 10.0 (* 0.1 loss-dB)) 1.0))))
	 (v0 (/ (asinh (/ 1.0 e)) (exact->inexact n)))
	 (len (/ (* n 3) 2))
	 (num (make-vct len))
	 (den (make-vct len))
	 (g 1.0))
    (let ((pl 0.0))
      (do ((l 1.0 (+ l 2.0))
	   (j 0 (+ j 3)))
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

(define* (make-inverse-chebyshev-lowpass n fc #:optional (loss-dB 60.0)) ; n = order, fc = cutoff freq (srate = 1.0)
  (if (odd? n) (set! n (1+ n)))
  (let ((proto (inverse-chebyshev-prototype n loss-dB)))
    (analog->digital n (cadr proto) (caddr proto) fc)))

(define* (make-inverse-chebyshev-highpass n fc #:optional (loss-dB 60.0)) ; n = order, fc = cutoff freq (srate = 1.0)
  (if (odd? n) (set! n (1+ n)))
  (let* ((proto (inverse-chebyshev-prototype n loss-dB))
	 (hproto (prototype->highpass n (cadr proto) (caddr proto))))
    (analog->digital n (car hproto) (cadr hproto) fc)))

(define* (make-inverse-chebyshev-bandpass n fl fh #:optional (loss-dB 60.0))
  (if (odd? n) (set! n (1+ n)))
  (let* ((lp (inverse-chebyshev-lowpass n fh loss-dB))
	 (hp (inverse-chebyshev-highpass n fl loss-dB)))
    (list lp hp)))

(define* (make-inverse-chebyshev-bandstop n fl fh #:optional (loss-dB 60.0))
  (if (odd? n) (set! n (1+ n)))
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
	 (len (/ (* n 3) 2))
	 (num (make-vct len))
	 (den (make-vct len))
	 (g 1.0)
	 (eps 0.0000001))
    (if (> (abs (- 1.0 (* k1p k1p))) eps)
	(set! kr (* (exact->inexact n) (/ (gsl-ellipk (* k1 k1)) (gsl-ellipk (* k1p k1p))))))
    (set! m (minimize-function findm 0.001 0.999 kr))
    (set! k (gsl-ellipk m))

    (let* ((cv (make-vct (inexact->exact (floor (* 0.5 (* 3 (1+ n))))))))
	   
      (do ((l 1.0 (+ l 2.0))
	   (i 0)
	   (j 0 (+ j 3))
	   (ctr 0))
	  ((>= i n))
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
	     (ctr 0))
	    ((>= i n))
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

    (set! g (/ g (sqrt (+ 1.0 (* e e)))))
    (set! g (abs g))
    (list g num den)))

(define* (make-elliptic-lowpass n fc #:optional (ripple 1.0) (loss-dB 60.0)) ; n = order, fc = cutoff freq (srate = 1.0)
  (if (odd? n) (set! n (1+ n)))
  (let ((proto (elliptic-prototype n ripple loss-dB)))
    (analog->digital n (cadr proto) (caddr proto) fc)))

(define* (make-elliptic-highpass n fc #:optional (ripple 1.0) (loss-dB 60.0)) ; n = order, fc = cutoff freq (srate = 1.0)
  (if (odd? n) (set! n (1+ n)))
  (let* ((proto (elliptic-prototype n ripple loss-dB))
	 (hproto (prototype->highpass n (cadr proto) (caddr proto))))
    (analog->digital n (car hproto) (cadr hproto) fc)))

(define* (make-elliptic-bandpass n fl fh #:optional (ripple 1.0) (loss-dB 60.0))
  (if (odd? n) (set! n (1+ n)))
  (let* ((lp (elliptic-lowpass n fh ripple loss-dB))
	 (hp (elliptic-highpass n fl ripple loss-dB)))
    (list lp hp)))

(define* (make-elliptic-bandstop n fl fh #:optional (ripple 1.0) (loss-dB 60.0))
  (if (odd? n) (set! n (1+ n)))
  (let* ((lp (elliptic-lowpass n fl ripple loss-dB))
	 (hp (elliptic-highpass n fh ripple loss-dB)))
    (list lp hp)))


;;; TODO: move af tests to snd-test
(if (not (provided? 'snd-dsp.scm)) (load-from-path "dsp.scm"))

  (define (sweep->bins flt bins)
    (let ((ind (new-sound "test.snd" mus-next mus-bfloat 22050 1 #f 22050)))
      (let ((phase 0.0)
	    (freq 0.0)
	    (incr (/ pi 22050)))
	(map-channel 
	 (lambda (y)
	   (let ((val (sin phase))) 
	     (set! phase (+ phase freq)) 
	     (set! freq (+ freq incr))
	     (* .5 val)))))
      (map-channel flt)
      (let* ((mx (maxamp))
	     (resp (make-vct bins))
	     (size (inexact->exact (round (/ 22050 bins)))))
	(do ((i 0 (1+ i)))
	    ((= i bins))
	  (let ((data (channel->vct (* i size) size)))
	    (vct-set! resp i (vct-peak data))))
	(close-sound ind)
	(list mx resp))))

(define (af-tests)


  (define (filter-equal? f1 f2) ; equalp in clm2xen is too restrictive
    (and (= (mus-order f1) (mus-order f2))
	 (vequal (mus-xcoeffs f1) (mus-xcoeffs f2))
	 (vequal (mus-ycoeffs f1) (mus-ycoeffs f2))))

  (define (filter-response-equal? f1 f2)
    (let ((v1 (make-vct 100))
	  (v2 (make-vct 100))
	  (signal 1.0))
      (do ((i 0 (1+ i)))
	  ((= i 100))
	(vct-set! v1 i (f1 signal))
	(vct-set! v2 i (f2 signal))
	(set! signal 0.0))
      (vequal v1 v2)))

  ;; ---------------- butterworth tests ----------------
  (let ((poles (list (vct 1.000 1.414 1.000) ; numerous references provide these tables (y[0] is ignored)
		     (vct 1.000 1.848 1.000 1.000 0.765 1.000)
		     (vct 1.000 1.932 1.000 1.000 1.414 1.000 1.000 0.518 1.000)
		     (vct 1.000 1.962 1.000 1.000 1.663 1.000 1.000 1.111 1.000 1.000 0.390 1.000)
		     (vct 1.000 1.975 1.000 1.000 1.782 1.000 1.000 1.414 1.000 1.000 0.908 1.000 1.000 0.313 1.000))))
    (do ((i 2 (+ i 2))
	 (k 0 (1+ k)))
	((>= i 12))
      (let ((vals (butterworth-prototype i)))
	(if (not (vequal (cadr vals) (list-ref poles k)))
	    (snd-display ";butterworth prototype poles ~A: ~A (~A)" i (cadr vals) (list-ref poles k)))
	(let ((zeros (make-vct (* (1- i) 3))))
	  (do ((j 2 (+ j 3)))
	      ((>= j (* (1- i) 3)))
	    (vct-set! zeros j 1.0))
	  (if (not (vequal (car vals) zeros))
	      (snd-display ";butterworth prototype zeros ~A: ~A (~A)" i (car vals) zeros)))))
    (do ((cutoff .1 (+ cutoff .1))
	 (m 0 (1+ m)))
	((= m 3))
      (do ((i 2 (+ i 2))
	   (k 1 (+ k 1)))
	  ((= i 16))
	(let ((local (make-butterworth-lowpass i cutoff))
	      (dsp (make-butter-lp k (* (mus-srate) cutoff))))
	  (if (not (filter-equal? local dsp))
	      (snd-display ";butterworth lowpass ~A ~A ~A" cutoff local dsp)))
	(let ((local (make-butterworth-highpass i cutoff))
	      (dsp (make-butter-hp k (* (mus-srate) cutoff))))
	  (if (not (filter-equal? local dsp))
	      (snd-display ";butterworth highpass ~A ~A ~A" cutoff local dsp)))))


    (let* ((f1 (make-butterworth-lowpass 8 .1))
	   (vals (sweep->bins f1 10)))
      (if (fneq (car vals) .5) (snd-display ";butterworth lp 8 max: ~A" (car vals)))
      (if (not (vequal (cadr vals) (vct 0.500 0.500 0.359 0.014 0.001 0.000 0.000 0.000 0.000 0.000)))
	  (snd-display ";butterworth lp 8 .1 spect: ~A" (cadr vals))))
    (let* ((f1 (make-butterworth-lowpass 12 .25))
	   (vals (sweep->bins f1 10)))
      (if (fneq (car vals) .5) (snd-display ";butterworth lp 12 max: ~A" (car vals)))
      (if (not (vequal (cadr vals) (vct 0.500 0.500 0.500 0.500 0.499 0.358 0.010 0.000 0.000 0.000)))
	  (snd-display ";butterworth lp 12 .25 spect: ~A" (cadr vals))))
    (let* ((f1 (make-butterworth-lowpass 10 .4))
	   (vals (sweep->bins f1 10)))
      (if (fneq (car vals) .5) (snd-display ";butterworth lp 10 max: ~A" (car vals)))
      (if (not (vequal (cadr vals) (vct 0.500 0.500 0.500 0.500 0.500 0.500 0.500 0.499 0.361 0.001)))
	  (snd-display ";butterworth lp 10 .4 spect: ~A" (cadr vals))))

    (let* ((f1 (make-butterworth-highpass 8 .1))
	   (vals (sweep->bins f1 10)))
      (if (fneq (car vals) .5) (snd-display ";butterworth hp 8 max: ~A" (car vals)))
      (if (not (vequal (cadr vals) (vct 0.001 0.348 0.500 0.500 0.500 0.500 0.500 0.500 0.500 0.500)))
	  (snd-display ";butterworth hp 8 .1 spect: ~A" (cadr vals))))
    (let* ((f1 (make-butterworth-highpass 12 .25))
	   (vals (sweep->bins f1 10)))
      (if (fneq (car vals) .5) (snd-display ";butterworth hp 12 max: ~A" (car vals)))
      (if (not (vequal (cadr vals) (vct 0.000 0.000 0.000 0.011 0.348 0.500 0.500 0.500 0.500 0.500)))
	  (snd-display ";butterworth hp 12 .25 spect: ~A" (cadr vals))))
    (let* ((f1 (make-butterworth-highpass 10 .4))
	   (vals (sweep->bins f1 10)))
      (if (fneq (car vals) .5) (snd-display ";butterworth hp 10 max: ~A" (car vals)))
      (if (not (vequal (cadr vals) (vct 0.000 0.000 0.000 0.000 0.000 0.000 0.005 0.343 0.501 0.501)))
	  (snd-display ";butterworth hp 10 .4 spect: ~A" (cadr vals))))

    (let* ((f1 (make-butterworth-bandpass 4 .1 .2))
	   (vals (sweep->bins f1 10)))
      (if (> (abs (- (car vals) .5)) .05) (snd-display ";butterworth bp 4 max: ~A" (car vals)))
      (if (not (vequal (cadr vals) (vct 0.028 0.350 0.481 0.479 0.346 0.132 0.038 0.009 0.002 0.000)))
	  (snd-display ";butterworth bp 4 .1 .2 spect: ~A" (cadr vals))))
    (let* ((f1 (make-butterworth-bandpass 12 .1 .2))
	   (vals (sweep->bins f1 10)))
      (if (> (abs (- (car vals) .5)) .05) (snd-display ";butterworth bp 12 max: ~A" (car vals)))
      (if (not (vequal (cadr vals) (vct 0.006 0.317 0.501 0.500 0.358 0.009 0.000 0.000 0.000 0.000)))
	  (snd-display ";butterworth bp 12 .1 .2 spect: ~A" (cadr vals))))
    (let* ((f1 (make-butterworth-bandpass 8 .3 .4))
	   (vals (sweep->bins f1 10)))
      (if (> (abs (- (car vals) .5)) .05) (snd-display ";butterworth bp 8 max: ~A" (car vals)))
      (if (not (vequal (cadr vals) (vct 0.000 0.000 0.000 0.003 0.034 0.344 0.499 0.499 0.353 0.002)))
	  (snd-display ";butterworth bp 8 .3 .4 spect: ~A" (cadr vals))))

    (let* ((f1 (make-butterworth-bandstop 4 .1 .2))
	   (vals (sweep->bins f1 10)))
      (if (> (abs (- (car vals) .5)) .05) (snd-display ";butterworth bs 4 max: ~A" (car vals)))
      (if (not (vequal (cadr vals) (vct 0.500 0.500 0.347 0.339 0.481 0.499 0.500 0.500 0.500 0.500)))
	  (snd-display ";butterworth bs 4 .1 .2 spect: ~A" (cadr vals))))
    (let* ((f1 (make-butterworth-bandstop 12 .1 .2))
	   (vals (sweep->bins f1 10)))
      (if (> (abs (- (car vals) .5)) .05) (snd-display ";butterworth bs 12 max: ~A" (car vals)))
      (if (not (vequal (cadr vals) (vct 0.503 0.503 0.364 0.334 0.500 0.500 0.500 0.500 0.500 0.500)))
	  (snd-display ";butterworth bs 12 .1 .2 spect: ~A" (cadr vals))))
    (let* ((f1 (make-butterworth-bandstop 8 .3 .4))
	   (vals (sweep->bins f1 10)))
      (if (> (abs (- (car vals) .5)) .05) (snd-display ";butterworth bs 8 max: ~A" (car vals)))
      (if (not (vequal (cadr vals) (vct 0.500 0.500 0.500 0.500 0.500 0.498 0.354 0.332 0.500 0.500)))
	  (snd-display ";butterworth bs 8 .3 .4 spect: ~A" (cadr vals))))

    ;; ---------------- Chebyshev ----------------

    ;; ripple .01 .1 1 for 2..10 even

      ))

