;;; a DSP-related grabbag

(use-modules (ice-9 optargs))
(use-modules (ice-9 format))


;;; -------- Dolph-Chebyshev window
;;; 
;;; formula taken from Richard Lyons, "Understanding DSP"
;;; see clm.c for C version (using either GSL's or GCC's complex trig functions)

(define (dolph N gamma)
  "(dolph n gamma) produces a Dolph-Chebyshev FFT data window of 'n' points using 'gamma' as the window parameter."
  (let* ((alpha (cosh (/ (acosh (expt 10.0 gamma)) N)))
	 (den (/ 1.0 (cosh (* N (acosh alpha)))))
	 (freq (/ pi N))
	 (rl (make-vct N))
	 (im (make-vct N)))
    (do ((i 0 (1+ i))
	 (phase 0.0 (+ phase freq)))
	((= i N))
      (let ((val (* den (cos (* N (acos (* alpha (cos phase))))))))
	(vct-set! rl i (real-part val))
	(vct-set! im i (imag-part val)))) ;this is actually always essentially 0.0
    (fft rl im -1)            ;direction could also be 1
    (vct-set! rl (/ N 2) 0.0) ;hmm... why is this needed?
    (let ((pk (vct-peak rl)))
      (vct-scale! rl (/ 1.0 pk)))
    (do ((i 0 (1+ i))
	 (j (/ N 2)))
	((= i N))
      (vct-set! im i (vct-ref rl j))
      (set! j (+ j 1))
      (if (= j N) (set! j 0)))
    im))


;;; -------- slow Hartley transform 

(define (dht data) 
  "(dht data) returns the Hartley transform of 'data'."
  ;; taken from Perry Cook's SignalProcessor.m (the slow version of the Hartley transform)
  ;; the function fht (see snd-sig.c) is the fast form of this transform
  (let* ((len (vct-length data)) 
	 (arr (make-vct len))
	 (w (/ (* 2.0 pi) len)))
    (do ((i 0 (1+ i)))
	((= i len))
      (do ((j 0 (1+ j)))
	  ((= j len))
	(vct-set! arr i (+ (vct-ref arr i) 
			   (* (vct-ref data j) 
			      (+ (cos (* i j w)) 
				 (sin (* i j w))))))))
    arr))

(define (find-sine freq beg dur)
  "(find-sine freq beg dur) returns the amplitude and initial-phase (for sin) at freq between beg and dur"
  (let ((incr (hz->radians freq))
	(sw 0.0)
	(cw 0.0)
	(reader (make-sample-reader beg)))
    (do ((i 0 (1+ i)))
	((= i dur))
      (let ((samp (next-sample reader)))
	(set! sw (+ sw (* samp (sin (* i incr)))))
	(set! cw (+ cw (* samp (cos (* i incr)))))))
    (list (* 2 (/ (sqrt (+ (* sw sw) (* cw cw))) dur))
	  (atan cw sw))))



;;; -------- Butterworth filters (see also further below -- make-butter-lp et al)
;;;
;; translated from CLM butterworth.cl:
;;
;;   Sam Heisz, January 1998
;;   inspired by some unit generators written for Csound by Paris Smaragdis
;;   who based his work on formulas from 
;;   Charles Dodge, Computer music: synthesis, composition, and performance.

(define (butter b sig) 
  "(butter b sig) is the generator side for the various make-butter procedure"
  (filter b sig))

(define (make-butter-high-pass fq)
  "(make-butter-high-pass freq) makes a Butterworth filter with high pass cutoff at 'freq'"
  ;; this is the same as iir-low-pass-2 below with 'din' set to (sqrt 2.0) -- similarly with the others
  (let* ((r (tan (/ (* pi fq) (srate))))
	 (r2 (* r r))
	 (c1 (/ 1.0 (+ 1.0 (* r (sqrt 2.0)) r2)))
	 (c2  (* -2.0 c1))
	 (c3 c1)
	 (c4 (* 2.0 (- r2 1.0) c1))
	 (c5 (* (+ (- 1.0 (* r (sqrt 2.0))) r2) c1)))
    (make-filter 3
		 (list->vct (list c1 c2 c3))
		 (list->vct (list 0.0 c4 c5)))))

(define (make-butter-low-pass fq)
  "(make-butter-low-pass freq) makes a Butterworth filter with low pass cutoff at 'freq'.  The result \
can be used directly: (filter-sound (make-butter-low-pass 500.0)), or via the 'butter' generator"
  (let* ((r (/ 1.0 (tan (/ (* pi fq) (srate)))))
	 (r2 (* r r))
	 (c1 (/ 1.0 (+ 1.0 (* r (sqrt 2.0)) r2)))
	 (c2 (* 2.0 c1))
	 (c3 c1)
	 (c4 (* 2.0 (- 1.0 r2) c1))
	 (c5  (* (+ (- 1.0 (* r (sqrt 2.0))) r2) c1)))
    (make-filter 3
		 (list->vct (list c1 c2 c3))
		 (list->vct (list 0.0 c4 c5)))))

(define (make-butter-band-pass fq bw)
  "(make-butter-band-pass freq band) makes a bandpass Butterworth filter with low edge at 'freq' and width 'band'"
  (let* ((d (* 2.0 (cos (/ (* 2.0 pi fq) (srate)))))
	 (c (/ 1.0 (tan (/ (* pi bw) (srate)))))
	 (c1 (/ 1.0 (+ 1.0 c)))
	 (c2 0.0)
	 (c3 (- c1))
	 (c4 (* (- c) d c1))
	 (c5 (* (- c 1.0) c1)))
    (make-filter 3
		 (list->vct (list c1 c2 c3))
		 (list->vct (list 0.0 c4 c5)))))

(define (make-butter-band-reject fq bw)
  "(make-butter-band-reject freq band) makes a band-reject Butterworth filter with low edge at 'freq' and width 'band'"
  (let* ((d  (* 2.0 (cos (/ (* 2.0 pi fq) (srate)))))
	 (c (tan (/ (* pi bw) (srate))))
	 (c1 (/ 1.0 (+ 1.0 c)))
	 (c2 (* (- d) c1))
	 (c3 c1)
	 (c4 c2)
	 (c5 (* (- 1.0 c) c1)))
    (make-filter 3
		 (list->vct (list c1 c2 c3))
		 (list->vct (list 0.0 c4 c5)))))

;;; simplest use is (filter-sound (make-butter-low-pass 500.0))
;;; see also effects.scm


;;; Snd's (very simple) spectrum->coefficients procedure is:

(define (spectrum->coeffs order spectr)
  "(spectrum->coeffs order spectr) returns FIR filter coefficients given the filter order and desired spectral envelope"
  (let* ((coeffs (make-vct order))
	 (n order)
	 (m (inexact->exact (floor (/ (+ n 1) 2))))
	 (am (* 0.5 (+ n 1)))
	 (q (/ (* 3.14159 2.0) n)))
    (do ((j 0 (1+ j))
	 (jj (- n 1) (1- jj)))
	((= j m) coeffs)
      (let ((xt (* 0.5 (vct-ref spectr 0))))
	(do ((i 1 (1+ i)))
	    ((= i m))
	  (set! xt (+ xt (* (vct-ref spectr i) (cos (* q i (- am j 1)))))))
	(let ((coeff (* 2.0 (/ xt n))))
	  (vct-set! coeffs j coeff)
	  (vct-set! coeffs jj coeff))))))

(define (fltit-1 order spectr)
  "(fltit-1 order spectrum) creates an FIR filter from spectrum and order and returns a closure that calls it: \
(map-chan (fltit-1 10 (list->vct '(0 1.0 0 0 0 0 0 0 1.0 0))))"
  (let* ((flt (make-fir-filter order (spectrum->coeffs order spectr))))
    (lambda (x)
      (fir-filter flt x))))

;(map-chan (fltit-1 10 (list->vct '(0 1.0 0 0 0 0 0 0 1.0 0))))
;
;(let ((notched-spectr (make-vct 40)))
;  (vct-set! notched-spectr 2 1.0)  
;  (vct-set! notched-spectr 37 1.0)
;  (map-chan (fltit-1 40 notched-spectr)))
;

;;; -------- move sound down 8ve using fft

(define (down-oct)
  "(down-oct) tries to move a sound down an octave"
  (let* ((len (frames))
	 (pow2 (ceiling (/ (log len) (log 2))))
	 (fftlen (inexact->exact (expt 2 pow2)))
	 (fftscale (/ 1.0 fftlen))
	 (rl1 (samples->vct 0 fftlen))
	 (im1 (make-vct fftlen)))
    (fft rl1 im1 1)
    (vct-scale! rl1 fftscale)
    (vct-scale! im1 fftscale)
    (let ((rl2 (make-vct (* 2 fftlen)))
	  (im2 (make-vct (* 2 fftlen))))
      (do ((i 0 (+ i 1))
	   (k (/ fftlen 2) (+ k 1))
	   (j (+ fftlen (/ fftlen 2)) (+ j 1)))
	  ((= i (/ fftlen 2)))
	(vct-set! rl2 i (vct-ref rl1 i))
	(vct-set! rl2 j (vct-ref rl1 k))
	(vct-set! im2 i (vct-ref im1 i))
	(vct-set! im2 j (vct-ref im1 k)))
      (fft rl2 im2 -1)
      (vct->samples 0 (* 2 fftlen) rl2))))


;;; -------- compute-uniform-circular-string
;;;
;;; this is a simplification of the underlying table-filling routine for "scanned synthesis".
;;; To watch the wave, open some sound (so Snd has some place to put the graph), turn off
;;; the time domain display (to give our graph all the window -- to do this in a much more
;;; elegant manner, see snd-motif.scm under scanned-synthesis).


(define compute-uniform-circular-string
  (lambda (size x0 x1 x2 mass xspring damp)
    (define circle-vct-ref 
      (lambda (v i)
	(if (< i 0)
	    (vct-ref v (+ size i))
	    (if (>= i size)
		(vct-ref v (- i size))
		(vct-ref v i)))))
    (let* ((dm (/ damp mass))
	   (km (/ xspring mass))
	   (denom (+ 1.0 dm))

	   (p1 (/ (+ 2.0 (- dm (* 2.0 km))) denom))
	   (p2 (/ km denom))
	   (p3 (/ -1.0 denom)))
      (do ((i 0 (1+ i)))
	  ((= i size))
	(vct-set! x0 i (+ (* p1 (vct-ref x1 i))
			  (* p2 (+ (circle-vct-ref x1 (- i 1)) (circle-vct-ref x1 (+ i 1))))
			  (* p3 (vct-ref x2 i)))))
      (vct-fill! x2 0.0)
      (vct-add! x2 x1)
      (vct-fill! x1 0.0)
      (vct-add! x1 x0))))

(define testunif
  (lambda (mass xspring damp)
    (let* ((size 128)
	   (x0 (make-vct size))	   
	   (x1 (make-vct size))	   
	   (x2 (make-vct size)))
      (do ((i 0 (1+ i)))
	  ((= i 12))
	(let ((val (sin (/ (* 2 pi i) 12.0))))
	  (vct-set! x1 (+ i (- (/ size 4) 6)) val)))
      (do ((i 0 (1+ i)))
	  ((or (c-g?) (= i 1024)))
	(compute-uniform-circular-string size x0 x1 x2 mass xspring damp)
	(graph x0 "string" 0 1.0 -10.0 10.0)))))

(define test-scanned-synthesis
  ;; check out scanned-synthesis
  (lambda (amp dur mass xspring damp)
    (let* ((size 256)
	   (x0 (make-vct size))	   
	   (x1 (make-vct size))	   
	   (x2 (make-vct size)))
      (do ((i 0 (1+ i)))
	  ((= i 12))
	(let ((val (sin (/ (* 2 pi i) 12.0))))
	  (vct-set! x1 (+ i (- (/ size 4) 6)) val)))
      (let ((gen1 (make-table-lookup 440.0 :wave x1))
	    (gen2 (make-table-lookup 440.0 :wave x2))
	    (recompute-samps 30) ;just a quick guess
	    (data (make-vct dur)))
	(do ((i 0 (1+ i))
	     (k 0.0)
	     (kincr (/ 1.0 recompute-samps)))
	    ((or (c-g?) (= i dur)))
	  (if (>= k 1.0)
	      (begin
		(set! k 0.0)
		(compute-uniform-circular-string size x0 x1 x2 mass xspring damp))
	      (set! k (+ k kincr)))
	  (let ((g1 (table-lookup gen1))
		(g2 (table-lookup gen2)))
	    (vct-set! data i (+ g2 (* k (- g1 g2))))))
	(let ((curamp (vct-peak data)))
	  (vct-scale! data (/ amp curamp)))
	(vct->samples 0 dur data)))))

(define compute-string
  ;; this is the more general form
  (lambda (size x0 x1 x2 masses xsprings esprings damps haptics)
    (define circle-vct-ref 
      (lambda (v i)
	(if (< i 0)
	    (vct-ref v (+ size i))
	    (if (>= i size)
		(vct-ref v (- i size))
		(vct-ref v i)))))
    (do ((i 0 (1+ i)))
	((= i size))
      (let* ((dm (/ (vct-ref damps i) (vct-ref masses i)))
	     (km (/ (vct-ref xsprings i) (vct-ref masses i)))
	     (cm (/ (vct-ref esprings i) (vct-ref masses i)))
	     (denom (+ 1.0 dm cm))
	     (p1 (/ (+ 2.0 (- dm (* 2.0 km))) denom))
	     (p2 (/ km denom))
	     (p3 (/ -1.0 denom))
	     (p4 (/ (vct-ref haptics i) (* (vct-ref masses i) denom))))
	(vct-set! x0 i (+ (* p1 (vct-ref x1 i))
			  (* p2 (+ (circle-vct-ref x1 (- i 1)) (circle-vct-ref x1 (+ i 1))))
			  (* p3 (vct-ref x2 i))
			  p4))))
    (do ((i 0 (1+ i)))
	((= i size))
      (vct-set! x2 i (vct-ref x1 i))
      (vct-set! x1 i (vct-ref x0 i)))))


;;; -------- "frequency division" -- an effect from sed_sed@my-dejanews.com

(define (freqdiv n)
  "(freqdiv n) repeats each nth sample n times (clobbering the intermediate samples): (freqdiv 8)"
  (let ((div 0)
	(curval 0.0))
    (map-chan (lambda (val)
		(if (= div 0)
		    (set! curval val))
		(set! div (1+ div))
		(if (= div n) (set! div 0))
		curval))))



;;; -------- "adaptive saturation" -- an effect from sed_sed@my-dejanews.com
;;;
;;; a more extreme effect is "saturation":
;;;   (map-chan (lambda (val) (if (< (abs val) .1) val (if (>= val 0.0) 0.25 -0.25))))

(define (adsat size)
  "(adsat size) is an 'adaptive saturation' sound effect"
  (let ((mn 0.0)
	(mx 0.0)
	(n 0)
	(vals (make-vct size)))
    (map-chan (lambda (val)
		(if (= n size)
		    (begin
		      (do ((i 0 (1+ i)))
			  ((= i size))
			(if (>= (vct-ref vals i) 0.0)
			    (vct-set! vals i mx)
			    (vct-set! vals i mn)))
		      (set! n 0)
		      (set! mx 0.0)
		      (set! mn 0.0)
		      vals)
		    (begin
		      (vct-set! vals n val)
		      (if (> val mx) (set! mx val))
		      (if (< val mn) (set! mn val))
		      (set! n (1+ n))
		      #f))))))


;;; -------- spike
;;;
;;; makes sound more spikey -- sometimes a nice effect

(define (spike)
  "(spike) multiplies successive samples together to make a sound more spikey"
  (map-chan (let ((x1 0.0) 
		  (x2 0.0) 
		  (amp (maxamp))) ; keep resultant peak at maxamp
	      (lambda (x0) 
		(let ((res (* (/ x0 (* amp amp)) 
			      (abs x2) 
			      (abs x1)))) 
		  (set! x2 x1) 
		  (set! x1 x0) 
		  res)))))

;;; the more successive samples we include in the product, the more we
;;;   limit the output to pulses placed at (just after) wave peaks


;;; -------- easily-fooled autocorrelation-based pitch tracker 

(define spot-freq
  (lambda args
    "(spot-freq samp #:optional snd chn) tries to determine the current pitch: (spot-freq (left-sample))"
    (let* ((s0 (car args))
	   (snd (if (> (length args) 1) (list-ref args 1) #f))
	   (chn (if (> (length args) 2) (list-ref args 2) #f))
	   (pow2 (ceiling (/ (log (/ (srate snd) 20.0)) (log 2))))
	   (fftlen (inexact->exact (expt 2 pow2)))
	   (data (autocorrelate (samples->vct s0 fftlen snd chn)))
	   (cor-peak (vct-peak data)))
      (call-with-current-continuation
       (lambda (return)
	 (do ((i 1 (1+ i)))
	     ((= i (- fftlen 2)) 0)
	   (if (and (< (vct-ref data i) (vct-ref data (+ i 1)))
		    (> (vct-ref data (+ i 1)) (vct-ref data (+ i 2))))
	       (begin
		 (let* ((logla (log10 (/ (+ cor-peak (vct-ref data i)) (* 2 cor-peak))))
			(logca (log10 (/ (+ cor-peak (vct-ref data (+ i 1))) (* 2 cor-peak))))
			(logra (log10 (/ (+ cor-peak (vct-ref data (+ i 2))) (* 2 cor-peak))))
			(offset (/ (* 0.5 (- logla logra))
				   (+ logla logra (* -2.0 logca)))))
		   (return (/ (srate snd)
			      (* 2 (+ i 1 offset)))))))))))))

;(add-hook! graph-hook 
;	   (lambda (snd chn y0 y1) 
;	     (report-in-minibuffer (format #f "~A" (spot-freq (left-sample))))))



;;; -------- chorus (doesn't always work and needs speedup)
(define chorus-size 5)
(define chorus-time .05)
(define chorus-amount 20.0)
(define chorus-speed 10.0)

(define (chorus)
  "(chorus) tries to produce the chorus sound effect"
  (define (make-flanger)
    (let* ((ri (make-rand-interp :frequency chorus-speed :amplitude chorus-amount))
	   (len (inexact->exact (floor (random (* 3.0 chorus-time (srate))))))
	   (gen (make-delay len :max-size (+ len chorus-amount 1))))
      (list gen ri)))
  (define (flanger dly inval)
    (+ inval 
       (delay (car dly)
	      inval
	      (rand-interp (cadr dly)))))
  (let ((dlys (make-vector chorus-size)))
    (do ((i 0 (1+ i)))
	((= i chorus-size))
      (vector-set! dlys i (make-flanger)))
    (lambda (inval)
      (do ((sum 0.0)
	   (i 0 (1+ i)))
	  ((= i chorus-size)
	   (* .25 sum))
	(set! sum (+ sum (flanger (vector-ref dlys i) inval)))))))


;;; -------- chordalize (comb filters to make a chord using chordalize-amount and chordalize-base)
(define chordalize-amount .95)
(define chordalize-base 100)
(define chordalize-chord '(1 3/4 5/4))

(define (chordalize)
  "(chordalize) uses harmonically-related comb-filters to bring out a chord in a sound"
  ;; chord is a list of members of chord such as '(1 5/4 3/2)
  (let ((combs (map (lambda (interval)
		      (make-comb chordalize-amount (* chordalize-base interval)))
		    chordalize-chord))
	(scaler (/ 0.5 (length chordalize-chord)))) ; just a guess -- maybe this should rescale to old maxamp
    (lambda (x)
      (* scaler (apply + (map (lambda (c) (comb c x)) combs))))))


;;; -------- zero-phase, rotate-phase
;;; fft games (from the "phazor" package of Scott McNab)

(define (zero-phase)
  "(zero-phase) calls fft, sets all phases to 0, and un-ffts"
  (let* ((len (frames))
	 (pow2 (ceiling (/ (log len) (log 2))))
	 (fftlen (inexact->exact (expt 2 pow2)))
	 (fftscale (/ 1.0 fftlen))
	 (rl (samples->vct 0 fftlen))
	 (old-pk (vct-peak rl))
	 (im (make-vct fftlen)))
    (fft rl im 1)
    (rectangular->polar rl im)
    (vct-scale! rl fftscale)
    (vct-scale! im 0.0)
    (fft rl im -1)
    (let ((pk (vct-peak rl)))
      (vct->samples 0 len (vct-scale! rl (/ old-pk pk))))))

(define (rotate-phase func)
  "(rotate-phase func) calls fft, applies func to each phase, then un-ffts"
  (let* ((len (frames))
	 (pow2 (ceiling (/ (log len) (log 2))))
	 (fftlen (inexact->exact (expt 2 pow2)))
	 (fftlen2 (inexact->exact (floor (/ fftlen 2))))
	 (fftscale (/ 1.0 fftlen))
	 (rl (samples->vct 0 fftlen))
	 (old-pk (vct-peak rl))
	 (im (make-vct fftlen)))
    (fft rl im 1)
    (rectangular->polar rl im)
    (vct-scale! rl fftscale)
    (vct-set! im 0 0.0)
    (do ((i 1 (1+ i))
	 (j (1- fftlen) (1- j)))
	((= i fftlen2))
      ;; rotate the fft vector by func, keeping imaginary part complex conjgate of real
      (vct-set! im i (func (vct-ref im i)))
      (vct-set! im j (- (vct-ref im i))))
    (polar->rectangular rl im)
    (fft rl im -1)
    (let ((pk (vct-peak rl)))
      (vct->samples 0 len (vct-scale! rl (/ old-pk pk))))))

;(rotate-phase (lambda (x) 0.0)) is the same as (zero-phase)
;(rotate-phase (lambda (x) (random 3.1415))) randomizes phases
;(rotate-phase (lambda (x) x)) returns original
;(rotate-phase (lambda (x) (- x))) reverses original (might want to write fftlen samps here)


;;; -------- asymmetric FM with the I0 form

(define* (make-asyfm #:key
		     (frequency 440.0) (initial-phase 0.0)
		     (ratio 1.0) (r 1.0)
		     (index 1.0))
  (list (hz->radians frequency) initial-phase ratio r index))
		     
(define asyfm-freq
  (make-procedure-with-setter
   (lambda (gen) (radians->hz (list-ref gen 0)))
   (lambda (gen val) (list-set! gen 0 (hz->radians val)))))

(define asyfm-phase
  (make-procedure-with-setter
   (lambda (gen) (list-ref gen 1))
   (lambda (gen val) (list-set! gen 1 val))))

(define asyfm-ratio
  (make-procedure-with-setter
   (lambda (gen) (list-ref gen 2))
   (lambda (gen val) (list-set! gen 2 val))))

(define asyfm-r
  (make-procedure-with-setter
   (lambda (gen) (list-ref gen 3))
   (lambda (gen val) (list-set! gen 3 val))))

(define asyfm-index
  (make-procedure-with-setter
   (lambda (gen) (list-ref gen 4))
   (lambda (gen val) (list-set! gen 4 val))))

(define (asyfm-J gen input)
  (let* ((freq (list-ref gen 0))
	 (phase (asyfm-phase gen))
	 (ratio (asyfm-ratio gen))
	 (r (asyfm-r gen))
	 (r1 (/ 1.0 r))
	 (index (asyfm-index gen))
	 (modphase (* ratio phase))
	 (result (* (exp (* 0.5 index (- r r1) (cos modphase)))
		    (sin (+ phase (* 0.5 index (+ r r1) (sin modphase)))))))
    (set! (asyfm-phase gen) (+ phase input freq))
    result))

;;; (let ((gen (make-asyfm :frequency 2000 :ratio .1))) (map-channel (lambda (n) (asyfm-J gen 0.0))))

(define (I0 x)
  (if (< (abs x) 3.75)
      (let* ((y (expt (/ x 3.75) 2)))
	(+ 1.0
	   (* y (+ 3.5156229
		   (* y (+ 3.0899424
			   (* y (+ 1.2067492
				   (* y (+ 0.2659732
					   (* y (+ 0.360768e-1
						   (* y 0.45813e-2)))))))))))))
    (let* ((ax (abs x))
	   (y (/ 3.75 ax)))
      (* (/ (exp ax) (sqrt ax)) 
	 (+ 0.39894228
	    (* y (+ 0.1328592e-1
		    (* y (+ 0.225319e-2
			    (* y (+ -0.157565e-2
				    (* y (+ 0.916281e-2
					    (* y (+ -0.2057706e-1
						    (* y (+ 0.2635537e-1
							    (* y (+ -0.1647633e-1
								    (* y 0.392377e-2))))))))))))))))))))

(define (asyfm-I gen input)
  (let* ((freq (list-ref gen 0))
	 (phase (asyfm-phase gen))
	 (ratio (asyfm-ratio gen))
	 (r (asyfm-r gen))
	 (r1 (/ 1.0 r))
	 (index (asyfm-index gen))
	 (modphase (* ratio phase))
	 (result (* (exp (- (* 0.5 index (+ r r1) (cos modphase))
			    (* 0.5 (log (I0 (* index (+ r r1)))))))
		    (sin (+ phase (* 0.5 index (- r r1) (sin modphase)))))))
    (set! (asyfm-phase gen) (+ phase input freq))
    result))


;;; -------- cosine-summation (a simpler version of sine-summation)
;;;
;;; from Andrews, Askey, Roy "Special Functions" 5.1.16

(define (cosine-summation gen r)
  ;; this could obviously be radically optimized
  (* (- (/ (- 1.0 (* r r))
	   (- (+ 1.0 (* r r))
	      (* 2 r (oscil gen))))
	1.0)
     (/ (- 1.0 (* r r)) ; amplitude normalization (not vital)
	(* 2 r (+ 1.0 (* r r))))))

(define make-cosine-summation make-oscil)

;;; (let ((gen (make-cosine-summation 100.0))) (map-channel (lambda (y) (* .2 (cosine-summation gen 0.5)))))


;;; -------- kosine-summation
;;;
;;; from Askey "Ramanujan and Hypergeometric Series" in Berndt and Rankin "Ramanujan: Essays and Surveys"
;;;
;;; this gives a sum of cosines of decreasing amp where the "k" parameter determines
;;;   the "index" (in FM nomenclature) -- higher k = more cosines; the actual amount
;;;   of the nth cos involves hypergeometric series (looks like r^n/n! with a million other terms).

(define (kosine-summation gen r k)
  (* (expt (- (+ 1.0 (* r r))
	      (* 2 r (oscil gen)))
	   (- k))
     (expt (- (+ 1.0 (* r r)) (* 2 r)) k))) ; amplitude normalization

(define make-kosine-summation make-oscil)

;;; (let ((gen (make-kosine-summation 100.0))) (map-channel (lambda (y) (* .2 (kosine-summation gen 0.5 5.0)))))
;;;
;;; there is still noticable DC offset if r != 0.5 -- could precompute it and subtract


;;; -------- legendre-summation (sum-of-cosines with descreasing amps)
;;;
;;; from Andrews, Askey, Roy "Special Functions" p 314

(define (legendre-summation gen)
  (let ((val (sum-of-cosines gen)))
    (* val val)))

(define (make-legendre-summation cosines frequency)
  (make-sum-of-cosines (inexact->exact (floor (/ cosines 2))) frequency))

;(let ((gen (make-legendre-summation 10 100))) (map-chan (lambda (y) (legendre-summation gen))))


;;; -------- variations on sum-of-cosines
;;; from "Trigonometric Delights" by Eli Maor

(define (sum-of-n-sines angle n)
  (let* ((a2 (* angle 0.5))
	 (den (sin a2)))
    (if (= den 0.0)
	0.0 ; I'm guessing...
	(/ (* (sin (* n a2)) (sin (* (1+ n) a2))) den))))
;(let ((angle 0.0)) (map-channel (lambda (y) (let ((val (sum-of-n-sines angle 3))) (set! angle (+ angle .1)) (* .1 val)))))

(define (sum-of-n-odd-sines angle n)
  (let ((den (sin angle))
	(na (sin (* n angle))))
    (if (= den 0.0)
	0.0
	(/ (* na na) den))))

(define (sum-of-n-odd-cosines angle n)
  (let ((den (* 2 (sin angle))))
    (if (= den 0.0)
	(exact->inexact n) ; just guessing -- floatification is for the run macro
	(/ (sin (* 2 n angle)) den))))



;;; -------- brighten-slightly

(define (brighten-slightly amount)
  ;; a slightly simplified form of contrast-enhancement ('amount' between ca .1 and 1)
  (let* ((mx (maxamp))
	 (brt (/ (* 2 pi amount) mx)))
    (map-channel (lambda (y)
		   (* mx (sin (* y brt)))))))


;;; -------- FIR filters

;;; -------- Hilbert transform

(define* (make-hilbert-transform #:optional (len 30))
  (let* ((arrlen (1+ (* 2 len)))
	 (arr (make-vct arrlen)))
    (do ((i (- len) (1+ i)))
	((= i len))
      (let* ((k (+ i len))
	     (denom (* pi i))
	     (num (- 1.0 (cos (* pi i)))))
	(if (= i 0)
	    (vct-set! arr k 0.0)
	    ;; this is the "ideal" -- rectangular window -- version:
	    ;; (vct-set! arr k (/ num denom))
            ;; this is the Hamming window version:
	    (vct-set! arr k (* (/ num denom) 
			       (+ .54 (* .46 (cos (/ (* i pi) len)))))) ; window
	    )))
    (make-fir-filter arrlen arr)))

(define (hilbert-transform f in)
  (fir-filter f in))

#!
  (let ((h (make-hilbert-transform 15)))
    (map-channel (lambda (y)
		   (hilbert-transform h y))))
!#

;;; -------- highpass filter 

(define* (make-highpass fc #:optional (len 30))
  (let* ((arrlen (1+ (* 2 len)))
	 (arr (make-vct arrlen)))
    (do ((i (- len) (1+ i)))
	((= i len))
      (let* ((k (+ i len))
	     (denom (* pi i))
	     (num (- (sin (* fc i)))))
	(if (= i 0)
	    (vct-set! arr k (- 1.0 (/ fc pi)))
	    (vct-set! arr k (* (/ num denom) 
			       (+ .54 (* .46 (cos (/ (* i pi) len)))))))))
    (make-fir-filter arrlen arr)))

(define (highpass f in)
  (fir-filter f in))

#!
  (let ((hp (make-highpass (* .1 pi))))
    (map-channel (lambda (y)
		   (highpass hp y))))
!#


;;; -------- lowpass filter

(define* (make-lowpass fc #:optional (len 30))
  (let* ((arrlen (1+ (* 2 len)))
	 (arr (make-vct arrlen)))
    (do ((i (- len) (1+ i)))
	((= i len))
      (let* ((k (+ i len))
	     (denom (* pi i))
	     (num (sin (* fc i))))
	(if (= i 0)
	    (vct-set! arr k (/ fc pi))
	    (vct-set! arr k (* (/ num denom) 
			       (+ .54 (* .46 (cos (/ (* i pi) len)))))))))
    (make-fir-filter arrlen arr)))

(define (lowpass f in)
  (fir-filter f in))

#!
  (let ((hp (make-lowpass (* .2 pi))))
    (map-channel (lambda (y)
		   (lowpass hp y))))
!#

;;; -------- bandpass filter

(define* (make-bandpass flo fhi #:optional (len 30))
  (let* ((arrlen (1+ (* 2 len)))
	 (arr (make-vct arrlen)))
    (do ((i (- len) (1+ i)))
	((= i len))
      (let* ((k (+ i len))
	     (denom (* pi i))
	     (num (- (sin (* fhi i)) (sin (* flo i)))))
	(if (= i 0)
	    (vct-set! arr k (/ (- fhi flo) pi))
	    (vct-set! arr k (* (/ num denom) 
			       (+ .54 (* .46 (cos (/ (* i pi) len)))))))))
    (make-fir-filter arrlen arr)))

(define (bandpass f in)
  (fir-filter f in))

#!
  (let ((hp (make-bandpass (* .1 pi) (* .2 pi))))
    (map-channel (lambda (y)
		   (bandpass hp y))))

;; for more bands, you can add the coeffs:

(define* (make-bandpass-2 flo1 fhi1 flo2 fhi2 #:optional (len 30))
  (let* ((f1 (make-bandpass flo1 fhi1 len))
	 (f2 (make-bandpass flo2 fhi2 len)))
    (vct-add! (mus-xcoeffs f1) (mus-xcoeffs f2))
    f1))

(let ((ind (new-sound "test.snd")))
  (map-channel (lambda (y) (- 1.0 (random 2.0))) 0 10000)
  (let ((f2 (make-bandpass-2 (* .12 pi) (* .15 pi) (* .22 pi) (* .25 pi) 100)))
    (map-channel (lambda (y) (fir-filter f2 y)))
    ))

!#

;;; -------- bandstop filter

(define* (make-bandstop flo fhi #:optional (len 30))
  (let* ((arrlen (1+ (* 2 len)))
	 (arr (make-vct arrlen)))
    (do ((i (- len) (1+ i)))
	((= i len))
      (let* ((k (+ i len))
	     (denom (* pi i))
	     (num (- (sin (* flo i)) (sin (* fhi i)))))
	(if (= i 0)
	    (vct-set! arr k (- 1.0 (/ (- fhi flo) pi)))
	    (vct-set! arr k (* (/ num denom) 
			       (+ .54 (* .46 (cos (/ (* i pi) len)))))))))
    (make-fir-filter arrlen arr)))

(define (bandstop f in)
  (fir-filter f in))

#!
  (let ((hp (make-bandstop (* .1 pi) (* .3 pi))))
    (map-channel (lambda (y)
		   (bandstop hp y))))
!#

;;; -------- differentiator

(define* (make-differentiator #:optional (len 30))
  (let* ((arrlen (1+ (* 2 len)))
	 (arr (make-vct arrlen)))
    (do ((i (- len) (1+ i)))
	((= i len))
      (let* ((k (+ i len)))
	(if (= i 0)
	    (vct-set! arr k 0.0)
	    (vct-set! arr k (* (- (/ (cos (* pi i)) i) (/ (sin (* pi i)) (* pi i i))) 
			       (+ .54 (* .46 (cos (/ (* i pi) len)))))))))
    (make-fir-filter arrlen arr)))

(define (differentiator f in)
  (fir-filter f in))

#!
  (let ((hp (make-differentiator)))
    (map-channel (lambda (y)
		   (differentiator hp y))))
!#


;;; -------- IIR filters

;;; from "DSP Filter Cookbook" by Lane et al, Prompt Pubs, 2001
;;; 
;;; use with the filter generator
;;;   (define gen (make-iir-high-pass-2 1000))
;;;   (filter gen 1.0)
;;;   etc

(define (make-biquad a0 a1 a2 b1 b2)
  (make-filter 3 
	       (vct a0 a1 a2) 
	       (vct 0.0 b1 b2)))

(define (make-iir-low-pass-1 fc)
  (let* ((theta (/ (* 2 pi fc) (mus-srate)))
	 (gamma (/ (cos theta) (+ 1.0 (sin theta))))
	 (xc (/ (- 1.0 gamma) 2.0)))
    (make-filter 2 
		 (vct xc xc) 
		 (vct 0.0 (- gamma)))))

(define (make-iir-high-pass-1 fc)
  (let* ((theta (/ (* 2 pi fc) (mus-srate)))
	 (gamma (/ (cos theta) (+ 1.0 (sin theta))))
	 (xc (/ (+ 1.0 gamma) 2.0)))
    (make-filter 2 
		 (vct xc (- xc)) 
		 (vct 0.0 (- gamma)))))

(define* (make-iir-low-pass-2 fc #:optional din) ; din=(sqrt 2.0) for example (suggested range 0.2.. 10)
  (let* ((theta (/ (* 2 pi fc) (mus-srate)))
	 (d (or din (sqrt 2.0)))
	 (beta (* 0.5 (/ (- 1.0 (* (/ d 2) (sin theta)))
			 (+ 1.0 (* (/ d 2) (sin theta))))))
	 (gamma (* (+ 0.5 beta) (cos theta)))
	 (alpha (* 0.5 (+ 0.5 beta (- gamma)))))
    (make-filter 3 
		 (vct alpha (* 2.0 alpha) alpha)
		 (vct 0.0 (* -2.0 gamma) (* 2.0 beta)))))

(define* (make-iir-high-pass-2 fc #:optional din)
  (let* ((theta (/ (* 2 pi fc) (mus-srate)))
	 (d (or din (sqrt 2.0)))
	 (beta (* 0.5 (/ (- 1.0 (* (/ d 2) (sin theta)))
			 (+ 1.0 (* (/ d 2) (sin theta))))))
	 (gamma (* (+ 0.5 beta) (cos theta)))
	 (alpha (* 0.5 (+ 0.5 beta gamma))))
    (make-filter 3
		 (vct alpha (* -2.0 alpha) alpha)
		 (vct 0.0 (* -2.0 gamma) (* 2.0 beta)))))

(define (make-iir-band-pass-2 f1 f2)
  (let* ((theta (/ (* 2 pi (sqrt (* f1 f2))) (mus-srate)))
	 (Q (/ (sqrt (* f1 f2)) (- f2 f1)))
	 (t2 (tan (/ theta (* 2 Q))))
	 (beta (* 0.5 (/ (- 1.0 t2)
			 (+ 1.0 t2))))
	 (gamma (* (+ 0.5 beta) (cos theta)))
	 (alpha (- 0.5 beta)))
    (make-filter 3
		 (vct alpha 0.0 (- alpha))
		 (vct 0.0 (* -2.0 gamma) (* 2.0 beta)))))

(define (make-iir-band-stop-2 f1 f2)
  (let* ((theta (/ (* 2 pi (sqrt (* f1 f2))) (mus-srate)))
	 (Q (/ (sqrt (* f1 f2)) (- f2 f1)))
	 (t2 (tan (/ theta (* 2 Q))))
	 (beta (* 0.5 (/ (- 1.0 t2)
			 (+ 1.0 t2))))
	 (gamma (* (+ 0.5 beta) (cos theta)))
	 (alpha (+ 0.5 beta)))
    (make-filter 3
		 (vct alpha (* -2.0 gamma) alpha)
		 (vct 0.0 (* -2.0 gamma) (* 2.0 beta)))))

(define* (make-eliminate-hum #:optional (hum-freq 60.0) (hum-harmonics 5) (bandwidth 10))
  (let ((gen (make-vector hum-harmonics)))
    (do ((i 0 (1+ i)))
	((= i hum-harmonics))
      (let ((center (* (+ i 1.0) hum-freq))
	    (b2 (* 0.5 bandwidth)))
	(vector-set! gen i (make-iir-band-stop-2 (- center b2) (+ center b2)))))
    gen))

(define (eliminate-hum gen x0)
  (let ((val x0))
    (do ((i 0 (1+ i)))
	((= i (vector-length gen)))
      (set! val (filter (vector-ref gen i) val))) ; "cascade" n filters
    val))

;;; (let ((hummer (make-eliminate-hum))) (map-channel (lambda (x) (eliminate-hum hummer x))))

(define (make-peaking-2 f1 f2 m)
  ;; bandpass, m is gain at center of peak
  ;; use map-channel with this one (not clm-channel or filter)
  (let* ((theta (/ (* 2 pi (sqrt (* f1 f2))) (mus-srate)))
	 (Q (/ (sqrt (* f1 f2)) (- f2 f1)))
	 (t2 (* (/ 4.0 (+ m 1)) (tan (/ theta (* 2 Q)))))
	 (beta (* 0.5 (/ (- 1.0 t2)
			 (+ 1.0 t2))))
	 (gamma (* (+ 0.5 beta) (cos theta)))
	 (alpha (- 0.5 beta))
	 (flt (make-filter 3
			   (vct alpha 0.0 (- alpha))
			   (vct 0.0 (* -2.0 gamma) (* 2.0 beta)))))
    (lambda (x) (+ x (* (- m 1.0) (filter flt x))))))


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
    a1))

(define (make-butter-lp M fc)
  ;; order is M*2, fc is cutoff freq (Hz)
  (let* ((xcoeffs '())
	 (ycoeffs '())
	 (theta (/ (* 2 pi fc) (mus-srate)))
	 (st (sin theta))
	 (ct (cos theta)))
    (do ((k 1 (1+ k)))
	((> k M))
      (let* ((d (* 2 (sin (/ (* pi (- (* 2 k) 1)) (* 4 M)))))
	     (beta (* 0.5 (/ (- 1.0 (* 0.5 d st))
			     (+ 1.0 (* 0.5 d st)))))
	     (gamma (* ct (+ 0.5 beta)))
	     (alpha (* 0.25 (+ 0.5 beta (- gamma)))))
	(set! xcoeffs (cons (vct (* 2 alpha) (* 4 alpha) (* 2 alpha)) xcoeffs))
	(set! ycoeffs (cons (vct 1.0 (* -2.0 gamma) (* 2.0 beta)) ycoeffs))))
    (make-filter (1+ (* 2 M))
		 (cascade->canonical (reverse xcoeffs))
		 (cascade->canonical (reverse ycoeffs)))))
	 
(define (make-butter-hp M fc)
  ;; order is M*2, fc is cutoff freq (Hz)
  (let* ((xcoeffs '())
	 (ycoeffs '())
	 (theta (/ (* 2 pi fc) (mus-srate)))
	 (st (sin theta))
	 (ct (cos theta)))
    (do ((k 1 (1+ k)))
	((> k M))
      (let* ((d (* 2 (sin (/ (* pi (- (* 2 k) 1)) (* 4 M)))))
	     (beta (* 0.5 (/ (- 1.0 (* 0.5 d st))
			     (+ 1.0 (* 0.5 d st)))))
	     (gamma (* ct (+ 0.5 beta)))
	     (alpha (* 0.25 (+ 0.5 beta gamma))))
	(set! xcoeffs (cons (vct (* 2 alpha) (* -4 alpha) (* 2 alpha)) xcoeffs))
	(set! ycoeffs (cons (vct 1.0 (* -2.0 gamma) (* 2.0 beta)) ycoeffs))))
    (make-filter (1+ (* 2 M))
		 (cascade->canonical (reverse xcoeffs))
		 (cascade->canonical (reverse ycoeffs)))))
	 
(define (make-butter-bp M f1 f2)
  ;; order is M*2, f1 and f2 are band edge freqs (Hz)
  (let* ((xcoeffs '())
	 (ycoeffs '())
	 (f0 (sqrt (* f1 f2)))
	 (Q (/ f0 (- f2 f1)))
	 (theta0 (/ (* 2 pi f0) (mus-srate)))
	 (de (/ (* 2 (tan (/ theta0 (* 2 Q)))) (sin theta0)))
	 (de2 (/ de 2))
	 (tn0 (tan (* theta0 0.5))))
    (do ((i 1 (1+ i))
	 (k 1)
	 (j 1))
	((> i M))
      (let* ((Dk (* 2 (sin (/ (* pi (- (* 2 k) 1)) (* 2 M)))))
	     (Ak (/ (+ 1 (* de2 de2)) (* Dk de2)))
	     (dk1 (sqrt (/ (* de Dk)
			   (+ Ak (sqrt (- (* Ak Ak) 1))))))
	     (Bk (* de2 (/ Dk dk1)))
	     (Wk (real-part (+ Bk (sqrt (- (* Bk Bk) 1.0))))) ; fp inaccuracies causing tiny (presumably bogus) imaginary part here
	     (thetajk (if (= j 1)
			  (* 2 (atan (/ tn0 Wk)))
			  (* 2 (atan (* tn0 Wk)))))
	     (betajk (* 0.5 (/ (- 1.0 (* 0.5 dk1 (sin thetajk)))
			       (+ 1.0 (* 0.5 dk1 (sin thetajk))))))
	     (gammajk (* (+ 0.5 betajk) (cos thetajk)))
	     (wk2 (/ (- Wk (/ 1.0 Wk)) dk1))
	     (alphajk (* 0.5 (- 0.5 betajk) (sqrt (+ 1.0 (* wk2 wk2))))))
	(set! xcoeffs (cons (vct (* 2 alphajk) 0.0 (* -2 alphajk)) xcoeffs))
	(set! ycoeffs (cons (vct 1.0 (* -2.0 gammajk) (* 2.0 betajk)) ycoeffs))
	(if (= j 1)
	    (set! j 2)
	    (begin
	      (set! k (1+ k))
	      (set! j 1)))))
    (make-filter (1+ (* 2 M))
		 (cascade->canonical (reverse xcoeffs))
		 (cascade->canonical (reverse ycoeffs)))))
	 
(define (make-butter-bs M f1 f2)
  ;; order is M*2, f1 and f2 are band edge freqs (Hz)
  (let* ((xcoeffs '())
	 (ycoeffs '())
	 (f0 (sqrt (* f1 f2)))
	 (Q (/ f0 (- f2 f1)))
	 (theta0 (/ (* 2 pi f0) (mus-srate)))
	 (de (/ (* 2 (tan (/ theta0 (* 2 Q)))) (sin theta0)))
	 (de2 (/ de 2))
	 (ct (cos theta0))
	 (tn0 (tan (* theta0 0.5))))
    (do ((i 1 (1+ i))
	 (k 1)
	 (j 1))
	((> i M))
      (let* ((Dk (* 2 (sin (/ (* pi (- (* 2 k) 1)) (* 2 M)))))
	     (Ak (/ (+ 1 (* de2 de2)) (* Dk de2)))
	     (dk1 (sqrt (/ (* de Dk)
			   (+ Ak (sqrt (- (* Ak Ak) 1))))))
	     (Bk (* de2 (/ Dk dk1)))
	     (Wk (real-part (+ Bk (sqrt (- (* Bk Bk) 1.0)))))
	     (thetajk (if (= j 1)
			  (* 2 (atan (/ tn0 Wk)))
			  (* 2 (atan (* tn0 Wk)))))
	     (betajk (* 0.5 (/ (- 1.0 (* 0.5 dk1 (sin thetajk)))
			       (+ 1.0 (* 0.5 dk1 (sin thetajk))))))
	     (gammajk (* (+ 0.5 betajk) (cos thetajk)))
	     (alphajk (* 0.5 (+ 0.5 betajk) (/ (- 1.0 (cos thetajk)) (- 1.0 ct)))))
	(set! xcoeffs (cons (vct (* 2 alphajk) (* -4 ct alphajk) (* 2 alphajk)) xcoeffs))
	(set! ycoeffs (cons (vct 1.0 (* -2.0 gammajk) (* 2.0 betajk)) ycoeffs))
	(if (= j 1)
	    (set! j 2)
	    (begin
	      (set! k (1+ k))
	      (set! j 1)))))
    (make-filter (1+ (* 2 M))
		 (cascade->canonical (reverse xcoeffs))
		 (cascade->canonical (reverse ycoeffs)))))
	 
