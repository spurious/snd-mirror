;;; various dsp procedures (leaving aside the more CLM-specific examples in examp.scm etc)


;;; -------- Dolph-Chebyshev window
;;; 
;;; formula taken from Richard Lyons, "Understanding DSP"
;;; see clm.c for C version (using GSL's complex trig functions)

(define (dolph N gamma)
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
  ;; taken from Perry Cook's SignalProcessor.m (the slow version of the Hartley transform)
  ;; the built-in function fht is the fast form of this transform
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


;;; -------- Butterworth filters
;;;
;; translated from CLM butterworth.cl:
;;
;;   Sam Heisz, January 1998
;;   inspired by some unit generators written for Csound by Paris Smaragdis
;;   who based his work on formulas from 
;;   Charles Doge, Computer music: synthesis, composition, and performance.

(define root-2 (sqrt 2.0))

(define (butter b sig) (filter b sig))

(define (make-butter-high-pass fq)
  (let* ((r (tan (/ (* pi fq) (srate))))
	 (r2 (* r r))
	 (c1 (/ 1.0 (+ 1.0 (* r root-2) r2)))
	 (c2  (* -2.0 c1))
	 (c3 c1)
	 (c4 (* 2.0 (- r2 1.0) c1))
	 (c5 (* (+ (- 1.0 (* r root-2)) r2) c1)))
    (make-filter 3
		 (list->vct (list c1 c2 c3))
		 (list->vct (list 0.0 c4 c5)))))

(define (make-butter-low-pass fq)
  (let* ((r (/ 1.0 (tan (/ (* pi fq) (srate)))))
	 (r2 (* r r))
	 (c1 (/ 1.0 (+ 1.0 (* r root-2) r2)))
	 (c2 (* 2.0 c1))
	 (c3 c1)
	 (c4 (* 2.0 (- 1.0 r2) c1))
	 (c5  (* (+ (- 1.0 (* r root-2)) r2) c1)))
    (make-filter 3
		 (list->vct (list c1 c2 c3))
		 (list->vct (list 0.0 c4 c5)))))

(define (make-butter-band-pass fq bw)
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

(define spectrum->coeffs 
  (lambda (order spectr)
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
	    (vct-set! coeffs jj coeff)))))))

(define fltit-1
  (lambda (order spectr) 
    "(fltit order spectrum) creates an FIR filter from spectrum and order and returns a closure that calls it"
    (let* ((coeffs (spectrum->coeffs order spectr))
	   (flt (make-fir-filter order coeffs)))
      (lambda (x)
	(fir-filter flt x)))))

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
;;; elegant manner, see snd-motif.scm under scanned-synthesis.


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
;;;
;;; (freqdiv n) repeats each nth sample n times (clobbering the intermediate samples)

(define freqdiv
  (lambda (n)
    (let ((div 0)
	  (curval 0.0))
      (map-chan (lambda (val)
		  (if (= div 0)
		      (set! curval val))
		  (set! div (1+ div))
		  (if (= div n) (set! div 0))
		  curval)))))

;(freqdiv 8)


;;; -------- "adaptive saturation" -- an effect from sed_sed@my-dejanews.com
;;;
;;; a more extreme effect is "saturation":
;;;   (map-chan (lambda (val) (if (< (abs val) .1) val (if (>= val 0.0) 0.25 -0.25))))

(define adsat
  (lambda (size)
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
			#f)))))))


;;; -------- spike
;;;
;;; makes sound more spikey -- sometimes a nice effect

(define (spike)
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
  (define (make-flanger)
    (let* ((ri (make-rand-interp :frequency chorus-speed :amplitude chorus-amount))
	   (len (inexact->exact (random (* 3.0 chorus-time (srate)))))
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
  (let* ((len (frames))
	 (pow2 (ceiling (/ (log len) (log 2))))
	 (fftlen (inexact->exact (expt 2 pow2)))
	 (fftlen2 (inexact->exact (/ fftlen 2)))
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
;(rotate-phase (lambda (x) x) returns original
;(rotate-phase (lambda (x) (- x))) reverses original (might want to write fftlen samps here)

