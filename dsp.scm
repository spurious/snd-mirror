;;; a DSP-related grabbag

(use-modules (ice-9 optargs))
(use-modules (ice-9 format))

(provide 'snd-dsp.scm)

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
    (let ((pk (vct-peak rl)))
      (vct-scale! rl (/ 1.0 pk)))
    (do ((i 0 (1+ i))
	 (j (/ N 2)))
	((= i N))
      (vct-set! im i (vct-ref rl j))
      (set! j (+ j 1))
      (if (= j N) (set! j 0)))
    im))


;;; this version taken from Julius Smith's "Spectral Audio..." with three changes
;;;   it does the DFT by hand, and is independent of anything from Snd (fft, vcts etc)

(define (dolph-1 N gamma)
  (let* ((alpha (cosh (/ (acosh (expt 10.0 gamma)) N)))
	 (den (/ 1.0 (cosh (* N (acosh alpha)))))
	 (freq (/ pi N))
	 (vals (make-vector N))
	 (w (make-vector N))
	 (pk 0.0)
	 (mult -1))
    (do ((i 0 (1+ i))
	 (phase (- (/ pi 2)) (+ phase freq)))
	((= i N))
      (vector-set! vals i (* mult den (cos (* N (acos (* alpha (cos phase)))))))
      (set! mult (* -1 mult)))
    ;; now take the DFT
    (do ((i 0 (1+ i)))
	((= i N))
      (let ((sum 0.0))
	(do ((k 0 (1+ k)))
	    ((= k N))
	  (set! sum (+ sum (* (vector-ref vals k) (exp (/ (* 2.0 0+1.0i pi k i) N))))))
	(vector-set! w i (magnitude sum))
	(if (> (vector-ref w i) pk) (set! pk (vector-ref w i)))))
    ;; scale to 1.0 (it's usually pretty close already, that is pk is close to 1.0)
    (do ((i 0 (1+ i)))
	((= i N))
      (vector-set! w i (/ (vector-ref w i) pk)))
    w))


;;; -------- move sound down by n (a power of 2)

(define (down-oct n)
  "(down-n n) moves a sound down by power of 2 n"
  ;; I think this is "stretch" in DSP jargon -- to interpolate in the time domain we're squeezing the frequency domain
  ;;  the power-of-2 limitation is based on the underlying fft function's insistence on power-of-2 data sizes
  ;;  see stretch-sound-via-dft below for a general version
  (let* ((len (frames))
	 (pow2 (ceiling (/ (log len) (log 2))))
	 (fftlen (inexact->exact (expt 2 pow2)))
	 (fftscale (/ 1.0 fftlen))
	 (rl1 (samples->vct 0 fftlen))
	 (im1 (make-vct fftlen)))
    (fft rl1 im1 1)
    (vct-scale! rl1 fftscale)
    (vct-scale! im1 fftscale)
    (let ((rl2 (make-vct (* n fftlen)))
	  (im2 (make-vct (* n fftlen))))
      (do ((i 0 (+ i 1)) ; lower half
	   (k (1- fftlen) (1- k))
	   (j (1- (* n fftlen)) (1- j)))
	  ((= i (/ fftlen 2)))
	(vct-set! rl2 i (vct-ref rl1 i))
	(vct-set! rl2 j (vct-ref rl1 k))
	(vct-set! im2 i (vct-ref im1 i))
	(vct-set! im2 j (vct-ref im1 k)))
      (fft rl2 im2 -1)
      (vct->samples 0 (* n len) rl2))))

(define (stretch-sound-via-dft factor)
  ;; this is very slow! factor>1.0
  (let* ((n (frames))
	 (n2 (inexact->exact (floor (/ n 2.0))))
	 (out-n (inexact->exact (round (* n factor))))
	 (in-data (channel->vct))
	 (out-data (make-vct out-n))
	 (fr (make-vector out-n 0.0))
	 (freq (/ (* 2 pi) n)))
    (do ((i 0 (1+ i)))
	((or (c-g?) (= i n)))
      ;; DFT + split
      (if (< i n2)
	  (vector-set! fr i (edot-product (* freq 0.0-1.0i i) in-data))
	  (vector-set! fr (+ i (- out-n n 1))  (edot-product (* freq 0.0-1.0i i) in-data))))
    (set! freq (/ (* 2 pi) out-n))
    (do ((i 0 (1+ i)))
	((or (c-g?) (= i out-n)))
      ;; inverse DFT
      (vct-set! out-data i (real-part (/ (edot-product (* freq 0.0+1.0i i) fr) n))))
    (vct->channel out-data)))



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
	   (gx1 (make-vct size))	   
	   (gx2 (make-vct size)))
      (do ((i 0 (1+ i)))
	  ((= i 12))
	(let ((val (sin (/ (* 2 pi i) 12.0))))
	  (vct-set! gx1 (+ i (- (/ size 4) 6)) val)))
      (let* ((gen1 (make-table-lookup 440.0 :wave gx1))
	     (gen2 (make-table-lookup 440.0 :wave gx2))
	     (x1 (mus-data gen1))
	     (x2 (mus-data gen2))
	     (recompute-samps 30) ;just a quick guess
	     (data (make-vct dur)))
	(do ((i 0 (1+ i))
	     (k 0.0)
	     (kincr (/ 1.0 recompute-samps)))
	    ((or (c-g?) 
		 (= i dur)))
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
  ;; this is the same as the CLM asymmetric-fm generator, set r != 1.0 to get the asymmetric spectra
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
;;;   of the nth cos involves hypergeometric series (looks like r^n/n! (~=e^n?) with a million other terms).

(define (kosine-summation gen r k)
  (* (expt (- (+ 1.0 (* r r))
	      (* 2 r (oscil gen)))
	   (- k))
     (expt (- (+ 1.0 (* r r)) (* 2 r)) k))) ; amplitude normalization

(define make-kosine-summation make-oscil)

;;; (let ((gen (make-kosine-summation 100.0))) (map-channel (lambda (y) (* .2 (kosine-summation gen 0.5 5.0)))))
;;;
;;; there is still noticable DC offset if r != 0.5 -- could precompute it and subtract


;;; -------- legendre, fejer

(define (fejer-sum angle n)
  ;; from "Trigonometric Series" Zygmund p88
  (let ((val (/ (sin (* 0.5 (+ n 1) angle)) (* 2 (sin (* 0.5 angle))))))
    (* 2 (/ (* val val) (+ n 1)))))

(define (legendre-sum angle n)
  ;; from Andrews, Askey, Roy "Special Functions" p 314
  (let* ((val (/ (sin (* angle (+ n 0.5))) (sin (* 0.5 angle)))))
    (* val val)))


;;; -------- variations on sum-of-cosines
;;; from "Trigonometric Delights" by Eli Maor

(define (sum-of-n-sines angle n)
  (let* ((a2 (* angle 0.5))
	 (den (sin a2)))
    (if (= den 0.0)
	0.0
	(/ (* (sin (* n a2)) (sin (* (1+ n) a2))) den))))

;;; identical to this is the "conjugate Dirichlet kernel" from "Trigonometric Series" Zygmund p49
;;;  (let* ((a2 (* 0.5 angle))
;;;	    (den (* 2 (sin a2))))
;;;    (if (= den 0.0)
;;;	  0.0
;;;	  (/ (- (cos a2) (cos (* (+ n 0.5) angle))) den))))


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

;;; (Gradshteyn and Ryzhik 1.342)


;;; -------- brighten-slightly

(define (brighten-slightly amount)
  "(brighten-slightly amount) is a form of contrast-enhancement ('amount' between ca .1 and 1)"
  (let* ((mx (maxamp))
	 (brt (/ (* 2 pi amount) mx)))
    (map-channel (lambda (y)
		   (* mx (sin (* y brt)))))))


;;; -------- FIR filters

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
  "(fltit-1 order spectrum) creates an FIR filter from spectrum and order and returns a closure that calls it: 
(map-chan (fltit-1 10 (vct 0 1.0 0 0 0 0 0 0 1.0 0)))"
  (let* ((flt (make-fir-filter order (spectrum->coeffs order spectr))))
    (lambda (x)
      (fir-filter flt x))))

;(map-chan (fltit-1 10 (vct 0 1.0 0 0 0 0 0 0 1.0 0)))
;
;(let ((notched-spectr (make-vct 40)))
;  (vct-set! notched-spectr 2 1.0)  
;  (vct-set! notched-spectr 37 1.0)
;  (map-chan (fltit-1 40 notched-spectr)))
;

;;; -------- Hilbert transform

(define* (make-hilbert-transform #:optional (len 30))
  "(make-hilbert-transform #:optional (len 30) makes a Hilbert transform filter"
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

;;; this comes from R Lyons:
(define (sound->amp-env)
  (let ((hlb (make-hilbert-transform 40))
	(d (make-delay 40)))
    (map-channel
     (lambda (y)
       (let ((hy (hilbert-transform hlb y))
	     (dy (delay d y)))
	 (sqrt (+ (* hy hy) (* dy dy))))))))

(define (hilbert-transform-via-fft)
  ;; same as FIR version but use FFT and change phases by hand
  (let* ((size (frames))
	 (len (inexact->exact (expt 2 (ceiling (/ (log size) (log 2.0))))))
	 (rl (make-vct len))
	 (im (make-vct len))
	 (rd (make-sample-reader 0)))
    (do ((i 0 (1+ i)))
	((= i size))
      (vct-set! rl i (rd)))
    (mus-fft rl im len)
    (do ((i 0 (1+ i)))
	((= i len))
      (let* ((c (make-rectangular (vct-ref rl i) (vct-ref im i)))
	     (ph (angle c))
	     (mag (magnitude c)))
	(if (< i (/ len 2))
	    (set! ph (+ ph (* 0.5 pi)))
	    (set! ph (- ph (* 0.5 pi))))
	(set! c (make-polar mag ph))
	(vct-set! rl i (real-part c))
	(vct-set! im i (imag-part c))))
    (mus-fft rl im len -1)
    (vct-scale! rl (/ 1.0 len))
    (vct->channel rl)))
!#

;;; -------- highpass filter 

(define* (make-highpass fc #:optional (len 30))
  "(make-highpass fc #:optional (len 30)) makes an FIR highpass filter"
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
  "(make-lowpass fc #:optional (len 30)) makes an FIR lowpass filter"
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
  "(make-bandpass flo fhi #:optional (len 30)) makes an FIR bandpass filter"
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
  "(make-bandstop flo fhi #:optional (len 30)) makes an FIR bandstop (notch) filter"
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
  "(make-differentiator #:optional (len 30)) makes an FIR differentiator (highpass) filter"
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
		 (vct c1 c2 c3)
		 (vct 0.0 c4 c5))))

(define (make-butter-low-pass fq)
  "(make-butter-low-pass freq) makes a Butterworth filter with low pass cutoff at 'freq'.  The result 
can be used directly: (filter-sound (make-butter-low-pass 500.0)), or via the 'butter' generator"
  (let* ((r (/ 1.0 (tan (/ (* pi fq) (srate)))))
	 (r2 (* r r))
	 (c1 (/ 1.0 (+ 1.0 (* r (sqrt 2.0)) r2)))
	 (c2 (* 2.0 c1))
	 (c3 c1)
	 (c4 (* 2.0 (- 1.0 r2) c1))
	 (c5  (* (+ (- 1.0 (* r (sqrt 2.0))) r2) c1)))
    (make-filter 3
		 (vct c1 c2 c3)
		 (vct 0.0 c4 c5))))

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
		 (vct c1 c2 c3)
		 (vct 0.0 c4 c5))))

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
		 (vct c1 c2 c3)
		 (vct 0.0 c4 c5))))

;;; simplest use is (filter-sound (make-butter-low-pass 500.0))
;;; see also effects.scm


;;; from "DSP Filter Cookbook" by Lane et al, Prompt Pubs, 2001
;;; 
;;; use with the filter generator
;;;   (define gen (make-iir-high-pass-2 1000))
;;;   (filter gen 1.0)
;;;   etc

(define (make-biquad a0 a1 a2 b1 b2)
  "(make-biquad a0 a1 a2 b1 b2) returns a biquad filter (use with the CLM filter gen)"
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
	 

;;; -------- notch filters

(define* (make-notch-frequency-response cur-srate freqs #:optional (notch-width 2))
  (let ((freq-response (list 1.0 0.0)))
    (for-each
     (lambda (i)
      (set! freq-response (cons (/ (* 2 (- i notch-width)) cur-srate) freq-response)) ; left upper y hz
      (set! freq-response (cons 1.0 freq-response)) ; left upper y resp
      (set! freq-response (cons (/ (* 2 (- i (/ notch-width 2))) cur-srate) freq-response)) ; left bottom y hz
      (set! freq-response (cons 0.0 freq-response)) ; left bottom y resp
      (set! freq-response (cons (/ (* 2 (+ i (/ notch-width 2))) cur-srate) freq-response)) ; right bottom y hz
      (set! freq-response (cons 0.0 freq-response)) ; right bottom y resp
      (set! freq-response (cons (/ (* 2 (+ i notch-width)) cur-srate) freq-response)) ; right upper y hz
      (set! freq-response (cons 1.0 freq-response))) ; right upper y resp
     freqs)
    (set! freq-response (cons 1.0 freq-response))
    (set! freq-response (cons 1.0 freq-response)) 
    (reverse freq-response)))

(define* (notch-channel freqs #:optional (filter-order #f) beg dur (snd #f) (chn #f) edpos (truncate #t) (notch-width 2))
  "(notch-channel freqs #:optional (filter-order #f) beg dur (snd #f) (chn #f) edpos (truncate #t) (notch-width 2)) -> notch filter removing freqs"
  (filter-channel (make-notch-frequency-response (exact->inexact (srate snd)) freqs notch-width)
		  (or filter-order (inexact->exact (expt 2 (ceiling (/ (log (/ (srate snd) notch-width)) (log 2.0))))))
		  beg dur snd chn edpos truncate))

(define* (notch-sound freqs #:optional (filter-order #f) (snd #f) (chn #f) (notch-width 2))
  "(notch-sound freqs #:optional (filter-order #f) (snd #f) (chn #f) (notch-width 2)) -> notch filter removing freqs"
  (filter-sound (make-notch-frequency-response (exact->inexact (srate snd)) freqs notch-width)
		(or filter-order (inexact->exact (expt 2 (ceiling (/ (log (/ (srate snd) notch-width)) (log 2.0))))))
		snd chn))

(define* (notch-selection freqs #:optional (filter-order #f) (notch-width 2))
  "(notch-selection freqs #:optional (filter-order #f) (notch-width 2)) -> notch filter removing freqs"
  (if (selection?)
      (filter-selection (make-notch-frequency-response (exact->inexact (selection-srate)) freqs notch-width)
			(or filter-order (inexact->exact (expt 2 (ceiling (/ (log (/ (selection-srate) notch-width)) (log 2.0)))))))))


;;; -------- fractional Fourier Transform, z transform
;;;
;;; translated from the fxt package of Joerg Arndt

(define (fractional-fourier-transform fr fi n v)
  "(fractional-fourier-transform real imaginary n angle) performs a fractional Fourier transform on data; if angle=1.0, you get a normal Fourier transform"
  ;; this is the slow (dft) form
  ;; v=1 -> normal fourier transform
  (let ((hr (make-vct n))
	(hi (make-vct n))
	(ph0 (/ (* v 2 pi) n)))
    (do ((w 0 (1+ w)))
	((= w n))
      (let ((sr 0.0)
	    (si 0.0))
	(do ((k 0 (1+ k)))
	    ((= k n))
	  (let* ((phase (* ph0 k w))
		 (c (cos phase))
		 (s (sin phase))
		 (x (vct-ref fr k))
		 (y (vct-ref fi k))
		 (r (- (* x c) (* y s)))
		 (i (+ (* y c) (* x s))))
	    (set! sr (+ sr r))
	    (set! si (+ si i))))
	(vct-set! hr w sr)
	(vct-set! hi w si)))
    (list hr hi)))

(define (z-transform f n z)
  ;; using vector to allow complex sums (z=e^2*pi*i/n -> fourier transform)
  ;;   (z-transform data n (exp (make-rectangular 0.0 (* (/ 2.0 n) 3.14159265))))
  "(z-transform data n z) performs a Z transform on data; if z=e^2*pi*j/n you get a Fourier transform; complex results in returned vector"
  (let ((res (make-vector n)))
    (do ((w 0 (1+ w)))
	((= w n))
      (let ((sum 0.0)
	    (t 1.0)
	    (m (expt z w)))
	(do ((k 0 (1+ k)))
	    ((= k n))
	  (set! sum (+ sum (* (vct-ref f k) t)))
	  (set! t (* t m)))
	(vector-set! res w sum)))
    res))

;;; -------- slow Hartley transform 

(define (dht data) 
  "(dht data) returns the Hartley transform of 'data'."
  ;; taken from Perry Cook's SignalProcessor.m (the slow version of the Hartley transform)
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
    (do ((i 0 (1+ i))) ; this could also use edot-product
	((= i dur))
      (let ((samp (next-sample reader)))
	(set! sw (+ sw (* samp (sin (* i incr)))))
	(set! cw (+ cw (* samp (cos (* i incr)))))))
    (list (* 2 (/ (sqrt (+ (* sw sw) (* cw cw))) dur))
	  (atan cw sw))))

;;; this is a faster version of find-sine using the "Goertzel algorithm" taken from R Lyons "Understanding DSP" p 529
;;; it returns the same result as find-sine above if you take (* 2 (/ (goertzel...) dur)) -- see snd-test.scm examples
(define* (goertzel freq #:optional beg dur)
  (let* ((sr (srate))
	 (y2 0.0)
	 (y1 0.0)
	 (y0 0.0)
	 (rfreq (/ (* 2.0 3.14159 freq) sr))
	 (cs (* 2.0 (cos rfreq))))
    (scan-channel (lambda (y)
		   (set! y2 y1)
		   (set! y1 y0)
		   (set! y0 (+ (- (* y1 cs) y2) y))
		   #f)
		  (or beg 0) (or dur (frames)))
    (magnitude (- y0 (* y1 (exp (make-rectangular 0.0 (- rfreq))))))))


(define (make-spencer-filter)
  (make-fir-filter 15 (apply vct (map (lambda (n) (/ n 320.0)) (list -3 -6 -5 3 21 46 67 74 67 46 21 3 -5 -6 -3)))))


;;; -------- any-random
;;;
;;; arbitrary random number distributions via the "rejection method"

(define* (any-random amount #:optional e)
  (if (= amount 0.0)
      0.0
      (if (not e)
	  (random amount)
	  (letrec ((next-random 
		    (lambda ()
		      (let* ((len (length e))
			     (x (random (exact->inexact (list-ref e (- len 2)))))
			     (y (random 1.0)))
			(if (or (<= y (envelope-interp x e))
				(c-g?))
			    x
			    (next-random))))))
	    (next-random)))))

(define (gaussian-distribution s)
  (let ((e '())
	(den (* 2.0 s s)))
    (do ((i 0 (1+ i))
	 (x 0.0 (+ x .05))
	 (y -4.0 (+ y .4)))
	((= i 21))
      (set! e (cons x e))
      (set! e (cons (exp (- (/ (* y y) den))) e)))
    (reverse e)))

(define (pareto-distribution a)
  (let ((e '())
	(scl (/ (expt 1.0 (+ a 1.0)) a)))
    (do ((i 0 (1+ i))
	 (x 0.0 (+ x .05))
	 (y 1.0 (+ y .2)))
	((= i 21))
      (set! e (cons x e))
      (set! e (cons (* scl (/ a (expt y (+ a 1.0)))) e)))
    (reverse e)))

;(map-chan (lambda (y) (any-random 1.0 '(0 1 1 1)))) ; uniform distribution
;(map-chan (lambda (y) (any-random 1.0 '(0 0 0.95 0.1 1 1)))) ; mostly toward 1.0
;(let ((g (gaussian-distribution 1.0))) (map-chan (lambda (y) (any-random 1.0 g))))
;(let ((g (pareto-distribution 1.0))) (map-chan (lambda (y) (any-random 1.0 g))))

;;; this is the inverse integration function used by CLM to turn a distribution function into a weighting function

(if (not (provided? 'snd-env.scm)) (load-from-path "env.scm"))

(define* (inverse-integrate dist #:optional (data-size 512) (e-size 50))
  (let* ((e '())
	 (sum (exact->inexact (cadr dist)))
	 (first-sum sum)
	 (data (make-vct data-size))
	 (x0 (car dist))
	 (x1 (list-ref dist (- (length dist) 2)))
	 (xincr (exact->inexact (/ (- x1 x0) e-size))))
    (do ((i 0 (1+ i))
	 (x x0 (+ x xincr)))
	((> i e-size))
      (set! e (cons sum e))
      (set! e (cons x e))
      (set! sum (+ sum (envelope-interp x dist))))
    (let* ((incr (/ (- (cadr e) first-sum) (- data-size 1))))
      (set! e (reverse e))
      (do ((i 0 (1+ i))
	   (x first-sum (+ x incr)))
	  ((= i data-size))
	(vct-set! data i (envelope-interp x e)))
      data)))

(define (gaussian-envelope s)
  (let ((e '())
	(den (* 2.0 s s)))
    (do ((i 0 (1+ i))
	 (x -1.0 (+ x .1))
	 (y -4.0 (+ y .4)))
	((= i 21))
      (set! e (cons x e))
      (set! e (cons (exp (- (/ (* y y) den))) e)))
    (reverse e)))

;;; (make-rand :envelope (gaussian-envelope 1.0))


;;; ---------------- Julius Smith stuff ----------------
;;;
;;; these are from "Mathematics of the DFT", W3K Pubs

(define* (channel-mean #:optional snd chn)
  (let ((sum 0.0)
	(N (frames snd chn)))
    (scan-channel (lambda (y) (set! sum (+ sum y)) #f) 0 N snd chn)
    (/ sum N)))

(define* (channel-total-energy #:optional snd chn)
  (let ((sum 0.0))
    (scan-channel (lambda (y) (set! sum (+ sum (* y y))) #f) 0 (frames snd chn) snd chn)
    sum))

(define* (channel-average-power #:optional snd chn)
  (/ (channel-total-energy snd chn) (frames snd chn)))

(define* (channel-rms #:optional snd chn)
  (sqrt (channel-average-power snd chn)))

(define* (channel-variance #:optional snd chn) ; "sample-variance" might be better
  (let* ((N (frames snd chn))
	 (mu (* (/ N (- N 1)) (channel-mean snd chn))) ; avoid bias sez JOS
	 (P (channel-total-energy snd chn)))
    (- P (* mu mu))))

(define* (channel-norm #:optional snd chn)
  (sqrt (channel-total-energy snd chn)))

(define* (channel-lp u-p #:optional snd chn)
  (let ((sum 0.0)
	(p u-p) ; for the optimizer's benefit -- it can't find define* args
	(N (frames snd chn)))
    (scan-channel (lambda (y) (set! sum (+ sum (expt (abs y) p))) #f) 0 N snd chn)
    (expt sum (/ 1.0 p))))

(define* (channel-lp-inf #:optional snd chn)
  (let ((mx 0.0)
	(N (frames snd chn)))
    (scan-channel (lambda (y) (set! mx (max mx (abs y))) #f) 0 N snd chn)
    mx))

(define (channel2-inner-product s1 c1 s2 c2)
  (let ((N (frames s1 c1))
	(sum 0.0)
	(r1 (make-sample-reader 0 s1 c1))
	(r2 (make-sample-reader 0 s2 c2)))
    (do ((i 0 (1+ i)))
	((= i N))
      (set! sum (+ sum (* (r1) (r2)))))
    sum))

(define (channel2-angle s1 c1 s2 c2)
  (let ((inprod (channel2-inner-product s1 c1 s2 c2))
	(norm1 (channel-norm s1 c1))
	(norm2 (channel-norm s2 c2)))
    (acos (/ inprod (* norm1 norm2)))))

(define (channel2-orthogonal? s1 c1 s2 c2)
  (= (channel2-inner-product s1 c1 s2 c2) 0.0))

(define (channel2-coefficient-of-projection s1 c1 s2 c2) ; s1,c1 = x, s2,c2 = y
  (let ((inprod (channel2-inner-product s1 c1 s2 c2))
	(norm1 (channel-norm s1 c1)))
    (/ inprod (* norm1 norm1))))

;;; the projection is now (scale-by coeff 0 (frames) s1 c1)
;;; end of JOS stuff


(define (periodogram N)
  (let* ((len (frames))
	 (average-data (make-vct N))
	 (rd (make-sample-reader 0))
	 (N2 (* 2 N))
	 (rl (make-vct N2))
	 (im (make-vct N2)))
    (do ((i 0 (+ i N)))
	((>= i len))
      (vct-scale! rl 0.0)
      (vct-scale! im 0.0)
      (do ((k 0 (1+ k)))
	  ((= k N))
	(vct-set! rl k (rd)))
      (mus-fft rl im)
      (do ((k 0 (1+ k)))
	  ((= k N))
	(vct-set! average-data k (+ (vct-ref average-data k) 
				    (+ (* (vct-ref rl k) (vct-ref rl k)) 
				       (* (vct-ref im k) (vct-ref im k)))))))
    (graph (vct-scale! average-data (/ 1.0 (ceiling (/ len N)))))))


;;; -------- ssb-am friends

(define* (map-ssb-am freq #:optional (order 40)) ; higher order = better cancellation
  ;; TODO: a better name -- perhaps channel-shift-spectrum? shift-channel-spectrum?
  (let* ((gen (make-ssb-am freq order)))
    (map-channel (lambda (y) (ssb-am gen y)))))

(define (hz->2pi freq) (/ (* 2 pi freq) (srate))) ; hz->radians follows mus-srate unfortunately

(define* (ssb-bank old-freq new-freq pairs-1 #:optional (order 40) (bw 50.0))
  (let* ((pairs pairs-1) ; for run's benefit
	 (ssbs (make-vector pairs))
	 (bands (make-vector pairs))
	 (factor (/ (- new-freq old-freq) old-freq))
	 (mx (maxamp)))
    (do ((i 1 (1+ i)))
	((> i pairs))
      (let* ((aff (* i old-freq))
	     (bwf (* bw (+ 1.0 (/ i (* 2 pairs))))))
	(vector-set! ssbs (1- i) (make-ssb-am (* i factor old-freq)))
	(vector-set! bands (1- i) (make-bandpass (hz->2pi (- aff bwf)) 
						 (hz->2pi (+ aff bwf)) 
						 order))))
    (as-one-edit
     (lambda ()
       (let ((nmx 0.0))
	 (map-channel
	  (lambda (y)
	    (let ((sum 0.0))
	      (do ((i 0 (1+ i)))
		  ((= i pairs))
		(set! sum (+ sum (ssb-am (vector-ref ssbs i) 
					 (bandpass (vector-ref bands i) 
						   y)))))
	      (set! nmx (max nmx (abs sum)))
	      sum)))
	 (scale-by (/ mx nmx)))))))


;;; TODO: auto-detect main freq so ssb-bank can work semi-automatically (bw/pairs choices also automated)
;;; TODO: freq env to add (or remove) pitch fluctuations [if pitch follower, this could be automated]
;;; TODO: make multiplier an arg (inharmonic or stretched partials) -- or an env [zip -> freq domain via these funcs]
;;; TODO: hz->radians should be smart (or someone should) about srates
;;; TODO: what about widely changing/noisy sounds?
;;; TODO: should some form of ssb-bank be moved into CLM?
;;; TODO: a channel (regularized) version of ssb-bank -- repitch-channel? (+ retime or whatever)
;;; TODO: a realtime interface to this -- a slider for pitch/bw etc

;;; TODO: complex-data freq processing funcs
;;; TODO: run support for complex data? (what about generators?)


#!
(define* (repitch-sound old-freq new-freq)
  (ssb-bank old-freq new-freq 10))

(define* (retime-sound new-time)
  (let* ((old-time (/ (frames) (srate)))
	 (factor (/ new-time old-time)))
    (ssb-bank 557 (* 557 factor) 10)
    (src-sound (/ 1.0 factor))))
!#

