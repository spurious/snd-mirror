;;; a DSP-related grabbag

(use-modules (ice-9 optargs) (ice-9 format))

(provide 'snd-dsp.scm)
(if (not (provided? 'snd-ws.scm)) (load-from-path "ws.scm"))

(if (not (defined? 'log10))
    (define (log10 a) 
      "(log10 a) returns the log base 10 of 'a'"
      (/ (log a) (log 10))))

;;; src-duration (see src-channel in extsnd.html)

(define (src-duration e)
  "(src-duration envelope) returns the new duration of a sound after using 'envelope' for time-varying sampling-rate conversion"
  (let* ((len (length e))
	 (ex0 (car e))
	 (ex1 (list-ref e (- len 2)))
	 (all-x (- ex1 ex0))
	 (dur 0.0))
    (do ((i 0 (+ i 2)))
	((>= i (- len 2)) dur)
      (let* ((x0 (list-ref e i))
	     (x1 (list-ref e (+ i 2)))
	     (xy0 (list-ref e (+ i 1))) ; 1/x x points
	     (y0 (/ 1.0 xy0))           ; related y value
	     (xy1 (list-ref e (+ i 3)))
	     (y1 (/ 1.0 xy1))
	     (area (if (< (abs (- xy0 xy1)) .0001)
		       (* y0 (/ (- x1 x0) all-x))
		       (* (/ (- (log y1) (log y0)) 
			     (- xy0 xy1)) 
			  (/ (- x1 x0) all-x)))))
	(set! dur (+ dur (abs area)))))))


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
  "(dolph-1 n gamma) produces a Dolph-Chebyshev FFT data window of 'n' points using 'gamma' as the window parameter."
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

(define* (down-oct n :optional snd chn)
  "(down-n n) moves a sound down by power of 2 n"
  ;; I think this is "stretch" in DSP jargon -- to interpolate in the time domain we're squeezing the frequency domain
  ;;  the power-of-2 limitation is based on the underlying fft function's insistence on power-of-2 data sizes
  ;;  see stretch-sound-via-dft below for a general version
  (let* ((len (frames snd chn))
	 (pow2 (inexact->exact (ceiling (/ (log len) (log 2)))))
	 (fftlen (inexact->exact (expt 2 pow2)))
	 (fftscale (/ 1.0 fftlen))
	 (rl1 (channel->vct 0 fftlen snd chn))
	 (im1 (make-vct fftlen)))
    (fft rl1 im1 1)
    (vct-scale! rl1 fftscale)
    (vct-scale! im1 fftscale)
    (let ((rl2 (make-vct (* n fftlen)))
	  (im2 (make-vct (* n fftlen))))
      (vct-set! rl2 0 (vct-ref rl1 0))
      (vct-set! im2 0 (vct-ref im1 0))
      (do ((i 1 (+ i 1)) ; lower half
	   (k (1- fftlen) (1- k))
	   (j (1- (* n fftlen)) (1- j)))
	  ((= i (/ fftlen 2)))
	(vct-set! rl2 i (vct-ref rl1 i))
	(vct-set! rl2 j (vct-ref rl1 k))
	(vct-set! im2 i (vct-ref im1 i))
	(vct-set! im2 j (vct-ref im1 k)))
      (fft rl2 im2 -1)
      (vct->channel rl2 0 (* n len) snd chn #f (format #f "down-oct ~A" n)))))

(define* (stretch-sound-via-dft factor :optional snd chn)
  "(stretch-sound-via-dft factor :optional snd chn) makes the given channel longer ('factor' should be > 1.0) by \
squeezing in the frequency domain, then using the inverse DFT to get the time domain result."
  ;; this is very slow! factor>1.0
  (let* ((n (frames snd chn))
	 (n2 (inexact->exact (floor (/ n 2.0))))
	 (out-n (inexact->exact (round (* n factor))))
	 (in-data (channel->vct 0 n snd chn))
	 (out-data (make-vct out-n))
	 (fr (make-vector out-n 0.0))
	 (freq (/ (* 2 pi) n)))
    (do ((i 0 (1+ i)))
	((or (c-g?) (= i n)))
      ;; DFT + split
      (if (< i n2)
	  (vector-set! fr i (edot-product (* freq 0.0-1.0i i) in-data))
	  (vector-set! fr (+ i (- out-n n 1)) (edot-product (* freq 0.0-1.0i i) in-data))))
    (set! freq (/ (* 2 pi) out-n))
    (do ((i 0 (1+ i)))
	((or (c-g?) (= i out-n)))
      ;; inverse DFT
      (vct-set! out-data i (real-part (/ (edot-product (* freq 0.0+1.0i i) fr) n))))
    (vct->channel out-data 0 out-n snd chn #f (format #f "stretch-sound-via-dft ~A" factor))))



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

(define* (freqdiv n1 :optional snd chn)
  "(freqdiv n) repeats each nth sample n times (clobbering the intermediate samples): (freqdiv 8)"
  (let ((div 0)
	(n n1) ; for run
	(curval 0.0))
    (map-channel (lambda (val)
		   (if (= div 0)
		       (set! curval val))
		   (set! div (1+ div))
		   (if (= div n) (set! div 0))
		   curval)
		 0 #f snd chn #f (format #f "freqdiv ~A" n))))



;;; -------- "adaptive saturation" -- an effect from sed_sed@my-dejanews.com
;;;
;;; a more extreme effect is "saturation":
;;;   (map-channel (lambda (val) (if (< (abs val) .1) val (if (>= val 0.0) 0.25 -0.25))))

(define* (adsat usize :optional beg dur snd chn)
  "(adsat size) is an 'adaptive saturation' sound effect"
  (let* ((mn 0.0)
	 (mx 0.0)
	 (n 0)
	 (size usize) ; for run
	 (vals (make-vct size)))
    (map-channel (lambda (val)
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
			 #f)))
		 beg dur snd chn #f (format #f "adsat ~A ~A ~A" size beg dur))))


;;; -------- spike
;;;
;;; makes sound more spikey -- sometimes a nice effect

(define* (spike :optional snd chn)
  "(spike) multiplies successive samples together to make a sound more spikey"
  (map-channel (let ((x1 0.0) 
		     (x2 0.0) 
		     (amp (maxamp snd chn))) ; keep resultant peak at maxamp
		 (lambda (x0) 
		   (let ((res (* (/ x0 (* amp amp)) 
				 (abs x2) 
				 (abs x1)))) 
		     (set! x2 x1) 
		     (set! x1 x0) 
		     res)))
	       0 #f snd chn #f "spike"))

;;; the more successive samples we include in the product, the more we
;;;   limit the output to pulses placed at (just after) wave peaks


;;; -------- easily-fooled autocorrelation-based pitch tracker 

(define spot-freq
  (lambda args
    "(spot-freq samp :optional snd chn) tries to determine the current pitch: (spot-freq (left-sample))"
    (let* ((s0 (car args))
	   (snd (if (> (length args) 1) (list-ref args 1) #f))
	   (chn (if (> (length args) 2) (list-ref args 2) #f))
	   (pow2 (inexact->exact (ceiling (/ (log (/ (srate snd) 20.0)) (log 2)))))
	   (fftlen (inexact->exact (expt 2 pow2)))
	   (data (autocorrelate (channel->vct s0 fftlen snd chn)))
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

(define+ (chorus)
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

(define* (zero-phase :optional snd chn)
  "(zero-phase) calls fft, sets all phases to 0, and un-ffts"
  (let* ((len (frames snd chn))
	 (pow2 (inexact->exact (ceiling (/ (log len) (log 2)))))
	 (fftlen (inexact->exact (expt 2 pow2)))
	 (fftscale (/ 1.0 fftlen))
	 (rl (channel->vct 0 fftlen snd chn))
	 (old-pk (vct-peak rl))
	 (im (make-vct fftlen)))
    (fft rl im 1)
    (rectangular->polar rl im)
    (vct-scale! rl fftscale)
    (vct-scale! im 0.0)
    (fft rl im -1)
    (let ((pk (vct-peak rl)))
      (vct->channel (vct-scale! rl (/ old-pk pk)) 0 len snd chn #f "zero-phase"))))

(define* (rotate-phase func :optional snd chn)
  "(rotate-phase func) calls fft, applies func to each phase, then un-ffts"
  (let* ((len (frames snd chn))
	 (pow2 (inexact->exact (ceiling (/ (log len) (log 2)))))
	 (fftlen (inexact->exact (expt 2 pow2)))
	 (fftlen2 (inexact->exact (floor (/ fftlen 2))))
	 (fftscale (/ 1.0 fftlen))
	 (rl (channel->vct 0 fftlen snd chn))
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
      (vct->channel (vct-scale! rl (/ old-pk pk)) 0 len snd chn #f 
		    (format #f "rotate-phase ~A" (procedure-source func))))))

;(rotate-phase (lambda (x) 0.0)) is the same as (zero-phase)
;(rotate-phase (lambda (x) (random pi))) randomizes phases
;(rotate-phase (lambda (x) x)) returns original
;(rotate-phase (lambda (x) (- x))) reverses original (might want to write fftlen samps here)
;(rotate-phase (lambda (x) (* x 2))) reverb-effect (best with voice)
;(rotate-phase (lambda (x) (* x 12)) "bruise blood" effect


;;; -------- asymmetric FM (bes-i0 case)

(define* (make-asyfm :key
		     (frequency 440.0) (initial-phase 0.0)
		     (ratio 1.0) (r 1.0)
		     (index 1.0))
  "(make-asyfm :key frequency initial-phase ratio r index) returns a new asyfm generator (dsp.scm)"
  (list (hz->radians frequency) initial-phase ratio r index))
		     
(define asyfm-freq
  (make-procedure-with-setter
   (lambda (gen) 
     "asyfm freq field accessor"
     (radians->hz (list-ref gen 0)))
   (lambda (gen val) 
     (list-set! gen 0 (hz->radians val)))))

(define asyfm-phase
  (make-procedure-with-setter
   (lambda (gen) 
     "asyfm phase field accessor"
     (list-ref gen 1))
   (lambda (gen val) 
     (list-set! gen 1 val))))

(define asyfm-ratio
  (make-procedure-with-setter
   (lambda (gen) 
     "asyfm ratio field accessor"
     (list-ref gen 2))
   (lambda (gen val) 
     (list-set! gen 2 val))))

(define asyfm-r
  (make-procedure-with-setter
   (lambda (gen) 
     "asyfm r field accessor"
     (list-ref gen 3))
   (lambda (gen val) 
     (list-set! gen 3 val))))

(define asyfm-index
  (make-procedure-with-setter
   (lambda (gen) 
     "asyfm index field accessor"
     (list-ref gen 4))
   (lambda (gen val) 
     (list-set! gen 4 val))))

(define (asyfm-J gen input)
  "(asyfm-J gen input) is the same as the CLM asymmetric-fm generator, set r != 1.0 to get the asymmetric spectra"
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

(define (asyfm-I gen input)
  "(asyfm-I gen input) is the I0 case of the asymmetric-fm generator (dsp.scm)"
  (let* ((freq (list-ref gen 0))
	 (phase (asyfm-phase gen))
	 (ratio (asyfm-ratio gen))
	 (r (asyfm-r gen))
	 (r1 (/ 1.0 r))
	 (index (asyfm-index gen))
	 (modphase (* ratio phase))
	 (result (* (exp (- (* 0.5 index (+ r r1) (cos modphase))
			    (* 0.5 (log (bes-i0 (* index (+ r r1)))))))
		    (sin (+ phase (* 0.5 index (- r r1) (sin modphase)))))))
    (set! (asyfm-phase gen) (+ phase input freq))
    result))


;;; -------- cosine-summation (a simpler version of sine-summation)
;;;
;;; from Andrews, Askey, Roy "Special Functions" 5.1.16

(define (cosine-summation gen r)
  "(cosine-summation gen r) is a variant of the CLM sine-summation generator; 'r' controls successive sinusoid amplitudes"
  (let* ((rr (* r r))
	 (rr+1 (+ 1.0 rr))
	 (rr-1 (- 1.0 rr))
	 (r2 (* 2 r)))
    (* (- (/ rr-1
	     (- rr+1
		(* r2 (oscil gen))))
	  1.0)
       (/ rr-1 ; amplitude normalization (not vital)
	  (* r2 rr+1)))))

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
  "(kosine-summation gen r k) is a variant of sum-of-cosines; 'r' controls successive sinusoid amplitude; 'k' controls how many sinusoids are produced"
  (* (expt (- (+ 1.0 (* r r))
	      (* 2 r (oscil gen)))
	   (- k))
     (expt (- (+ 1.0 (* r r)) (* 2 r)) k))) ; amplitude normalization

(define make-kosine-summation make-oscil)

;;; (let ((gen (make-kosine-summation 100.0))) (map-channel (lambda (y) (* .2 (kosine-summation gen 0.5 5.0)))))
;;;
;;; there is still noticable DC offset if r != 0.5 -- could precompute it and subtract


;;; -------- legendre, fejer, poussin, jackson

(define (fejer-sum angle n)
  "(fejer-sum angle n) produces a band-limited pulse train"
  ;; from "Trigonometric Series" Zygmund p88 with changes suggested by Katznelson "Introduction to Harmonic Analysis" p12, and
  ;;   scaling by an extra factor of 1/n+1 to make sure we always peak at 1.0 (I assume callers in this context are interested 
  ;;   in the pulse-train aspect and want easily predictable peak amp).  Harmonics go as (n-i)/n+1.
  (if (= angle 0.0)
      1.0
      (let ((val (/ (sin (* 0.5 (+ n 1) angle)) 
		    (* (+ n 1) 
		       (sin (* 0.5 angle))))))
	(* val val))))

;;; here's Zygmund's version:
;;  (if (= angle 0.0)
;;      1.0
;;      (let ((val (/ (sin (* 0.5 (+ n 1) angle)) (* 2 (sin (* 0.5 angle))))))
;;	(* 2 (/ (* val val) (+ n 1))))))

;;; (let ((angle 0.0)) (map-channel (lambda (y) (let ((val (fejer-sum angle 3))) (set! angle (+ angle .1)) (* .1 val)))))

(define (poussin-sum angle n)
  "(poussin-sum angle n) produces a pulse train"
  ;; this and next taken from Katznelson p16
  (- (* 2 (fejer-sum angle (+ (* 2 n) 1)))
     (fejer-sum angle n)))

(define (jackson-sum angle n)
  "(poussin-sum angle n) produces a pulse train"
  (let ((val (fejer-sum angle n)))
    (* val val))) ; we already normalized this to 1.0

(define (legendre-sum angle n)
  "(legendre-sum angle n) produces a band-limited pulse train"
  ;; from Andrews, Askey, Roy "Special Functions" p 314 with my amplitude scaling
  (if (= angle 0.0)
      1.0
      (let* ((val (/ (sin (* angle (+ n 0.5))) 
		     (* (sin (* 0.5 angle))
			(+ (* 2 n) 1))))) ; amplitude normalization -- we want a peak amp of 1.0
	(* val val))))

;;; (let ((angle 0.0)) (map-channel (lambda (y) (let ((val (legendre-sum angle 3))) (set! angle (+ angle .1)) (* .1 val)))))


(define (signum n)
  (if (positive? n) 1
      (if (zero? n) 0
	  -1)))


;;; -------- variations on sum-of-cosines
;;; from "Trigonometric Delights" by Eli Maor

(define (sum-of-n-sines angle n)
  "(sum-of-n-sines angle n) produces the sum of 'n' sines"
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
  "(sum-of-n-odd-sines angle n) produces the sum of 'n' odd-numbered sines"
  (let ((den (sin angle))
	(na (sin (* n angle))))
    (if (= den 0.0)
	0.0
	(/ (* na na) den))))

(define (sum-of-n-odd-cosines angle n)
  "(sum-of-n-odd-cosines angle n) produces the sum of 'n' odd-numbered cosines"
  (let ((den (* 2 (sin angle))))
    (if (= den 0.0)
	(exact->inexact n) ; just guessing -- floatification is for the run macro
	(/ (sin (* 2 n angle)) den))))

;;; (Gradshteyn and Ryzhik 1.342)

;;; or take advantage of 1/(1-x):
;;; (map-channel (lambda (y) (set! i (1+ i)) (/ 0.001 (- 1.0 (* .99 (cos (/ (* i 2.0 pi) 100.0)))))))
;;;   here the .99 controls the number of cosines, like an "index", and it can be matched at
;;;   run time to keep the amplitude constant via the 0.001 (set above to get a maxamp of .1),
;;;   and the frequency can be swept without problems.


;;; and another...
(define (band-limited-sawtooth x a N fi)
  "(band-limited-sawtooth x a N fi) produces a band-limited sawtooth; 'x' is the current phase, 'a' is \
the amp (more or less), 'N'  is 1..10 or thereabouts, 'fi' is the phase increment"
  ;;   Alexander Kritov suggests time-varying "a" is good (this is a translation of his code)
  ;;   from Stilson/Smith apparently -- was named "Discrete Summation Formula" which doesn't convey anything to me
  (let ((s4 (+ 1.0 (* -2.0 a (cos x)) (* a a))))
    (if (= s4 0.0)
	0.0
	(let* ((s1 (* (expt a (- N 1.0)) (sin (+ (* (- N 1.0) x) fi))))
	       (s2 (* (expt a N) (sin (+ (* N x) fi))))
	       (s3 (* a (sin (+ x fi)))))
	  (/ (+ (sin fi) (- s3) (- s2) s1) s4)))))

;;; (let ((angle 0.0)) (map-channel (lambda (y) (let ((val (band-limited-sawtooth angle 0.5 8 .2))) (set! angle (+ angle .2)) val))))


;;; square-wave in the same mold

(define (band-limited-square-wave theta n)
  "(band-limited-square-wave theta n) produces a square-wave; 'n' sets how squared-off it is, 'theta' is instantaneous phase"
  (tanh (* n (sin theta))))

;;; (let ((angle 0.0)) (map-channel (lambda (y) (let ((val (band-limited-square-wave angle 10))) (set! angle (+ angle .2)) val))))



;;; -------- brighten-slightly

(define* (brighten-slightly amount :optional snd chn)
  "(brighten-slightly amount) is a form of contrast-enhancement ('amount' between ca .1 and 1)"
  (let* ((mx (maxamp))
	 (brt (/ (* 2 pi amount) mx)))
    (map-channel (lambda (y)
		   (* mx (sin (* y brt))))
		 0 #f snd chn #f (format #f "brighten-slightly ~A" amount))))

(define (brighten-slightly-1 coeffs)
  "(brighten-slightly-1 coeffs) is a form of contrast-enhancement: (brighten-slightly-1 '(1 .5 3 1))"
  (let ((pcoeffs (partials->polynomial coeffs))
	(mx (maxamp)))
    (map-channel
     (lambda (y)
       (* mx (polynomial pcoeffs (/ y mx)))))))
;; I think this could be a virtual op (ptree)



;;; -------- FIR filters

;;; Snd's (very simple) spectrum->coefficients procedure is:

(define (spectrum->coeffs order spectr)
  "(spectrum->coeffs order spectr) returns FIR filter coefficients given the filter order and desired spectral envelope"
  (let* ((coeffs (make-vct order))
	 (n order)
	 (m (inexact->exact (floor (/ (+ n 1) 2))))
	 (am (* 0.5 (+ n 1)))
	 (q (/ (* pi 2.0) n)))
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
(map-channel (fltit-1 10 (vct 0 1.0 0 0 0 0 0 0 1.0 0)))"
  (let* ((flt (make-fir-filter order (spectrum->coeffs order spectr))))
    (lambda (x)
      (fir-filter flt x))))

;(map-channel (fltit-1 10 (vct 0 1.0 0 0 0 0 0 0 1.0 0)))
;
;(let ((notched-spectr (make-vct 40)))
;  (vct-set! notched-spectr 2 1.0)  
;  (vct-set! notched-spectr 37 1.0)
;  (map-channel (fltit-1 40 notched-spectr)))
;

;;; -------- Hilbert transform

(define* (make-hilbert-transform :optional (len 30))
  "(make-hilbert-transform :optional (len 30) makes a Hilbert transform filter"
  (let* ((arrlen (1+ (* 2 len)))
	 (arr (make-vct arrlen))
	 (lim (if (even? len) len (1+ len))))
    (do ((i (- len) (1+ i)))
	((= i lim))
      (let* ((k (+ i len))
	     (denom (* pi i))
	     (num (- 1.0 (cos (* pi i)))))
	(if (or (= num 0.0) (= i 0))
	    (vct-set! arr k 0.0)
	    ;; this is the "ideal" -- rectangular window -- version:
	    ;; (vct-set! arr k (/ num denom))
            ;; this is the Hamming window version:
	    (vct-set! arr k (* (/ num denom) 
			       (+ .54 (* .46 (cos (/ (* i pi) len)))))) ; window
	    )))
    (make-fir-filter arrlen arr)))

(define (hilbert-transform f in)
  "(hilbert-transform f in) is the generator corresponding to make-hilbert-transform"
  (fir-filter f in))

#|
  (let ((h (make-hilbert-transform 15)))
    (map-channel (lambda (y)
		   (hilbert-transform h y))))

;;; this comes from R Lyons:
(define* (sound->amp-env :optional snd chn)
  (let ((hlb (make-hilbert-transform 40))
	(d (make-delay 40)))
    (map-channel
     (lambda (y)
       (let ((hy (hilbert-transform hlb y))
	     (dy (delay d y)))
	 (sqrt (+ (* hy hy) (* dy dy)))))
     0 #f snd chn #f "sound->amp-env")))

(define* (hilbert-transform-via-fft :optional snd chn)
  ;; same as FIR version but use FFT and change phases by hand
  (let* ((size (frames snd chn))
	 (len (expt 2 (inexact->exact (ceiling (/ (log size) (log 2.0))))))
	 (rl (make-vct len))
	 (im (make-vct len))
	 (rd (make-sample-reader 0 snd chn)))
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
    (vct->channel rl 0 len snd chn #f "hilbert-transform-via-fft")))
|#

;;; -------- highpass filter 

(define* (make-highpass fc :optional (len 30))
  "(make-highpass fc :optional (len 30)) makes an FIR highpass filter"
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
  "(highpass f in) is the generator corresponding to make-highpass"
  (fir-filter f in))

#|
  (let ((hp (make-highpass (* .1 pi))))
    (map-channel (lambda (y)
		   (highpass hp y))))
|#


;;; -------- lowpass filter

(define* (make-lowpass fc :optional (len 30))
  "(make-lowpass fc :optional (len 30)) makes an FIR lowpass filter"
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
  "(lowpass f in) is the generator corresponding to make-lowpass"
  (fir-filter f in))

#|
  (let ((hp (make-lowpass (* .2 pi))))
    (map-channel (lambda (y)
		   (lowpass hp y))))
|#

;;; -------- bandpass filter

(define* (make-bandpass flo fhi :optional (len 30))
  "(make-bandpass flo fhi :optional (len 30)) makes an FIR bandpass filter"
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
  "(bandpass f in) is the generator corresponding to make-bandpass"
  (fir-filter f in))

#|
  (let ((hp (make-bandpass (* .1 pi) (* .2 pi))))
    (map-channel (lambda (y)
		   (bandpass hp y))))

;; for more bands, you can add the coeffs:

(define* (make-bandpass-2 flo1 fhi1 flo2 fhi2 :optional (len 30))
  (let* ((f1 (make-bandpass flo1 fhi1 len))
	 (f2 (make-bandpass flo2 fhi2 len)))
    (vct-add! (mus-xcoeffs f1) (mus-xcoeffs f2))
    f1))

(let ((ind (new-sound "test.snd")))
  (map-channel (lambda (y) (- 1.0 (random 2.0))) 0 10000)
  (let ((f2 (make-bandpass-2 (* .12 pi) (* .15 pi) (* .22 pi) (* .25 pi) 100)))
    (map-channel (lambda (y) (fir-filter f2 y)))
    ))

|#

;;; -------- bandstop filter

(define* (make-bandstop flo fhi :optional (len 30))
  "(make-bandstop flo fhi :optional (len 30)) makes an FIR bandstop (notch) filter"
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
  "(bandstop f in) is the generator corresponding to make-bandstop"
  (fir-filter f in))

#|
  (let ((hp (make-bandstop (* .1 pi) (* .3 pi))))
    (map-channel (lambda (y)
		   (bandstop hp y))))
|#

;;; -------- differentiator

(define* (make-differentiator :optional (len 30))
  "(make-differentiator :optional (len 30)) makes an FIR differentiator (highpass) filter"
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
  "(differentiator f in) is the generator corresponding to make-differentiator"
  (fir-filter f in))

#|
  (let ((hp (make-differentiator)))
    (map-channel (lambda (y)
		   (differentiator hp y))))
|#


;;; -------- IIR filters
;;; see analog-filter.scm for the usual suspects

;;; -------- Butterworth filters (see also further below -- make-butter-lp et al)
;;;
;; translated from CLM butterworth.cl:
;;
;;   Sam Heisz, January 1998
;;   inspired by some unit generators written for Csound by Paris Smaragdis
;;   who based his work on formulas from 
;;   Charles Dodge, Computer music: synthesis, composition, and performance.

(define (butter b sig) ; kinda pointless, but defined as a function here for run's sake (not (define butter filter))
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

(define* (make-iir-low-pass-2 fc :optional din) ; din=(sqrt 2.0) for example (suggested range 0.2.. 10)
  (let* ((theta (/ (* 2 pi fc) (mus-srate)))
	 (d (or din (sqrt 2.0)))
	 (beta (* 0.5 (/ (- 1.0 (* (/ d 2) (sin theta)))
			 (+ 1.0 (* (/ d 2) (sin theta))))))
	 (gamma (* (+ 0.5 beta) (cos theta)))
	 (alpha (* 0.5 (+ 0.5 beta (- gamma)))))
    (make-filter 3 
		 (vct alpha (* 2.0 alpha) alpha)
		 (vct 0.0 (* -2.0 gamma) (* 2.0 beta)))))

(define* (make-iir-high-pass-2 fc :optional din)
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

(define* (make-eliminate-hum :optional (hum-freq 60.0) (hum-harmonics 5) (bandwidth 10))
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


(define+ (cascade->canonical A)
  "(cascade->canonical A) converts a list of cascade coeffs (vcts with 3 entries) to canonical form"
  ;; from Orfanidis "Introduction to Signal Processing"

  (define (conv M h L x y)
    ;; x * h -> y
    (do ((n 0 (1+ n)))
	((= n (+ L M)))
      (vct-set! y n 0.0)
      (do ((m (max 0 (- n (+ 1 L))) (1+ m)))  ; m always starts at 0 here since the other expression is always <= 0
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
  "(make-butter-lp M fc) returns a butterworth low-pass filter; its order is 'M' * 2, 'fc' is the cutoff frequency in Hz"
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
		 (cascade->canonical xcoeffs)
		 (cascade->canonical ycoeffs))))
	 
(define (make-butter-hp M fc)
  "(make-butter-hp M fc) returns a butterworth high-pass filter; its order is 'M' * 2, 'fc' is the cutoff frequency in Hz"
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
		 (cascade->canonical xcoeffs)
		 (cascade->canonical ycoeffs))))
	 
(define (make-butter-bp M f1 f2)
  "(make-butter-bp M f1 f2) returns a butterworth band-pass filter; its order is 'M' * 2, 'f1' and 'f2' are the band edge frequencies in Hz"
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
		 (cascade->canonical xcoeffs)
		 (cascade->canonical ycoeffs))))
	 
(define (make-butter-bs M f1 f2)
  "(make-butter-bs M f1 f2) returns a butterworth band-stop filter; its order is 'M' * 2, 'f1' and 'f2' are the band edge frequencies in Hz"
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
		 (cascade->canonical xcoeffs)
		 (cascade->canonical ycoeffs))))
	 

;;; -------- notch filters

(define* (make-notch-frequency-response cur-srate freqs :optional (notch-width 2))
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

(define* (notch-channel freqs :optional (filter-order #f) beg dur snd chn edpos (truncate #t) (notch-width 2))
  "(notch-channel freqs :optional (filter-order #f) beg dur snd chn edpos (truncate #t) (notch-width 2)) -> notch filter removing freqs"
  (filter-channel (make-notch-frequency-response (exact->inexact (srate snd)) freqs notch-width)
		  (or filter-order (expt 2 (inexact->exact (ceiling (/ (log (/ (srate snd) notch-width)) (log 2.0))))))
		  beg dur snd chn edpos truncate
		  (format #f "notch-channel '~A ~A ~A ~A" freqs filter-order beg dur)))

(define* (notch-sound freqs :optional filter-order snd chn (notch-width 2))
  "(notch-sound freqs :optional filter-order snd chn (notch-width 2)) -> notch filter removing freqs"
  (filter-sound (make-notch-frequency-response (exact->inexact (srate snd)) freqs notch-width)
		(or filter-order (expt 2 (inexact->exact (ceiling (/ (log (/ (srate snd) notch-width)) (log 2.0))))))
		snd chn #f
		(format #f "notch-channel '~A ~A 0 #f" freqs filter-order)))

(define* (notch-selection freqs :optional filter-order (notch-width 2))
  "(notch-selection freqs :optional filter-order (notch-width 2)) -> notch filter removing freqs"
  (if (selection?)
      (filter-selection (make-notch-frequency-response (exact->inexact (selection-srate)) freqs notch-width)
			(or filter-order (expt 2 (inexact->exact (ceiling (/ (log (/ (selection-srate) notch-width)) (log 2.0)))))))))


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
  ;;   (z-transform data n (exp (make-rectangular 0.0 (* (/ 2.0 n) pi))))
  "(z-transform data n z) performs a Z transform on data; if z=e^2*pi*j/n you get a Fourier transform; complex results in returned vector"
  (let ((res (make-vector n)))
    (do ((w 0 (1+ w)))
	((= w n))
      (let ((sum 0.0)
	    (t 1.0)
	    (m (expt z w)))
	;; -w?? there seems to be confusion here -- slowzt.cc in the fxt package uses +w
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
  "(find-sine freq beg dur) returns the amplitude and initial-phase (for sin) at freq"
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

(define* (goertzel freq :optional beg dur)
  "(goertzel freq :optional beg dur) returns the amplitude of the 'freq' spectral component"
  (let* ((sr (srate))
	 (y2 0.0)
	 (y1 0.0)
	 (y0 0.0)
	 (rfreq (/ (* 2.0 pi freq) sr))
	 (cs (* 2.0 (cos rfreq))))
    (scan-channel (lambda (y)
		   (set! y2 y1)
		   (set! y1 y0)
		   (set! y0 (+ (- (* y1 cs) y2) y))
		   #f)
		  (or beg 0) (or dur (frames)))
    (magnitude (- y0 (* y1 (exp (make-rectangular 0.0 (- rfreq))))))))


(define (make-spencer-filter)
  "(make-spencer-filter) is a version of make-fir-filter; it returns one of the standard smoothing filters from \
the era when computers were human beings"
  (make-fir-filter 15 (apply vct (map (lambda (n) (/ n 320.0)) (list -3 -6 -5 3 21 46 67 74 67 46 21 3 -5 -6 -3)))))


;;; -------- any-random
;;;
;;; arbitrary random number distributions via the "rejection method"

(define* (any-random amount :optional e)
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

;(map-channel (lambda (y) (any-random 1.0 '(0 1 1 1)))) ; uniform distribution
;(map-channel (lambda (y) (any-random 1.0 '(0 0 0.95 0.1 1 1)))) ; mostly toward 1.0
;(let ((g (gaussian-distribution 1.0))) (map-channel (lambda (y) (any-random 1.0 g))))
;(let ((g (pareto-distribution 1.0))) (map-channel (lambda (y) (any-random 1.0 g))))

;;; this is the inverse integration function used by CLM to turn a distribution function into a weighting function

(if (not (provided? 'snd-env.scm)) (load-from-path "env.scm"))

(define* (inverse-integrate dist :optional (data-size 512) (e-size 50))
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

(define* (channel-mean :optional snd chn)            ; <f, 1> / n
  "(channel-mean :optional snd chn) returns the average of the samples in the given channel: <f,1>/n"
  (let ((sum 0.0)
	(N (frames snd chn)))
    (scan-channel (lambda (y) (set! sum (+ sum y)) #f) 0 N snd chn)
    (/ sum N)))

(define* (channel-total-energy :optional snd chn)    ; <f, f>
  "(channel-total-energy :optional snd chn) returns the sum of the squares of all the samples in the given channel: <f,f>"
  (let ((sum 0.0))
    (scan-channel (lambda (y) (set! sum (+ sum (* y y))) #f) 0 (frames snd chn) snd chn)
    sum))

(define* (channel-average-power :optional snd chn)   ; <f, f> / n
  "(channel-average-power :optional snd chn) returns the average power in the given channel: <f,f>/n"
  (/ (channel-total-energy snd chn) (frames snd chn)))

(define* (channel-rms :optional snd chn)             ; sqrt(<f, f> / n)
  "(channel-rms :optional snd chn) returns the RMS value of the samples in the given channel: sqrt(<f,f>/n)"
  (sqrt (channel-average-power snd chn)))

(define* (channel-variance :optional snd chn) ; "sample-variance" might be better, <f, f> - (<f, 1> / n) ^ 2 with quibbles
  "(channel-variance :optional snd chn) returns the sample variance in the given channel: <f,f>-((<f,1>/ n)^2"
  (let* ((N (frames snd chn))
	 (mu (* (/ N (- N 1)) (channel-mean snd chn))) ; avoid bias sez JOS
	 (P (channel-total-energy snd chn)))
    (- P (* mu mu))))

(define* (channel-norm :optional snd chn)            ; sqrt(<f, f>)
  "(channel-norm :optional snd chn) returns the norm of the samples in the given channel: sqrt(<f,f>)"
  (sqrt (channel-total-energy snd chn)))

(define* (channel-lp u-p :optional snd chn)
  "(channel-lp p :optional snd chn) returns the Lp norm of the samples in the given channel"
  (let ((sum 0.0)
	(p u-p) ; for the optimizer's benefit -- it can't find define* args
	(N (frames snd chn)))
    (scan-channel (lambda (y) (set! sum (+ sum (expt (abs y) p))) #f) 0 N snd chn)
    (expt sum (/ 1.0 p))))

(define* (channel-lp-inf :optional snd chn)
  "(channel-lp-inf :optional snd chn) returns the maxamp in the given channel (the name is just math jargon for maxamp)"
  (let ((mx 0.0)
	(N (frames snd chn)))
    (scan-channel (lambda (y) (set! mx (max mx (abs y))) #f) 0 N snd chn)
    mx))

(define (channel2-inner-product s1 c1 s2 c2)         ; <f, g>
  "(channel2-inner-product s1 c1 s2 c2) returns the inner-product of the two channels: <f,g>"
  (let ((N (frames s1 c1))
	(sum 0.0)
	(r1 (make-sample-reader 0 s1 c1))
	(r2 (make-sample-reader 0 s2 c2)))
    (do ((i 0 (1+ i)))
	((= i N))
      (set! sum (+ sum (* (r1) (r2)))))
    sum))

(define (channel2-angle s1 c1 s2 c2)                 ; acos(<f, g> / (sqrt(<f, f>) * sqrt(<g, g>)))
  "(channel2-angle s1 c1 s2 c2) treats the two channels as vectors, returning the 'angle' between them: acos(<f,g>/(sqrt(<f,f>)*sqrt(<g,g>)))"
  (let ((inprod (channel2-inner-product s1 c1 s2 c2))
	(norm1 (channel-norm s1 c1))
	(norm2 (channel-norm s2 c2)))
    (acos (/ inprod (* norm1 norm2)))))

(define (channel2-orthogonal? s1 c1 s2 c2)           ; <f, g> == 0
  "(channel2-orthogonal? s1 c1 s2 c2) returns #t if the two channels' inner-product is 0: <f,g>==0"
  (= (channel2-inner-product s1 c1 s2 c2) 0.0))

(define (channel2-coefficient-of-projection s1 c1 s2 c2) ; s1,c1 = x, s2,c2 = y, <f, g> / <f, f>
  "(channel2-coefficient-of-projection s1 c1 s2 c2) returns <f,g>/<f,f>"
  (/ (channel2-inner-product s1 c1 s2 c2)
     (channel-total-energy s1 c1)))

;;; -------- end of JOS stuff --------


(define (channel-distance s1 c1 s2 c2)               ; sqrt(<f - g, f - g>)
  "(channel-distance s1 c1 s2 c2) returns the euclidean distance between the two channels: sqrt(<f-g,f-g>)"
  (let* ((r1 (make-sample-reader 0 s1 c1))
	 (r2 (make-sample-reader 0 s2 c2))
	 (sum 0.0)
	 (N (min (frames s1 c1) (frames s2 c2))))
    (do ((i 0 (1+ i)))
	((= i N))
      (let ((diff (- (r1) (r2))))
	(set! sum (+ sum (* diff diff)))))
    (sqrt sum)))


(define (periodogram N)
  "(periodogram N) displays an 'N' point Bartlett periodogram of the samples in the current channel"
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

(define* (shift-channel-pitch freq :optional (order 40) (beg 0) dur snd chn edpos)
  "(shift-channel-pitch freq :optional (order 40) (beg 0) dur snd chn edpos) uses the ssb-am CLM generator to \
shift the given channel in pitch without changing its length.  The higher 'order', the better usually."
  ;; higher order = better cancellation
  (let* ((gen (make-ssb-am freq order)))
    (map-channel (lambda (y) 
		   (ssb-am gen y)) 
		 beg dur snd chn edpos 
		 (format #f "shift-channel-pitch ~A ~A ~A ~A" freq order beg dur))))

(define (hz->2pi freq)
  "(hz->2pi freq) is like hz->radians but uses the current sound's srate, not mus-srate"
  (/ (* 2 pi freq) (srate))) 

(define* (ssb-bank old-freq new-freq pairs-1 :optional (order 40) (bw 50.0) (beg 0) dur snd chn edpos)
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
	      sum))
	  beg dur snd chn edpos)
	 (scale-channel (/ mx nmx) beg dur snd chn))) ; not edpos here -- we're scaling the new stuff
     (format #f "ssb-bank ~A ~A ~A ~A ~A ~A ~A" old-freq new-freq pairs-1 order bw beg dur))))

(define* (ssb-bank-env old-freq new-freq freq-env pairs-1 :optional (order 40) (bw 50.0) (beg 0) dur snd chn edpos)
  ;; this version adds a frequency envelope
  ;; (ssb-bank-env 557 880 '(0 0 1 100.0) 7)
  (let* ((pairs pairs-1) ; for run's benefit
	 (ssbs (make-vector pairs))
	 (bands (make-vector pairs))
	 (factor (/ (- new-freq old-freq) old-freq))
	 (frenvs (make-vector pairs))
	 (mx (maxamp)))
    (do ((i 1 (1+ i)))
	((> i pairs))
      (let* ((aff (* i old-freq))
	     (bwf (* bw (+ 1.0 (/ i (* 2 pairs))))))
	(vector-set! ssbs (1- i) (make-ssb-am (* i factor old-freq)))
	(vector-set! bands (1- i) (make-bandpass (hz->2pi (- aff bwf)) 
						 (hz->2pi (+ aff bwf)) 
						 order))
	(vector-set! frenvs (1- i) (make-env freq-env 
					     :scaler (hz->radians (exact->inexact i)) 
					     :end (1- (frames))))))
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
						   y)
					 (env (vector-ref frenvs i))))))
	      (set! nmx (max nmx (abs sum)))
	      sum))
	  beg dur snd chn edpos)
	 (scale-channel (/ mx nmx) beg dur snd chn)))
     (format #f "ssb-bank-env ~A ~A '~A ~A ~A ~A ~A ~A" old-freq new-freq freq-env pairs-1 order bw beg dur))))

#|
(define* (repitch-sound old-freq new-freq)
  (ssb-bank old-freq new-freq 10))

(define* (retime-sound new-time)
  (let* ((old-time (/ (frames) (srate)))
	 (factor (/ new-time old-time)))
    (ssb-bank 557 (* 557 factor) 10)
    (src-sound (/ 1.0 factor))))


;; echoes with each echo at a new pitch via ssb-am etc

(define* (make-transposer old-freq new-freq pairs :optional (order 40) (bw 50.0))
  (let* ((ssbs (make-vector pairs))
	 (bands (make-vector pairs))
	 (factor (/ (- new-freq old-freq) old-freq)))
    (do ((i 1 (1+ i)))
	((> i pairs))
      (let* ((aff (* i old-freq))
	     (bwf (* bw (+ 1.0 (/ i (* 2 pairs))))))
	(vector-set! ssbs (1- i) (make-ssb-am (* i factor old-freq)))
	(vector-set! bands (1- i) (make-bandpass (hz->radians (- aff bwf)) 
						 (hz->radians (+ aff bwf)) 
						 order))))
    (list ssbs bands)))

(define (transpose transposer input)
  (let* ((sum 0.0)
	 (ssbs (car transposer))
	 (bands (cadr transposer))
	 (pairs (vector-length ssbs)))
    (do ((i 0 (1+ i)))
	((= i pairs) sum)
      (set! sum (+ sum (ssb-am (vector-ref ssbs i) 
			       (bandpass (vector-ref bands i) 
					 input)))))))

(define (fdelay gen input)
  (gen input))	

(define (make-fdelay len pitch scaler)
  (let ((dly (make-delay len))
        (ssb (make-transposer 440.0 (* 440.0 pitch) 10)))
    (lambda (input)
      (delay dly (+ input (* scaler (transpose ssb (tap dly))))))))


(define (transposed-echo pitch scaler secs)
  (let ((del (make-fdelay (inexact->exact (round (* secs (srate)))) pitch scaler)))
    (map-channel (lambda (y) (fdelay del y)))))

|#


#|
;;; a "bump function" (Stein and Shakarchi)
(define (bumpy)
  (let* ((x 0.0) 
	 (xi (/ 1.0 (frames)))
	 (start 0)
	 (end 1)
	 (scl (exp (/ 4.0 (- end start))))) ; normalize it
    (map-channel (lambda (y) 
		   (let ((val (if (and (>= x start)
				       (<= x end))
				  (* (exp (/ -1.0 (- x start))) 
				     (exp (/ -1.0 (- end x))))
				  0.0)))
		     (set! x (+ x xi))
		     (* scl val))))))
|#


;;; vct|channel|spectral-polynomial

(define (vct-polynomial v coeffs)
  ;; Horner's rule applied to entire vct
  (let* ((v-len (vct-length v))
	 (num-coeffs (vct-length coeffs))
	 (new-v (make-vct v-len (vct-ref coeffs (1- num-coeffs)))))
    (do ((i (- num-coeffs 2) (1- i)))
	((< i 0))
      (vct-offset! (vct-multiply! new-v v) (vct-ref coeffs i)))
    new-v))

(define* (channel-polynomial coeffs :optional snd chn)
  (let ((len (frames snd chn)))
    (vct->channel 
     (vct-polynomial 
      (channel->vct 0 len snd chn) 
      coeffs) 
     0 len snd chn #f (format #f "channel-polynomial ~A" (vct->string coeffs)))))

;;; (channel-polynomial (vct 0.0 .5)) = x*.5
;;; (channel-polynomial (vct 0.0 1.0 1.0 1.0)) = x*x*x + x*x + x

;;; convolution -> * in freq

(define* (spectral-polynomial coeffs :optional snd chn)
  (let* ((len (frames snd chn))
	 (sound (channel->vct 0 len snd chn))
	 (num-coeffs (vct-length coeffs))
	 (fft-len (if (< num-coeffs 2) 
		      len 
		      (expt 2 (inexact->exact (ceiling (/ (log (* (1- num-coeffs) len)) (log 2)))))))
	 (rl1 (make-vct fft-len 0.0))
	 (rl2 (make-vct fft-len 0.0))
	 (new-sound (make-vct fft-len)))
    (if (> (vct-ref coeffs 0) 0.0)
	(let ((dither (vct-ref coeffs 0)))
	  (do ((i 0 (1+ i)))
	      ((= i fft-len))
	    (vct-set! new-sound i (mus-random dither)))))
    (if (> num-coeffs 1)
	(begin
	  (vct-add! new-sound (vct-scale! (vct-copy sound) (vct-ref coeffs 1)))
	  (if (> num-coeffs 2)
	      (let ((peak (maxamp snd chn)))
		(vct-add! (vct-scale! rl1 0.0) sound)
		(do ((i 2 (1+ i)))
		    ((= i num-coeffs))
		  (convolution rl1 (vct-add! (vct-scale! rl2 0.0) sound) fft-len)
		  (let ((pk (vct-peak rl1)))
		    (vct-add! new-sound (vct-scale! (vct-copy rl1) (/ (* (vct-ref coeffs i) peak) pk)))))
		(let ((pk (vct-peak new-sound)))
		  (vct-scale! new-sound (/ peak pk)))))))
    (vct->channel new-sound 0 (max len (* len (1- num-coeffs))) snd chn #f (format #f "spectral-polynomial ~A" (vct->string coeffs)))))


;;; ----------------
;;; SCENTROID
;;;
;;; by Bret Battey
;;; Version 1.0 July 13, 2002
;;; translated to Snd/Scheme Bill S 19-Jan-05
;;;
;;; Returns the continuous spectral centroid envelope of a sound.
;;; The spectral centroid is the "center of gravity" of the spectrum, and it
;;; has a rough correlation to our sense of "brightness" of a sound. 
;;;
;;; [Beauchamp, J., "Synthesis by spectral amplitude and 'brightness' matching
;;; analyzed musical sounds". Journal of Audio Engineering Society 30(6), 396-406]
;;;
;;; The formula used is:
;;;    C = [SUM<n=1toj>F(n)A(n)] / [SUM<n=1toj>A(n)]
;;;    Where j is the number of bins in the analysis, 
;;;    F(n) is the frequency of a given bin,
;;;    A(n) is the magnitude of the given bin.
;;;
;;; If a pitch envelope for the analyzed sound is available, the results
;;; of SCENTROID can be used with the function NORMALIZE-CENTROID, below, 
;;; to provide a "normalized spectral centroid". 
;;;
;;; DB-FLOOR -- Frames below this decibel level (0 dB = max) will be discarded
;;; and returned with spectral centroid = 0
;;;
;;; RFREQ -- Rendering frequency. Number of  measurements per second.
;;;
;;; FFTSIZE -- FFT window size. Must be a power of 2. 4096 is recommended.

(define* (scentroid file :key (beg 0.0) dur (db-floor -40.0) (rfreq 100.0) (fftsize 4096))
  "(scentroid file :key (beg 0.0) dur (db-floor -40.0) (rfreq 100.0) (fftsize 4096)) returns the spectral centroid envelope of a sound; 'rfreq' is \
the rendering frequency, the number of measurements per second; 'db-floor' is the level below which data will be ignored"
  (let* ((fsr (mus-sound-srate file))
	 (incrsamps (inexact->exact (floor (/ fsr rfreq))))
	 (start (inexact->exact (floor (* beg fsr))))
	 (end (+ start (if dur (inexact->exact (* dur fsr)) (- (mus-sound-frames file) beg))))
	 (fdr (make-vct fftsize))
	 (fdi (make-vct fftsize))
	 (windows (1+ (inexact->exact (floor (/ (- end start) incrsamps)))))
	 (results (make-vct windows))
	 (fft2 (inexact->exact (floor (/ fftsize 2))))
	 (binwidth (exact->inexact (/ fsr fftsize)))
	 (rd (make-readin file)))
    (run
     (lambda ()
       (do ((i start (+ i incrsamps))
	    (loc 0 (1+ loc)))
	   ((>= i end) results)
	 (set! (mus-location rd) i)
	 (let ((sum-of-squares 0.0))
	   (do ((j 0 (1+ j)))
	       ((= j fftsize))
	     (let ((val (readin rd)))
	       (set! sum-of-squares (+ sum-of-squares (* val val)))
	       (vct-set! fdr j val)))
	   (if (>= (linear->db (sqrt (/ sum-of-squares fftsize))) db-floor)
	       (let ((numsum 0.0)
		     (densum 0.0))
		 (clear-array fdi)
		 (mus-fft fdr fdi fftsize)
		 (rectangular->polar fdr fdi)
		 (do ((k 0 (1+ k)))
		     ((= k fft2))
		   (set! numsum (+ numsum (* k binwidth (vct-ref fdr k))))
		   (set! densum (+ densum (vct-ref fdr k))))
		 (vct-set! results loc (/ numsum densum))))))))))
	     

;;; ----------------
;;;
;;; invert-filter inverts an FIR filter
;;;
;;; say we previously filtered a sound via (filter-channel (vct .5 .25 .125))
;;;   and we want to undo it without using (undo):
;;;   (filter-channel (invert-filter (vct .5 .25 .125)))
;;;
;;; there are a million gotchas here.  The primary one is that the inverse filter
;;;   can "explode" -- the coefficients can grow without bound.  For example, any
;;;   filter returned by spectrum->coeffs above will be a problem (it always returns
;;;   a "linear phase" filter).

(define (invert-filter fcoeffs)
  "(invert-filter coeffs) tries to return an inverse filter to undo the effect of the FIR filter coeffs."
  (let* ((flen (vct-length fcoeffs))
	 (coeffs (make-vct (+ 32 flen))) ; add room for coeffs to die away
	 (order (vct-length coeffs)))
    (do ((i 0 (1+ i)))
	((= i flen))
      (vct-set! coeffs i (vct-ref fcoeffs i)))
    (let ((nfilt (make-vct order)))
      (vct-set! nfilt 0 (/ 1.0 (vct-ref coeffs 0)))
      (do ((i 1 (1+ i)))
	  ((= i order))
	(let ((sum 0.0))
	  (do ((j 0 (1+ j))
	       (k i (1- k)))
	      ((= j i))
	    (set! sum (+ sum (* (vct-ref nfilt j) (vct-ref coeffs k)))))
	  (vct-set! nfilt i (/ sum (- (vct-ref coeffs 0))))))
      nfilt)))


;;; ----------------
;;;
;;; Volterra filter
;;;
;;; one of the standard non-linear filters
;;; this version is taken from Monson Hayes "Statistical DSP and Modeling"
;;;   it is a slight specialization of the form mentioned by J O Smith and others

(define (make-volterra-filter acoeffs bcoeffs)
  "(make-volterra-filter acoeffs bcoeffs) returns a list for use with volterra-filter, producing one of the standard non-linear filters"
  (list acoeffs 
	bcoeffs 
	(make-vct (max (vct-length acoeffs) (vct-length bcoeffs)))))

(define (volterra-filter flt x)
  "(volterra-filter flt x) takes 'flt', a list returned by make-volterra-filter, and an input 'x', and returns the (non-linear filtered) result"
  (let* ((as (car flt))
	 (bs (cadr flt))
	 (xs (caddr flt))
	 (xlen (vct-length xs))
	 (x1len (vct-length as))
	 (x2len (vct-length bs))
	 (sum 0.0))
    (vct-move! xs (- xlen 1) (- xlen 2) #t)
    (vct-set! xs 0 x)
    (set! sum (dot-product as xs x1len))
    (do ((i 0 (1+ i)))
	((= i x2len))
      (do ((j i (1+ j)))
	  ((= j x2len))
	(set! sum (+ sum (* (vct-ref bs j) (vct-ref xs i) (vct-ref xs j))))))
    sum))

;;; (define flt (make-volterra-filter (vct .5 .1) (vct .3 .2 .1)))
;;; (map-channel (lambda (x) (volterra-filter flt x)))


;;; ----------------
;;;
;;; moving-max generator (the max norm, or uniform norm, infinity-norm)

(define* (make-moving-max :optional (size 128))
  "(make-moving-max (size 128) returns a moving-max generator.  The generator keeps \
a running window of the last 'size' inputs, returning the maxamp in that window."
  (let ((gen (make-delay size)))
    (set! (mus-scaler gen) 0.0)
    gen))

(define (moving-max gen y)
  "(moving-max gen input) returns the maxamp in a moving window over the last few inputs."
  (let* ((absy (abs y))
	 (mx (delay gen absy)))
    (if (>= absy (mus-scaler gen))
	(set! (mus-scaler gen) absy)
	(if (>= mx (mus-scaler gen))
	    (set! (mus-scaler gen) (vct-peak (mus-data gen)))))
    (mus-scaler gen)))


;;; ----------------
;;;
;;; moving-sum generator (the sum norm or 1-norm)

(define* (make-moving-sum :optional (size 128))
  "(make-moving-sum (size 128) returns a moving-sum generator.  The generator keeps \
a running window of the last 'size' inputs, returning the sum of the absolute values of the samples in that window."
  (let ((gen (make-moving-average size)))
    (set! (mus-increment gen) 1.0) ; this is 1/size by default
    gen))

(define (moving-sum gen y)
  "(moving-sum gen input) returns the sum of the absolute values in a moving window over the last few inputs."
  (moving-average gen (abs y)))


;;; ----------------
;;;
;;; moving-rms generator

(define* (make-moving-rms :optional (size 128))
  "(make-moving-rms (size 128) returns a moving-rms generator.  The generator keeps \
a running window of the last 'size' inputs, returning the rms of the samples in that window."
  (make-moving-average size))

(define (moving-rms gen y)
  "(moving-rms gen input) returns the rms of the values in a window over the last few inputs."
  (sqrt (moving-average gen (* y y))))


;;; ----------------
;;;
;;; moving-length generator (euclidean norm or 2-norm)

(define* (make-moving-length :optional (size 128))
  "(make-moving-length (size 128) returns a moving-length generator.  The generator keeps \
a running window of the last 'size' inputs, returning the euclidean length of the vector in that window."
  (let ((gen (make-moving-average size)))
    (set! (mus-increment gen) 1.0)
    gen))

(define (moving-length gen y)
  "(moving-length gen input) returns the length of the values in a window over the last few inputs."
  (sqrt (moving-average gen (* y y))))

#|
;; perhaps also use moving-rms gen to avoid amplifying noise-sections (or even squlech them)
(define* (agc :optional (ramp-speed .001) (window-size 512))
  (let ((maxer (make-moving-max window-size))
	(mult 1.0))
    (map-channel
     (lambda (y)
       (let* ((curmax (moving-max maxer y))
	      (diff (- 0.5 (* mult curmax)))
	      (this-incr (* diff ramp-speed)))
	 (set! mult (+ mult this-incr))
	 (* y mult))))))

;;; moving-mean = average
;;; moving-variance? = (sum of (y - average)^2) n = (average (* (- y average-overall) (- y average-overall)))
|#
	 


;;; ----------------
;;;
;;; harmonicizer (each harmonic is split into a set of harmonics via Chebyshev polynomials)
;;;   obviously very similar to ssb-bank above, but splits harmonics individually, rather than pitch-shifting them

(define* (harmonicizer freq coeffs pairs-1 :optional (order 40) (bw 50.0) (beg 0) dur snd chn edpos)
  "(harmonicizer freq coeffs pairs (order 40) (bw 50.0) (beg 0) dur snd chn edpos) splits out each harmonic \
and replaces it with the spectrum given in coeffs"
  (let* ((pairs pairs-1) ; for run's benefit
	 (bands (make-vector pairs))
	 (pcoeffs (partials->polynomial coeffs))
	 (avgs (make-vector pairs))
	 (peaks (make-vector pairs))
	 (flt (make-filter 2 (vct 1 -1) (vct 0 -0.9)))
	 (old-mx (maxamp))
	 (new-mx 0.0)
	 (ctr 40))
    (do ((i 1 (1+ i)))
	((> i pairs))
      (let* ((aff (* i freq))
	     (bwf (* bw (+ 1.0 (/ i (* 2 pairs))))))
	(vector-set! peaks (1- i) (make-moving-max 128))
	(vector-set! avgs (1- i) (make-moving-average 128))
	(vector-set! bands (1- i) (make-bandpass (hz->2pi (- aff bwf)) 
						 (hz->2pi (+ aff bwf)) 
						 order))))
    (as-one-edit
     (lambda ()
       (map-channel
	(lambda (y)
	  (let ((sum 0.0))
	    (do ((i 0 (1+ i)))
		((= i pairs))
	      (let* ((sig (bandpass (vector-ref bands i) y))
		     (mx (moving-max (vector-ref peaks i) sig)))
		(let ((amp (moving-average (vector-ref avgs i) (if (> mx 0.0) (min 100.0 (/ 1.0 mx)) 0.0))))
		  (if (> amp 0.0)
		      (set! sum (+ sum (* mx (polynomial pcoeffs (* amp sig)))))))))
	    (let ((val (filter flt sum))) ; get rid of DC
	      (set! new-mx (max new-mx (abs val)))
	      (if (= ctr 0) ; flush filter initial junk
		  val
		  (begin
		    (set! ctr (1- ctr))
		    0.0)))))
	beg dur snd chn edpos)
       (if (> new-mx 0.0)
	   (scale-channel (/ old-mx new-mx) beg dur snd chn))))))


;;; ----------------
;;;
;;; linear sampling rate conversion

(define* (linear-src-channel srinc :optional snd chn)
  "(linear-src-channel sr snd chn performs sampling rate conversion using linear interpolation."
  (let* ((rd (make-sample-reader 0 snd chn))
	 (last (rd))
	 (next (rd))
	 (intrp 0.0)
	 (sr srinc)
	 (tempfile 
	  (with-sound (:output (snd-tempnam) :srate (srate snd) :to-snd #f)
	    (run (lambda ()
		   (do ((samp 0 (1+ samp)))
		       ((sample-reader-at-end? rd))
		     (out-any samp
			      (let ((pos intrp))
				(if (>= pos 1.0)
				    (let ((num (inexact->exact (floor pos))))
				      (do ((i 0 (1+ i)))
					  ((= i num))
					(set! last next)
					(set! next (rd)))
				      (set! pos (- pos num))))
				(set! intrp (+ pos sr))
				(+ last (* pos (- next last))))
			      0 *output*))))))
	 (len (mus-sound-frames tempfile)))
    (set-samples 0 (1- len) tempfile snd chn #t "linear-src" 0 #f #t)
    ;; first #t=truncate to new length, #f=at current edpos, #t=auto delete temp file
    ))

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
	 (k (1- fftlen) (1- k)))
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


#|
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

(define (binomial n m) ; "n-choose-m" might be a better name (there are much better ways to compute this -- see below)
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
|#



;;; Mathews/Smith High-Q filter as described in http://ccrma.stanford.edu/~jos/smac03maxjos/

(def-clm-struct mflt
  (decay 0.99 :type float)
  (frequency 1000.0 :type float)
  (eps 0.0 :type float)
  (xn 0.0 :type float)
  (yn 0.0 :type float))

(def-optkey-fun (make-mfilter (decay .99) (frequency 1000.0))
  (make-mflt :decay decay 
	     :frequency frequency 
	     :eps (* 2.0 (sin (/ (* pi frequency) (mus-srate))))))

(define (mfilter-1 m x-input y-input)
  ;; no optional args, for 'run'
  (let* ((xn1 (+ x-input
		 (* (mflt-decay m) (- (mflt-xn m) 
				      (* (mflt-eps m) (mflt-yn m))))))
	 (yn1 (+ y-input
		 (* (mflt-decay m) (+ (* (mflt-eps m) xn1) (mflt-yn m))))))
    (set! (mflt-xn m) xn1)
    (set! (mflt-yn m) yn1)
    yn1))

(define* (mfilter m :optional (x-input 0.0) (y-input 0.0))
  ;; assume the "b1" business is handled by the caller
  (mfilter-1 m x-input y-input))

#|
(with-sound () 
  (let ((rd (make-sample-reader 0 "now.snd")) 
	(m (make-mfilter))) 
    (run (lambda () 
	   (do ((i 0 (1+ i))) 
	       ((= i 10000))
	     (outa i (mfilter m (* .1 (rd))) *output*))))))

;;; sweep center freq:
(with-sound () 
  (let ((rd (make-sample-reader 0 "oboe.snd")) 
        (m (make-mfilter :decay .99 :frequency 1000)) 
        (e (make-env '(0 100 1 2000) :end 10000))) 
    (run (lambda () 
	   (do ((i 0 (1+ i))) 
	       ((= i 10000))
	     (outa i (mfilter m (* .1 (rd))) *output*) 
	     (set! (mflt-eps m) (* 2.0 (sin (/ (* pi (env e)) (mus-srate))))))))))

;;; harmonics:
(with-sound (:statistics #t)
  (let* ((filters (make-vector 9))
	 (noi (make-rand 10000)))
    (do ((i 0 (1+ i)))
	((= i 9))
      (vector-set! filters i (make-mfilter :decay .999 :frequency (* 400 (+ i 1)))))
    (run
     (lambda ()
       (declare (clm-vector filters))
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (let ((sum 0.0)
	       (input (* .01 (rand noi))))
	   (do ((j 0 (1+ j)))
	       ((= j 9))
	     (set! sum (+ sum (* (/ 1.0 (+ j 1)) (mfilter (vector-ref filters j) input)))))
	   (outa i sum *output*)))))))
|#



;;; -------- spectrum displayed in various frequency scales

(define display-bark-fft
  ;; click in lisp-graph to change the tick placement choice

  (let ((bark-fft-size 0)
	(bark-tick-function 0))

    (define (bark f) 
      (let ((f2 (/ f 7500))) 
	(+ (* 13.5 (atan (* .00076 f))) (* 3.5 (atan (* f2 f2))))))
    
    (define (mel f) 
      (* 1127 (log (+ 1.0 (/ f 700.0)))))
    
    (define (erb f) 
      (+ 43.0 (* 11.17 (log (/ (+ f 312) (+ f 14675))))))
    
    (define (display-bark-fft-1 snd chn)
      (let* ((ls (left-sample snd chn))
	     (rs (right-sample snd chn))
	     (fftlen (inexact->exact (expt 2 (inexact->exact (ceiling (/ (log (1+ (- rs ls))) (log 2))))))))
	(if (> fftlen 0)
	    (let ((data (channel->vct ls fftlen snd chn))
		  (normalized (not (= (transform-normalization snd chn) dont-normalize)))
		  (linear #t))                               ; can't currently show lisp graph in dB 
	      ;; snd-axis make_axes: WITH_LOG_Y_AXIS, but LINEAR currently in snd-chn.c 3250
	      (if (vct? data)
		  (let ((fft (snd-spectrum data              ; returns fftlen / 2 data points
					   (fft-window snd chn) fftlen linear 
					   (fft-window-beta snd chn) #f normalized)))
		    (if (vct? fft)
			(let* ((sr (srate snd))
			       (mx (vct-peak fft))
			       (data-len (vct-length fft))
			       
			       ;; bark settings
			       (bark-low (floor (bark 20.0)))
			       (bark-high (ceiling (bark (* 0.5 sr))))
			       (bark-frqscl (/ data-len (- bark-high bark-low)))
			       (bark-data (make-vct data-len))
			       
			       ;; mel settings
			       (mel-low (floor (mel 20.0)))
			       (mel-high (ceiling (mel (* 0.5 sr))))
			       (mel-frqscl (/ data-len (- mel-high mel-low)))
			       (mel-data (make-vct data-len))
			       
			       ;; erb settings
			       (erb-low (floor (erb 20.0)))
			       (erb-high (ceiling (erb (* 0.5 sr))))
			       (erb-frqscl (/ data-len (- erb-high erb-low)))
			       (erb-data (make-vct data-len)))
			  
			  (set! bark-fft-size fftlen)
			  
			  (run 
			   (lambda ()
			     (do ((i 0 (1+ i)))
				 ((= i data-len))
			       (let* ((val (vct-ref fft i))
				      (frq (* sr (/ i fftlen)))
				      (bark-bin (inexact->exact (round (* bark-frqscl (- (bark frq) bark-low)))))
				      (mel-bin (inexact->exact (round (* mel-frqscl (- (mel frq) mel-low)))))
				      (erb-bin (inexact->exact (round (* erb-frqscl (- (erb frq) erb-low))))))
				 (if (and (>= bark-bin 0)
					  (< bark-bin data-len))
				     (vct-set! bark-data bark-bin (+ val (vct-ref bark-data bark-bin))))
				 (if (and (>= mel-bin 0)
					  (< mel-bin data-len))
				     (vct-set! mel-data mel-bin (+ val (vct-ref mel-data mel-bin))))
				 (if (and (>= erb-bin 0)
					  (< erb-bin data-len))
				     (vct-set! erb-data erb-bin (+ val (vct-ref erb-data erb-bin))))))
			     
			     (if normalized
				 (let ((bmx (vct-peak bark-data))
				       (mmx (vct-peak mel-data))
				       (emx (vct-peak erb-data)))
				   (if (> (abs (- mx bmx)) .01)
				       (vct-scale! bark-data (/ mx bmx)))
				   (if (> (abs (- mx mmx)) .01)
				       (vct-scale! mel-data (/ mx mmx)))
				   (if (> (abs (- mx emx)) .01)
				       (vct-scale! erb-data (/ mx emx)))))))
			  
			  (graph (list bark-data mel-data erb-data) 
				 "ignored" 
				 20.0 (* 0.5 sr) 
				 0.0 (if normalized 1.0 (* data-len (y-zoom-slider snd chn)))
				 snd chn 
				 #f show-bare-x-axis)))))))
	
	#f)) ; not pixel list or thunk
    
    (define (make-bark-labels snd chn)
      ;; at this point the x axis has no markings, but there is room for labels and ticks
      
      (let ((old-foreground-color (foreground-color snd chn copy-context)))
	;; assume at start the foreground color is correct
	
	(let* ((axinfo (axis-info snd chn lisp-graph))
	       (axis-x0 (list-ref axinfo 10))
	       (axis-x1 (list-ref axinfo 12))
	       (axis-y0 (list-ref axinfo 13))
	       (axis-y1 (list-ref axinfo 11))
	       (label-height 15)
	       (char-width 8)
	       (sr2 (* 0.5 (srate snd)))
	       (minor-tick-len 6)
	       (major-tick-len 12)
	       (tick-y0 axis-y1)
	       (minor-y0 (+ axis-y1 minor-tick-len))
	       (major-y0 (+ axis-y1 major-tick-len))
	       (bark-label-font (snd-font 3))
	       (bark-numbers-font (snd-font 2))
	       (label-pos (inexact->exact (+ axis-x0 (* .45 (- axis-x1 axis-x0))))))
	  
	  (define (scale-position scale f)
	    (let ((b20 (scale 20.0)))
	      (inexact->exact (round (+ axis-x0 
					(/ (* (- axis-x1 axis-x0) (- (scale f) b20)) 
					   (- (scale sr2) b20)))))))
	  
	  (define (bark-position f) (scale-position bark f))
	  (define (mel-position f) (scale-position mel f))
	  (define (erb-position f) (scale-position erb f))
	  
	  (define (draw-bark-ticks bark-function)
	    (if bark-numbers-font (set! (current-font snd chn copy-context) bark-numbers-font))
	    
	    (draw-line axis-x0 tick-y0 axis-x0 major-y0 snd chn copy-context)
	    (let* ((i1000 (scale-position bark-function 1000.0))
		   (i10000 (scale-position bark-function 10000.0)))
	      
	      (draw-line i1000 tick-y0 i1000 major-y0 snd chn copy-context)
	      (draw-line i10000 tick-y0 i10000 major-y0 snd chn copy-context)
	      
	      (draw-string "20" axis-x0 major-y0 snd chn copy-context)
	      (draw-string "1000" (- i1000 (* 3 4)) major-y0 snd chn copy-context)
	      (draw-string "10000" (- i10000 (* 6 4)) major-y0 snd chn copy-context)
	      
	      (draw-string (format #f "fft size: ~D" bark-fft-size) (+ axis-x0 10) axis-y0 snd chn copy-context)
	      
	      (do ((i 100 (+ i 100)))
		  ((= i 1000))
		(let* ((i100 (scale-position bark-function i)))
		  (draw-line i100 tick-y0 i100 minor-y0 snd chn copy-context)))
	      
	      (do ((i 2000 (+ i 1000)))
		  ((= i 10000))
		(let* ((i1000 (scale-position bark-function i)))
		  (draw-line i1000 tick-y0 i1000 minor-y0 snd chn copy-context)))))

	  ;; bark label/ticks
	  (if (= bark-tick-function 0) (draw-bark-ticks bark-position))
	  (if bark-label-font (set! (current-font snd chn copy-context) bark-label-font))
	  (draw-string "bark," label-pos (+ axis-y1 label-height) snd chn copy-context)
	  
	  ;; mel label/ticks
	  (set! (foreground-color snd chn copy-context) (snd-color 2))
	  (if (= bark-tick-function 1) (draw-bark-ticks mel-position))
	  (if bark-label-font (set! (current-font snd chn copy-context) bark-label-font))
	  (draw-string "mel," (+ (* char-width 6) label-pos) (+ axis-y1 label-height) snd chn copy-context)
	  
	  ;; erb label/ticks
	  (set! (foreground-color snd chn copy-context) (snd-color 4))
	  (if (= bark-tick-function 2) (draw-bark-ticks erb-position))
	  (if bark-label-font (set! (current-font snd chn copy-context) bark-label-font))
	  (draw-string "erb" (+ (* char-width (+ 6 5)) label-pos) (+ axis-y1 label-height) snd chn copy-context))
	
	(set! (foreground-color snd chn copy-context) old-foreground-color)))
    
    ;; mouse click = move to next scale's ticks
    (define (choose-bark-ticks snd chn button state x y axis)
      (if (= axis lisp-graph)
	  (begin
	    (set! bark-tick-function (1+ bark-tick-function))
	    (if (> bark-tick-function 2)
		(set! bark-tick-function 0))
	    (update-lisp-graph snd chn))))
    
    ;; user's view of display-bark-fft function
    (lambda* (:optional off)
      (if (not off)
	  (begin
	    (add-hook! lisp-graph-hook display-bark-fft-1)
	    (add-hook! after-lisp-graph-hook make-bark-labels)
	    (add-hook! mouse-click-hook choose-bark-ticks)
	    (for-each (lambda (snd)
			(do ((c 0 (1+ c)))
			    ((= c (chans snd)))
			  (update-lisp-graph snd c)))
		      (sounds)))
	  (begin
	    (remove-hook! lisp-graph-hook display-bark-fft-1)
	    (remove-hook! after-lisp-graph-hook make-bark-labels)
	    (remove-hook! mouse-click-hook choose-bark-ticks)
	    (for-each (lambda (snd)
			(do ((c 0 (1+ c)))
			    ((= c (chans snd)))
			  (set! (lisp-graph? snd c) #f)))
		      (sounds)))))))

(define (undisplay-bark-fft) (display-bark-fft #t))



;;; -------- lpc-coeffs, lpc-predict

(define (lpc-coeffs data n m)
  ;; translated and changed to use 0-based arrays from memcof of NRinC
  ;; not vcts except incoming data here because we need double precision

  "(lpc-coeffs data n m) returns 'm' LPC coeffients (in a vector) given 'n' data points in the vct 'data'"

  (letrec ((sqr (lambda (x) (* x x))))
    (let ((d (make-vector m 0.0))
	  (wk1 (make-vector n 0.0))
	  (wk2 (make-vector n 0.0))
	  (wkm (make-vector n 0.0)))
      (vector-set! wk1 0 (vct-ref data 0))
      (vector-set! wk2 (- n 2) (vct-ref data (1- n)))
      (do ((j 1 (1+ j)))
	  ((= j (1- n)))
	(vector-set! wk1 j (vct-ref data j))
	(vector-set! wk2 (1- j) (vct-ref data j)))
      (do ((k 0 (1+ k)))
	  ((= k m) d)
	(let ((num 0.0)
	      (denom 0.0))
	  (do ((j 0 (1+ j)))
	      ((= j (- n k 1)))
	    (set! num (+ num (* (vector-ref wk1 j) (vector-ref wk2 j))))
	    (set! denom (+ denom (sqr (vector-ref wk1 j)) (sqr (vector-ref wk2 j)))))
	  (if (not (= denom 0.0))
	      (vector-set! d k (/ (* 2.0 num) denom)))
	  (do ((i 0 (1+ i)))
	      ((= i k)) ; 1st time is skipped presumably
	    (vector-set! d i (- (vector-ref wkm i) (* (vector-ref d k) (vector-ref wkm (- k i 1))))))
	  (if (< k (1- m))
	      (begin
		(do ((i 0 (1+ i)))
		    ((= i (1+ k)))
		  (vector-set! wkm i (vector-ref d i)))
		(do ((j 0 (1+ j)))
		    ((= j (- n k 2)))
		  (vector-set! wk1 j (- (vector-ref wk1 j) (* (vector-ref wkm k) (vector-ref wk2 j))))
		  (vector-set! wk2 j (- (vector-ref wk2 (1+ j)) (* (vector-ref wkm k) (vector-ref wk1 (1+ j)))))))))))))
  
(define* (lpc-predict data n coeffs m nf :optional clipped)
  ;; translated and changed to use 0-based arrays from predic of NRinC
  ;; incoming coeffs are assumed to be in a vector (from lpc-coeffs)

  "(lpc-predict data n coeffs m nf :optional clipped) takes the output of lpc-coeffs ('coeffs', a vector) and the length thereof ('m'), \
'n' data points of 'data' (a vct), and produces 'nf' new data points (in a vct) as its prediction. If 'clipped' is #t, the new data \
is assumed to be outside -1.0 to 1.0."

  (let ((future (make-vct nf 0.0))
	(reg (make-vct m 0.0)))
    (do ((i 0 (1+ i))
	 (j (1- n) (1- j)))
	((= i m))
	(vct-set! reg i (vct-ref data j)))
    (do ((j 0 (1+ j)))
	((= j nf) future)
      (let ((sum 0.0))
	(do ((k 0 (1+ k)))
	    ((= k m))
	  (set! sum (+ sum (* (vector-ref coeffs k) (vct-ref reg k)))))
	(do ((k (1- m) (1- k)))
	    ((= k 0))
	  (vct-set! reg k (vct-ref reg (1- k))))

	;; added this block
	(if clipped
	    (if (> sum 0.0)
		(if (< sum 1.0)
		    (set! sum 1.0))
		(if (> sum -1.0)
		    (set! sum -1.0))))

	(vct-set! reg 0 sum)
	(vct-set! future j sum)))))



;;; -------- unclip-channel

(define* (unclip-channel :optional snd chn)
  "(unclip-channel :optional snd chn) looks for clipped portions and tries to reconstruct the original using LPC"
  (let* ((clip-size 256)                        ; current clip-data size
	 (clip-data (make-vector clip-size 0))  ; clipped portion begin and end points
	 (clips 0)                              ; number of clipped portions * 2
	 (unclipped-max 0.0))

    ;; find clipped portions
    (let ((clip-beg 0)
	  (clip-end 0)
	  (in-clip #f)
	  (samp 0))
      (scan-channel
       (lambda (y)
	 (let ((absy (abs y)))
	   (if (> absy .9999)                    ; this sample is clipped
	       (if (not in-clip)
		   (begin                        ;   start a new clipped portion
		     (set! in-clip #t)
		     (set! clip-beg samp)))
	       (begin                            ; not clipped
		 (set! unclipped-max (max unclipped-max absy))
		 (if in-clip                     ; if we were in a clipped portion
		     (begin                      ;   save the bounds in clip-data
		       (set! in-clip #f)
		       (vector-set! clip-data clips clip-beg)
		       (vector-set! clip-data (1+ clips) (1- samp))
		       (set! clips (+ clips 2))
		       (if (>= clips clip-size)    ; clip-data full -- make more room
			   (let* ((old-size clip-size)
				  (old-data clip-data))
			     (set! clip-size (* 2 clip-size))
			     (set! clip-data (make-vector clip-size 0))
			     (do ((i 0 (1+ i)))
				 ((= i old-size))
			       (vector-set! clip-data i (vector-ref old-data i)))))))))
	   (set! samp (1+ samp))
	   #f))
       0 (frames snd chn) snd chn))

    ;; try to restore clipped portions
    (if (> clips 0)                             ; we did find clipped portions
	(let ((min-data-len 32)
	      (max-diff 0.0)
	      (max-len 0))
	  (as-one-edit
	   (lambda ()
	     (do ((clip 0 (+ clip 2)))               ;   so go through all...
		 ((>= clip clips))
	       (let* ((clip-beg (vector-ref clip-data clip))  ; clip-beg to clip-end inclusive are clipped
		      (clip-end (vector-ref clip-data (1+ clip)))
		      (clip-len (1+ (- clip-end clip-beg)))
		      (data-len (max min-data-len (* clip-len 4))))

		 (if (> clip-len max-len) 
		     (set! max-len clip-len))

		 (let ((forward-data-len data-len)
		       (backward-data-len data-len)
		       (previous-end (if (= clip 0) 0 (vector-ref clip-data (1- clip))))
		       (next-beg (if (< clip (- clips 3)) (vector-ref clip-data (+ clip 2)) (frames snd chn))))

		   (if (< (- clip-beg data-len) previous-end)  ; current beg - data collides with previous
		       (begin
			 ;; (snd-display ";[~A] collision at ~A -> [~A : ~A]" clip previous-end clip-beg clip-end)
			 (set! forward-data-len (max 4 (- clip-beg previous-end)))))

		   (if (> (+ clip-end data-len) next-beg)    ; current end + data collides with next
		       (begin
			 ;; (snd-display ";[~A] collision at [~A : ~A] -> ~A" clip clip-beg clip-end next-beg)
			 (set! backward-data-len (max 4 (- next-beg clip-end)))))

		   (let ((forward-predict-len (max clip-len (inexact->exact (floor (/ forward-data-len 2)))))
			 (backward-predict-len (max clip-len (inexact->exact (floor (/ backward-data-len 2))))))

		     ;; use LPC to reconstruct going both forwards and backwards

		     (let* ((data (channel->vct (- clip-beg forward-data-len) forward-data-len snd chn))
			    (future (lpc-predict 
				     data forward-data-len 
				     (lpc-coeffs data forward-data-len forward-predict-len)
				     forward-predict-len
				     clip-len #f))

			    (rdata (vct-reverse! (channel->vct (1+ clip-end) backward-data-len snd chn)))
			    (past (lpc-predict 
				   rdata backward-data-len 
				   (lpc-coeffs rdata backward-data-len backward-predict-len)
				   backward-predict-len
				   clip-len #f))

			    (new-data (make-vct clip-len 0.0)))

		       (if (> clip-len 1)
			   (do ((i 0 (1+ i))
				(j (1- clip-len) (1- j)))
			       ((= i clip-len))
			     (let* ((sn (* 0.5 (+ 1.0 (cos (* pi (/ i (1- clip-len))))))))
			       (vct-set! new-data i (+ (* sn 
							  (vct-ref future i))
						       (* (- 1.0 sn) 
							  (vct-ref past j))))))

			   ;; todo perhaps move this mix dependent on data-lens?
			   ;; todo perhaps special case for 2 samps (what if both 1.0 for example?)
			   ;; todo perhaps if multichannel and channels are correlated and one is not clipped -- use
			   ;;   its data to help reconstruct clipped case?

			   (vct-set! new-data 0 (if (> (vct-ref future 0) 0.0)
						    (max (vct-ref future 0) (vct-ref past 0))
						    (min (vct-ref future 0) (vct-ref past 0)))))

		       ;; write reconstruction
		       (vct->channel new-data clip-beg clip-len snd chn))))))))

	  (if (> unclipped-max .95) (set! unclipped-max .999))
	  (scale-channel (/ unclipped-max (maxamp snd chn)) 0 (frames snd chn) snd chn)
	  (list 'max unclipped-max 'clips (/ clips 2) 'max-len max-len))

    'no-clips)))

(define* (unclip-sound :optional snd)
  "(unclip-sound :optional snd) applies unclip-channel to each channel of 'snd'."
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (not (sound? index))
	(throw 'no-such-sound (list "unclip-sound" snd))
	(let ((chns (chans index)))
	  (do ((chn 0 (1+ chn)))
	      ((= chn chns))
	    (unclip-channel index chn))))))

