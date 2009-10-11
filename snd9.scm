;;; backwards compatibility for snd 9

(provide 'snd-snd9.scm)


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
       (/ (- 1.0 r) ; amplitude normalization (not vital)
	  r2))))

(define make-cosine-summation make-oscil)

;;; (let ((gen (make-cosine-summation 100.0))) (map-channel (lambda (y) (* .2 (cosine-summation gen 0.5)))))


;;; -------- kosine-summation
;;;
;;; from Askey "Ramanujan and Hypergeometric Series" in Berndt and Rankin "Ramanujan: Essays and Surveys" p283
;;;
;;; this gives a sum of cosines of decreasing amp where the "k" parameter determines
;;;   the "index" (in FM nomenclature) -- higher k = more cosines; the actual amount
;;;   of the nth cos involves hypergeometric series (looks like r^n/n! (~=e^n?) with a million other terms).

(define (kosine-summation gen r k)
  "(kosine-summation gen r k) is a variant of ncos; 'r' controls successive sinusoid amplitude; 'k' controls how many sinusoids are produced"
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
  (if (< (abs angle) 1.0e-9)
      1.0
      (let ((val (/ (sin (* 0.5 (+ n 1) angle)) 
		    (* (+ n 1) 
		       (sin (* 0.5 angle))))))
	(* val val))))

;;; here's Zygmund's version:
;;  (if (< (abs angle) 1.0e-9)
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
  (if (< (abs angle) 1.0e-9)
      1.0
      (let* ((val (/ (sin (* angle (+ n 0.5))) 
		     (* (sin (* 0.5 angle))
			(+ (* 2 n) 1))))) ; amplitude normalization -- we want a peak amp of 1.0
	(* val val))))

;;; (let ((angle 0.0)) (map-channel (lambda (y) (let ((val (legendre-sum angle 3))) (set! angle (+ angle .1)) (* .1 val)))))



;;; -------- variations on ncos
;;; from "Trigonometric Delights" by Eli Maor

(define (sum-of-n-sines angle n)
  "(sum-of-n-sines angle n) produces the sum of 'n' sines"
  (let* ((a2 (* angle 0.5))
	 (den (sin a2)))
    (if (< (abs den) 1.0e-9)
	0.0
	(/ (* (sin (* n a2)) (sin (* (+ 1 n) a2))) den))))

;;; identical to this is the "conjugate Dirichlet kernel" from "Trigonometric Series" Zygmund p49
;;;  (let* ((a2 (* 0.5 angle))
;;;	    (den (* 2 (sin a2))))
;;;    (if (< (abs den) 1.0e-9)
;;;	  0.0
;;;	  (/ (- (cos a2) (cos (* (+ n 0.5) angle))) den))))


;(let ((angle 0.0)) (map-channel (lambda (y) (let ((val (sum-of-n-sines angle 3))) (set! angle (+ angle .1)) (* .1 val)))))

(define (sum-of-n-odd-sines angle n)
  "(sum-of-n-odd-sines angle n) produces the sum of 'n' odd-numbered sines"
  (let ((den (sin angle))
	(na (sin (* n angle))))
    (if (< (abs den) 1.0e-9)
	0.0
	(/ (* na na) den))))

(define (sum-of-n-odd-cosines angle n)
  "(sum-of-n-odd-cosines angle n) produces the sum of 'n' odd-numbered cosines"
  (let ((den (* 2 (sin angle))))
    (if (< (abs den) 1.0e-9)
	(exact->inexact n) ; just guessing -- floatification is for the run macro
	(/ (sin (* 2 n angle)) den))))

;;; (Gradshteyn and Ryzhik 1.342)

;;; or take advantage of 1/(1-x):
;;; (map-channel (lambda (y) (set! i (+ 1 i)) (/ 0.001 (- 1.0 (* .99 (cos (/ (* i 2.0 pi) 100.0)))))))
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
    (if (< (abs s4) 1.0e-9)
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




(define mus-phase-vocoder-outctr
  (make-procedure-with-setter
   (lambda (gen) 
     (mus-location gen))
   (lambda (gen val) 
     (set! (mus-location gen) val))))



;;; -------- mfilter -- this has now been implemented in CLM as firmant

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
  (let ((rd (make-sampler 0 "now.snd")) 
	(m (make-mfilter))) 
    (run (lambda () 
	   (do ((i 0 (+ 1 i))) 
	       ((= i 10000))
	     (outa i (mfilter m (* .1 (rd)))))))))

;;; sweep center freq:
(with-sound () 
  (let ((rd (make-sampler 0 "oboe.snd")) 
        (m (make-mfilter :decay .99 :frequency 1000)) 
        (e (make-env '(0 100 1 2000) :length 10000))) 
    (run (lambda () 
	   (do ((i 0 (+ 1 i))) 
	       ((= i 10000))
	     (outa i (mfilter m (* .1 (rd)))) 
	     (set! (mflt-eps m) (* 2.0 (sin (/ (* pi (env e)) (mus-srate))))))))))

;;; harmonics:
(with-sound (:statistics #t)
  (let* ((filters (make-vector 9))
	 (noi (make-rand 10000)))
    (do ((i 0 (+ 1 i)))
	((= i 9))
      (vector-set! filters i (make-mfilter :decay .999 :frequency (* 400 (+ i 1)))))
    (run
     (lambda ()
       (declare (clm-vector filters))
       (do ((i 0 (+ 1 i)))
	   ((= i 10000))
	 (let ((sum 0.0)
	       (input (* .01 (rand noi))))
	   (do ((j 0 (+ 1 j)))
	       ((= j 9))
	     (set! sum (+ sum (* (/ 1.0 (+ j 1)) (mfilter (vector-ref filters j) input)))))
	   (outa i sum)))))))
|#




(define mus-formant-radius
  (make-procedure-with-setter
   (lambda (gen) 
     (mus-scaler gen))
   (lambda (gen val) 
     (set! (mus-scaler gen) val))))

(define mus-cosines
  (make-procedure-with-setter
   (lambda (gen) 
     (length gen))
   (lambda (gen val) 
     (set! (length gen) val))))


(define* (old-make-formant :optional (radius .5) (freq 0.0) gain)
  (if gain (snd-warning "make-formant gain argument is now ignored"))
  (make-formant freq radius))

  
(def-optkey-fun (make-ppolar (radius 0.9) (frequency 440.0))
  (make-two-pole :frequency frequency :radius radius))

(def-optkey-fun (make-zpolar (radius 0.9) (frequency 440.0))
  (make-two-zero :frequency frequency :radius radius))


;;; -------- smoothing-filter

;;; a simple modification is to round-off the edges:

(define make-smoothing-filter make-moving-average)

(define (smoothing-filter gen sig)
  (let* ((val (moving-average gen sig))
	 (old-sig (tap gen))
	 (n (mus-order gen)))
    (* (/ n (- n 1))   ; scale back to 1.0
       (- val (* (mus-increment gen) 0.5 (+ old-sig sig))))))


;;; -------- sine-bank

(define* (sine-bank amps phases :optional size)
  (let ((len (or size (length amps)))
	(sum 0.0))
    (do ((i 0 (+ 1 i)))
	((= i len))
      (set! sum (+ sum (* (vct-ref amps i)
			  (sin (vct-ref phases i))))))
    sum))



    