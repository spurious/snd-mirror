(provide 'snd-generators.scm)
(if (not (provided? 'snd-ws.scm)) (load-from-path "ws.scm"))


;;; these try to mimic existing gens (mainly oscil), so "frequency" and "initial-phase" are placed first
;;;   so the make function places those two args first; where a factor is involved, I'll try to use "r".
;;;   ssb cases are prefixed "ssb" and use "carfreq" and "modfreq".  Where the limit of the sum is settable,
;;;   I'll use "n".


#|
;;; do we need fmod 2*pi for the angles? (it is not used in clm.c)

:(let ((ph 0.0)) (do ((i 0 (1+ i))) ((= i 22050)) (set! ph (+ ph (hz->radians 100.0)))) ph)
628.31850751536

:(let ((ph (* 2 pi 1000000))) (do ((i 0 (1+ i))) ((= i 22050)) (set! ph (+ ph (hz->radians 100.0)))) (- ph (* 2 pi 1000000)))
628.318502381444

:(let ((ph (* 2 pi 1000000000))) (do ((i 0 (1+ i))) ((= i 22050)) (set! ph (+ ph (hz->radians 100.0)))) (- ph (* 2 pi 1000000000)))
628.311109542847

:(let ((ph (* 2 pi 1000000000000))) (do ((i 0 (1+ i))) ((= i 22050)) (set! ph (+ ph (hz->radians 100.0)))) (- ph (* 2 pi 1000000000000)))
624.462890625

;; similar results from running oscil with 0.0 initial-phase, and 2*pi*1000000000, or running one
;;   oscil for 3 hours at 6000 Hz -- the sinusoid is clean even around an angle of a billion -- worst 
;;   case increment is pi, so we get (say) a billion samples before we may notice a sag => ca. 8 hours.  
;;   I think that's a long enough tone...  (In clm.c and here, the phase and increment are both doubles;
;;   53 bits of mantissa, billion=30, so we still have about 23 bits, which actually matches results above).
|#


;;; --------------------------------------------------------------------------------

;;; sndclm.html G&R 2nd col last row (with normalization)

;;; ercos: sum of cosines with exponentially decaying amps

(def-clm-struct (ercos
		 :make-wrapper 
		 (lambda (g)
		   (set! (ercos-osc g) (make-oscil (ercos-frequency g) (ercos-initial-phase g)))
		   (if (<= (ercos-r g) 0.0) (set! (ercos-r g) 0.00001))
		   (set! (ercos-cosh-t g) (cosh (ercos-r g)))
		   (let ((exp-t (exp (- (ercos-r g)))))
		     (set! (ercos-offset g) (/ (- 1.0 exp-t) (* 2.0 exp-t)))
		     (set! (ercos-scaler g) (* (sinh (ercos-r g)) (ercos-offset g))))
		   g))
  (frequency 440.0) (initial-phase 0.0) (r 1.0 :type float)
  (osc #f :type clm) scaler offset cosh-t)


(define (ercos gen fm)
  (declare (gen ercos) (fm float))
  (- (/ (ercos-scaler gen) 
	(- (ercos-cosh-t gen) (oscil (ercos-osc gen) fm)))
     (ercos-offset gen)))


#|
(with-sound (:clipped #f)
  (let ((gen (make-ercos 100 :r 1.0)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 10000))
	(outa i (ercos gen 0.0) *output*))))))

;; change "t" during note -- smoothly changing sum-of-cosines spectra (damped "lute-stop" effect)
(with-sound (:clipped #f)  
  (let ((gen (make-ercos 100 :r 0.1))
	(t-env (make-env '(0 .1 1 2) :end 20000)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 20000))
	(set! (ercos-r gen) (env t-env))
	(set! (ercos-cosh-t gen) (cosh (ercos-r gen)))
	(let ((exp-t (exp (- (ercos-r gen)))))
	  (set! (ercos-offset gen) (/ (- 1.0 exp-t) (* 2.0 exp-t)))
	  (set! (ercos-scaler gen) (* (sinh (ercos-r gen)) (ercos-offset gen))))
	(outa i (ercos gen 0.0) *output*))))))
|#


(def-clm-struct (ssber
		 :make-wrapper
		 (lambda (g)
		   (set! (ssber-carincr g) (hz->radians (- (ssber-carfreq g) (ssber-modfreq g))))
		   (set! (ssber-modincr g) (hz->radians (ssber-modfreq g)))
		   g))
  (carfreq 0.0) (modfreq 0.0) (r 0.0)
  (carangle 0.0) (carincr 0.0) (modangle 0.0) (modincr 0.0))

(define (ssber gen fm)
  (declare (gen ssber) (fm float))
  (let* ((cx (ssber-carangle gen))
	 (mx (ssber-modangle gen))
	 (r (ssber-r gen))
	 (ccmx (- (cosh r) (cos mx))))

    (set! (ssber-carangle gen) (+ (* (/ (ssber-modincr gen) (ssber-carincr gen)) fm) cx (ssber-carincr gen)))
    (set! (ssber-modangle gen) (+ fm mx (ssber-modincr gen)))

    (/ (- (* (cos cx)
	     (- (/ (sinh r) ccmx)
		1.0))
	  (* (sin cx)
	     (/ (sin mx) ccmx)))
       (* 2.0 (- (/ 1.0 (- 1.0 (exp (- r)))) 1.0))))) ; normalization

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ssber 1000.0 100.0 1.0)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 20000))
	(outa i (ssber gen 0.0) *output*))))))
|#


;;; --------------------------------------------------------------------------------


;;; sndclm.html (G&R) 1st col 5th row (sum of odd sines)

;;; sum of n odd sines

(def-clm-struct (noddsin 
		 :make-wrapper
		 (lambda (g)
		   (if (< (noddsin-n g) 1) (set! (noddsin-n g) 1))
		   (set! (noddsin-osc g) (make-oscil 
					 (noddsin-frequency g) 
					 (noddsin-initial-phase g)))
		   (set! (noddsin-nosc g) (make-oscil 
					  (* (noddsin-n g) (noddsin-frequency g)) 
					  (* (noddsin-n g) (noddsin-initial-phase g))))
		   (set! (noddsin-norm g) (if (= (noddsin-n g) 1) 1.0
					     (/ (if (= (noddsin-n g) 2) 1.29
						    (if (= (noddsin-n g) 3) 1.34
							(if (< (noddsin-n g) 6) 1.36
							    (if (< (noddsin-n g) 18) 1.37
								1.379))))
						(noddsin-n g))))
		   g))
  (frequency 440.0) (initial-phase 0.0) (n 1 :type int)
  (osc #f :type clm) (nosc #f :type clm) (norm 0.0 :type float))

(define (noddsin gen fm)
  (declare (gen noddsin) (fm float))
  (let ((o1 (oscil (noddsin-nosc gen) (* (noddsin-n gen) fm)))
	(o2 (oscil (noddsin-osc gen) fm))) 
    (if (< (abs o2) 1.0e-9)
	0.0
	(/ (* (noddsin-norm gen) o1 o1) o2))))
	   
#|
;;; get normalization:
(do ((i 1 (1+ i))) 
    ((= i 30))
  (let ((v (with-sound (:output (make-vct 1000) :clipped #f)
		       (let ((gen (make-noddsin (radians->hz .002) :n i)))
			 (do ((k 0 (1+ k)))
			     ((= k 1000))
			   (outa k (noddsin gen 0.0) *output*))))))
    (let ((pos 0)
	  (pk (vct-peak v)))
      (do ((k 0 (1+ k)))
	  ((or (= k 1000)
	       (> pos 0)))
	(if (>= (abs (vct-ref v k)) pk)
	    (set! pos k)))
      (snd-display "~A: ~A ~A, ~A ~A ~A" i pk (/ i pk) pos (/ (* pos 0.002) (* 2 pi)) (inexact->exact (round (/ 1.0 (/ (* pos 0.002) (* 2 pi)))))))))

;;; so max is about at 2pi/(5n+4)
;;; for sum-of-sines it's about half that 2pi/(2.5n+4) -- zero cross at 2pi/4n min at pi/n 
|#

#|
(with-sound (:clipped #f)
  (let ((gen (make-noddsin 100 :n 10)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 10000))
	(outa i (noddsin gen 0.0) *output*))))))
|#


(def-clm-struct (noddcos
		 :make-wrapper
		 (lambda (g)
		   (set! (noddcos-incr g) (hz->radians (noddcos-frequency g)))
		   g))
  (frequency 0.0) (initial-phase 0.0) (n 1 :type int)
  (angle 0.0) (incr 0.0))

(define (noddcos gen fm)
  (declare (gen noddcos) (fm float))
  (let* ((angle (noddcos-angle gen))
	 (n (noddcos-n gen))
	 (den (* 2 n (sin angle)))) ; "n" here is normalization
    (set! (noddcos-angle gen) (+ angle (noddcos-incr gen) fm))
    (if (< (abs den) 1.0e-9)
	(exact->inexact n) ; just guessing -- floatification is for the run macro
	(/ (sin (* 2 n angle)) den))))

;;; (Gradshteyn and Ryzhik 1.342)

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-noddcos 100 :n 10)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 10000))
	(outa i (noddcos gen 0.0) *output*))))))
|#

(def-clm-struct (ssbnodd
		 :make-wrapper
		 (lambda (g)
		   (set! (ssbnodd-carincr g) (hz->radians (- (ssbnodd-carfreq g) (ssbnodd-modfreq g))))
		   (set! (ssbnodd-modincr g) (hz->radians (ssbnodd-modfreq g)))
		   g))
  (carfreq 0.0) (modfreq 0.0) (n 1 :type int)
  (carangle 0.0) (carincr 0.0) (modangle 0.0) (modincr 0.0))

(define (ssbnodd gen fm)
  (declare (gen ssbnodd) (fm float))
  (let* ((cx (ssbnodd-carangle gen))
	 (mx (ssbnodd-modangle gen))
	 (n (ssbnodd-n gen))
	 (sinnx (sin (* n mx)))
	 (den (* n (sin mx)))) ; "n" is normalization
    (set! (ssbnodd-carangle gen) (+ cx (ssbnodd-carincr gen) fm))
    (set! (ssbnodd-modangle gen) (+ mx (ssbnodd-modincr gen)))
    (if (< (abs den) 1.0e-9)
	0.0
	(- (* (sin cx)
	      (/ (* sinnx sinnx) den))
	   (* (cos cx)
	      (/ (sin (* 2 n mx))
		 (* 2 den)))))))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ssbnodd 1000.0 100.0 5)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 10000))
	(outa i (ssbnodd gen 0.0) *output*))))))
|#


;;; --------------------------------------------------------------------------------

;;; asymmetric fm gens

(def-clm-struct (asyfm :make-wrapper (lambda (gen)
				       (set! (asyfm-freq gen) (hz->radians (asyfm-frequency gen)))
				       (set! (asyfm-phase gen) (asyfm-initial-phase gen))
				       gen))
  (frequency 0.0) (initial-phase 0.0) (ratio 1.0) (r 1.0) (index 1.0)
  (freq 0.0) (phase 0.0))

(define (asyfm-J gen input)
  "(asyfm-J gen input) is the same as the CLM asymmetric-fm generator (index=1.0), set r != 1.0 to get the asymmetric spectra"
  (declare (gen asyfm) (input float))
  (let* ((phase (asyfm-phase gen))
	 (r (asyfm-r gen))
	 (r1 (/ 1.0 r))
	 (index (asyfm-index gen))
	 (one (if (or (> r 1.0) 
		      (and (< r 0.0)
			   (> r -1.0)))
		  -1.0 1.0))
	 (modphase (* (asyfm-ratio gen) phase))
	 (result (* (exp (* 0.5 index (- r r1) (+ one (cos modphase))))
		    (cos (+ phase (* 0.5 index (+ r r1) (sin modphase))))))) ; use cos, not sin, to get predictable amp
    (set! (asyfm-phase gen) (+ phase input (asyfm-freq gen)))
    result))

#|
(with-sound (:clipped #f :statistics #t) 
  (let ((gen (make-asyfm 2000.0 :ratio .1))) 
    (run 
     (lambda () 
       (do ((i 0 (1+ i)))
	   ((= i 1000))
	 (outa i (asyfm-J gen 0.0) *output*))))))

(with-sound (:clipped #f :statistics #t) 
  (let ((gen (make-asyfm 2000.0 :ratio .1 :index 1))
	(r-env (make-env '(0 -4 1 -1) :end 20000)))
    (run 
     (lambda () 
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (set! (asyfm-r gen) (env r-env))
	 (outa i (asyfm-J gen 0.0) *output*))))))

(define (val index r)
  (let ((sum 0.0))
    (do ((i -20 (1+ i)))
	((= i 21))
      (set! sum (+ sum (* (expt r i) (bes-jn i index)))))
    (let ((norm (exp (* 0.5 index (- r (/ 1.0 r))))))
      (list sum norm))))

(for-each
 (lambda (index)
   (for-each
    (lambda (r)
      (let ((peak (vct-peak (with-sound (:clipped #f :output (make-vct 1000))
					(let ((gen (make-asymmetric-fm 2000.0 :ratio .1 :r r)))
					  (run 
					   (lambda () 
					     (do ((i 0 (1+ i)))
						 ((= i 1000))
					       (outa i (asymmetric-fm gen index) *output*)))))))))
	(if (> (abs (- peak 1.0)) .1)
	    (snd-display ";asymmetric-fm peak: ~A, index: ~A, r: ~A" peak index r))))
    (list -10.0 -1.5 -0.5 0.5 1.0 1.5 10.0)))
 (list 1.0 3.0 10.0))
|#

(define (asyfm-I gen input)
  "(dsp-asyfm-I gen input) is the I0 case of the asymmetric-fm generator (dsp.scm)"
  (declare (gen asyfm) (input float))
  (let* ((phase (asyfm-phase gen))
	 (r (asyfm-r gen))
	 (r1 (/ 1.0 r))
	 (index (asyfm-index gen))
	 (modphase (* (asyfm-ratio gen) phase))
	 (result (* (exp (* 0.5 index (+ r r1) (- (cos modphase) 1.0)))
		    (cos (+ phase (* 0.5 index (- r r1) (sin modphase)))))))
    (set! (asyfm-phase gen) (+ phase input (asyfm-freq gen)))
    result))

#|
(with-sound (:clipped #f :statistics #t) 
  (let ((gen (make-asyfm 2000.0 :ratio .1))) 
    (run 
     (lambda () 
       (do ((i 0 (1+ i)))
	   ((= i 1000))
	 (outa i (asyfm-I gen 0.0) *output*))))))
|#


;;; --------------------------------------------------------------------------------

;;; various kernels (see sum-of-cosines and dsp.scm)

(def-clm-struct (ncos2
		 :make-wrapper
		 (lambda (g)
		   (set! (ncos2-n1 g) (+ 1 (ncos2-n g)))
		   (set! (ncos2-incr g) (hz->radians (ncos2-frequency g)))
		   (set! (ncos2-angle g) (ncos2-initial-phase g))
		   g))
  (frequency 0.0) (initial-phase 0.0) (n 1 :type int) 
  (n1 1 :type int)
  (angle 0.0) (incr 0.0))

(define (ncos2 gen fm)
  "(ncos2-pulse gen fm) produces a band-limited pulse train"
  (declare (gen ncos2) (fm float))

  ;; from "Trigonometric Series" Zygmund p88 with changes suggested by Katznelson "Introduction to Harmonic Analysis" p12, and
  ;;   scaling by an extra factor of 1/n+1 to make sure we always peak at 1.0 (I assume callers in this context are interested 
  ;;   in the pulse-train aspect and want easily predictable peak amp).  Harmonics go as (n-i)/n+1.

  (let* ((angle (ncos2-angle gen))
	 (n1 (ncos2-n1 gen))
	 (result (if (< (abs angle) 1.0e-9)
		     1.0
		     (let ((val (/ (sin (* 0.5 n1 angle)) 
				   (* n1
				      (sin (* 0.5 angle))))))
		       (* val val)))))
    (set! (ncos2-angle gen) (+ (ncos2-angle gen) fm (ncos2-incr gen)))
    result))

;;; can't use two oscils here because the angles have to line up perfectly

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ncos2 100.0 :n 10)))
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (ncos2 gen 0.0) *output*))))))
|#


(define make-ncos4 make-ncos2)

(define (ncos4 gen fm)
  ;; Katznelson p16
  (declare (gen ncos2) (fm float))
  (let ((val (ncos2 gen fm)))
    (* val val))) ; we already normalized this to 1.0


#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ncos4 100.0 :n 10)))
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (ncos4 gen 0.0) *output*))))))
|#


(def-clm-struct (npcos
		 :make-wrapper
		 (lambda (g)
		   (set! (npcos-incr g) (hz->radians (npcos-frequency g)))
		   (set! (npcos-angle g) (npcos-initial-phase g))
		   g))
  (frequency 0.0) (initial-phase 0.0) (n 1 :type int)
  (angle 0.0) (incr 0.0))

(define (npcos gen)
  (declare (gen npcos))
  (let* ((angle (npcos-angle gen))
	 (result (if (< (abs angle) 1.0e-9)
		     1.0
		     (let* ((n1 (+ (npcos-n gen) 1))
			    (result1 
			     (let ((val (/ (sin (* 0.5 n1 angle)) 
					   (* n1
					      (sin (* 0.5 angle))))))
			       (* val val)))
			    (p2n2 (+ (* 2 (npcos-n gen)) 2))
			    (result2 
			     (let ((val (/ (sin (* 0.5 p2n2 angle)) 
					   (* p2n2
					      (sin (* 0.5 angle))))))
			       (* val val))))
		       (- (* 2 result2) result1)))))
    (set! (npcos-angle gen) (+ (npcos-angle gen) (npcos-incr gen)))
    result))
    

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-npcos 100.0 :n 10)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 20000))
      (outa i (npcos gen) *output*))))))
|#


(def-clm-struct (rcos
		 :make-wrapper
		 (lambda (g)
		   (set! (rcos-osc g) (make-oscil (rcos-frequency g) (rcos-initial-phase g)))
		   g))
  (frequency 0.0) (initial-phase (* 0.5 pi)) (r 1.0) ; 'r' controls successive sinusoid amplitudes
  (osc #f :type clm))

(define (rcos gen fm)
  ;; from Andrews, Askey, Roy "Special Functions" 5.1.16, p243. r^k cos sum
  ;; a variant of the G&R 2nd col 4th row
  (declare (gen rcos) (fm float))
  (let* ((r (rcos-r gen))
	 (rr (* r r)))
    (* (- (/ (- 1.0 rr)
	     (- (+ 1.0 rr)
		(* 2.0 r (oscil (rcos-osc gen)))))
	  1.0)
       (/ (- 1.0 r) (* 2.0 r))))) ; normalization

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-rcos 100.0 :r 0.5)))
    (run (lambda () 
	   (do ((i 0 (1+ i)))
	       ((= i 20000))
	     (outa i (rcos gen 0.0) *output*))))))
|#


;;; --------------------------------------------------------------------------------

;;; Jolley 2nd col 2nd row (1st row is cos tweak of this)

(def-clm-struct (r2sin
		 :make-wrapper
		 (lambda (g)
		   (set! (r2sin-cosx g) (make-oscil (r2sin-frequency g) (r2sin-initial-phase g)))
		   (set! (r2sin-sinx g) (make-oscil (r2sin-frequency g) (r2sin-initial-phase g)))
		   (if (>= (* (r2sin-r g) (r2sin-r g)) 1.0)
		       (set! (r2sin-r g) 0.9999999))
		   g))
  (frequency 0.0) (initial-phase 0.0) (r 0.0)
  (cosx #f :type clm) (sinx #f :type clm))

(define (r2sin gen fm)
  (declare (gen r2sin))
  (* (sinh (* (r2sin-r gen)
	      (oscil (r2sin-cosx gen) fm)))
     (cos (* (r2sin-r gen)
	     (oscil (r2sin-sinx gen) fm)))))

#|
;;; odd harmonics, but we can't push the upper partials past the (2k)! range, so not very flexible

(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-r2sin 100.0 :r 0.5)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 20000))
      (outa i (r2sin gen 0.0) *output*))))))
|#


(def-clm-struct (ssbr2
		 :make-wrapper
		 (lambda (g)
		   (set! (ssbr2-carincr g) (hz->radians (ssbr2-carfreq g)))
		   (set! (ssbr2-modincr g) (hz->radians (ssbr2-modfreq g)))
		   g))
  (carfreq 0.0) (modfreq 0.0) (r 1.0)
  (carangle 0.0) (carincr 0.0)
  (modangle 0.0) (modincr 0.0))

;;; TODO: fm arg here
(define (ssbr2 gen)
  (declare (gen ssbr2))
  (let* ((mx (ssbr2-modangle gen))
	 (cx (ssbr2-carangle gen))
	 (a (ssbr2-r gen))
	 (asinx (* a (sin mx)))
	 (acosx (* a (cos mx))))

    (set! (ssbr2-carangle gen) (+ (ssbr2-carangle gen) (ssbr2-carincr gen)))
    (set! (ssbr2-modangle gen) (+ (ssbr2-modangle gen) (ssbr2-modincr gen)))

    (/ (- (* (cos cx)
	     (cosh acosx)
	     (cos asinx))
	  (* (sin cx)
	     (sinh acosx)
	     (sin asinx)))
       (cosh a)))) ; normalization

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ssbr2 1000.0 100.0 0.5)))
    (run 
     (lambda () 
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (ssbr2 gen) *output*))))))
|#


;;; --------------------------------------------------------------------------------

;;; bess (returns bes-jn, like oscil returns sin) normalized to peak at 1.0
;;;   frequency here is the frequency in Hz of the damped sinusoid part of the bessel function

(define bessel-peaks (vct 1.000 0.582 0.487 0.435 0.400 0.375 0.355 0.338 0.325 0.313 0.303 0.294 0.286 0.279 0.273 0.267 0.262 0.257 0.252 0.248
			  0.244 0.240 0.237 0.233 0.230 0.227 0.224 0.221 0.219 0.216 0.214 0.212 0.210 0.208 0.206 0.204 0.202 0.200 0.198 0.197
			  0.195 0.194 0.192 0.191 0.189 0.188 0.187 0.185 0.184 0.183 0.182 0.180 0.179 0.178 0.177 0.176 0.175 0.174 0.173 0.172
			  0.171 0.170 0.169 0.168 0.168 0.167 0.166 0.165 0.164 0.163 0.163 0.162 0.161 0.161 0.160 0.159 0.158 0.158 0.157 0.156
			  0.156 0.155 0.155 0.154 0.153 0.153 0.152 0.152 0.151 0.150 0.150 0.149 0.149 0.148 0.148 0.147 0.147 0.146 0.146 0.145))
;;; is there a formula for the max?

(def-clm-struct (bess
		 :make-wrapper
		 (lambda (g)
		   (set! (bess-incr g) (hz->radians (bess-frequency g)))
		   (set! (bess-arg g) (bess-initial-phase g))
		   (if (>= (bess-n g) (vct-length bessel-peaks)) 
		       (set! (bess-norm g) 0.145) 
		       (set! (bess-norm g) (vct-ref bessel-peaks (bess-n g))))
		   g))
  (frequency 0.0) (initial-phase 0.0) (n 0 :type int)
  (arg 0.0) (incr 0.0) (norm 1.0))

(define (bess gen fm)
  (declare (gen bess) (fm float))
  (let ((result (/ (bes-jn (bess-n gen) (bess-arg gen)) 
		   (bess-norm gen))))
    (set! (bess-arg gen) (+ (bess-arg gen) (bess-incr gen) fm))
    result))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-bess 100.0 :n 0)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 1000))
      (outa i (bess gen 0.0) *output*))))))

(with-sound (:clipped #f :statistics #t)
  (let ((gen1 (make-bess 400.0 :n 1))
	(gen2 (make-bess 400.0 :n 1))
	(vol (make-env '(0 0 1 1 9 1 10 0) :scaler 2.0 :end 20000)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 20000))
      (outa i (bess gen1 (* (env vol) (bess gen2 0.0))) *output*))))))

;;; max amps:
(do ((i 1 (1+ i)))
    ((= i 100))
  (let ((mx 0.0))
    (do ((k 0.0 (+ k .001)))
	((> k 200))
      (let ((val (bes-jn i k)))
	(if (> (abs val) mx)
	    (set! mx (abs val)))))
    (snd-display ";~A" (+ mx .001))))

(with-sound (:clipped #f :statistics #t)
  (let ((gen1 (make-bess 400.0 :n 1))
	(gen2 (make-oscil 400.0))
	(vol (make-env '(0 1 1 0) :scaler 1.0 :end 20000)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 20000))
      (outa i (bess gen1 (* (env vol) (oscil gen2 0.0))) *output*))))))

;;; also gen2 800, env scl 0.2
|#

;;; --------------------------------------------------------------------------------

;;; Jolley 1st col 2nd row
;;;   heads toward a square wave as "r" -> 0.0 (odd harmonics, 1/k amp)

;;; this is the cos side of ssbodddrk with r=e^-a

(def-clm-struct (eoddcos 
		 :make-wrapper
		 (lambda (g)
		   (set! (eoddcos-osc g) (make-oscil (eoddcos-frequency g) (eoddcos-initial-phase g)))
		   g))
  (frequency 0.0) (initial-phase (* 0.5 pi)) (r 1.0)
  (osc #f :type clm))

(define (eoddcos gen fm)
  (declare (gen eoddcos) (fm float))
  (let* ((a (eoddcos-r gen))
	 (sinha (sinh a)))
    (/ (atan (/ (oscil (eoddcos-osc gen) fm) sinha))
       (atan (/ 1.0 sinha))))) ; normalization

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-eoddcos 400.0 :r 1.0)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 10000))
      (outa i (eoddcos gen 0.0) *output*))))))

(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-eoddcos 400.0 :r 0.0))
	(a-env (make-env '(0 0 1 1) :end 10000)))
    (run (lambda () 
	   (do ((i 0 (1+ i)))
	       ((= i 10000))
	     (set! (eoddcos-r gen) (env a-env))
	     (outa i (eoddcos gen 0.0) *output*))))))

(with-sound (:clipped #f :statistics #t)
  (let ((gen1 (make-eoddcos 400.0 :r 0.0))
	(gen2 (make-oscil 400.0))
	(a-env (make-env '(0 0 1 1) :end 10000)))
    (run (lambda () 
	   (do ((i 0 (1+ i)))
	       ((= i 10000))
	     (set! (eoddcos-r gen1) (env a-env))
	     (outa i (eoddcos gen1 (* .1 (oscil gen2))) *output*))))))
|#


;;; --------------------------------------------------------------------------------


;;; Jolley 1st col 3rd row 

(define make-koddcos make-oscil)

(define (koddcos gen fm)
  (declare (gen clm) (fm float))
  (let ((arg (* 2.0 (oscil gen fm))))
    (if (>= arg 0.0)
	(* 0.5 (acos (- 1.0 arg)))
	(* -0.5 (acos (+ 1.0 arg))))))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-koddcos 400.0)))
    (run (lambda ()
	   (do ((i 0 (1+ i)))
	       ((= i 10000))
	     (outa i (* .3 (koddcos gen 0.0)) *output*))))))
|#

;;; as printed in J, this is not usable -- 1-2sin can be 3 so acos will be complex -- looks like we're missing: x < pi
;;; needs normalization and it's not right anyway -- we get odd harmonics but wrong amps

;;; --------------------------------------------------------------------------------

;;; straight sums as ssb

(def-clm-struct (ssbn 
		 :make-wrapper
		 (lambda (g)
		   (set! (ssbn-incr1 g) (hz->radians (ssbn-carfreq g)))
		   (set! (ssbn-incr2 g) (hz->radians (ssbn-modfreq g)))
		   g))
  (carfreq 0.0) (modfreq 0.0) (n 1 :type int)
  (angle1 0.0) (angle2 0.0) (incr1 0.0) (incr2 0.0))

(define (ssbn gen fm)
  (declare (gen ssbn) (fm float))
  (let* ((n (ssbn-n gen))
	 (angle1 (ssbn-angle1 gen))
	 (angle2 (ssbn-angle2 gen))
	 (carsin (sin angle1))
	 (canrcos (cos angle1))
	 (den (sin (* 0.5 angle2)))
	 (sumsin (* (sin (* angle2 (/ (+ n 1) 2))) 
		    (sin (/ (* n angle2) 2))))
	 (sumcos (* 0.5 (+ den (sin (* angle2 (+ n 0.5))))))
	 (result (/ (- (* carsin sumsin)
		       (* canrcos sumcos))
		    (* 2 den))))
    (set! (ssbn-angle1 gen) (+ (ssbn-angle1 gen) (ssbn-incr1 gen)))
    (set! (ssbn-angle2 gen) (+ (ssbn-angle2 gen) (ssbn-incr2 gen)))
    result))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ssbn 2000.0 100.0 3)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (* .3 (ssbn gen 0.0)) *output*))))))
|#


;;; --------------------------------------------------------------------------------

;;; ssb r^n case

(def-clm-struct (ssbnr 
		 :make-wrapper
		 (lambda (g)
		   (set! (ssbnr-incr1 g) (hz->radians (ssbnr-carfreq g)))
		   (set! (ssbnr-incr2 g) (hz->radians (ssbnr-modfreq g)))
		   g))
  (carfreq 0.0) (modfreq 0.0) (n 1 :type int) (r 0.0)
  (angle1 0.0) (angle2 0.0) (incr1 0.0) (incr2 0.0))

(define (ssbnr gen fm)
  (declare (gen ssbnr) (fm float))
  (let* ((n (ssbnr-n gen))
	 (angle1 (ssbnr-angle1 gen))
	 (angle2 (ssbnr-angle2 gen))
	 (carsin (sin angle1))
	 (canrcos (cos angle1))
	 (r (ssbnr-r gen))
	 (den (+ 1.0 (* r r) (* -2.0 r (cos angle2))))
	 (sumsin (* r (sin angle2)))
	 (sumcos (- 1.0 (* r (cos angle2))))
	 (result (/ (- (* carsin sumsin)
		       (* canrcos sumcos))
		    (* 2 den))))
    (set! (ssbnr-angle1 gen) (+ (ssbnr-angle1 gen) (ssbnr-incr1 gen)))
    (set! (ssbnr-angle2 gen) (+ (ssbnr-angle2 gen) (ssbnr-incr2 gen)))
    result))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ssbnr 2000.0 103.0 3 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (ssbnr gen 0.0) *output*))))))
|#


;;; G&R 2nd col 1st and 2nd rows

(def-clm-struct (ssbnr1
		 :make-wrapper
		 (lambda (g)
		   (set! (ssbnr1-carincr g) (hz->radians (ssbnr1-carfreq g)))
		   (set! (ssbnr1-modincr g) (hz->radians (ssbnr1-modfreq g)))
		   g))
  (carfreq 0.0) (modfreq 0.0) (n 1 :type int) (r 0.0)
  (carangle 0.0) (modangle 0.0) (carincr 0.0) (modincr 0.0))

(define (ssbnr1 gen)
  (declare (gen ssbnr1))
  (let* ((cx (ssbnr1-carangle gen))
	 (mx (ssbnr1-modangle gen))
	 (ci (ssbnr1-carincr gen))
	 (mi (ssbnr1-modincr gen))
	 (r (ssbnr1-r gen))
	 (n (ssbnr1-n gen))
	 (rn (- (expt r n)))
	 (rn1 (expt r (+ n 1)))
	 (nmx (* n mx))
	 (n1mx (* (- n 1) mx))
	 (norm (/ (- (expt r n) 1) (- r 1))) ; this could use rn
	 (den (* norm (+ 1.0 (* -2.0 r (cos mx)) (* r r)))))
    (set! (ssbnr1-carangle gen) (+ cx ci))
    (set! (ssbnr1-modangle gen) (+ mx mi))
    (/ (- (* (sin cx)
	     (+ (* r (sin mx))
		(* rn (sin nmx))
		(* rn1 (sin n1mx))))
	  (* (cos cx)
	     (+ 1.0
		(* -1.0 r (cos mx))
		(* rn (cos nmx))
		(* rn1 (cos n1mx)))))
       den)))

	  
#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ssbnr1 1000 100 5 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (ssbnr1 gen) *output*))))))
|#


;;; --------------------------------------------------------------------------------


;;; G&R 2nd col 6th row
;;; r^k/k -- this sums to ln(1/(1-x)) if x<1 (J 118)

(def-clm-struct (rkcos 
		 :make-wrapper
		 (lambda (g)
		   (set! (rkcos-osc g) (make-oscil (rkcos-frequency g) (rkcos-initial-phase g)))
		   g))
  (frequency 0.0) (initial-phase (* 0.5 pi)) (r 0.0)
  (osc #f :type clm))

(define (rkcos gen fm)
  (declare (gen rkcos) (fm float))
  (let ((cs (oscil (rkcos-osc gen) fm))
	(r (rkcos-r gen)))
    (/ (log (/ 1.0 (sqrt (+ 1.0 (* -2.0 r cs) (* r r)))))
       (log (/ 1.0 (- 1.0 r)))))) ; normalization

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-rkcos 440.0 :r 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (rkcos gen 0.0) *output*))))))
|#

;;; --------------------------------------------------------------------------------


;;; G&R 2nd col 3rd from last

(def-clm-struct (rk!cos
		 :make-wrapper
		 (lambda (g)
		   (set! (rk!cos-incr g) (hz->radians (rk!cos-frequency g)))
		   g))
  (frequency 0.0) (r 0.0)
  (angle 0.0) (incr 0.0))

;;; TODO: fm ?

(define (rk!cos gen)
  (declare (gen rk!cos))
  (let* ((r (rk!cos-r gen))
	 (x (rk!cos-angle gen))
	 (result (/ (* (exp (* r (cos x)))
		       (cos (* r (sin x))))
		    (exp r)))) ; normalization (would be e^x-1 but we have an extra 1)
    (set! (rk!cos-angle gen) (+ (rk!cos-angle gen) (rk!cos-incr gen)))
    result))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-rk!cos 440.0 :r 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (rk!cos gen) *output*))))))
|#


;;; --------------------------------------------------------------------------------


;;; from Askey "Ramanujan and Hypergeometric Series" in Berndt and Rankin "Ramanujan: Essays and Surveys" p283
;;;
;;; this gives a sum of cosines of decreasing amp where the "k" parameter determines
;;;   the "index" (in FM nomenclature) -- higher k = more cosines; the actual amount
;;;   of the nth cos involves hypergeometric series (looks like r^n/n! (~=e^n?) with a million other terms).

(def-clm-struct (r2k!cos
		 :make-wrapper
		 (lambda (g)
		   (set! (r2k!cos-osc g) (make-oscil (r2k!cos-frequency g) (r2k!cos-initial-phase g)))
		   g))
  (frequency 0.0) (initial-phase 0.0) (r 0.0) (k 0.0)
  (osc #f :type clm))

(define (r2k!cos gen fm)
  "r2k!cos is a variant of sum-of-cosines; 'r' controls successive sinusoid amplitude; 'k' controls how many sinusoids are produced"
  (declare (gen r2k!cos) (fm float))
  (let* ((r (r2k!cos-r gen))
	 (k (r2k!cos-k gen))
	 (rr1 (+ 1.0 (* r r)))
	 (r2 (* 2 r)))
    (* (expt (- rr1
		(* r2 (oscil (r2k!cos-osc gen) fm)))
	     (- k))
       (expt (- rr1 r2) k)))) ; amplitude normalization

;;; there is still noticable DC offset if r != 0.5 -- could precompute it and subtract (and there's lots of DC anyway)

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-r2k!cos 440.0 :r 0.5 :k 3.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (r2k!cos gen 0.0) *output*))))))
|#


;;; --------------------------------------------------------------------------------


;;;  from Stilson/Smith apparently -- was named "Discrete Summation Formula" which doesn't convey anything to me
;;;    Alexander Kritov suggests time-varying "a" is good (this is a translation of his code)

(def-clm-struct (blsaw
		 :make-wrapper
		 (lambda (g)
		   (set! (blsaw-incr g) (hz->radians (blsaw-frequency g)))
		   g))
  (frequency 0.0) (n 1 :type int) (r 0.0)
  (angle 0.0) (incr 0.0))

(define (blsaw gen)
  "blsaw produces a band-limited sawtooth"
  (declare (gen blsaw))
  (let* ((a (blsaw-r gen))
	 (N (blsaw-n gen))
	 (x (blsaw-angle gen))
	 (incr (blsaw-incr gen))
	 (den (+ 1.0 (* -2.0 a (cos x)) (* a a))))
    (set! (blsaw-angle gen) (+ x incr))
    (if (< (abs den) 1.0e-9)
	0.0
	(let* ((s1 (* (expt a (- N 1.0)) (sin (+ (* (- N 1.0) x) incr))))
	       (s2 (* (expt a N) (sin (+ (* N x) incr))))
	       (s3 (* a (sin (+ x incr)))))
	  (/ (+ (sin incr) 
		(- s3) 
		(- s2) 
		s1) 
	     den)))))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-blsaw 440.0 :r 0.5 :n 3)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (blsaw gen) *output*))))))
|#

;;; needs normalization


;;; --------------------------------------------------------------------------------


;;; Jolley 1st col 1st row

(def-clm-struct (k2sin
		 :make-wrapper
		 (lambda (g)
		   (set! (k2sin-angle g) (k2sin-initial-phase g))
		   (set! (k2sin-incr g) (hz->radians (k2sin-frequency g)))
		   g))
  (frequency 0.0) (initial-phase 0.0)
  (angle 0.0) (incr 0.0))

(define (k2sin gen fm)
  (declare (gen k2sin) (fm float))
  (let ((x (k2sin-angle gen)))
    (set! (k2sin-angle gen) (+ x (k2sin-incr gen)))
    (/ (* 3.0 (sin x)) ; 3 rather than 4 for normalization
       (- 5.0 (* 4.0 (cos x))))))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-k2sin 440.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (k2sin gen 0.0) *output*))))))
|#

;;; --------------------------------------------------------------------------------


;;; this was inspired by Andrews, Askey, Roy "Special Functions" p396, but there's an error somewhere...
;;;   it produces sum r^k sin(k+1/2)x
;;;   (not normalized)

(def-clm-struct (dblsum
		 :make-wrapper
		 (lambda (g)
		   (set! (dblsum-incr g) (hz->radians (* 2 (dblsum-frequency g))))
		   g))
  (frequency 0.0) (r 0.0)
  (angle 0.0) (incr 0.0))

(define (dblsum gen)
  (declare (gen dblsum))
  (let* ((x (dblsum-angle gen))
	 (r (dblsum-r gen)))
    (set! (dblsum-angle gen) (+ (dblsum-angle gen) (dblsum-incr gen)))
    (/ (* (+ 1 r) (sin (* 0.5 x)))
       (* (- 1 r) (+ 1.0 (* -2.0 r (cos x)) (* r r))))))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-dblsum 100 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (dblsum gen) *output*))))))
|#


;;; --------------------------------------------------------------------------------

;;; G&R 1st col ksinkx cases

(def-clm-struct (ssbnk
		 :make-wrapper
		 (lambda (g)
		   (set! (ssbnk-carincr g) (hz->radians (- (ssbnk-carfreq g) (ssbnk-modfreq g))))
		   (set! (ssbnk-modincr g) (hz->radians (ssbnk-modfreq g)))
		   (set! (ssbnk-n g) (+ 1 (ssbnk-n g))) ; sum goes 1 to n-1
		   g))
  (carfreq 0.0) (modfreq 0.0) (n 1 :type int)
  (carangle 0.0) (carincr 0.0)
  (modangle 0.0) (modincr 0.0))


(define (ssbnk gen fm)
  (declare (gen ssbnk))
  (let* ((n (ssbnk-n gen))
	 (x (ssbnk-modangle gen))
	 (sx2 (sin (* 0.5 x)))
	 (sx22 (* 2 sx2))
	 (sxsx (* 4 sx2 sx2))
	 (nx (* n x))
	 (nx2 (* 0.5 (- (* 2 n) 1) x))
	 (cx (ssbnk-carangle gen))
	 (cfm (* fm (/ (ssbnk-carincr gen) (ssbnk-modincr gen)))))

    (set! (ssbnk-carangle gen) (+ cfm (ssbnk-carangle gen) (ssbnk-carincr gen)))
    (set! (ssbnk-modangle gen) (+ fm (ssbnk-modangle gen) (ssbnk-modincr gen)))

    (if (< (abs sx2) 1.0e-9)
	0.0
	(let* ((s1 (- (/ (sin nx) sxsx)
		      (/ (* n (cos nx2)) sx22)))
	       (c1 (- (/ (* n (sin nx2)) sx22)
		      (/ (- 1.0 (cos nx)) sxsx))))
	  (/ (- (* (sin cx) s1)
		(* (cos cx) c1))
	     (* 0.5 n (- n 1))))))) ; normalization, nominal n is off by 1
	       
(define (ssbnk-interp gen fm interp)
  (declare (gen ssbnk))
  (let* ((n (ssbnk-n gen))
	 (x (ssbnk-modangle gen))
	 (sx2 (sin (* 0.5 x)))
	 (sx22 (* 2 sx2))
	 (sxsx (* 4 sx2 sx2))
	 (nx (* n x))
	 (nx2 (* 0.5 (- (* 2 n) 1) x))
	 (cx (ssbnk-carangle gen))
	 (cfm (* fm (/ (ssbnk-carincr gen) (ssbnk-modincr gen)))))

    (set! (ssbnk-carangle gen) (+ cfm (ssbnk-carangle gen) (ssbnk-carincr gen)))
    (set! (ssbnk-modangle gen) (+ fm (ssbnk-modangle gen) (ssbnk-modincr gen)))

    (if (< (abs sx2) 1.0e-9)
	0.0
	(let* ((s1 (- (/ (sin nx) sxsx)
		      (/ (* n (cos nx2)) sx22)))
	       (c1 (- (/ (* n (sin nx2)) sx22)
		      (/ (- 1.0 (cos nx)) sxsx))))
	  (/ (- (* (cos cx) c1)
		(* interp (* (sin cx) s1)))
	     (* 0.5 n (- n 1))))))) ; normalization, nominal n is off by 1, peak seems to be solid right through the interpolation

	       
#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ssbnk 1000.0 100.0 5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (ssbnk gen 0.0) *output*))))))

(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ssbnk 1000.0 100.0 5))
	(vib (make-oscil 5.0))
	(vibamp (hz->radians 5.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (ssbnk gen (* vibamp (oscil vib))) *output*))))))

(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ssbnk 1000.0 100.0 5))
	(move (make-env '(0 1 1 -1) :end 30000))
	(vib (make-oscil 5.0))
	(vibamp (hz->radians 5.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (* 0.5 (ssbnk-interp gen (* vibamp (oscil vib)) (env move))) *output*))))))

|#


;;; --------------------------------------------------------------------------------


;;;  G&R 2nd col rows 7&8 (odd r^k/k) 

(def-clm-struct (ssboddrk
		 :make-wrapper
		 (lambda (g)
		   (set! (ssboddrk-carincr g) (hz->radians (- (ssboddrk-carfreq g) (ssboddrk-modfreq g))))
		   (set! (ssboddrk-modincr g) (hz->radians (ssboddrk-modfreq g)))
		   g))
  (carfreq 0.0) (modfreq 0.0) (r 0.0)
  (carangle 0.0) (carincr 0.0)
  (modangle 0.0) (modincr 0.0))

(define (ssboddrk gen fm)
  (declare (gen ssboddrk))
  (let* ((r (ssboddrk-r gen))
	 (mx (ssboddrk-modangle gen))
	 (cx (ssboddrk-carangle gen))
	 (cfm (* fm (/ (ssboddrk-carincr gen) (ssboddrk-modincr gen)))))

    (set! (ssboddrk-carangle gen) (+ cfm (ssboddrk-carangle gen) (ssboddrk-carincr gen)))
    (set! (ssboddrk-modangle gen) (+ fm (ssboddrk-modangle gen) (ssboddrk-modincr gen)))

    (/ (- (* (cos cx)
	     0.5
	     (log (/ (+ 1.0 (* 2.0 r (cos mx)) (* r r))
		     (+ 1.0 (* -2.0 r (cos mx)) (* r r)))))
	  (* (sin cx)
	     (atan (/ (* 2.0 r (sin mx))
		      (- 1.0 (* r r))))))

       (- (log (+ 1 r))    ; normalization (r^k/k for odd k)
	  (log (- 1 r))))))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ssboddrk 1000.0 100.0 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (ssboddrk gen 0.0) *output*))))))
|#


;;; --------------------------------------------------------------------------------


;;; Zygmund 1st
;;;   this looks interesting, but how to normalize?  sum of sines is bad enough, kr^k -> 1/(1-x)^2 if x^2<1 (G&R 113)

(def-clm-struct (krksin
		 :make-wrapper
		 (lambda (g)
		   (set! (krksin-incr g) (hz->radians (krksin-frequency g)))
		   g))
  (frequency 0.0) (r 0.0)
  (angle 0.0) (incr 0.0))

(define (krksin gen fm)
  (declare (gen krksin) (fm float))
  (let* ((x (krksin-angle gen))
	 (r (krksin-r gen))
	 (r2 (* r r))
	 (den (+ 1.0 (* -2.0 r (cos x)) r2)))
    (set! (krksin-angle gen) (+ fm (krksin-angle gen) (krksin-incr gen)))
    (/ (* r (- 1 r2) (sin x))
       (* den den))))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-krksin 440.0 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (krksin gen 0.0) *output*))))))

(do ((i 0 (1+ i)))
    ((= i 10))
  (let ((mx (vct-peak (with-sound (:clipped #f :output (make-vct 10000))
				  (let ((gen (make-krksin 20.0 (* i 0.1))))
				    (run 
				     (lambda ()
				       (do ((i 0 (1+ i)))
					   ((= i 10000))
					 (outa i (krksin gen 0.0) *output*)))))))))
    (snd-display ";~A: ~A" (* 0.1 i) mx)))


|#

;;; --------------------------------------------------------------------------------

;;; Zygmund 2nd -- not actually very useful, but shows sin 2nx of abs

(def-clm-struct (abssin
		 :make-wrapper
		 (lambda (g)
		   (set! (abssin-osc g) (make-oscil (abssin-frequency g) (abssin-initial-phase g)))
		   g))
  (frequency 0.0) (initial-phase 0.0)
  (osc #f :type clm))

(define (abssin gen fm)
  (declare (gen abssin) (fm float))
  (abs (oscil (abssin-osc gen) fm)))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-abssin 440.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (abssin gen 0.0) *output*))))))
|#


;;; --------------------------------------------------------------------------------


;;; from Sansone, p182, assumptions: a not 0, b not 0, b/a real, abs(b/a)<1 (b less than a)

(def-clm-struct (abcos
		 :make-wrapper
		 (lambda (g)
		   (set! (abcos-incr g) (hz->radians (abcos-frequency g)))
		   g))
  (frequency 0.0) (a 0.0) (b 0.0)
  (angle 0.0) (incr 0.0))

;;; TODO: fm?

(define (abcos gen)
  (declare (gen abcos))
  (let* ((x (abcos-angle gen))
	 (a (abcos-a gen))
	 (b (abcos-b gen))
	 (norm (/ 0.5 (- (/ 1.0 
			    (- 1.0 (/ (abs (- (sqrt (- (* a a) (* b b))) 
					      a)) 
				      b))) 
			 1.0)))) ;; i.e. 1/(1-r) -1 because we start at k=1, r=the complicated a/b business

    (set! (abcos-angle gen) (+ (abcos-angle gen) (abcos-incr gen)))

    (* norm (- (/ (sqrt (- (* a a) (* b b)))
		  (+ a (* b (cos x))))
	       1.0))))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-abcos 100.0 0.5 0.25)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (abcos gen) *output*))))))
|#

;;; --------------------------------------------------------------------------------

;;; J 2nd col 3rd row

(def-clm-struct (r2k2cos
		 :make-wrapper
		 (lambda (g)
		   (set! (r2k2cos-incr g) (hz->radians (r2k2cos-frequency g)))
		   g))
  (frequency 0.0) (r 1.0)
  (angle 0.0) (incr 0.0))

(define (r2k2cos-norm a)
  ;; J 124
  (- (* (/ pi (* 2 a))
	(/ (+ (exp (* pi a)) (exp (* pi (- a))))
	   (- (exp (* pi a)) (exp (* pi (- a))))))
     (/ 1.0
	(* 2 a a))))

;;; TODO: fm?

(define (r2k2cos gen)
  (declare (gen r2k2cos))
  (let* ((x (r2k2cos-angle gen))
	 (a (r2k2cos-r gen)))
    (if (> x (* 2 pi))
	(set! x (fmod x (* 2 pi))))

    (set! (r2k2cos-angle gen) (+ x (r2k2cos-incr gen)))

    (/ (- (* pi (/ (cosh (* a (- pi x)))
		   (sinh (* a pi))))
	  (/ 1.0 a))
       (* 2 a (r2k2cos-norm a)))))


#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-r2k2cos 100.0 1.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (r2k2cos gen) *output*))))))
|#


;;; --------------------------------------------------------------------------------


(define ncos sum-of-cosines)
(define make-ncos make-sum-of-cosines)
(define ncos? sum-of-cosines?)

(define nsin sum-of-sines)
(define make-nsin make-sum-of-sines)
(define nsin? sum-of-sines?)

(define nrsin sine-summation)
(define make-nrsin make-sine-summation)
(define nrsin? sine-summation?)



;;; --------------------------------------------------------------------------------

;;; TODO: in docs: add a ref to each gen from the formula (may need to split into bazillions of cases)
;;; PERHAPS: a generator table for quick.html?

;;; --------------------------------------------------------------------------------


