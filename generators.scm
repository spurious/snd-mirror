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

;;; expcs: sum of cosines with exponentially decaying amps

(def-clm-struct (ekrcos
		 :make-wrapper 
		 (lambda (g)
		   (set! (ekrcos-osc g) (make-oscil (ekrcos-frequency g) (ekrcos-initial-phase g)))
		   (if (<= (ekrcos-r g) 0.0) (set! (ekrcos-r g) 0.00001))
		   (set! (ekrcos-cosh-t g) (cosh (ekrcos-r g)))
		   (let ((exp-t (exp (- (ekrcos-r g)))))
		     (set! (ekrcos-offset g) (/ (- 1.0 exp-t) (* 2.0 exp-t)))
		     (set! (ekrcos-scaler g) (* (sinh (ekrcos-r g)) (ekrcos-offset g))))
		   g))
  (frequency 440.0) (initial-phase 0.0) (r 1.0 :type float)
  (osc #f :type clm) scaler offset cosh-t)


(define (ekrcos gen fm)
  (declare (gen ekrcos) (fm float))
  (- (/ (ekrcos-scaler gen) 
	(- (ekrcos-cosh-t gen) (oscil (ekrcos-osc gen) fm)))
     (ekrcos-offset gen)))


#|
(with-sound (:clipped #f)
  (let ((gen (make-ekrcos 100 :r 1.0)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 10000))
	(outa i (ekrcos gen 0.0) *output*))))))

;; change "t" during note -- smoothly changing sum-of-cosines spectra (damped "lute-stop" effect)
(with-sound (:clipped #f)  
  (let ((gen (make-ekrcos 100 :r 0.1))
	(t-env (make-env '(0 .1 1 2) :end 20000)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 20000))
	(set! (ekrcos-r gen) (env t-env))
	(set! (ekrcos-cosh-t gen) (cosh (ekrcos-r gen)))
	(let ((exp-t (exp (- (ekrcos-r gen)))))
	  (set! (ekrcos-offset gen) (/ (- 1.0 exp-t) (* 2.0 exp-t)))
	  (set! (ekrcos-scaler gen) (* (sinh (ekrcos-r gen)) (ekrcos-offset gen))))
	(outa i (ekrcos gen 0.0) *output*))))))
|#


(def-clm-struct (ssbekt
		 :make-wrapper
		 (lambda (g)
		   (set! (ssbekt-carincr g) (hz->radians (- (ssbekt-carfreq g) (ssbekt-modfreq g))))
		   (set! (ssbekt-modincr g) (hz->radians (ssbekt-modfreq g)))
		   g))
  (carfreq 0.0) (modfreq 0.0) (r 0.0)
  (carangle 0.0) (carincr 0.0) (modangle 0.0) (modincr 0.0))

(define (ssbekt gen fm)
  (declare (gen ssbekt) (fm float))
  (let* ((cx (ssbekt-carangle gen))
	 (mx (ssbekt-modangle gen))
	 (r (ssbekt-r gen))
	 (ccmx (- (cosh r) (cos mx))))

    (set! (ssbekt-carangle gen) (+ (* (/ (ssbekt-modincr gen) (ssbekt-carincr gen)) fm) cx (ssbekt-carincr gen)))
    (set! (ssbekt-modangle gen) (+ fm mx (ssbekt-modincr gen)))

    (/ (- (* (cos cx)
	     (- (/ (sinh r) ccmx)
		1.0))
	  (* (sin cx)
	     (/ (sin mx) ccmx)))
       (* 2.0 (- (/ 1.0 (- 1.0 (exp (- r)))) 1.0))))) ; normalization

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ssbekt 1000.0 100.0 1.0)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 20000))
	(outa i (ssbekt gen 0.0) *output*))))))
|#


;;; --------------------------------------------------------------------------------


;;; sndclm.html (G&R) 1st col 5th row (sum of odd sines)

;;; sum of n odd sines

(def-clm-struct (oddsin 
		 :make-wrapper
		 (lambda (g)
		   (if (< (oddsin-n g) 1) (set! (oddsin-n g) 1))
		   (set! (oddsin-osc g) (make-oscil 
					 (oddsin-frequency g) 
					 (oddsin-initial-phase g)))
		   (set! (oddsin-nosc g) (make-oscil 
					  (* (oddsin-n g) (oddsin-frequency g)) 
					  (* (oddsin-n g) (oddsin-initial-phase g))))
		   (set! (oddsin-norm g) (if (= (oddsin-n g) 1) 1.0
					     (/ (if (= (oddsin-n g) 2) 1.29
						    (if (= (oddsin-n g) 3) 1.34
							(if (< (oddsin-n g) 6) 1.36
							    (if (< (oddsin-n g) 18) 1.37
								1.379))))
						(oddsin-n g))))
		   g))
  (frequency 440.0) (initial-phase 0.0) (n 1 :type int)
  (osc #f :type clm) (nosc #f :type clm) (norm 0.0 :type float))

(define (oddsin gen fm)
  (declare (gen oddsin) (fm float))
  (let ((o1 (oscil (oddsin-nosc gen) (* (oddsin-n gen) fm)))
	(o2 (oscil (oddsin-osc gen) fm))) 
    (if (< (abs o2) 1.0e-9)
	0.0
	(/ (* (oddsin-norm gen) o1 o1) o2))))
	   
#|
;;; get normalization:
(do ((i 1 (1+ i))) 
    ((= i 30))
  (let ((v (with-sound (:output (make-vct 1000) :clipped #f)
		       (let ((gen (make-oddsin (radians->hz .002) :n i)))
			 (do ((k 0 (1+ k)))
			     ((= k 1000))
			   (outa k (oddsin gen 0.0) *output*))))))
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
  (let ((gen (make-oddsin 100 :n 10)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 10000))
	(outa i (oddsin gen 0.0) *output*))))))
|#


(def-clm-struct (oddcos
		 :make-wrapper
		 (lambda (g)
		   (set! (oddcos-incr g) (hz->radians (oddcos-frequency g)))
		   g))
  (frequency 0.0) (initial-phase 0.0) (n 1 :type int)
  (angle 0.0) (incr 0.0))

(define (oddcos gen fm)
  (declare (gen oddcos) (fm float))
  (let* ((angle (oddcos-angle gen))
	 (n (oddcos-n gen))
	 (den (* 2 n (sin angle)))) ; "n" here is normalization
    (set! (oddcos-angle gen) (+ angle (oddcos-incr gen) fm))
    (if (< (abs den) 1.0e-9)
	(exact->inexact n) ; just guessing -- floatification is for the run macro
	(/ (sin (* 2 n angle)) den))))

;;; (Gradshteyn and Ryzhik 1.342)

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-oddcos 100 :n 10)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 10000))
	(outa i (oddcos gen 0.0) *output*))))))
|#

(def-clm-struct (ssbodd
		 :make-wrapper
		 (lambda (g)
		   (set! (ssbodd-carincr g) (hz->radians (- (ssbodd-carfreq g) (ssbodd-modfreq g))))
		   (set! (ssbodd-modincr g) (hz->radians (ssbodd-modfreq g)))
		   g))
  (carfreq 0.0) (modfreq 0.0) (n 1 :type int)
  (carangle 0.0) (carincr 0.0) (modangle 0.0) (modincr 0.0))

(define (ssbodd gen fm)
  (declare (gen ssbodd) (fm float))
  (let* ((cx (ssbodd-carangle gen))
	 (mx (ssbodd-modangle gen))
	 (n (ssbodd-n gen))
	 (sinnx (sin (* n mx)))
	 (den (* n (sin mx)))) ; "n" is normalization
    (set! (ssbodd-carangle gen) (+ cx (ssbodd-carincr gen) fm))
    (set! (ssbodd-modangle gen) (+ mx (ssbodd-modincr gen)))
    (if (< (abs den) 1.0e-9)
	0.0
	(- (* (sin cx)
	      (/ (* sinnx sinnx) den))
	   (* (cos cx)
	      (/ (sin (* 2 n mx))
		 (* 2 den)))))))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ssbodd 1000.0 100.0 5)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 10000))
	(outa i (ssbodd gen 0.0) *output*))))))
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

(def-clm-struct (fejer
		 :make-wrapper
		 (lambda (g)
		   (set! (fejer-n1 g) (+ 1 (fejer-n g)))
		   (set! (fejer-incr g) (hz->radians (fejer-frequency g)))
		   (set! (fejer-angle g) (fejer-initial-phase g))
		   g))
  (frequency 0.0) (initial-phase 0.0) (n 1 :type int) 
  (n1 1 :type int)
  (angle 0.0) (incr 0.0))

(define (fejer gen fm)
  "(fejer-pulse gen fm) produces a band-limited pulse train"
  (declare (gen fejer) (fm float))

  ;; from "Trigonometric Series" Zygmund p88 with changes suggested by Katznelson "Introduction to Harmonic Analysis" p12, and
  ;;   scaling by an extra factor of 1/n+1 to make sure we always peak at 1.0 (I assume callers in this context are interested 
  ;;   in the pulse-train aspect and want easily predictable peak amp).  Harmonics go as (n-i)/n+1.

  (let* ((angle (fejer-angle gen))
	 (n1 (fejer-n1 gen))
	 (result (if (< (abs angle) 1.0e-9)
		     1.0
		     (let ((val (/ (sin (* 0.5 n1 angle)) 
				   (* n1
				      (sin (* 0.5 angle))))))
		       (* val val)))))
    (set! (fejer-angle gen) (+ (fejer-angle gen) fm (fejer-incr gen)))
    result))

;;; can't use two oscils here because the angles have to line up perfectly

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-fejer 100.0 :n 10)))
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (fejer gen 0.0) *output*))))))
|#


(define make-fejer2 make-fejer)

(define (fejer2 gen fm)
  ;; Katznelson p16
  (declare (gen fejer) (fm float))
  (let ((val (fejer gen fm)))
    (* val val))) ; we already normalized this to 1.0


#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-fejer2 100.0 :n 10)))
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (fejer2 gen 0.0) *output*))))))
|#


(def-clm-struct (poussin
		 :make-wrapper
		 (lambda (g)
		   (set! (poussin-incr g) (hz->radians (poussin-frequency g)))
		   (set! (poussin-angle g) (poussin-initial-phase g))
		   g))
  (frequency 0.0) (initial-phase 0.0) (n 1 :type int)
  (angle 0.0) (incr 0.0))

(define (poussin gen)
  (declare (gen poussin))
  (let* ((angle (poussin-angle gen))
	 (result (if (< (abs angle) 1.0e-9)
		     1.0
		     (let* ((n1 (+ (poussin-n gen) 1))
			    (result1 
			     (let ((val (/ (sin (* 0.5 n1 angle)) 
					   (* n1
					      (sin (* 0.5 angle))))))
			       (* val val)))
			    (p2n2 (+ (* 2 (poussin-n gen)) 2))
			    (result2 
			     (let ((val (/ (sin (* 0.5 p2n2 angle)) 
					   (* p2n2
					      (sin (* 0.5 angle))))))
			       (* val val))))
		       (- (* 2 result2) result1)))))
    (set! (poussin-angle gen) (+ (poussin-angle gen) (poussin-incr gen)))
    result))
    

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-poussin 100.0 :n 10)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 20000))
      (outa i (poussin gen) *output*))))))
|#


(def-clm-struct (soc2
		 :make-wrapper
		 (lambda (g)
		   (set! (soc2-n1 g) (+ 1 (* 2 (soc2-n g))))
		   (set! (soc2-incr g) (hz->radians (soc2-frequency g)))
		   (set! (soc2-angle g) (soc2-initial-phase g))
		   g))
  (frequency 0.0) (initial-phase 0.0) (n 1 :type int) 
  (n1 1 :type int)
  (angle 0.0) (incr 0.0))
		   
(define (soc2 gen)
  "(soc2-sum angle n) produces a band-limited pulse train"
  ;; from Andrews, Askey, Roy "Special Functions" p 314 with my amplitude scaling
  (declare (gen soc2))
  (let* ((angle (soc2-angle gen))
	 (result (if (< (abs angle) 1.0e-9)
		     1.0
		     (let* ((n (soc2-n gen))
			    (n1 (soc2-n1 gen))
			    (val (/ (sin (* angle (+ n 0.5)))
				   (* (sin (* 0.5 angle)) n1))))
		       (* val val)))))
    (set! (soc2-angle gen) (+ (soc2-angle gen) (soc2-incr gen)))
    result))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-soc2 100.0 :n 10)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 20000))
      (outa i (soc2 gen) *output*))))))
|#


(def-clm-struct (rkcos
		 :make-wrapper
		 (lambda (g)
		   (set! (rkcos-osc g) (make-oscil (rkcos-frequency g) (rkcos-initial-phase g)))
		   g))
  (frequency 0.0) (initial-phase (* 0.5 pi)) (r 1.0) ; 'r' controls successive sinusoid amplitudes
  (osc #f :type clm))

(define (rkcos gen fm)
  ;; from Andrews, Askey, Roy "Special Functions" 5.1.16, p243. r^k cos sum
  ;; a variant of the G&R 2nd col 4th row
  (declare (gen rkcos) (fm float))
  (let* ((r (rkcos-r gen))
	 (rr (* r r)))
    (* (- (/ (- 1.0 rr)
	     (- (+ 1.0 rr)
		(* 2.0 r (oscil (rkcos-osc gen)))))
	  1.0)
       (/ (- 1.0 r) (* 2.0 r))))) ; normalization

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-rkcos 100.0 :r 0.5)))
    (run (lambda () 
	   (do ((i 0 (1+ i)))
	       ((= i 20000))
	     (outa i (rkcos gen 0.0) *output*))))))
|#


;;; --------------------------------------------------------------------------------

;;; Jolley 2nd col 2nd row (1st row is cos tweak of this)

(def-clm-struct (r2ksin
		 :make-wrapper
		 (lambda (g)
		   (set! (r2ksin-cosx g) (make-oscil (r2ksin-frequency g) (r2ksin-initial-phase g)))
		   (set! (r2ksin-sinx g) (make-oscil (r2ksin-frequency g) (r2ksin-initial-phase g)))
		   (if (>= (* (r2ksin-r g) (r2ksin-r g)) 1.0)
		       (set! (r2ksin-r g) 0.9999999))
		   g))
  (frequency 0.0) (initial-phase 0.0) (r 0.0)
  (cosx #f :type clm) (sinx #f :type clm))

(define (r2ksin gen fm)
  (declare (gen r2ksin))
  (* (sinh (* (r2ksin-r gen)
	      (oscil (r2ksin-cosx gen) fm)))
     (cos (* (r2ksin-r gen)
	     (oscil (r2ksin-sinx gen) fm)))))

#|
;;; odd harmonics, but we can't push the upper partials past the (2k)! range, so not very flexible

(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-r2ksin 100.0 :r 0.5)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 20000))
      (outa i (r2ksin gen 0.0) *output*))))))
|#


(def-clm-struct (ssbr2k
		 :make-wrapper
		 (lambda (g)
		   (set! (ssbr2k-carincr g) (hz->radians (ssbr2k-carfreq g)))
		   (set! (ssbr2k-modincr g) (hz->radians (ssbr2k-modfreq g)))
		   g))
  (carfreq 0.0) (modfreq 0.0) (r 1.0)
  (carangle 0.0) (carincr 0.0)
  (modangle 0.0) (modincr 0.0))

;;; TODO: fm arg here
(define (ssbr2k gen)
  (declare (gen ssbr2k))
  (let* ((mx (ssbr2k-modangle gen))
	 (cx (ssbr2k-carangle gen))
	 (a (ssbr2k-r gen))
	 (asinx (* a (sin mx)))
	 (acosx (* a (cos mx))))

    (set! (ssbr2k-carangle gen) (+ (ssbr2k-carangle gen) (ssbr2k-carincr gen)))
    (set! (ssbr2k-modangle gen) (+ (ssbr2k-modangle gen) (ssbr2k-modincr gen)))

    (/ (- (* (cos cx)
	     (cosh acosx)
	     (cos asinx))
	  (* (sin cx)
	     (sinh acosx)
	     (sin asinx)))
       (cosh a)))) ; normalization

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ssbr2k 1000.0 100.0 0.5)))
    (run 
     (lambda () 
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (ssbr2k gen) *output*))))))
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

(def-clm-struct (ekoddcos 
		 :make-wrapper
		 (lambda (g)
		   (set! (ekoddcos-osc g) (make-oscil (ekoddcos-frequency g) (ekoddcos-initial-phase g)))
		   g))
  (frequency 0.0) (initial-phase (* 0.5 pi)) (r 1.0)
  (osc #f :type clm))

(define (ekoddcos gen fm)
  (declare (gen ekoddcos) (fm float))
  (let* ((a (ekoddcos-r gen))
	 (sinha (sinh a)))
    (/ (atan (/ (oscil (ekoddcos-osc gen) fm) sinha))
       (atan (/ 1.0 sinha))))) ; normalization

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ekoddcos 400.0 :r 1.0)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 10000))
      (outa i (ekoddcos gen 0.0) *output*))))))

(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ekoddcos 400.0 :r 0.0))
	(a-env (make-env '(0 0 1 1) :end 10000)))
    (run (lambda () 
	   (do ((i 0 (1+ i)))
	       ((= i 10000))
	     (set! (ekoddcos-r gen) (env a-env))
	     (outa i (ekoddcos gen 0.0) *output*))))))

(with-sound (:clipped #f :statistics #t)
  (let ((gen1 (make-ekoddcos 400.0 :r 0.0))
	(gen2 (make-oscil 400.0))
	(a-env (make-env '(0 0 1 1) :end 10000)))
    (run (lambda () 
	   (do ((i 0 (1+ i)))
	       ((= i 10000))
	     (set! (ekoddcos-r gen1) (env a-env))
	     (outa i (ekoddcos gen1 (* .1 (oscil gen2))) *output*))))))
|#

;;; TODO: ssb case for e^odd k

;;; --------------------------------------------------------------------------------


;;; Jolley 1st col 3rd row 

(define make-oddacos make-oscil)

(define (oddacos gen fm)
  (declare (gen clm) (fm float))
  (let ((arg (* 2.0 (oscil gen fm))))
    (if (>= arg 0.0)
	(* 0.5 (acos (- 1.0 arg)))
	(* -0.5 (acos (+ 1.0 arg))))))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-oddacos 400.0)))
    (run (lambda ()
	   (do ((i 0 (1+ i)))
	       ((= i 10000))
	     (outa i (* .3 (oddacos gen 0.0)) *output*))))))
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
	 (carcos (cos angle1))
	 (den (sin (* 0.5 angle2)))
	 (sumsin (* (sin (* angle2 (/ (+ n 1) 2))) 
		    (sin (/ (* n angle2) 2))))
	 (sumcos (* 0.5 (+ den (sin (* angle2 (+ n 0.5))))))
	 (result (/ (- (* carsin sumsin)
		       (* carcos sumcos))
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

(def-clm-struct (ssbr 
		 :make-wrapper
		 (lambda (g)
		   (set! (ssbr-incr1 g) (hz->radians (ssbr-carfreq g)))
		   (set! (ssbr-incr2 g) (hz->radians (ssbr-modfreq g)))
		   g))
  (carfreq 0.0) (modfreq 0.0) (n 1 :type int) (r 0.0)
  (angle1 0.0) (angle2 0.0) (incr1 0.0) (incr2 0.0))

(define (ssbr gen fm)
  (declare (gen ssbr) (fm float))
  (let* ((n (ssbr-n gen))
	 (angle1 (ssbr-angle1 gen))
	 (angle2 (ssbr-angle2 gen))
	 (carsin (sin angle1))
	 (carcos (cos angle1))
	 (r (ssbr-r gen))
	 (den (+ 1.0 (* r r) (* -2.0 r (cos angle2))))
	 (sumsin (* r (sin angle2)))
	 (sumcos (- 1.0 (* r (cos angle2))))
	 (result (/ (- (* carsin sumsin)
		       (* carcos sumcos))
		    (* 2 den))))
    (set! (ssbr-angle1 gen) (+ (ssbr-angle1 gen) (ssbr-incr1 gen)))
    (set! (ssbr-angle2 gen) (+ (ssbr-angle2 gen) (ssbr-incr2 gen)))
    result))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ssbr 2000.0 103.0 3 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (ssbr gen 0.0) *output*))))))
|#


;;; --------------------------------------------------------------------------------


;;; G&R 2nd col 6th row
;;; r^k/k -- this sums to ln(1/(1-x)) if x<1 (J 118)

(def-clm-struct (krcos 
		 :make-wrapper
		 (lambda (g)
		   (set! (krcos-osc g) (make-oscil (krcos-frequency g) (krcos-initial-phase g)))
		   g))
  (frequency 0.0) (initial-phase (* 0.5 pi)) (r 0.0)
  (osc #f :type clm))

(define (krcos gen fm)
  (declare (gen krcos) (fm float))
  (let ((cs (oscil (krcos-osc gen) fm))
	(r (krcos-r gen)))
    (/ (log (/ 1.0 (sqrt (+ 1.0 (* -2.0 r cs) (* r r)))))
       (log (/ 1.0 (- 1.0 r)))))) ; normalization

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-krcos 440.0 :r 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (krcos gen 0.0) *output*))))))
|#

;;; --------------------------------------------------------------------------------


;;; G&R 2nd col 3rd from last

(def-clm-struct (k!rcos
		 :make-wrapper
		 (lambda (g)
		   (set! (k!rcos-incr g) (hz->radians (k!rcos-frequency g)))
		   g))
  (frequency 0.0) (r 0.0)
  (angle 0.0) (incr 0.0))

;;; TODO: fm ?

(define (k!rcos gen)
  (declare (gen k!rcos))
  (let* ((r (k!rcos-r gen))
	 (x (k!rcos-angle gen))
	 (result (/ (* (exp (* r (cos x)))
		       (cos (* r (sin x))))
		    (exp r)))) ; normalization (would be e^x-1 but we have an extra 1)
    (set! (k!rcos-angle gen) (+ (k!rcos-angle gen) (k!rcos-incr gen)))
    result))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-k!rcos 440.0 :r 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (k!rcos gen) *output*))))))
|#


;;; --------------------------------------------------------------------------------


;;; from Askey "Ramanujan and Hypergeometric Series" in Berndt and Rankin "Ramanujan: Essays and Surveys" p283
;;;
;;; this gives a sum of cosines of decreasing amp where the "k" parameter determines
;;;   the "index" (in FM nomenclature) -- higher k = more cosines; the actual amount
;;;   of the nth cos involves hypergeometric series (looks like r^n/n! (~=e^n?) with a million other terms).

(def-clm-struct (kosine
		 :make-wrapper
		 (lambda (g)
		   (set! (kosine-osc g) (make-oscil (kosine-frequency g) (kosine-initial-phase g)))
		   g))
  (frequency 0.0) (initial-phase 0.0) (r 0.0) (k 0.0)
  (osc #f :type clm))

(define (kosine gen fm)
  "kosine is a variant of sum-of-cosines; 'r' controls successive sinusoid amplitude; 'k' controls how many sinusoids are produced"
  (declare (gen kosine) (fm float))
  (let* ((r (kosine-r gen))
	 (k (kosine-k gen))
	 (rr1 (+ 1.0 (* r r)))
	 (r2 (* 2 r)))
    (* (expt (- rr1
		(* r2 (oscil (kosine-osc gen) fm)))
	     (- k))
       (expt (- rr1 r2) k)))) ; amplitude normalization

;;; there is still noticable DC offset if r != 0.5 -- could precompute it and subtract (and there's lots of DC anyway)

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-kosine 440.0 :r 0.5 :k 3.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (kosine gen 0.0) *output*))))))
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


;;; G&R 2nd col 1st and 2nd rows

(def-clm-struct (ssbrk
		 :make-wrapper
		 (lambda (g)
		   (set! (ssbrk-carincr g) (hz->radians (ssbrk-carfreq g)))
		   (set! (ssbrk-modincr g) (hz->radians (ssbrk-modfreq g)))
		   g))
  (carfreq 0.0) (modfreq 0.0) (n 1 :type int) (r 0.0)
  (carangle 0.0) (modangle 0.0) (carincr 0.0) (modincr 0.0))

(define (ssbrk gen)
  (declare (gen ssbrk))
  (let* ((cx (ssbrk-carangle gen))
	 (mx (ssbrk-modangle gen))
	 (ci (ssbrk-carincr gen))
	 (mi (ssbrk-modincr gen))
	 (r (ssbrk-r gen))
	 (n (ssbrk-n gen))
	 (rn (- (expt r n)))
	 (rn1 (expt r (+ n 1)))
	 (nmx (* n mx))
	 (n1mx (* (- n 1) mx))
	 (norm (/ (- (expt r n) 1) (- r 1))) ; this could use rn
	 (den (* norm (+ 1.0 (* -2.0 r (cos mx)) (* r r)))))
    (set! (ssbrk-carangle gen) (+ cx ci))
    (set! (ssbrk-modangle gen) (+ mx mi))
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
  (let ((gen (make-ssbrk 1000 100 5 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (ssbrk gen) *output*))))))
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

(def-clm-struct (ssbk
		 :make-wrapper
		 (lambda (g)
		   (set! (ssbk-carincr g) (hz->radians (- (ssbk-carfreq g) (ssbk-modfreq g))))
		   (set! (ssbk-modincr g) (hz->radians (ssbk-modfreq g)))
		   (set! (ssbk-n g) (+ 1 (ssbk-n g))) ; sum goes 1 to n-1
		   g))
  (carfreq 0.0) (modfreq 0.0) (n 1 :type int)
  (carangle 0.0) (carincr 0.0)
  (modangle 0.0) (modincr 0.0))


(define (ssbk gen fm)
  (declare (gen ssbk))
  (let* ((n (ssbk-n gen))
	 (x (ssbk-modangle gen))
	 (sx2 (sin (* 0.5 x)))
	 (sx22 (* 2 sx2))
	 (sxsx (* 4 sx2 sx2))
	 (nx (* n x))
	 (nx2 (* 0.5 (- (* 2 n) 1) x))
	 (cx (ssbk-carangle gen))
	 (cfm (* fm (/ (ssbk-carincr gen) (ssbk-modincr gen)))))

    (set! (ssbk-carangle gen) (+ cfm (ssbk-carangle gen) (ssbk-carincr gen)))
    (set! (ssbk-modangle gen) (+ fm (ssbk-modangle gen) (ssbk-modincr gen)))

    (if (< (abs sx2) 1.0e-9)
	0.0
	(let* ((s1 (- (/ (sin nx) sxsx)
		      (/ (* n (cos nx2)) sx22)))
	       (c1 (- (/ (* n (sin nx2)) sx22)
		      (/ (- 1.0 (cos nx)) sxsx))))
	  (/ (- (* (sin cx) s1)
		(* (cos cx) c1))
	     (* 0.5 n (- n 1))))))) ; normalization, nominal n is off by 1
	       
(define (ssbk-interp gen fm interp)
  (declare (gen ssbk))
  (let* ((n (ssbk-n gen))
	 (x (ssbk-modangle gen))
	 (sx2 (sin (* 0.5 x)))
	 (sx22 (* 2 sx2))
	 (sxsx (* 4 sx2 sx2))
	 (nx (* n x))
	 (nx2 (* 0.5 (- (* 2 n) 1) x))
	 (cx (ssbk-carangle gen))
	 (cfm (* fm (/ (ssbk-carincr gen) (ssbk-modincr gen)))))

    (set! (ssbk-carangle gen) (+ cfm (ssbk-carangle gen) (ssbk-carincr gen)))
    (set! (ssbk-modangle gen) (+ fm (ssbk-modangle gen) (ssbk-modincr gen)))

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
  (let ((gen (make-ssbk 1000.0 100.0 5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (ssbk gen 0.0) *output*))))))

(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ssbk 1000.0 100.0 5))
	(vib (make-oscil 5.0))
	(vibamp (hz->radians 5.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (ssbk gen (* vibamp (oscil vib))) *output*))))))

(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ssbk 1000.0 100.0 5))
	(move (make-env '(0 1 1 -1) :end 30000))
	(vib (make-oscil 5.0))
	(vibamp (hz->radians 5.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (* 0.5 (ssbk-interp gen (* vibamp (oscil vib)) (env move))) *output*))))))

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

(def-clm-struct (r2k2
		 :make-wrapper
		 (lambda (g)
		   (set! (r2k2-incr g) (hz->radians (r2k2-frequency g)))
		   g))
  (frequency 0.0) (r 1.0)
  (angle 0.0) (incr 0.0))

(define (r2k2-norm a)
  ;; J 124
  (- (* (/ pi (* 2 a))
	(/ (+ (exp (* pi a)) (exp (* pi (- a))))
	   (- (exp (* pi a)) (exp (* pi (- a))))))
     (/ 1.0
	(* 2 a a))))

;;; TODO: fm?

(define (r2k2 gen)
  (declare (gen r2k2))
  (let* ((x (r2k2-angle gen))
	 (a (r2k2-r gen)))
    (if (> x (* 2 pi))
	(set! x (fmod x (* 2 pi))))

    (set! (r2k2-angle gen) (+ x (r2k2-incr gen)))

    (/ (- (* pi (/ (cosh (* a (- pi x)))
		   (sinh (* a pi))))
	  (/ 1.0 a))
       (* 2 a (r2k2-norm a)))))


#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-r2k2 100.0 1.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (r2k2 gen) *output*))))))
|#


;;; --------------------------------------------------------------------------------





;;; --------------------------------------------------------------------------------

;;; TODO: in docs: add a ref to each gen from the formula (may need to split into bazillions of cases)
;;; PERHAPS: a generator table for quick.html?

;;; --------------------------------------------------------------------------------


