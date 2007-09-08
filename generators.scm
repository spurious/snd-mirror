(provide 'snd-generators.scm)
(if (not (provided? 'snd-ws.scm)) (load-from-path "ws.scm"))


;;; these try to mimic existing gens (mainly oscil), so "frequency" and "initial-phase" are placed first
;;;   so the make function places those two args first


;;; --------------------------------------------------------------------------------

;;; sndclm.html G&R 2nd col last row (with normalization)

;;; expcs: sum of cosines with exponentially decaying amps

(def-clm-struct (expcs
		 :make-wrapper 
		 (lambda (g)
		   (set! (expcs-osc g) (make-oscil (expcs-frequency g) (expcs-initial-phase g)))
		   (if (<= (expcs-t g) 0.0) (set! (expcs-t g) 0.00001))
		   (set! (expcs-cosh-t g) (cosh (expcs-t g)))
		   (let ((exp-t (exp (- (expcs-t g)))))
		     (set! (expcs-offset g) (/ (- 1.0 exp-t) (* 2.0 exp-t)))
		     (set! (expcs-scaler g) (* (sinh (expcs-t g)) (expcs-offset g))))
		   g))
  (frequency 440.0) 
  (initial-phase 0.0)
  (t 1.0 :type float)
  (osc #f :type clm) scaler offset cosh-t)


(define (expcs gen fm)
  (declare (gen expcs) (fm float))
  (- (/ (expcs-scaler gen) 
	(- (expcs-cosh-t gen) (oscil (expcs-osc gen) fm)))
     (expcs-offset gen)))


#|
(with-sound (:clipped #f)
  (let ((gen (make-expcs 100 :t 1.0)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 10000))
	(outa i (expcs gen 0.0) *output*))))))


;; change "t" during note -- smoothly changing sum-of-cosines spectra (damped "lute-stop" effect)
(with-sound (:clipped #f)  
  (let ((gen (make-expcs 100 :t 0.1))
	(t-env (make-env '(0 .1 1 2) :end 20000)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 20000))
	(set! (expcs-t gen) (env t-env))
	(set! (expcs-cosh-t gen) (cosh (expcs-t gen)))
	(let ((exp-t (exp (- (expcs-t gen)))))
	  (set! (expcs-offset gen) (/ (- 1.0 exp-t) (* 2.0 exp-t)))
	  (set! (expcs-scaler gen) (* (sinh (expcs-t gen)) (expcs-offset gen))))
	(outa i (expcs gen 0.0) *output*))))))
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
  (frequency 440.0) 
  (initial-phase 0.0)
  (n 1 :type int)
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
		       (let ((gen (make-oddsin 40.0 :n i)))
			 (do ((k 0 (1+ k)))
			     ((= k 1000))
			   (outa k (oddsin gen 0.0) *output*))))))
    (snd-display "~A: ~A ~A" i (vct-peak v) (/ i (vct-peak v)))))
|#

#|
(with-sound (:clipped #f)
  (let ((gen (make-oddsin 100 :n 10)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 10000))
	(outa i (oddsin gen 0.0) *output*))))))
|#


;;; TODO: what happens here if "n" changes?

;;; --------------------------------------------------------------------------------

;;; asymmetric fm gens

(def-clm-struct (asyfm :make-wrapper (lambda (gen)
				       (set! (asyfm-freq gen) (hz->radians (asyfm-frequency gen)))
				       (set! (asyfm-phase gen) (asyfm-initial-phase gen))
				       gen))
  (frequency 0.0) (initial-phase 0.0) 
  (ratio 1.0) (r 1.0) (index 1.0)
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

;;; TODO: need to check asyfm-I amp norm


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


(define make-jackson make-fejer)

(define (jackson gen fm)
  "(poussin-sum angle n) produces a pulse train."
  ;; Katznelson p16

  (declare (gen fejer) (fm float))
  (let ((val (fejer gen fm)))
    (* val val))) ; we already normalized this to 1.0


#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-jackson 100.0 :n 10)))
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (jackson gen 0.0) *output*))))))
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


(def-clm-struct (legendre
		 :make-wrapper
		 (lambda (g)
		   (set! (legendre-n1 g) (+ 1 (* 2 (legendre-n g))))
		   (set! (legendre-incr g) (hz->radians (legendre-frequency g)))
		   (set! (legendre-angle g) (legendre-initial-phase g))
		   g))
  (frequency 0.0) (initial-phase 0.0) (n 1 :type int) 
  (n1 1 :type int)
  (angle 0.0) (incr 0.0))
		   
(define (legendre gen)
  "(legendre-sum angle n) produces a band-limited pulse train"
  ;; from Andrews, Askey, Roy "Special Functions" p 314 with my amplitude scaling
  (declare (gen legendre))
  (let* ((angle (legendre-angle gen))
	 (result (if (< (abs angle) 1.0e-9)
		     1.0
		     (let* ((n (legendre-n gen))
			    (n1 (legendre-n1 gen))
			    (val (/ (sin (* angle (+ n 0.5)))
				   (* (sin (* 0.5 angle)) n1))))
		       (* val val)))))
    (set! (legendre-angle gen) (+ (legendre-angle gen) (legendre-incr gen)))
    result))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-legendre 100.0 :n 10)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 20000))
      (outa i (legendre gen) *output*))))))
|#


;;; "kosine-summation" kernel is just a form of oscil, as is cosine-summation (both in dsp.scm)
;;; cosine-summation is a variant of the G&R 2nd col 4th row

;;; SOMEDAY: move cosine-summation and kosine-summation to this file

;;; TODO: from dsp: sum-of-whatever, bl-saw, bl-sq

;;; TODO: make a list of the gens in dsp.scm etc that use filter or some other built-in

;;; --------------------------------------------------------------------------------

;;; Jolley 2nd col 2nd row (1st row is cos tweak of this)

(def-clm-struct (sinhfm
		 :make-wrapper
		 (lambda (g)
		   (set! (sinhfm-cosx g) (make-oscil (sinhfm-frequency g) (sinhfm-initial-phase g)))
		   (set! (sinhfm-sinx g) (make-oscil (sinhfm-frequency g) (sinhfm-initial-phase g)))
		   (if (>= (* (sinhfm-a g) (sinhfm-a g)) 1.0)
		       (set! (sinhfm-a g) 0.9999999))
		   g))
  (frequency 0.0) (initial-phase 0.0) (a 0.0)
  (cosx #f :type clm) (sinx #f :type clm))

(define (sinhfm gen fm)
  (declare (gen sinhfm))
  (* (sinh (* (sinhfm-a gen)
	      (oscil (sinhfm-cosx gen) fm)))
     (cos (* (sinhfm-a gen)
	     (oscil (sinhfm-sinx gen) fm)))))

#|
;;; odd harmonics, but we can't push the upper partials past the (2k)! range, so not very flexible

(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-sinhfm 100.0 :a 0.5)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 20000))
      (outa i (sinhfm gen 0.0) *output*))))))
|#

;;; TODO: use with the cos version to make ssb version


;;; --------------------------------------------------------------------------------

;;; bess (returns bes-jn, like oscil returns sin)
;;;   frequency here is the frequency in Hz of the damped sinusoid part of the bessel function

(def-clm-struct (bess
		 :make-wrapper
		 (lambda (g)
		   (set! (bess-incr g) (hz->radians (bess-frequency g)))
		   (set! (bess-arg g) (bess-initial-phase g))
		   g))
  (frequency 0.0) (initial-phase 0.0) (n 0 :type int)
  (arg 0.0) (incr 0.0))

(define (bess gen fm)
  (declare (gen bess) (fm float))
  (let ((result (bes-jn (bess-n gen) (bess-arg gen))))
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
|#

;;; PERHAPS: peak amp here depends on n -- should (can) we fix that?
;;; --------------------------------------------------------------------------------

;;; Jolley 1st col 2nd row
;;;   heads toward a square wave as "a" -> 0.0 (odd harmonics, 1/k amp)

(def-clm-struct (ecos 
		 :make-wrapper
		 (lambda (g)
		   (set! (ecos-osc g) (make-oscil (ecos-frequency g) (ecos-initial-phase g)))
		   g))
  (frequency 0.0) (initial-phase (* 0.5 pi)) (a 1.0)
  (osc #f :type clm))

(define (ecos gen fm)
  (declare (gen ecos) (fm float))
  (* -0.5 (atan (/ (oscil (ecos-osc gen) fm) (sinh (ecos-a gen))))))


#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ecos 400.0 :a 1.0)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 10000))
      (outa i (ecos gen 0.0) *output*))))))

(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ecos 400.0 :a 0.0))
	(a-env (make-env '(0 0 1 1) :end 10000)))
    (run (lambda () 
	   (do ((i 0 (1+ i)))
	       ((= i 10000))
	     (set! (ecos-a gen) (env a-env))
	     (outa i (ecos gen 0.0) *output*))))))

(with-sound (:clipped #f :statistics #t)
  (let ((gen1 (make-ecos 400.0 :a 0.0))
	(gen2 (make-oscil 400.0))
	(a-env (make-env '(0 0 1 1) :end 10000)))
    (run (lambda () 
	   (do ((i 0 (1+ i)))
	       ((= i 10000))
	     (set! (ecos-a gen1) (env a-env))
	     (outa i (ecos gen1 (* .1 (oscil gen2))) *output*))))))
|#

;;; TODO: amp depends on a -- fixup

;;; G&R 2nd col 7th row is sine form of this -- perhaps that form is simpler to normalize?

;;; --------------------------------------------------------------------------------

;;; Jolley 1st col 3rd row -- using def-clm-struct to simplify make-*

(def-clm-struct (acosum
		 :make-wrapper
		 (lambda (g)
		   (set! (acosum-osc g) (make-oscil (acosum-frequency g) (acosum-initial-phase g)))
		   g))
  (frequency 0.0) (initial-phase 0.0)
  (osc #f :type clm))

(define (acosum gen fm)
  (declare (gen acosum) (fm float))
  (let ((arg (* 2.0 (oscil (acosum-osc gen) fm))))
    (if (>= arg 0.0)
	(* 0.5 (acos (- 1.0 arg)))
	(* -0.5 (acos (+ 1.0 arg))))))

(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-acosum 400.0)))
    (run (lambda ()
	   (do ((i 0 (1+ i)))
	       ((= i 10000))
	     (outa i (* .3 (acosum gen 0.0)) *output*))))))


;;; as printed in J, this is not usable -- 1-2sin can be 3 so acos will be complex -- looks like we're missing: x < pi
;;; needs normalization and it's not right anyway -- we get odd harmonics but wrong amps

;;; --------------------------------------------------------------------------------


