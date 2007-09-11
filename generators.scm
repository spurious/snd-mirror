(provide 'snd-generators.scm)
(if (not (provided? 'snd-ws.scm)) (load-from-path "ws.scm"))


;;; these try to mimic existing gens (mainly oscil), so "frequency" and "initial-phase" are placed first
;;;   so the make function places those two args first


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

(def-clm-struct (ssboddsum
		 :make-wrapper
		 (lambda (g)
		   (set! (ssboddsum-carincr g) (hz->radians (- (ssboddsum-carfreq g) (ssboddsum-modfreq g))))
		   (set! (ssboddsum-modincr g) (hz->radians (ssboddsum-modfreq g)))
		   g))
  (carfreq 0.0) (modfreq 0.0) (n 1 :type int)
  (carangle 0.0) (carincr 0.0) (modangle 0.0) (modincr 0.0))

(define (ssboddsum gen fm)
  (declare (gen ssboddsum) (fm float))
  (let* ((cx (ssboddsum-carangle gen))
	 (mx (ssboddsum-modangle gen))
	 (n (ssboddsum-n gen))
	 (sinnx (sin (* n mx)))
	 (den (* n (sin mx)))) ; "n" is normalization
    (set! (ssboddsum-carangle gen) (+ cx (ssboddsum-carincr gen) fm))
    (set! (ssboddsum-modangle gen) (+ mx (ssboddsum-modincr gen)))
    (if (< (abs den) 1.0e-9)
	0.0
	(- (* (sin cx)
	      (/ (* sinnx sinnx) den))
	   (* (cos cx)
	      (/ (sin (* 2 n mx))
		 (* 2 den)))))))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ssboddsum 1000.0 100.0 5)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 10000))
	(outa i (ssboddsum gen 0.0) *output*))))))
|#


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



(def-clm-struct (rcos
		 :make-wrapper
		 (lambda (g)
		   (set! (rcos-osc g) (make-oscil (rcos-frequency g) (rcos-initial-phase g)))
		   g))
  (frequency 0.0) (initial-phase (* 0.5 pi)) (r 1.0) ; 'r' controls successive sinusoid amplitudes
  (osc #f :type clm))

(define (rcos gen fm)
  ;; from Andrews, Askey, Roy "Special Functions" 5.1.16
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


(def-clm-struct (ssbh
		 :make-wrapper
		 (lambda (g)
		   (set! (ssbh-carincr g) (hz->radians (ssbh-carfreq g)))
		   (set! (ssbh-modincr g) (hz->radians (ssbh-modfreq g)))
		   g))
  (carfreq 0.0) (modfreq 0.0) (a 1.0)
  (carangle 0.0) (carincr 0.0)
  (modangle 0.0) (modincr 0.0))

(define (ssbh gen)
  (declare (gen ssbh))
  (let* ((mx (ssbh-modangle gen))
	 (cx (ssbh-carangle gen))
	 (a (ssbh-a gen))
	 (asinx (* a (sin mx)))
	 (acosx (* a (cos mx))))

    (set! (ssbh-carangle gen) (+ (ssbh-carangle gen) (ssbh-carincr gen)))
    (set! (ssbh-modangle gen) (+ (ssbh-modangle gen) (ssbh-modincr gen)))

    (/ (- (* (cos cx)
	     (cosh acosx)
	     (cos asinx))
	  (* (sin cx)
	     (sinh acosx)
	     (sin asinx)))
       (cosh a)))) ; normalization

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ssbh 1000.0 100.0 0.5)))
    (run 
     (lambda () 
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (ssbh gen) *output*))))))
|#


;;; --------------------------------------------------------------------------------

;;; bess (returns bes-jn, like oscil returns sin)
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
  (let ((result (/ (bes-jn (bess-n gen) (bess-arg gen)) (bess-norm gen))))
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

|#

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


;;; Jolley 1st col 3rd row 

(define make-acosum make-oscil)

(define (acosum gen fm)
  (declare (gen clm) (fm float))
  (let ((arg (* 2.0 (oscil gen fm))))
    (if (>= arg 0.0)
	(* 0.5 (acos (- 1.0 arg)))
	(* -0.5 (acos (+ 1.0 arg))))))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-acosum 400.0)))
    (run (lambda ()
	   (do ((i 0 (1+ i)))
	       ((= i 10000))
	     (outa i (* .3 (acosum gen 0.0)) *output*))))))
|#

;;; as printed in J, this is not usable -- 1-2sin can be 3 so acos will be complex -- looks like we're missing: x < pi
;;; needs normalization and it's not right anyway -- we get odd harmonics but wrong amps

;;; --------------------------------------------------------------------------------


;;; this has poor side-band cancellation (0.093)

(def-clm-struct (ssbsum
		 :make-wrapper
		 (lambda (g)
		   (set! (ssbsum-carsin g) (make-oscil (ssbsum-carrier-frequency g) 0.0))
		   (set! (ssbsum-carcos g) (make-oscil (ssbsum-carrier-frequency g) (* 0.5 pi)))
		   (set! (ssbsum-sinsum g) (make-sum-of-sines (ssbsum-n g) (ssbsum-sum-frequency g) (ssbsum-initial-phase g)))		  
		   (set! (ssbsum-cossum g) (make-sum-of-cosines (ssbsum-n g) (ssbsum-sum-frequency g) (ssbsum-initial-phase g)))   	  
		   g))
  (n 1 :type int) (carrier-frequency 440.0) (initial-phase 0.0) ; this order mimics make-sum-of-(co)sines
  (sum-frequency 440.0) (carsin #f :type clm) (carcos #f :type clm) (sinsum #f :type clm) (cossum #f :type clm))

(define (ssbsum gen fm)
  (declare (gen ssbsum) (fm float))
  (* (- (* (oscil (ssbsum-carsin gen))
	   (sum-of-sines (ssbsum-sinsum gen) fm))
	(* (oscil (ssbsum-carcos gen))
	   (sum-of-cosines (ssbsum-cossum gen) fm) 1.0))
     0.5))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ssbsum 3 400.0 0.0 100.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (* .3 (ssbsum gen 0.0)) *output*))))))
|#


;;; much better cancellation:

(def-clm-struct (ssbsum1 
		 :make-wrapper
		 (lambda (g)
		   (set! (ssbsum1-incr1 g) (hz->radians (ssbsum1-carfreq g)))
		   (set! (ssbsum1-incr2 g) (hz->radians (ssbsum1-modfreq g)))
		   g))
  (n 0 :type int) (carfreq 0.0) (modfreq 0.0) 
  (angle1 0.0) (angle2 0.0) (incr1 0.0) (incr2 0.0))

(define (ssbsum1 gen fm)
  (declare (gen ssbsum1) (fm float))
  (let* ((n (ssbsum1-n gen))
	 (angle1 (ssbsum1-angle1 gen))
	 (angle2 (ssbsum1-angle2 gen))
	 (carsin (sin angle1))
	 (carcos (cos angle1))
	 (den (sin (* 0.5 angle2)))
	 (sumsin (* (sin (* angle2 (/ (+ n 1) 2))) 
		    (sin (/ (* n angle2) 2))))
	 (sumcos (* 0.5 (+ den (sin (* angle2 (+ n 0.5))))))
	 (result (/ (- (* carsin sumsin)
		       (* carcos sumcos))
		    (* 2 den))))
    (set! (ssbsum1-angle1 gen) (+ (ssbsum1-angle1 gen) (ssbsum1-incr1 gen)))
    (set! (ssbsum1-angle2 gen) (+ (ssbsum1-angle2 gen) (ssbsum1-incr2 gen)))
    result))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ssbsum1 3 2000.0 100.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (* .3 (ssbsum1 gen 0.0)) *output*))))))
|#


;;; --------------------------------------------------------------------------------


;;; ssb r^n case

(def-clm-struct (ssbr 
		 :make-wrapper
		 (lambda (g)
		   (set! (ssbr-incr1 g) (hz->radians (ssbr-carfreq g)))
		   (set! (ssbr-incr2 g) (hz->radians (ssbr-modfreq g)))
		   g))
  (n 0 :type int) (carfreq 0.0) (modfreq 0.0) (r 0.0)
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
  (let ((gen (make-ssbr 3 2000.0 103.0 0.5)))
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
  (frequency 0.0) (a 0.0) (n 1 :type int)
  (angle 0.0) (incr 0.0))

(define (blsaw gen)
  "blsaw produces a band-limited sawtooth"
  (declare (gen blsaw))
  (let* ((a (blsaw-a gen))
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
  (let ((gen (make-blsaw 440.0 :a 0.5 :n 3)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (blsaw gen) *output*))))))
|#

;;; needs normalization


;;; --------------------------------------------------------------------------------


;;; Jolley 1st col 1st row

;;; named "mehler" because the general form 1/(a + bcos x) is assoicated with "Mehler's formula"
;;;   in Sansone, "Orthogonal Functions" p182

(def-clm-struct (mehler
		 :make-wrapper
		 (lambda (g)
		   (set! (mehler-angle g) (mehler-initial-phase g))
		   (set! (mehler-incr g) (hz->radians (mehler-frequency g)))
		   g))
  (frequency 0.0) (initial-phase 0.0)
  (angle 0.0) (incr 0.0))

(define (mehler gen fm)
  (declare (gen mehler) (fm float))
  (let ((x (mehler-angle gen)))
    (set! (mehler-angle gen) (+ x (mehler-incr gen)))
    (/ (* 3.0 (sin x)) ; 3 rather than 4 for normalization
       (- 5.0 (* 4.0 (cos x))))))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-mehler 440.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (mehler gen 0.0) *output*))))))
|#

;;; --------------------------------------------------------------------------------


;;; G&R 2nd col 1st and 2nd rows

(def-clm-struct (ssbrksum
		 :make-wrapper
		 (lambda (g)
		   (set! (ssbrksum-carincr g) (hz->radians (ssbrksum-carfreq g)))
		   (set! (ssbrksum-modincr g) (hz->radians (ssbrksum-modfreq g)))
		   g))
  (carfreq 0.0) (modfreq 0.0) (n 1 :type int) (r 0.0)
  (carangle 0.0) (modangle 0.0) (carincr 0.0) (modincr 0.0))

(define (ssbrksum gen)
  (declare (gen ssbrksum))
  (let* ((cx (ssbrksum-carangle gen))
	 (mx (ssbrksum-modangle gen))
	 (ci (ssbrksum-carincr gen))
	 (mi (ssbrksum-modincr gen))
	 (r (ssbrksum-r gen))
	 (n (ssbrksum-n gen))
	 (rn (- (expt r n)))
	 (rn1 (expt r (+ n 1)))
	 (nmx (* n mx))
	 (n1mx (* (- n 1) mx))
	 (norm (/ (- (expt r n) 1) (- r 1))) ; this could use rn
	 (den (* norm (+ 1.0 (* -2.0 r (cos mx)) (* r r)))))
    (set! (ssbrksum-carangle gen) (+ cx ci))
    (set! (ssbrksum-modangle gen) (+ mx mi))
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
  (let ((gen (make-ssbrksum 1000 100 5 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (ssbrksum gen) *output*))))))
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

(def-clm-struct (ssbor
		 :make-wrapper
		 (lambda (g)
		   (set! (ssbor-carincr g) (hz->radians (- (ssbor-carfreq g) (ssbor-modfreq g))))
		   (set! (ssbor-modincr g) (hz->radians (ssbor-modfreq g)))
		   g))
  (carfreq 0.0) (modfreq 0.0) (r 0.0)
  (carangle 0.0) (carincr 0.0)
  (modangle 0.0) (modincr 0.0))

(define (ssbor gen fm)
  (declare (gen ssbor))
  (let* ((r (ssbor-r gen))
	 (mx (ssbor-modangle gen))
	 (cx (ssbor-carangle gen))
	 (cfm (* fm (/ (ssbor-carincr gen) (ssbor-modincr gen)))))

    (set! (ssbor-carangle gen) (+ cfm (ssbor-carangle gen) (ssbor-carincr gen)))
    (set! (ssbor-modangle gen) (+ fm (ssbor-modangle gen) (ssbor-modincr gen)))

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
  (let ((gen (make-ssbor 1000.0 100.0 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (ssbor gen 0.0) *output*))))))
|#



;;; --------------------------------------------------------------------------------

;;; DSP.SCM:
;;; fir-filter: hilbert-transform, highpass, lowpass, bandpass, bandstop, differentiator
;;;             make-spencer-filter, savitzky-golay-filter
;;;
;;; filter: butter-high-pass, butter-low-pass, butter-band-pass, butter-band-reject, biquad,
;;;         iir-low-pass, iir-high-pass, iir-band-pass, iir-band-stop, peaking
;;;         butter-lp, butter-hp, butter-bp, butter-bs
;;;
;;; delay: moving-max
;;; average:  moving-sum, moving-rms, moving-length, weighted-moving-average
;;; one-pole: exponentially-weighted-moving-average 
;;; mfilter, volterra-filter

;;; ANALOG-FILTER.SCM:
;;; filter: butterworth-lowpass|highpass|bandpass|bandstop, chebyshev-lowpass|highpass|bandpass|bandstop, 
;;;         inverse-chebyshev-lowpass|highpass|bandpass|bandstop, elliptic-lowpass|highpass|bandpass|bandstop,
;;;         bessel-lowpass|highpass|bandpass|bandstop

;;; ENV.SCM:
;;; power-env (and many env makers/modifiers)

;;; EXAMP.SCM:
;;; ramp, sound-interp
;;; [filtered-env?]

;;; GREEN.SCM:
;;; rand and rand-interp: green-noise, brownian-noise

;;; MOOG.SCM:
;;; moog-filter

;;; PRC95.SCM:
;;; reed, bowtable, jettable, onep, lip, dc-block, delaya, delayl

;;; SNDCLM.HTML:
;;; band-limited-triangle-wave, sinc-train, sum-of-odd-sines, 1f-noise


;;; --------------------------------------------------------------------------------

;;; TODO: in docs, dsp->gen or snd9
;;; TODO: in docs: add a ref to each gen from the formula (may need to split into bazillions of cases)


;;; PERHAPS: a generators table for quick.html?

;;; TODO: gegen+cos as gen (and legendre(cos) bessel(cos)?) -- find expansions of these if only for doc

;;; --------------------------------------------------------------------------------


