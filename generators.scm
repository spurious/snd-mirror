 (provide 'snd-generators.scm)
(if (not (provided? 'snd-ws.scm)) (load-from-path "ws.scm"))


;;; these try to mimic existing gens (mainly oscil), so "frequency" is placed first.
;;;   Where a factor is involved, I'll try to use "r".
;;;   Where the limit of the sum is settable, I'll use "n".


(define nearly-zero 1.0e-12) ; 1.0e-14 in clm.c, but that is trouble here (noddcos)


;;; --------------------------------------------------------------------------------

;;; n sinusoids, equal amps: ncos, nsin, nssb

(define ncos sum-of-cosines)
(define make-ncos make-sum-of-cosines)
(define ncos? sum-of-cosines?)

(define nsin sum-of-sines)
(define make-nsin make-sum-of-sines)
(define nsin? sum-of-sines?)

(def-clm-struct (nssb 
		 :make-wrapper (lambda (g)
				 (set! (nssb-carincr g) (hz->radians (nssb-carfreq g)))
				 (set! (nssb-modincr g) (hz->radians (nssb-modfreq g)))
				 g))
  (carfreq 0.0) (modfreq 1.0) (n 1 :type int)
  (carangle 0.0) (modangle 0.0) (carincr 0.0) (modincr 0.0))

(define (nssb gen fm)
  (declare (gen nssb) (fm float))
  (let* ((n (nssb-n gen))
	 (cx (nssb-carangle gen))
	 (mx (nssb-modangle gen))
	 (den (sin (* 0.5 mx)))
	 (cfm (* fm (/ (nssb-carincr gen) (nssb-modincr gen)))))

    (set! (nssb-carangle gen) (+ cfm cx (nssb-carincr gen)))
    (set! (nssb-modangle gen) (+ fm mx (nssb-modincr gen)))

    (if (< (abs den) nearly-zero)
	-1.0
	(/ (- (* (sin cx) 
		 (sin (* mx (/ (+ n 1) 2)))
		 (sin (/ (* n mx) 2)))
	      (* (cos cx) 
		 0.5 (+ den (sin (* mx (+ n 0.5))))))
	   (* (+ n 1) den)))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nssb 1000.0 100.0 3)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (nssb gen 0.0) *output*))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nssb 1000.0 100.0 3))
	(vib (make-oscil 5.0))
	(ampf (make-env '(0 0 1 1 2 1 3 0) :end 20000 :scaler 1.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (* (env ampf) 
		    (nssb gen (* (hz->radians 10.0) 
				 (oscil vib))))
	       *output*))))))
|#


;;; --------------------------------------------------------------------------------

;;; G&R 1st col rows 1&2

(def-clm-struct (nxysin
		 :make-wrapper (lambda (g)
				 (set! (nxysin-xincr g) (hz->radians (nxysin-xfrequency g))) ; can be 0 if just x=pi/2 for example
				 (set! (nxysin-yincr g) (hz->radians (nxysin-yfrequency g)))
				 g))
  (xfrequency 0.0) (yfrequency 1.0) (n 1 :type int)
  (xangle 0.0) (xincr 0.0) (yangle 0.0) (yincr 0.0))

(define (nxysin gen fm)
  (declare (gen nxysin) (fm float))
  (let* ((x (nxysin-xangle gen))
	 (y (nxysin-yangle gen))
	 (n (nxysin-n gen))
	 (den (sin (* y 0.5))))
    (set! (nxysin-xangle gen) (+ x (nxysin-xincr gen) (* fm (/ (nxysin-xincr gen) (nxysin-yincr gen)))))
    (set! (nxysin-yangle gen) (+ y (nxysin-yincr gen) fm))
    (if (< (abs den) nearly-zero)
	0.0
	(/ (* (sin (+ x (* 0.5 (- n 1) y)))
	      (sin (* 0.5 n y)))
	   den))))

;;; normalization here is hard (depends on x and y)
#|
(with-sound (:clipped #f :statistics #t :play #t :scaled-to .5)
  (let ((gen (make-nxysin 300 100 3)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (nxysin gen 0.0) *output*))))))
|#

(def-clm-struct (nxycos
		 :make-wrapper (lambda (g)
				 (set! (nxycos-xincr g) (hz->radians (nxycos-xfrequency g))) ; can be 0 if just x=pi/2 for example
				 (set! (nxycos-yincr g) (hz->radians (nxycos-yfrequency g)))
				 g))
  (xfrequency 0.0) (yfrequency 1.0) (n 1 :type int)
  (xangle 0.0) (xincr 0.0) (yangle 0.0) (yincr 0.0))

(define (nxycos gen fm)
  (declare (gen nxycos) (fm float))
  (let* ((x (nxycos-xangle gen))
	 (y (nxycos-yangle gen))
	 (n (nxycos-n gen))
	 (den (sin (* y 0.5))))
    (set! (nxycos-xangle gen) (+ x (nxycos-xincr gen) (* fm (/ (nxycos-xincr gen) (nxycos-yincr gen)))))
    (set! (nxycos-yangle gen) (+ y (nxycos-yincr gen) fm))
    (if (< (abs den) nearly-zero)
	1.0
	(/ (* (cos (+ x (* 0.5 (- n 1) y)))
	      (sin (* 0.5 n y)))
	   (* n den))))) ; n=normalization

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nxycos 300 100 3)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (nxycos gen 0.0) *output*))))))
|#

;;; does ssb make any sense here?  x=carrier effectively
;;; is there any need for the (-1)^k case?  


;;; --------------------------------------------------------------------------------
;;;
;;; G&R 1st col rows 3 4

(def-clm-struct (nxy1cos
		 :make-wrapper (lambda (g)
				 (set! (nxy1cos-xincr g) (hz->radians (nxy1cos-xfrequency g))) ; can be 0 if just x=pi/2 for example
				 (set! (nxy1cos-yincr g) (hz->radians (nxy1cos-yfrequency g)))
				 g))
  (xfrequency 0.0) (yfrequency 0.0) (n 1 :type int)
  (xangle 0.0) (xincr 0.0) (yangle 0.0) (yincr 0.0))

(define (nxy1cos gen fm)
  (declare (gen nxy1cos) (fm float))
  (let* ((x (nxy1cos-xangle gen))
	 (y (nxy1cos-yangle gen))
	 (n (nxy1cos-n gen))
	 (den (cos (* y 0.5))))
    (set! (nxy1cos-xangle gen) (+ x (nxy1cos-xincr gen) (* fm (/ (nxy1cos-xincr gen) (nxy1cos-yincr gen)))))
    (set! (nxy1cos-yangle gen) (+ y (nxy1cos-yincr gen) fm))
    (max -1.0
	 (min 1.0
	      (/ (* (sin (* n y))
		    (sin (+ x (* (- n 0.5) y))))
		 (* 2 n den))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nxy1cos 300 100 3)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (nxy1cos gen 0.0) *output*))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nxy1cos 300 100 3))
	(gen1 (make-nxycos 300 100 6)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (* 0.4 (+ (nxycos gen1 0.0) (nxy1cos gen 0.0))) *output*))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nxy1cos (radians->hz (* .01 pi)) (radians->hz (* .01 pi)) 3)))
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (nxy1cos gen 0.0) *output*))))
|#


(def-clm-struct (nxy1sin
		 :make-wrapper (lambda (g)
				 (set! (nxy1sin-xincr g) (hz->radians (nxy1sin-xfrequency g))) ; can be 0 if just x=pi/2 for example
				 (set! (nxy1sin-yincr g) (hz->radians (nxy1sin-yfrequency g)))
				 g))
  (xfrequency 0.0) (yfrequency 0.0) (n 1 :type int)
  (xangle 0.0) (xincr 0.0) (yangle 0.0) (yincr 0.0))

(define (nxy1sin gen fm)
  (declare (gen nxy1sin) (fm float))
  (let* ((x (nxy1sin-xangle gen))
	 (y (nxy1sin-yangle gen))
	 (n (nxy1sin-n gen))
	 (den (cos (* y 0.5))))
    (set! (nxy1sin-xangle gen) (+ x (nxy1sin-xincr gen) (* fm (/ (nxy1sin-xincr gen) (nxy1sin-yincr gen)))))
    (set! (nxy1sin-yangle gen) (+ y (nxy1sin-yincr gen) fm))

    (/ (* (sin (+ x (* 0.5 (- n 1) (+ y pi))))
	  (sin (* 0.5 n (+ y pi))))
       (* n den)))) ; norm not right...

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nxy1sin 300 100 3)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (nxy1sin gen 0.0) *output*))))))
|#


;;; --------------------------------------------------------------------------------

;;; n odd sinusoids: noddsin, noddcos, noddssb

;;; sndclm.html (G&R) 1st col 5th row (sum of odd sines)

(def-clm-struct (noddsin 
		 :make-wrapper (lambda (g)
				 (if (< (noddsin-n g) 1) (set! (noddsin-n g) 1))
				 (set! (noddsin-incr g) (hz->radians (noddsin-frequency g)))
				 (set! (noddsin-norm g) (if (= (noddsin-n g) 1) 1.0
							    (/ (if (= (noddsin-n g) 2) 1.29
								   (if (= (noddsin-n g) 3) 1.34
								       (if (< (noddsin-n g) 6) 1.36
									   (if (< (noddsin-n g) 18) 1.37
									       1.379))))
							       (noddsin-n g))))
				 g))
  (frequency 440.0) (n 1 :type int)
  (angle 0.0) (incr 0.0) (norm 1.0))

(define (noddsin gen fm)
  (declare (gen noddsin) (fm float))
  (let* ((x (noddsin-angle gen))
	 (n (noddsin-n gen))
	 (norm (noddsin-norm gen))
	 (snx (sin (* n x)))
	 (den (sin x)))
    (set! (noddsin-angle gen) (+ x fm (noddsin-incr gen)))
    (if (< (abs den) nearly-zero)
	0.0
	(/ (* norm snx snx) den))))
	   
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
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-noddsin 300 :n 3))) ; clarinety
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 10000))
	(outa i (noddsin gen 0.0) *output*))))))
|#


(def-clm-struct (noddcos
		 :make-wrapper (lambda (g)
				 (set! (noddcos-incr g) (hz->radians (noddcos-frequency g)))
				 g))
  (frequency 0.0) (n 1 :type int)
  (angle 0.0) (incr 0.0))

(define (noddcos gen fm)
  (declare (gen noddcos) (fm float))
  (let* ((angle (noddcos-angle gen))
	 (n (noddcos-n gen))
	 (den (* 2 n (sin angle)))) ; "n" here is normalization
    (set! (noddcos-angle gen) (+ angle fm (noddcos-incr gen)))
    (if (< (abs den) nearly-zero)
	(let ((fang (fmod (abs angle) (* 2 pi))))
	  ;; hopefully this almost never happens...
	  (if (or (< fang 0.001)
		  (< (abs (- fang (* 2 pi))) 0.001))
	      1.0
	      -1.0))
	(/ (sin (* 2 n angle)) den))))

;;; (Gradshteyn and Ryzhik 1.342)

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-noddcos 100 :n 10)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 10000))
	(outa i (noddcos gen 0.0) *output*))))))
|#

(def-clm-struct (noddssb
		 :make-wrapper (lambda (g)
				 (set! (noddssb-carincr g) (hz->radians (- (noddssb-carfreq g) (noddssb-modfreq g))))
				 (set! (noddssb-modincr g) (hz->radians (noddssb-modfreq g)))
				 g))
  (carfreq 0.0) (modfreq 1.0) (n 1 :type int)
  (carangle 0.0) (carincr 0.0) (modangle 0.0) (modincr 0.0))

(define (noddssb gen fm)
  (declare (gen noddssb) (fm float))
  (let* ((cx (noddssb-carangle gen))
	 (mx (noddssb-modangle gen))
	 (n (noddssb-n gen))
	 (sinnx (sin (* n mx)))
	 (cfm (* fm (/ (noddssb-carincr gen) (noddssb-modincr gen))))
	 (den (* n (sin mx)))) ; "n" is normalization
    (set! (noddssb-carangle gen) (+ cx cfm (noddssb-carincr gen) fm))
    (set! (noddssb-modangle gen) (+ mx fm (noddssb-modincr gen)))
    (max -1.0  ; -1.0 is probably the peak, trying to catch bad cases is too much trouble here
	 (min 1.0
	      (- (* (sin cx)
		    (/ (* sinnx sinnx) den))
		 (* (cos cx)
		    (/ (sin (* 2 n mx))
		       (* 2 den))))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-noddssb 1000.0 100.0 5)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 10000))
	(outa i (noddssb gen 0.0) *output*))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-noddssb 1000.0 100.0 5))
	(vib (make-oscil 5.0)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 10000))
	(outa i (noddssb gen (* (hz->radians 10.0) (oscil vib))) *output*))))))
|#



;;; --------------------------------------------------------------------------------

;;; various kernels: ncos2 = ncos squared (Fejer), ncos4 = ncos2 squared (Jackson), npcos = Poussin kernel

(def-clm-struct (ncos2
		 :make-wrapper (lambda (g)
				 (set! (ncos2-incr g) (hz->radians (ncos2-frequency g)))
				 g))
  (frequency 0.0) (n 1 :type int) 
  (angle 0.0) (incr 0.0))

(define (ncos2 gen fm)
  "(ncos2-pulse gen fm) produces a band-limited pulse train"
  (declare (gen ncos2) (fm float))

  ;; from "Trigonometric Series" Zygmund p88 with changes suggested by Katznelson "Introduction to Harmonic Analysis" p12, and
  ;;   scaling by an extra factor of 1/n+1 to make sure we always peak at 1.0 (I assume callers in this context are interested 
  ;;   in the pulse-train aspect and want easily predictable peak amp).  Harmonics go as (n-i)/n+1.

  (let* ((x (ncos2-angle gen))
	 (n (ncos2-n gen))
	 (den (sin (* 0.5 x))))

    (set! (ncos2-angle gen) (+ x fm (ncos2-incr gen)))

    (if (< (abs den) nearly-zero)
	1.0
	(let ((val (/ (sin (* 0.5 (+ n 1) x)) 
		      (* (+ n 1) den))))
	  (* val val)))))

;;; can't use two oscils here because the angles have to line up perfectly

#|
(with-sound (:clipped #f :statistics #t :play #t)
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
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-ncos4 100.0 :n 10)))
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (ncos4 gen 0.0) *output*))))))
|#


(def-clm-struct (npcos
		 :make-wrapper (lambda (g)
				 (set! (npcos-incr g) (hz->radians (npcos-frequency g)))
				 g))
  (frequency 0.0) (n 1 :type int)
  (angle 0.0) (incr 0.0))

(define (npcos gen fm)
  (declare (gen npcos) (fm float))
  (let* ((angle (npcos-angle gen))
	 (den (sin (* 0.5 angle)))
	 (n (npcos-n gen))
	 (result (if (< (abs den) nearly-zero)
		     1.0
		     (let* ((n1 (+ n 1))
			    (result1 
			     (let ((val (/ (sin (* 0.5 n1 angle)) 
					   (* n1 den))))
			       (* val val)))
			    (p2n2 (+ (* 2 n) 2))
			    (result2 
			     (let ((val (/ (sin (* 0.5 p2n2 angle)) 
					   (* p2n2 den))))
			       (* val val))))
		       (- (* 2 result2) result1)))))
    (set! (npcos-angle gen) (+ fm angle (npcos-incr gen)))
    result))
    

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-npcos 100.0 :n 10)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 20000))
      (outa i (npcos gen) *output*))))))
|#



;;; --------------------------------------------------------------------------------

;;; n sinusoids scaled by r: nrsin, nrcos, nrssb

(define nrsin sine-summation)
(define make-nrsin make-sine-summation)
(define nrsin? sine-summation?)

(def-clm-struct (nrcos
		 :make-wrapper (lambda (g)
				 (set! (nrcos-incr g) (hz->radians (nrcos-frequency g)))
				 (set! (nrcos-n g) (+ 1 (nrcos-n g)))
				 g))
  (frequency 0.0) (n 1 :type int) (r 0.0)
  (angle 0.0) (incr 0.0))

(define (nrcos gen fm)
  (declare (gen nrcos) (fm float))
  (let* ((x (nrcos-angle gen))
	 (n (nrcos-n gen))
	 (r (nrcos-r gen))
	 (norm (- (/ (- (expt r n) 1) (- r 1)) 1.0)))

    (set! (nrcos-angle gen) (+ fm x (nrcos-incr gen)))

    (/ (+ (- (* r (cos x)) (* (expt r n) (cos (* n x))) (* r r)) 
	  (* (expt r (+ n 1)) (cos (* (- n 1) x))))
       (* norm (+ 1.0 (* -2.0 r (cos x)) (* r r))))))

;;; formula changed to start at k=1 and n increased so we get 1 to n

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nrcos 400.0 :n 5 :r 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (nrcos gen 0.0) *output*))))))

(with-sound (:srate 44100 :clipped #f :statistics #t :play #t)
  (let ((gen (make-nrcos 2000.0 :n 3 :r 0.5))
	(mod (make-oscil 400.0)) ; multi-carrier fm
	(index 0.02))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (nrcos gen (* index (oscil mod))) *output*))))))

(with-sound (:srate 44100 :clipped #f :statistics #t :play #t)
  (let ((gen (make-nrcos 2000.0 :n 3 :r 0.5))
	(mod (make-oscil 400.0))
	(index (make-env '(0 0 1 .1) :end 30000))) ; or '(0 .4 1 0)
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (nrcos gen (* (env index) (oscil mod))) *output*))))))

(definstrument (lutish beg dur freq amp)
  (let* ((res1 (max 1 (inexact->exact (round (/ 1000.0 (max 1.0 (min 1000.0 freq)))))))
	 (gen (make-nrcos (* freq res1) :n (max 1 (- res1 2))))
	 (mod (make-oscil freq))
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (maxind (max .01 (min .3 (/ (- (log freq) 3.5) 8.0))))
	 (index (make-env (list 0 maxind 1 (* maxind .25) (max dur 2.0) 0.0) :duration dur))
	 (amplitude (make-env (list 0 0  .01 1  .2 1  .5 .5  1 .25  (max dur 2.0) 0.0) :duration dur :scaler amp)))
    (run 
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((ind (env index)))
	   (set! (nrcos-r gen) ind)
	   (outa i (* (env amplitude)
		      (nrcos gen (* ind (oscil mod))))
		 *output*)))))))

(with-sound (:srate 44100 :clipped #f :statistics #t :play #t)
  (lutish 0 1 440 .1))

(with-sound (:srate 44100 :clipped #f :statistics #t :play #t)
  (do ((i 0 (1+ i)))
      ((= i 10))
    (lutish (* i .1) 2 (* 100 (1+ i)) .05)))
|#

;;; G&R 2nd col 1st and 2nd rows

(def-clm-struct (nrssb
		 :make-wrapper (lambda (g)
				 (set! (nrssb-carincr g) (hz->radians (nrssb-carfreq g)))
				 (set! (nrssb-modincr g) (hz->radians (nrssb-modfreq g)))
				 g))
  (carfreq 0.0) (modfreq 1.0) (n 1 :type int) (r 0.0)
  (carangle 0.0) (modangle 0.0) (carincr 0.0) (modincr 0.0))

(define (nrssb gen fm)
  (declare (gen nrssb) (fm float))
  (let* ((cx (nrssb-carangle gen))
	 (mx (nrssb-modangle gen))
	 (ci (nrssb-carincr gen))
	 (mi (nrssb-modincr gen))
	 (r (nrssb-r gen))
	 (n (nrssb-n gen))
	 (rn (- (expt r n)))
	 (rn1 (expt r (+ n 1)))
	 (nmx (* n mx))
	 (n1mx (* (- n 1) mx))
	 (norm (/ (- (expt r n) 1) (- r 1))) ; this could use rn
	 (den (* norm (+ 1.0 (* -2.0 r (cos mx)) (* r r))))
	 (cfm (* fm (/ (nrssb-carincr gen) (nrssb-modincr gen)))))

    (set! (nrssb-carangle gen) (+ cx cfm ci))
    (set! (nrssb-modangle gen) (+ mx fm mi))

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

(define (nrssb-interp gen fm interp)
  (declare (gen nrssb) (fm float))
  (let* ((cx (nrssb-carangle gen))
	 (mx (nrssb-modangle gen))
	 (ci (nrssb-carincr gen))
	 (mi (nrssb-modincr gen))
	 (r (nrssb-r gen))
	 (n (nrssb-n gen))
	 (rn (- (expt r n)))
	 (rn1 (expt r (+ n 1)))
	 (nmx (* n mx))
	 (n1mx (* (- n 1) mx))
	 (norm (/ (- (expt r n) 1) (- r 1))) ; this could use rn
	 (den (* norm (+ 1.0 (* -2.0 r (cos mx)) (* r r))))
	 (cfm (* fm (/ (nrssb-carincr gen) (nrssb-modincr gen)))))

    (set! (nrssb-carangle gen) (+ cx cfm ci))
    (set! (nrssb-modangle gen) (+ mx fm mi))

    (/ (- (* interp 
	     (sin cx)
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
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nrssb 1000 100 5 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (nrssb gen 0.0) *output*))))))

(definstrument (oboish beg dur freq amp aenv)
  (let* ((res1 (max 1 (inexact->exact (round (/ 1400.0 (max 1.0 (min 1400.0 freq)))))))
	 (gen (make-nrssb (* freq res1) freq :n res1 :r 0.75))
	 (mod (make-oscil 5.0))
	 (res2 (max 1 (inexact->exact (round (/ 2400.0 (max 1.0 (min 2400.0 freq)))))))
	 (gen2 (make-oscil (* freq res2)))
	 (gen3 (make-oscil freq))
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (amplitude (make-env aenv :duration dur :base 4 :scaler amp))
	 (skenv (make-env (list 0.0 0.0 1 1 2.0 (mus-random 1.0) 3.0 0.0 (max 4.0 (* dur 20.0)) 0.0) 
			  :duration dur :scaler (hz->radians (random (* freq .05)))))
	 (relamp (+ .85 (random .1)))
	 (avib (make-rand-interp 5 .2)))
    (run 
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let* ((vol (* (+ .8 (rand-interp avib)) 
			(env amplitude)))
		(vib (+ (* (hz->radians (* freq 0.003)) 
			   (oscil mod))
			(env skenv)))
		(vola (* 0.05 (/ vol amp)))
		(result (* vol
			   (+ (* (- relamp vola) 
				 (nrssb-interp gen vib -1.0))
			      (* (+ (- 1.0 relamp) vola) 
				 (oscil gen2 (+ (* vib res2)
						(* (hz->radians freq)
						   (oscil gen3 vib)))))))))
	   (outa i result *output*)
	   (if *reverb* (outa i (* .01 result) *reverb*))))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (oboish 0 1 300 .1 '(0 0 1 1 2 0)))

(with-sound (:clipped #f :statistics #t :play #t)
  (do ((i 0 (1+ i)))
      ((= i 10))
    (oboish (* i .3) .4 (+ 100 (* 50 i)) .05 '(0 0 1 1 2 1 3 0))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((rats (vector 1 256/243 9/8 32/27 81/64 4/3 1024/729 3/2 128/81 27/16 16/9 243/128 2))
	(mode (vector 0 0 2 4 11 11 5 6 7 9 2 12 0)))
    (do ((i 0 (1+ i)))
	((= i 20))
      (oboish (/ (random 32) 8) 
		(/ (+ 3 (random 8)) 8)
		(* 16.351 16 (vector-ref rats (vector-ref mode (random 12))))
		(+ .25 (random .25))
		(let* ((pt1 (random 1.0))
		       (pt2 (+ pt1 (random 1.0)))
		       (pt3 (+ pt2 (random 1.0))))
		  (list 0 0 pt1 1 pt2 .5 pt3 0))))))

;;; .85 .15 (* 2 freq) 300, 2400 + 0.5*vib
|#


;;; --------------------------------------------------------------------------------

;;; n sinusoids scaled by k: nkssb

;;; G&R 1st col ksinkx cases

(def-clm-struct (nkssb
		 :make-wrapper (lambda (g)
				 (set! (nkssb-carincr g) (hz->radians (- (nkssb-carfreq g) (nkssb-modfreq g))))
				 (set! (nkssb-modincr g) (hz->radians (nkssb-modfreq g)))
				 (set! (nkssb-n g) (+ 1 (nkssb-n g))) ; sum goes 1 to n-1
				 g))
  (carfreq 0.0) (modfreq 1.0) (n 1 :type int)
  (carangle 0.0) (carincr 0.0)
  (modangle 0.0) (modincr 0.0))


(define (nkssb gen fm)
  (declare (gen nkssb) (fm float))
  (let* ((n (nkssb-n gen))
	 (x (nkssb-modangle gen))
	 (sx2 (sin (* 0.5 x)))
	 (sx22 (* 2 sx2))
	 (sxsx (* 4 sx2 sx2))
	 (nx (* n x))
	 (nx2 (* 0.5 (- (* 2 n) 1) x))
	 (cx (nkssb-carangle gen))
	 (cfm (* fm (/ (nkssb-carincr gen) (nkssb-modincr gen)))))

    (set! (nkssb-carangle gen) (+ cfm (nkssb-carangle gen) (nkssb-carincr gen)))
    (set! (nkssb-modangle gen) (+ fm (nkssb-modangle gen) (nkssb-modincr gen)))

    (if (< (abs sx2) 1.0e-8)
	-1.0
	(let* ((s1 (- (/ (sin nx) sxsx)
		      (/ (* n (cos nx2)) sx22)))
	       (c1 (- (/ (* n (sin nx2)) sx22)
		      (/ (- 1.0 (cos nx)) sxsx))))
	  (/ (- (* (sin cx) s1)
		(* (cos cx) c1))
	     (* 0.5 n (- n 1))))))) ; normalization, nominal n is off by 1
	       
(define (nkssb-interp gen fm interp)
  (declare (gen nkssb) (fm float) (interp float))
  (let* ((n (nkssb-n gen))
	 (x (nkssb-modangle gen))
	 (sx2 (sin (* 0.5 x)))
	 (sx22 (* 2 sx2))
	 (sxsx (* 4 sx2 sx2))
	 (nx (* n x))
	 (nx2 (* 0.5 (- (* 2 n) 1) x))
	 (cx (nkssb-carangle gen))
	 (cfm (* fm (/ (nkssb-carincr gen) (nkssb-modincr gen)))))

    (set! (nkssb-carangle gen) (+ cfm (nkssb-carangle gen) (nkssb-carincr gen)))
    (set! (nkssb-modangle gen) (+ fm (nkssb-modangle gen) (nkssb-modincr gen)))

    (if (< (abs sx2) nearly-zero)
	-1.0
	(let* ((s1 (- (/ (sin nx) sxsx)
		      (/ (* n (cos nx2)) sx22)))
	       (c1 (- (/ (* n (sin nx2)) sx22)
		      (/ (- 1.0 (cos nx)) sxsx))))
	  (/ (- (* (cos cx) c1)
		(* interp (* (sin cx) s1)))
	     (* 0.5 n (- n 1))))))) ; normalization, nominal n is off by 1, peak seems to be solid right through the interpolation

	       
#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nkssb 1000.0 100.0 5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (nkssb gen 0.0) *output*))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nkssb 1000.0 100.0 5))
	(vib (make-oscil 5.0))
	(vibamp (hz->radians 5.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (nkssb gen (* vibamp (oscil vib))) *output*))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-nkssb 1000.0 100.0 5))
	(move (make-env '(0 1 1 -1) :end 30000))
	(vib (make-oscil 5.0))
	(vibamp (hz->radians 5.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (* 0.5 (nkssb-interp gen (* vibamp (oscil vib)) (env move))) *output*))))))

|#


;;; --------------------------------------------------------------------------------

;;; inf sinusoids scaled by r: rcos, rssb

(def-clm-struct (rcos
		 :make-wrapper (lambda (g)
				 (set! (rcos-osc g) (make-oscil (rcos-frequency g) (* 0.5 pi)))
				 g))
  (frequency 0.0) (r 1.0)
  (osc #f :type clm))

(define (rcos gen fm)
  ;; from Andrews, Askey, Roy "Special Functions" 5.1.16, p243. r^k cos sum
  ;; a variant of the G&R 2nd col 4th row
  (declare (gen rcos) (fm float))
  (let* ((r (rcos-r gen))
	 (rr (* r r)))
    (* (- (/ (- 1.0 rr)
	     (- (+ 1.0 rr)
		(* 2.0 r (oscil (rcos-osc gen) fm))))
	  1.0)
       (/ (- 1.0 r) (* 2.0 r))))) ; normalization

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-rcos 100.0 :r 0.5)))
    (run (lambda () 
	   (do ((i 0 (1+ i)))
	       ((= i 20000))
	     (outa i (rcos gen 0.0) *output*))))))

(definstrument (stringy beg dur freq amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (n (inexact->exact (floor (/ (mus-srate) (* 3 freq)))))
	 (r (expt .001 (/ 1 n)))
	 (carrier (make-rcos freq (* .5 r)))
	 (clang (make-rkoddssb (* freq 2) (* freq 1.618) r))
	 (ampf (make-env '(0 0 1 1 2 .5 4 .25 10 0) :scaler amp :duration dur))
	 (clangf (make-env (list 0 0 .1 1 .2 .1 .3 0) :scaler (* amp .5) :duration .1))
	 (rf (make-env (list 0 1 1 0) :scaler (* 0.5 r) :duration dur))
	 (crf (make-env (list 0 1 1 0) :scaler r :duration .1)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (set! (rkoddssb-r clang) (env crf))
	 (set! (rcos-r carrier) (env rf))
	 (outa i (+ (* (env clangf)
		       (rkoddssb clang 0.0))
		    (* (env ampf)
		       (rcos carrier 0.0)))
	       *output*))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (stringy 0 1 1000 .5))

(with-sound (:clipped #f :statistics #t :play #t)
  (do ((i 0 (1+ i)))
      ((= i 10))
    (stringy (* i .3) .3 (+ 200 (* 100 i)) .5)))
|#


(def-clm-struct (rssb 
		 :make-wrapper (lambda (g)
				 (set! (rssb-incr1 g) (hz->radians (rssb-carfreq g)))
				 (set! (rssb-incr2 g) (hz->radians (rssb-modfreq g)))
				 g))
  (carfreq 0.0) (modfreq 1.0) (n 1 :type int) (r 0.0)
  (angle1 0.0) (angle2 0.0) (incr1 0.0) (incr2 0.0))

(define (rssb gen fm)
  (declare (gen rssb) (fm float))
  (let* ((n (rssb-n gen))
	 (angle1 (rssb-angle1 gen))
	 (angle2 (rssb-angle2 gen))
	 (carsin (sin angle1))
	 (canrcos (cos angle1))
	 (r (rssb-r gen))
	 (den (+ 1.0 (* r r) (* -2.0 r (cos angle2))))
	 (sumsin (* r (sin angle2)))
	 (sumcos (- 1.0 (* r (cos angle2))))
	 (result (/ (- (* carsin sumsin)
		       (* canrcos sumcos))
		    (* 2 den)))
	 (cfm (* fm (/ (rssb-incr1 gen) (rssb-incr2 gen)))))
    (set! (rssb-angle1 gen) (+ cfm angle1 (rssb-incr1 gen)))
    (set! (rssb-angle2 gen) (+ fm angle2 (rssb-incr2 gen)))
    result))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-rssb 2000.0 103.0 3 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (rssb gen 0.0) *output*))))))
|#


;;; --------------------------------------------------------------------------------

;;; inf cosines scaled by e^-r (special case of rcos): ercos, erssb

;;; sndclm.html G&R 2nd col last row (with normalization)

(def-clm-struct (ercos
		 :make-wrapper (lambda (g)
				 (set! (ercos-osc g) (make-oscil (ercos-frequency g)))
				 (if (<= (ercos-r g) 0.0) (set! (ercos-r g) 0.00001))
				 (set! (ercos-cosh-t g) (cosh (ercos-r g)))
				 (let ((exp-t (exp (- (ercos-r g)))))
				   (set! (ercos-offset g) (/ (- 1.0 exp-t) (* 2.0 exp-t)))
				   (set! (ercos-scaler g) (* (sinh (ercos-r g)) (ercos-offset g))))
				 g))
  (frequency 440.0) (r 1.0 :type float)
  (osc #f :type clm) scaler offset cosh-t)

(define (ercos gen fm)
  (declare (gen ercos) (fm float))
  (- (/ (ercos-scaler gen) 
	(- (ercos-cosh-t gen) (oscil (ercos-osc gen) fm)))
     (ercos-offset gen)))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-ercos 100 :r 1.0)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 10000))
	(outa i (ercos gen 0.0) *output*))))))

;; change "t" during note -- smoothly changing sum-of-cosines spectra (damped "lute-stop" effect)
(with-sound (:clipped #f :statistics #t :play #t)  
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


(def-clm-struct (erssb
		 :make-wrapper (lambda (g)
				 (set! (erssb-carincr g) (hz->radians (- (erssb-carfreq g) (erssb-modfreq g))))
				 (set! (erssb-modincr g) (hz->radians (erssb-modfreq g)))
				 g))
  (carfreq 0.0) (modfreq 1.0) (r 0.0)
  (carangle 0.0) (carincr 0.0) (modangle 0.0) (modincr 0.0))

(define (erssb gen fm)
  (declare (gen erssb) (fm float))
  (let* ((cx (erssb-carangle gen))
	 (mx (erssb-modangle gen))
	 (r (erssb-r gen))
	 (ccmx (- (cosh r) (cos mx))))

    (set! (erssb-carangle gen) (+ (* (/ (erssb-modincr gen) (erssb-carincr gen)) fm) cx (erssb-carincr gen)))
    (set! (erssb-modangle gen) (+ fm mx (erssb-modincr gen)))

    (if (< (abs ccmx) nearly-zero)
	1.0
	(/ (- (* (cos cx)
		 (- (/ (sinh r) ccmx)
		    1.0))
	      (* (sin cx)
		 (/ (sin mx) ccmx)))
	   (* 2.0 (- (/ 1.0 (- 1.0 (exp (- r)))) 1.0)))))) ; normalization

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-erssb 1000.0 100.0 1.0)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 20000))
	(outa i (erssb gen 0.0) *output*))))))
|#


;;; --------------------------------------------------------------------------------

;;; inf sinusoids scaled by r^2: r2cos, r2sin, r2ssb

;;; Jolley 2nd col 2nd row (1st row is cos tweak of this)

(def-clm-struct (r2sin
		 :make-wrapper (lambda (g)
				 (set! (r2sin-incr g) (hz->radians (r2sin-frequency g)))
				 (if (>= (* (r2sin-r g) (r2sin-r g)) 1.0)
				     (set! (r2sin-r g) 0.9999999))
				 g))
  (frequency 0.0) (r 0.0)
  (angle 0.0) (incr 0.0))

(define (r2sin gen fm)
  (declare (gen r2sin) (fm float))
  (let* ((x (r2sin-angle gen))
	 (r (r2sin-r gen)))

    (set! (r2sin-angle gen) (+ x fm (r2sin-incr gen)))

    (* (sinh (* r (cos x)))
       (sin (* r (sin x))))))

#|
;;; even harmonics, but we can't push the upper partials past the (2k)! range, so not very flexible

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-r2sin 100.0 :r 0.5)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 20000))
      (outa i (r2sin gen 0.0) *output*))))))
|#


(def-clm-struct (r2cos
		 :make-wrapper (lambda (g)
				 (set! (r2cos-incr g) (hz->radians (r2cos-frequency g)))
				 (if (>= (* (r2cos-r g) (r2sin-r g)) 1.0)
				     (set! (r2cos-r g) 0.9999999))
				 g))
  (frequency 0.0) (r 0.0)
  (angle 0.0) (incr 0.0))

(define (r2cos gen fm)
  (declare (gen r2cos) (fm float))
  (let* ((x (r2cos-angle gen))
	 (r (r2cos-r gen)))

    (set! (r2cos-angle gen) (+ x fm (r2cos-incr gen)))

    (/ (- (* (cosh (* r (cos x)))
	     (cos (* r (sin x))))
	  1.0)                   ; omit DC
       (- (cosh r) 1.0))))       ; normalize

#|
;;; odd harmonics, but we can't push the upper partials past the (2k)! range, so not very flexible

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-r2cos 100.0 :r 0.5)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 20000))
      (outa i (r2cos gen 0.0) *output*))))))
|#


(def-clm-struct (r2ssb
		 :make-wrapper (lambda (g)
				 (set! (r2ssb-carincr g) (hz->radians (r2ssb-carfreq g)))
				 (set! (r2ssb-modincr g) (hz->radians (r2ssb-modfreq g)))
				 g))
  (carfreq 0.0) (modfreq 1.0) (r 1.0)
  (carangle 0.0) (carincr 0.0)
  (modangle 0.0) (modincr 0.0))

(define (r2ssb gen fm)
  (declare (gen r2ssb) (fm float))
  (let* ((mx (r2ssb-modangle gen))
	 (cx (r2ssb-carangle gen))
	 (a (r2ssb-r gen))
	 (asinx (* a (sin mx)))
	 (acosx (* a (cos mx)))
	 (cfm (* fm (/ (r2ssb-carincr gen) (r2ssb-modincr gen)))))

    (set! (r2ssb-carangle gen) (+ cfm cx (r2ssb-carincr gen)))
    (set! (r2ssb-modangle gen) (+ fm mx (r2ssb-modincr gen)))

    (/ (- (* (cos cx)
	     (cosh acosx)
	     (cos asinx))
	  (* (sin cx)
	     (sinh acosx)
	     (sin asinx)))
       (cosh a)))) ; normalization

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-r2ssb 1000.0 100.0 0.5)))
    (run 
     (lambda () 
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (r2ssb gen 0.0) *output*))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-r2ssb 1000.0 100.0 0.5))
	(vib (make-oscil 5)))
    (run 
     (lambda () 
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (r2ssb gen (* (hz->radians 10.0) (oscil vib))) *output*))))))
|#


;;; --------------------------------------------------------------------------------

;;; inf odd cosines scaled by e^-r: eoddcos

;;; Jolley 1st col 2nd row
;;;   heads toward a square wave as "r" -> 0.0 (odd harmonics, 1/k amp)

;;; this is the cos side of rkoddssb with r=e^-a

(def-clm-struct (eoddcos 
		 :make-wrapper (lambda (g)
				 (set! (eoddcos-osc g) (make-oscil (eoddcos-frequency g) (* 0.5 pi)))
				 g))
  (frequency 0.0) (r 1.0)
  (osc #f :type clm))

(define (eoddcos gen fm)
  (declare (gen eoddcos) (fm float))
  (let* ((a (eoddcos-r gen))
	 (sinha (sinh a)))
    (/ (atan (/ (oscil (eoddcos-osc gen) fm) sinha))
       (atan (/ 1.0 sinha))))) ; normalization

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-eoddcos 400.0 :r 1.0)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 10000))
      (outa i (eoddcos gen 0.0) *output*))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-eoddcos 400.0 :r 0.0))
	(a-env (make-env '(0 0 1 1) :end 10000)))
    (run (lambda () 
	   (do ((i 0 (1+ i)))
	       ((= i 10000))
	     (set! (eoddcos-r gen) (env a-env))
	     (outa i (eoddcos gen 0.0) *output*))))))

(with-sound (:clipped #f :statistics #t :play #t)
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

;;; inf odd cosines scaled by complicated mess: koddcos

;;; Jolley 1st col 3rd row 

(define make-koddcos make-oscil)

(define (koddcos gen fm)
  (declare (gen clm) (fm float))
  (let ((arg (* 2.0 (oscil gen fm))))
    (if (>= arg 0.0)
	(/ (acos (- 1.0 arg)) pi)
	(/ (acos (+ 1.0 arg)) (- pi)))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-koddcos 400.0)))
    (run (lambda ()
	   (do ((i 0 (1+ i)))
	       ((= i 10000))
	     (outa i (* .3 (koddcos gen 0.0)) *output*))))))
|#

;;; as printed in J, this is not usable -- 1-2sin can be 3 so acos will be complex -- looks like we're missing: x < pi
;;; we get odd harmonics but wrong amps


;;; --------------------------------------------------------------------------------

;;; inf cosines scaled by r^k/k: rkcos, rksin, rkssb

;;; G&R 2nd col 6th row
;;; r^k/k -- this sums to ln(1/(1-x)) if x<1 (J 118)

(def-clm-struct (rkcos 
		 :make-wrapper (lambda (g)
				 (set! (rkcos-osc g) (make-oscil (rkcos-frequency g) (* 0.5 pi)))
				 g))
  (frequency 0.0) (r 0.0)
  (osc #f :type clm))

(define (rkcos gen fm)
  (declare (gen rkcos) (fm float))
  (let ((cs (oscil (rkcos-osc gen) fm))
	(r (rkcos-r gen)))
    (/ (log (/ 1.0 (sqrt (+ 1.0 (* -2.0 r cs) (* r r)))))
       (log (/ 1.0 (- 1.0 r)))))) ; normalization

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-rkcos 440.0 :r 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (rkcos gen 0.0) *output*))))))
|#


(def-clm-struct (rksin
		 :make-wrapper (lambda (g)
				 (set! (rksin-incr g) (hz->radians (rksin-frequency g)))
				 g))
  (frequency 0.0) (r 1.0)
  (angle 0.0) (incr 0.0))

;;; normalization based on 0 of derivative of atan arg (for max) at cos x = r,
;;;   so we get a maxamp here of (atan (/ (* r (sin (acos r))) (- 1.0 (* r r))))

(define (rksin gen fm)
  (declare (gen rksin) (fm float))
  (let* ((x (rksin-angle gen))
	 (r (rksin-r gen)))

    (set! (rksin-angle gen) (+ fm x (rksin-incr gen)))

    (/ (atan (/ (* r (sin x))
		(- 1.0 (* r (cos x)))))
       (atan (/ (* r (sin (acos r)))   ; normalization
		(- 1.0 (* r r)))))))       

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-rksin 100.0 :r 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (rksin gen 0.0) *output*))))))
|#

(def-clm-struct (rkssb
		 :make-wrapper (lambda (g)
				 (set! (rkssb-carincr g) (hz->radians (rkssb-carfreq g)))
				 (set! (rkssb-modincr g) (hz->radians (rkssb-modfreq g)))
				 g))
  (carfreq 0.0) (modfreq 1.0) (r 1.0)
  (carangle 0.0) (carincr 0.0) (modangle 0.0) (modincr 0.0))

(define (rkssb gen fm)
  (declare (gen rkssb) (fm float))
  (let* ((mx (rkssb-modangle gen))
	 (cx (rkssb-carangle gen))
	 (r (rkssb-r gen))
	 (rcosmx (* r (cos mx)))
    	 (cfm (* fm (/ (rkssb-carincr gen) (rkssb-modincr gen)))))

    (set! (rkssb-carangle gen) (+ cfm (rkssb-carangle gen) (rkssb-carincr gen)))
    (set! (rkssb-modangle gen) (+ fm (rkssb-modangle gen) (rkssb-modincr gen)))

    (/ (- (* (cos cx)
	     (log (/ 1.0
		     (sqrt (+ 1.0 (* -2.0 rcosmx) (* r r))))))
	  (* (sin cx)
	     (atan (/ (* r (sin mx))
		      (- 1.0 rcosmx)))))
       (log (/ 1.0 (- 1.0 r)))))) ; normalization

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-rkssb 1000.0 100.0 :r 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (rkssb gen 0.0) *output*))))))
|#




;;; --------------------------------------------------------------------------------

;;; inf cosines scaled by r^k/k!: rk!cos, rk!ssb

;;; G&R 2nd col 3rd from last

(def-clm-struct (rk!cos
		 :make-wrapper (lambda (g)
				 (set! (rk!cos-incr g) (hz->radians (rk!cos-frequency g)))
				 g))
  (frequency 0.0) (r 0.0)
  (angle 0.0) (incr 0.0))

(define (rk!cos gen fm)
  (declare (gen rk!cos) (fm float))
  (let* ((r (rk!cos-r gen))
	 (x (rk!cos-angle gen))
	 (result (/ (- (* (exp (* r (cos x)))
			  (cos (* r (sin x))))
		       1.0) ; omit DC
		    (- (exp r) 1.0)))) ; normalization
    (set! (rk!cos-angle gen) (+ fm (rk!cos-angle gen) (rk!cos-incr gen)))
    result))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-rk!cos 440.0 :r 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (rk!cos gen 0.0) *output*))))))
|#

;;; the k! denominator dominates, so r * modfreq = formant center approximately; (n!)^(1/n) 
;;;   so freq=100, r=30, the center of the spectrum is around 3kHz:

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-rk!cos 100.0 :r 40.0)) 
	(r 40.0) 
	(incr (/ -40.0 100000)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 100000)) 
	 (set! (rk!cos-r gen) r) 
	 (set! r (+ r incr))
	 (outa i (rk!cos gen 0.0) *output*))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-rk!cos 300.0 :r 10.0)) 
	(ampf (make-env '(0 0 .1 1 .2 1 3 .5 5 .25 10 0) :scaler .5 :end 10000))
	(r 10.0) 
	(incr (/ -10.0 10000)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000)) 
	 (set! (rk!cos-r gen) r) 
	 (set! r (+ r incr))
	 (outa i (* (env ampf) (rk!cos gen 0.0)) *output*))))))
|#

(def-clm-struct (rk!ssb
		 :make-wrapper (lambda (g)
				 (set! (rk!ssb-carincr g) (hz->radians (rk!ssb-carfreq g)))
				 (set! (rk!ssb-modincr g) (hz->radians (rk!ssb-modfreq g)))
				 g))
  (carfreq 0.0) (modfreq 1.0) (r 1.0)
  (carangle 0.0) (carincr 0.0) (modangle 0.0) (modincr 0.0))

(define (rk!ssb gen fm)
  (declare (gen rk!ssb) (fm float))
  (let* ((mx (rk!ssb-modangle gen))
	 (cx (rk!ssb-carangle gen))
	 (r (rk!ssb-r gen))
	 (ercosmx (exp (* r (cos mx))))
	 (rsinmx (* r (sin mx)))
    	 (cfm (* fm (/ (rk!ssb-carincr gen) (rk!ssb-modincr gen)))))

    (set! (rk!ssb-carangle gen) (+ cfm cx (rk!ssb-carincr gen)))
    (set! (rk!ssb-modangle gen) (+ fm mx (rk!ssb-modincr gen)))

    (/ (- (* (cos cx) ercosmx (cos rsinmx))
	  (* (sin cx) ercosmx (sin rsinmx)))
       (exp r)))) ; normalization (keeping DC term here to get "carrier")
	  
#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-rk!ssb 1000.0 100.0 :r 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (rk!ssb gen 0.0) *output*))))))

(definstrument (bouncy beg dur freq amp :optional (bounce-freq 5) (bounce-amp 20))
  (let* ((gen (make-rk!ssb (* freq 4) freq :r 1.0)) 
	 (gen1 (make-oscil bounce-freq)) 
	 (bouncef (make-env '(0 1 1 0) :base 32 :scaler bounce-amp :duration 1.0))
	 (rf (make-env (list 0 0 1 1 (max 2.0 dur) 0) :base 32 :scaler 3 :duration dur))
	 (ampf (make-env (list 0 0 .01 1 .03 1 1 .15 (max 2 dur) 0.0) :base 32 :scaler amp :duration dur))
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur))))
    (run 
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (set! (rk!ssb-r gen) (+ (abs (* (env bouncef) 
					 (oscil gen1))) 
				 (env rf)))
	 (outa i (* (env ampf)
		    (rk!ssb gen 0.0)) 
	       *output*))))))

(with-sound (:statistics #t :play #t :clipped #f)
  (bouncy 0 2 300 .5 5 10))

(with-sound (:statistics #t :play #t :clipped #f)
  (bouncy 0 2 200 .5 3 2))
|#
			  


;;; --------------------------------------------------------------------------------

;;; inf cosines scaled by complicated mess: r2k!cos

;;; from Askey "Ramanujan and Hypergeometric Series" in Berndt and Rankin "Ramanujan: Essays and Surveys" p283
;;;
;;; this gives a sum of cosines of decreasing amp where the "k" parameter determines
;;;   the "index" (in FM nomenclature) -- higher k = more cosines; the actual amount
;;;   of the nth cos involves hypergeometric series (looks like r^n/n! (~=e^n?) with a million other terms).

(def-clm-struct (r2k!cos
		 :make-wrapper (lambda (g)
				 (set! (r2k!cos-osc g) (make-oscil (r2k!cos-frequency g)))
				 g))
  (frequency 0.0) (r 0.0) (k 0.0)
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
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-r2k!cos 440.0 :r 0.5 :k 3.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (r2k!cos gen 0.0) *output*))))))

(definstrument (pianoy beg dur freq amp)
  (let* ((gen (make-r2k!cos freq :r 0.5 :k 3.0)) 
	 (ampf (make-env (list 0 0 .01 1 .03 1 1 .15 (max 2 dur) 0.0) :base 32 :scaler amp :duration dur))
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur))))
    (run 
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (r2k!cos gen 0.0)) 
	       *output*))))))

(with-sound (:statistics #t :play #t :clipped #f)
  (pianoy 0 3 100 .5))
;;; this can be combined with bouncy-like changes to get an evolving sound

(definstrument (pianoy1 beg dur freq amp :optional (bounce-freq 5) (bounce-amp 20))
  (let* ((gen (make-r2k!cos freq :r 0.5 :k 3.0)) 
	 (gen1 (make-oscil bounce-freq)) 
	 (bouncef (make-env '(0 1 1 0) :base 32 :scaler bounce-amp :duration 1.0))
	 (rf (make-env (list 0 0 1 1 (max 2.0 dur) 0) :base 32 :scaler .1 :duration dur))
	 (ampf (make-env (list 0 0 .01 1 .03 1 1 .15 (max 2 dur) 0.0) :base 32 :scaler amp :duration dur))
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur))))
    (run 
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (set! (r2k!cos-r gen) (+ .25 (abs (* (env bouncef) 
					 (oscil gen1)))
				 (env rf)))
	 (outa i (* (env ampf)
		    (r2k!cos gen 0.0)) 
	       *output*))))))

(with-sound (:statistics #t :play #t :clipped #f)
  (pianoy1 0 4 200 .5 1 .1))

(definstrument (pianoy2 beg dur freq amp)
  (let* ((gen (make-r2k!cos freq :r 0.5 :k 3.0)) 
	 (ampf (make-env (list 0 0 .01 1 .03 1 1 .15 (max 2 dur) 0.0) :base 32 :scaler amp :duration dur))
	 (knock (make-fmssb 10.0 20.0 :index 1.0))
	 (kmpf (make-env '(0 0 1 1 3 1 100 0) :base 3 :scaler .05 :end 30000))
	 (indf (make-env '(0 1 1 0) :end 30000 :base 3 :scaler 10))
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur))))
    (run 
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (set! (fmssb-index knock) (env indf))
	 (outa i (+ (* (env ampf)
		       (r2k!cos gen 0.0))
		    (* (env kmpf) 
		       (fmssb knock 0.0)))
	       *output*))))))

(with-sound (:clipped #f :statistics #t :play #t) 
  (pianoy2 0 1 100 .5))
|#


;;; --------------------------------------------------------------------------------

;;; inf sines scaled by 1/2^k: k2sin

;;; Jolley 1st col 1st row

(def-clm-struct (k2sin
		 :make-wrapper (lambda (g)
				 (set! (k2sin-incr g) (hz->radians (k2sin-frequency g)))
				 g))
  (frequency 0.0)
  (angle 0.0) (incr 0.0))

(define (k2sin gen fm)
  (declare (gen k2sin) (fm float))
  (let ((x (k2sin-angle gen)))

    (set! (k2sin-angle gen) (+ x fm (k2sin-incr gen)))

    (/ (* 3.0 (sin x)) ; 3 rather than 4 for normalization
       (- 5.0 (* 4.0 (cos x))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-k2sin 440.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (k2sin gen 0.0) *output*))))))
|#


;;; using the 2nd Sansone formula, we get the sum of cos case by using a=-5b/4 or 3/(4cosx-5)

(def-clm-struct (k2cos
		 :make-wrapper (lambda (g)
				 (set! (k2cos-incr g) (hz->radians (k2cos-frequency g)))
				 g))
  (frequency 0.0)
  (angle 0.0) (incr 0.0))

(define (k2cos gen fm)
  (declare (gen k2cos) (fm float))
  (let ((x (k2cos-angle gen)))

    (set! (k2cos-angle gen) (+ x fm (k2cos-incr gen)))

   (* 0.5 (- (/ 3.0
		(- 5.0 (* 4.0 (cos x))))
	     1.0))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-k2cos 440.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (k2cos gen 0.0) *output*))))))
|#


(def-clm-struct (k2ssb
		 :make-wrapper (lambda (g)
				 (set! (k2ssb-carincr g) (hz->radians (k2ssb-carfreq g)))
				 (set! (k2ssb-modincr g) (hz->radians (k2ssb-modfreq g)))
				 g))
  (carfreq 0.0) (modfreq 1.0)
  (carangle 0.0) (carincr 0.0) (modangle 0.0) (modincr 0.0))

(define (k2ssb gen fm)
  (declare (gen k2ssb) (fm float))
  (let* ((mx (k2ssb-modangle gen))
	 (cx (k2ssb-carangle gen))
    	 (cfm (* fm (/ (k2ssb-carincr gen) (k2ssb-modincr gen)))))

    (set! (k2ssb-carangle gen) (+ cfm cx (k2ssb-carincr gen)))
    (set! (k2ssb-modangle gen) (+ fm mx (k2ssb-modincr gen)))

    (/ (- (* 3 (cos cx))
	  (* (sin cx) 
	     (* 4.0 (sin mx))))
       (* 3.0 (- 5.0 (* 4.0 (cos mx)))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-k2ssb 1000.0 100.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (k2ssb gen 0.0) *output*))))))
|#



;;; --------------------------------------------------------------------------------


;;; this was inspired by Andrews, Askey, Roy "Special Functions" p396, but there's an error somewhere...
;;;   it produces sum r^k sin(2k-1)x (not exactly what they derive)
;;;   (not normalized)

(def-clm-struct (dblsum
		 :make-wrapper (lambda (g)
				 (set! (dblsum-incr g) (hz->radians (* 2 (dblsum-frequency g))))
				 g))
  (frequency 0.0) (r 0.0)
  (angle 0.0) (incr 0.0))

(define (dblsum gen fm)
  (declare (gen dblsum) (fm float))
  (let* ((x (dblsum-angle gen))
	 (r (dblsum-r gen)))
    (set! (dblsum-angle gen) (+ fm (dblsum-angle gen) (dblsum-incr gen)))
    (/ (* (+ 1 r) (sin (* 0.5 x)))
       (* (- 1 r) (+ 1.0 (* -2.0 r (cos x)) (* r r))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-dblsum 100 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (dblsum gen 0.0) *output*))))))
|#


;;; --------------------------------------------------------------------------------

;;; inf odd sinusoids scaled by r^odd-k/odd-k: rkoddssb

;;;  G&R 2nd col rows 7&8 (odd r^k/k) 

(def-clm-struct (rkoddssb
		 :make-wrapper (lambda (g)
				 (set! (rkoddssb-carincr g) (hz->radians (- (rkoddssb-carfreq g) (rkoddssb-modfreq g))))
				 (set! (rkoddssb-modincr g) (hz->radians (rkoddssb-modfreq g)))
				 g))
  (carfreq 0.0) (modfreq 1.0) (r 0.0)
  (carangle 0.0) (carincr 0.0) (modangle 0.0) (modincr 0.0))

(define (rkoddssb gen fm)
  (declare (gen rkoddssb) (fm float))
  (let* ((r (rkoddssb-r gen))
	 (mx (rkoddssb-modangle gen))
	 (cx (rkoddssb-carangle gen))
	 (cfm (* fm (/ (rkoddssb-carincr gen) (rkoddssb-modincr gen)))))

    (set! (rkoddssb-carangle gen) (+ cfm cx (rkoddssb-carincr gen)))
    (set! (rkoddssb-modangle gen) (+ fm mx (rkoddssb-modincr gen)))

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
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-rkoddssb 1000.0 100.0 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (rkoddssb gen 0.0) *output*))))))

(definstrument (glassy beg dur freq amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (n (inexact->exact (floor (/ (mus-srate) (* 3 freq)))))
	 (r (expt .001 (/ 1 n)))
	 (clang (make-rkoddssb (* freq 2) (* freq 1.618) r))
	 (clangf (make-env (list 0 0 .01 1 .1 1 .2 .4 (max .3 dur) 0) :scaler amp :duration dur))
	 (crf (make-env (list 0 1 1 0) :scaler r :duration dur)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (set! (rkoddssb-r clang) (env crf))
	 (outa i (* (env clangf)
		    (rkoddssb clang 0.0))
	       *output*))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (glassy 0 .1 1000 .5))

(with-sound (:clipped #f :statistics #t :play #t)
  (do ((i 0 (1+ i)))
      ((= i 10))
    (glassy (* i .3) .1 (+ 400 (* 100 i)) .5)))

(with-sound (:statistics #t :play #t :scaled-to .5)
  (let ((gen (make-rkoddssb 5000.0 500.0 0.95))
	(ampf (make-env '(0 0 9 1 10 0) :base 32 :end 10000))
	(noi (make-rand 10000 .01)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (* (env ampf) (sin (rkoddssb gen (rand noi)))) *output*))))))
|#



;;; --------------------------------------------------------------------------------

;;; inf sinusoids scaled by kr^k: krksin

;;; Zygmund 1st
;;;   this looks interesting, but how to normalize?  sum of sines is bad enough, kr^k -> 1/(1-x)^2 if x^2<1 (G&R 113)
;;;   for low n, we could use the Tn roots stuff (clm.c)

(def-clm-struct (krksin
		 :make-wrapper (lambda (g)
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
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-krksin 440.0 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (krksin gen 0.0) *output*))))))

(with-sound (:clipped #f :statistics #t :scaled-to .5 :play #t)
  (let ((gen (make-krksin 6.0 0.965))) ; 60 .6 also
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 100000))
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

;;; absolute value of oscil: abssin

;;; Zygmund 2nd -- not actually very useful, but shows sin 2nx of abs

(def-clm-struct (abssin
		 :make-wrapper (lambda (g)
				 (set! (abssin-osc g) (make-oscil (abssin-frequency g)))
				 g))
  (frequency 0.0)
  (osc #f :type clm))

(define (abssin gen fm)
  (declare (gen abssin) (fm float))
  (abs (oscil (abssin-osc gen) fm)))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-abssin 440.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (abssin gen 0.0) *output*))))))
|#


;;; --------------------------------------------------------------------------------

;;; inf cosines, scaled by (-a+sqrt(a^2-b^2))^n/b^n: abcos

;;; from Sansone, p182, assumptions: a not 0, b not 0, b/a real, abs(b/a)<1 (b less than a)

(def-clm-struct (abcos
		 :make-wrapper (lambda (g)
				 (set! (abcos-incr g) (hz->radians (abcos-frequency g)))
				 g))
  (frequency 0.0) (a 0.0) (b 0.0)
  (angle 0.0) (incr 0.0))

(define (abcos gen fm)
  (declare (gen abcos) (fm float))
  (let* ((x (abcos-angle gen))
	 (a (abcos-a gen))
	 (b (abcos-b gen))
	 (norm (/ 0.5 (- (/ 1.0 
			    (- 1.0 (/ (abs (- (sqrt (- (* a a) (* b b))) 
					      a)) 
				      b))) 
			 1.0)))) ;; i.e. 1/(1-r) -1 because we start at k=1, r=the complicated a/b business

    (set! (abcos-angle gen) (+ fm (abcos-angle gen) (abcos-incr gen)))

    (* norm (- (/ (sqrt (- (* a a) (* b b)))
		  (+ a (* b (cos x))))
	       1.0))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-abcos 100.0 0.5 0.25)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (abcos gen 0.0) *output*))))))
|#


(def-clm-struct (absin
		 :make-wrapper (lambda (g)
				 (set! (absin-incr g) (hz->radians (absin-frequency g)))
				 g))
  (frequency 0.0) (a 0.0) (b 0.0)
  (angle 0.0) (incr 0.0))

(define (absin gen fm)
  (declare (gen absin) (fm float))
  (let* ((x (absin-angle gen))
	 (a (absin-a gen))
	 (b (absin-b gen)))

    (set! (absin-angle gen) (+ fm (absin-angle gen) (absin-incr gen)))

    (/ (* (sin x) 
	  (sqrt (- (* a a) (* b b))))
       (+ a (* b (cos x))))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-absin 100.0 0.5 0.25)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (absin gen 0.0) *output*))))))
|#



;;; --------------------------------------------------------------------------------

;;; inf cosines scaled by 1/(a^2+n^2): r2k2cos

;;; J 2nd col 3rd row

(def-clm-struct (r2k2cos
		 :make-wrapper (lambda (g)
				 (set! (r2k2cos-incr g) (hz->radians (r2k2cos-frequency g)))
				 g))
  (frequency 0.0) (r 1.0)
  (angle 0.0) (incr 0.0))

(define (r2k2cos-norm a)
  ;; J 124
  (- (* (/ pi (* 2 a))
	(/ (cosh (* pi a))
	   (sinh (* pi a))))
     (/ 1.0
	(* 2 a a))))

(define (r2k2cos gen fm)
  (declare (gen r2k2cos) (fm float))
  (let* ((x (r2k2cos-angle gen))
	 (a (r2k2cos-r gen)))
    (if (> x (* 2 pi))
	(set! x (fmod x (* 2 pi))))

    (set! (r2k2cos-angle gen) (+ x fm (r2k2cos-incr gen)))

    (/ (- (* pi (/ (cosh (* a (- pi x)))
		   (sinh (* a pi))))
	  (/ 1.0 a))
       (* 2 a (r2k2cos-norm a)))))


#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-r2k2cos 100.0 1.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (r2k2cos gen 0.0) *output*))))))
|#



;;; --------------------------------------------------------------------------------

;;; coskx/k = -ln(2sin(x/2)) or 1/2ln(1/(2-2cosx))
;;; sinkx/k = (pi-x)/2 both 0..2pi
;;; similarly -1^k : x/2 and ln(2cos(x/2)) (p44..46)
;;; 2k-1: pi/x and 1/2ln cot (x/2) 0..2pi and 0..pi
;;; but all of these are unbounded, and discontinuous

;;; --------------------------------------------------------------------------------

;;;  from Stilson/Smith apparently -- was named "Discrete Summation Formula" which doesn't convey anything to me
;;;    Alexander Kritov suggests time-varying "a" is good (this is a translation of his code)

(def-clm-struct (blsaw
		 :make-wrapper (lambda (g)
				 (set! (blsaw-incr g) (hz->radians (blsaw-frequency g)))
				 g))
  (frequency 0.0) (n 1 :type int) (r 0.0)
  (angle 0.0) (incr 0.0))

(define (blsaw gen fm)
  "blsaw produces a band-limited sawtooth"
  (declare (gen blsaw) (fm float))
  (let* ((a (blsaw-r gen))
	 (N (blsaw-n gen))
	 (x (blsaw-angle gen))
	 (incr (blsaw-incr gen))
	 (den (+ 1.0 (* -2.0 a (cos x)) (* a a))))
    (set! (blsaw-angle gen) (+ x fm incr))
    (if (< (abs den) nearly-zero)
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
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-blsaw 440.0 :r 0.5 :n 3)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (blsaw gen 0.0) *output*))))))
|#

;;; needs normalization



;;; --------------------------------------------------------------------------------

;;; asymmetric fm gens

(def-clm-struct (asyfm
		 :make-wrapper (lambda (gen)
				 (set! (asyfm-freq gen) (hz->radians (asyfm-frequency gen)))
				 gen))
  (frequency 0.0) (ratio 1.0) (r 1.0) (index 1.0)
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
(with-sound (:clipped #f :statistics #t :play #t) 
  (let ((gen (make-asyfm 2000.0 :ratio .1))) 
    (run 
     (lambda () 
       (do ((i 0 (1+ i)))
	   ((= i 1000))
	 (outa i (asyfm-J gen 0.0) *output*))))))

(with-sound (:clipped #f :statistics #t :play #t) 
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
(with-sound (:clipped #f :statistics #t :play #t) 
  (let ((gen (make-asyfm 2000.0 :ratio .1))) 
    (run 
     (lambda () 
       (do ((i 0 (1+ i)))
	   ((= i 1000))
	 (outa i (asyfm-I gen 0.0) *output*))))))
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
		 :make-wrapper (lambda (g)
				 (set! (bess-incr g) (hz->radians (bess-frequency g)))
				 (if (>= (bess-n g) (vct-length bessel-peaks)) 
				     (set! (bess-norm g) 0.145) 
				     (set! (bess-norm g) (vct-ref bessel-peaks (bess-n g))))
				 g))
  (frequency 0.0) (n 0 :type int)
  (arg 0.0) (incr 0.0) (norm 1.0))

(define (bess gen fm)
  (declare (gen bess) (fm float))
  (let ((result (/ (bes-jn (bess-n gen) (bess-arg gen)) 
		   (bess-norm gen))))
    (set! (bess-arg gen) (+ (bess-arg gen) (bess-incr gen) fm))
    result))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-bess 100.0 :n 0)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 1000))
      (outa i (bess gen 0.0) *output*))))))

(with-sound (:clipped #f :statistics #t :play #t)
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

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen1 (make-bess 400.0 :n 1))
	(gen2 (make-oscil 400.0))
	(vol (make-env '(0 1 1 0) :scaler 1.0 :end 20000)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 20000))
      (outa i (bess gen1 (* (env vol) (oscil gen2 0.0))) *output*))))))

;;; also gen2 800, env scl 0.2
|#


;;; --------------------------------------------------------------------------------

;;; Watson "Bessel Functions" p358 127 128 (J0(k sqrt(r^2+a^2- 2ar cos x)) = sum em Jm(ka)Jm(kr) cos mx
;;;   em here is "Neumann's factor" (p22) = 1 if m=0, 2 otherwise

(def-clm-struct (jjcos
		 :make-wrapper (lambda (g)
				 (set! (jjcos-incr g) (hz->radians (jjcos-frequency g)))
				 g))
  (frequency 0.0) (r 0.0) (a 0.0) (k 1 :type int)
  (angle 0.0) (incr 0.0))

(define (jjcos gen fm)
  (declare (gen jjcos) (fm float))
  (let* ((x (jjcos-angle gen))
	 (a (jjcos-a gen))
	 (r (jjcos-r gen))
	 (k (jjcos-k gen))
	 (dc (* (bes-j0 (* k a)) (bes-j0 (* k r))))
	 (norm (- (bes-j0 (* k (sqrt (+ (* a a) (* r r) (* -2 a r))))) dc)))
    ;; this norm only works if the a/r/k values all small enough that the initial J0 bump dominates
    ;;   if they're large (k=10 for example), later maxes come into play.
    ;; we need a formula for a sum of JJ's
    ;;
    ;; the resultant spectra are similar to FM (we can get sharper bumps, or low-passed bumps, etc)

    (set! (jjcos-angle gen) (+ x fm (jjcos-incr gen)))

    (/ (- (bes-j0 (* k (sqrt (+ (* r r) 
				(* a a)
				(* a r -2.0 (cos x))))))
	  dc)             ; get rid of DC component
       norm)))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-jjcos 100.0 :a 1.0 :r 1.0 :k 1)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (jjcos gen 0.0) *output*))))))

;;; example:
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-jjcos 100.0 :a 2.0 :r 1.0 :k 1)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (jjcos gen 0.0) *output*))))))

:(* (bes-jn 1 1) (bes-jn 1 2))
0.253788089467046
:(* (bes-jn 2 1) (bes-jn 2 2))
0.0405418594904987
:(* (bes-jn 3 1) (bes-jn 3 2))
0.00252256243314325
:(* (bes-jn 4 1) (bes-jn 4 2))
8.41951242883886e-5
which matches perfectly

set k=10
:(* (bes-jn 1 10) (bes-jn 1 20))
0.00290541944296873
:(* (bes-jn 2 10) (bes-jn 2 20))
-0.0408277687368493
:(* (bes-jn 3 10) (bes-jn 3 20))
-0.00577380202685643
:(* (bes-jn 4 10) (bes-jn 4 20))
-0.0286956880041051
:(* (bes-jn 5 10) (bes-jn 5 20))
-0.0353830269096024
:(* (bes-jn 6 10) (bes-jn 6 20))
7.96480491715688e-4
:(* (bes-jn 7 10) (bes-jn 7 20))
-0.0399227881572529
:(* (bes-jn 8 10) (bes-jn 8 20))
-0.0234795438775677
:(* (bes-jn 9 10) (bes-jn 9 20))
0.0365188087949483
:(* (bes-jn 10 10) (bes-jn 10 20))
0.0386925399194178
:(* (bes-jn 11 10) (bes-jn 11 20))
0.00755397504265978
:(* (bes-jn 12 10) (bes-jn 12 20))
-0.00754046620160803
:(* (bes-jn 13 10) (bes-jn 13 20))
-0.00591450759566936
:(* (bes-jn 14 10) (bes-jn 14 20))
-0.00175050411436045
:(* (bes-jn 15 10) (bes-jn 15 20))
-3.66078549147997e-6

which again matches


(define (jjsin gen fm)
  (declare (gen jjcos) (fm float))
  (let* ((x (jjcos-angle gen))
	 (a (jjcos-a gen))
	 (r (jjcos-r gen))
	 (k (jjcos-k gen)))

    (set! (jjcos-angle gen) (+ x fm (jjcos-incr gen)))

    (* (sin x)
       (bes-j0 (* k (sqrt (+ (* r r) 
			     (* a a)
			     (* a r -2.0 (cos x)))))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-jjcos 100.0 :a 1.0 :r 1.0 :k 1)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (jjsin gen 0.0) *output*))))))

(define (jjesin gen fm)
  (declare (gen jjcos) (fm float))
  (let* ((x (jjcos-angle gen))
	 (r (jjcos-r gen)))

    (set! (jjcos-angle gen) (+ x fm (jjcos-incr gen)))

    (* (exp (* r (- (cos x) 1.0))) ; -1 for norm , but there's huge DC offset
       (bes-j0 (* r (sin x))))))


(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-jjcos 100.0 :a 1.0 :r 1.0 :k 1)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (jjesin gen 0.0) *output*))))))

|#


;;; --------------------------------------------------------------------------------

;;; check J0(zsinx) formula 
;;; main difference from FM: index is divided by 2, J value is squared, else just like cos(sin)

(def-clm-struct (j0evencos
		 :make-wrapper (lambda (g)
				 (set! (j0evencos-incr g) (hz->radians (j0evencos-frequency g)))
				 g))
  (frequency 0.0) (index 1.0)
  (angle 0.0) (incr 0.0))

(define (j0evencos gen fm)
  (declare (gen j0evencos) (fm float))
  (let* ((x (j0evencos-angle gen))
	 (z (j0evencos-index gen))
	 (j0 (bes-j0 (* 0.5 z)))
	 (dc (* j0 j0)))
    (set! (j0evencos-angle gen) (+ x fm (j0evencos-incr gen)))
    (/ (- (bes-j0 (* z (sin x)))
	  dc)        ; get rid of DC component
       (- 1.0 dc)))) ; normalize

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-j0evencos 100.0 1.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (j0evencos gen 0.0) *output*))))))

index 10 (so 10/2 is the bes-jn arg):

(let ((base (* (bes-jn 4 5.0) (bes-jn 4 5.0)))) ; max (fft norms -> 1.0)
  (do ((i 1 (1+ i)))
      ((= i 11))
    (snd-display ";~A: ~A ~A" i (* (bes-jn i 5.0) (bes-jn i 5.0)) (/ (* (bes-jn i 5.0) (bes-jn i 5.0)) base))))
;1: 0.107308091385168 0.701072497819036
;2: 0.00216831005396058 0.0141661502497507
;3: 0.133101826831083 0.86958987897572
;4: 0.153062759870046 1.0
;5: 0.0681943848279407 0.445532178342005
;6: 0.0171737701015899 0.112200839160164
;7: 0.00284904116112987 0.0186135488707298
;8: 3.38752000110201e-4 0.00221315753353599
;9: 3.04735259399795e-5 1.99091705688911e-4
;10: 2.15444461145164e-6 1.4075563600714e-5

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-j0evencos 100.0 0.0)) 
	(indf (make-env '(0 0 1 20) :end 30000)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000)) 
	 (set! (j0evencos-index gen) (env indf))
	 (outa i (* 0.5 (j0evencos gen 0.0)) *output*))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-j0evencos 100.0 0.0)) 
	(indf (make-env '(0 0 1 20) :end 30000))
	(carrier (make-oscil 2000.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000)) 
	 (set! (j0evencos-index gen) (env indf))
	 (outa i (* 0.5 (oscil carrier) (j0evencos gen 0.0)) *output*))))))

;;; why no "carrier"?  I subtracted DC out above -- to make this look right, I need to use the bes(sin) without any fixup.

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-j0evencos 100.0 0.0)) 
	(indf (make-env '(0 20 1 0) :end 30000))
	(carrier (make-oscil 2000.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000)) 
	 (set! (j0evencos-index gen) (env indf))
	 (outa i (* 0.5 (j0evencos gen (oscil carrier))) *output*))))))

(with-sound (:clipped #f :statistics #t :play #t :srate 44100)
  (let ((gen (make-j0evencos 100.0 0.0)) 
	(indf (make-env '(0 10 1 0) :end 30000))
	(carrier (make-oscil 2000.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000)) 
	 (set! (j0evencos-index gen) (env indf))
	 (outa i (* 0.5 (j0evencos gen (* .1 (oscil carrier)))) *output*))))))

(define (j0even beg dur freq amp mc-ratio index)
  (let* ((gen (make-j0evencos (* mc-ratio freq) 0.0)) 
	 (indf (make-env '(0 10 1 0) :duration dur))
	 (carrier (make-oscil freq))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur))))
    (run 
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i end))
	 (set! (j0evencos-index gen) (env indf))
	 (outa i (* 0.5 (j0evencos gen (* index (oscil carrier)))) *output*))))))

(with-sound (:clipped #f :statistics #t :play #t :srate 44100)
	    (do ((i 0 (1+ i)))
		((= i 10))
	      (j0even i 1.0 2000.0 0.5 (+ .1 (* .05 i)) 0.1)))
|#


;;; --------------------------------------------------------------------------------

(def-clm-struct (j2cos
		 :make-wrapper (lambda (g)
				 (set! (j2cos-incr g) (hz->radians (j2cos-frequency g)))
				 g))
  (frequency 0.0) (r 0.0) (n 1 :type int)
  (angle 0.0) (incr 0.0))

(define (j2cos gen fm)
  (declare (gen j2cos) (fm float))
  (let* ((x (j2cos-angle gen))
	 (r (j2cos-r gen))
	 (rsinx2 (* 2.0 r (sin (* 0.5 x))))
	 (n (j2cos-n gen)))

    (set! (j2cos-angle gen) (+ x fm (j2cos-incr gen)))

    (if (< (abs rsinx2) nearly-zero)
	1.0
	(/ (bes-jn n rsinx2)
	   rsinx2))))

;;; this goes berserk if n=0, needs normalization, dc omission, doc/test
;;; if n=1, sample 0 = 1, the rest are in the .5 range!
;;; maybe j2cos isn't all that useful...

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-j2cos 100.0 :r 1.0 :n 0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (j2cos gen 0.0) *output*))))))
|#


;;; --------------------------------------------------------------------------------

(def-clm-struct (jpcos
		 :make-wrapper (lambda (g)
				 (set! (jpcos-incr g) (hz->radians (jpcos-frequency g)))
				 g))
  (frequency 0.0) (r 0.0) (a 0.0) (k 1 :type int)
  (angle 0.0) (incr 0.0))

(define (jpcos gen fm)
  (declare (gen jpcos) (fm float))
  (let* ((x (jpcos-angle gen))
	 (a (jpcos-a gen))
	 (r (jpcos-r gen))
	 (k (jpcos-k gen))
	 (dc (/ (* (sin (* k a)) (sin (* k r))) (* k a r)))
	 ;; from P0(x)=1, J[1/2](x)=sqrt(2/(pi x))sin(x), omitting original 1/pi
	 ;;   G&R 914 (8.464), 974 (8.912), but it's missing some remaining (small) component
	 (norm 1.0)
	 (arg (sqrt (+ (* r r) 
		       (* a a)
		       (* a r -2.0 (cos x))))))

    (set! (jpcos-angle gen) (+ x fm (jpcos-incr gen)))

    (/ (- (/ (sin (* k arg))
	     arg)
	  dc)
       norm)))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-jpcos 100.0 :a 1.0 :r 1.0 :k 1)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 210000))
	 (outa i (jpcos gen 0.0) *output*))))))

;;; -.725, 1/.275
|#

;;; TODO: dc is not right if a!=r, no norm


;;; --------------------------------------------------------------------------------

;;; use J0(cos)+J1(cos) to get full spectrum

(def-clm-struct (j0j1cos
		 :make-wrapper (lambda (g)
				 (set! (j0j1cos-incr g) (hz->radians (j0j1cos-frequency g)))
				 g))
  (frequency 0.0) (index 1.0)
  (angle 0.0) (incr 0.0))

(define (j0j1cos gen fm)
  (declare (gen j0j1cos) (fm float))
  (let* ((x (j0j1cos-angle gen))
	 (z (j0j1cos-index gen))
	 (j0 (bes-j0 (* 0.5 z)))
	 (dc (* j0 j0))
	 (arg (* z (cos x))))
    (set! (j0j1cos-angle gen) (+ x fm (j0j1cos-incr gen)))
    (/ (- (+ (bes-j0 arg)
	     (bes-j1 arg))
	  dc)        ; get rid of DC component
       1.215)))      ; not the best...

; TODO: normalize j0j1cos -- min depends on index, so peak depends on max and min and dc
;       (max (- 1.2154 dc)
;	    (- -0.5530 dc)

#|
(let ((mx 0.0) (x 0.0) (saved-x 0.0))
  (do ((i 0 (1+ i)))
      ((= i 1000))
    (let ((val (+ (bes-j0 x) (bes-j1 x))))
      (if (> (abs val) mx)
	  (begin
	    (set! mx (abs val))
	    (set! saved-x x)))
      (set! x (+ x .001))))
  (list mx saved-x))
(1.21533317877749 0.825000000000001)
(1.21533318495717 0.824863000002882)
(1.21533318495718 0.824863061409846)

(-0.552933995255066 4.57000000000269)
(-0.552933995483144 4.56997100028488)

(do ((i 0 (1+ i)))
    ((= i 10))
  (let ((pk (vct-peak 
	     (with-sound (:output (make-vct 10000))
  	       (let ((gen (make-j0j1cos 100.0 i)))
		 (run 
		  (lambda ()
		    (do ((i 0 (1+ i)))
			((= i 10000))
		      (outa i (j0j1cos gen 0.0) *output*)))))))))
    (snd-display ";~A: ~A" i pk)))
;0: 0.0
;1: 0.555559098720551
;2: 0.938335597515106
;3: 0.953315675258636
;4: 1.16509592533112
;5: 1.21275520324707
;6: 1.14727067947388
;7: 1.07083106040955
;8: 1.05760526657104
;9: 1.11238932609558
;10: 1.1824289560318
;11: 1.21528387069702
;12: 1.19094204902649
;13: 1.14720714092255
;14: 1.12512302398682
				  
|#

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-j0j1cos 100.0 1.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (j0j1cos gen 0.0) *output*))))))
|#

;;; --------------------------------------------------------------------------------


(def-clm-struct (jycos
		 :make-wrapper (lambda (g)
				 (set! (jycos-incr g) (hz->radians (jycos-frequency g)))
				 (let ((a (jycos-a g)) ; "c"
				       (r (jycos-r g))); "b"
				   (if (<= r a)
				       (format #t ";jycos a: ~A must be < r: ~A" a r))
				   (if (<= (+ (* a a) (* r r)) (* 2 a r))
				       (format #t ";jycos a: ~A, r: ~A will cause bes-y0 to return -inf!" a r)))
				 g))
  (frequency 0.0) (r 1.0) (a 0.5) ; "b" and "c" in the docs
  (angle 0.0) (incr 0.0))

(define (jycos gen fm)
  (declare (gen jycos) (fm float))
  (let* ((x (jycos-angle gen))
	 (b (jycos-r gen))
	 (c (jycos-a gen))
	 (b2c2 (+ (* b b) (* c c)))
	 (dc (* (bes-y0 b) (bes-j0 c)))
	 (norm (abs (- (bes-y0 (sqrt (+ b2c2 (* -2 b c)))) dc))))
    (set! (jycos-angle gen) (+ x fm (jycos-incr gen)))
    (/ (- (bes-y0 (sqrt (+ b2c2 (* -2.0 b c (cos x)))))
	  dc)
       norm)))

;;; oops -- bes-y0(0) is -inf!
;;; norm only works for "reasonable" a and r

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-jycos 100.0 1.5 1.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (jycos gen 0.0) *output*))))))

:(* (bes-yn 1 1.5) (bes-jn 1 1.0))
-0.181436652807559
:(* (bes-yn 2 1.5) (bes-jn 2 1.0))
-0.107112311628537
:(* (bes-yn 3 1.5) (bes-jn 3 1.0))
-0.0405654243875417

:(/ .107 .181)
0.591160220994475  [0.600]
:(/ .040 .181)
0.220994475138122  [0.228]
|#

;;; --------------------------------------------------------------------------------

#|
(def-clm-struct (jcos
		 :make-wrapper (lambda (g)
				 (set! (jcos-incr g) (hz->radians (jcos-frequency g)))
				 g))
  (frequency 0.0) (n 0 :type int) (r 1.0) (a 0.5) ; "b" and "c" in the docs
  (angle 0.0) (incr 0.0))

(define (jcos gen fm)
  (declare (gen jcos) (fm float))
  (let* ((x (jcos-angle gen))
	 (b (jcos-r gen))
	 (c (jcos-a gen))
	 (n (jcos-n gen))
	 (dc (* (bes-j0 b) (bes-j0 c)))
	 )
    (set! (jcos-angle gen) (+ x fm (jcos-incr gen)))
    (- (bes-jn n (* (+ n 1) (sqrt (+ (* b b) (* c c) (* -2.0 b c (cos x))))))
       dc)))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-jcos 100.0 0 1.0 1.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (jcos gen 0.0) *output*))))))
|#

;;; --------------------------------------------------------------------------------


#|
(let ((test-zero-stability 
       (lambda (make-func run-func angle-func zero)
	 (let ((gen (make-func)))
	   (angle-func gen zero)
	   (let ((zero-val (run-func gen zero)))
	     (for-each
	      (lambda (val)
		(set! gen (make-func)) ; remake else carrier drifts away in ssb cases
		(angle-func gen (+ zero val))
		(let ((new-val (run-func gen 0.0)))
		  (if (> (abs (- new-val zero-val)) .01)
		      (snd-display ";~A:~%;   zero check at (+ ~A ~A): ~A ~A~%" gen zero val zero-val new-val))))
	      (list 1.0e-11 1.0e-10 1.0e-9 1.0e-8 1.0e-7 1.0e-6
		    -1.0e-11 -1.0e-10 -1.0e-9 -1.0e-8 -1.0e-7 -1.0e-6)))))))
  
  (for-each
   (lambda (zero)
     (test-zero-stability (lambda () (make-oscil 0.0)) oscil (lambda (gen val) (set! (mus-phase gen) val)) zero)
     
     (for-each
      (lambda (n)
	(test-zero-stability (lambda () (make-sum-of-sines n 0.0)) sum-of-sines (lambda (gen val) (set! (mus-phase gen) val)) zero)
	(test-zero-stability (lambda () (make-sum-of-cosines n 0.0)) sum-of-cosines (lambda (gen val) (set! (mus-phase gen) val)) zero)
	(test-zero-stability (lambda () (make-sine-summation 0.0 :n n)) sine-summation (lambda (gen val) (set! (mus-phase gen) val)) zero)
	
	(test-zero-stability (lambda () (make-nssb 0.0 1.0 n)) nssb (lambda (gen val) (set! (nssb-modangle gen) val)) zero)
	(test-zero-stability (lambda () (make-noddssb 0.0 1.0 n)) noddssb (lambda (gen val) (set! (noddssb-modangle gen) val)) zero)
	(test-zero-stability (lambda () (make-nrssb 0.0 1.0 n)) nrssb (lambda (gen val) (set! (nrssb-modangle gen) val)) zero)
	(test-zero-stability (lambda () (make-nxycos 0.0 1.0 :n n)) nxycos (lambda (gen val) (set! (nxycos-yangle gen) val)) zero)
	(test-zero-stability (lambda () (make-nxy1sin 0.0 1.0 :n n)) nxy1sin (lambda (gen val) (set! (nxy1sin-yangle gen) val)) zero)
	(test-zero-stability (lambda () (make-nxy1cos 0.0 1.0 :n n)) nxy1cos (lambda (gen val) (set! (nxy1cos-yangle gen) val)) zero)
	(test-zero-stability (lambda () (make-nxysin 0.0 1.0 :n n)) nxysin (lambda (gen val) (set! (nxysin-yangle gen) val)) zero)

	(test-zero-stability (lambda () (make-nssb 0.0 1.0 n)) nssb (lambda (gen val) (set! (nssb-modangle gen) val) (set! (nssb-carangle gen) val)) zero)
	(test-zero-stability (lambda () (make-noddssb 0.0 1.0 n)) noddssb (lambda (gen val) (set! (noddssb-modangle gen) val) (set! (noddssb-carangle gen) val)) zero)
	(test-zero-stability (lambda () (make-nrssb 0.0 1.0 n)) nrssb (lambda (gen val) (set! (nrssb-modangle gen) val) (set! (nrssb-carangle gen) val)) zero)
	(test-zero-stability (lambda () (make-nxycos 0.0 1.0 :n n)) nxycos (lambda (gen val) (set! (nxycos-yangle gen) val) (set! (nxycos-xangle gen) val)) zero)
	(test-zero-stability (lambda () (make-nxy1sin 0.0 1.0 :n n)) nxy1sin (lambda (gen val) (set! (nxy1sin-yangle gen) val) (set! (nxy1sin-xangle gen) val)) zero)
	(test-zero-stability (lambda () (make-nxy1cos 0.0 1.0 :n n)) nxy1cos (lambda (gen val) (set! (nxy1cos-yangle gen) val) (set! (nxy1cos-xangle gen) val)) zero)
	(test-zero-stability (lambda () (make-nxysin 0.0 1.0 :n n)) nxysin (lambda (gen val) (set! (nxysin-yangle gen) val) (set! (nxysin-xangle gen) val)) zero)

	(test-zero-stability (lambda () (make-nkssb 0.0 1.0 n)) nkssb (lambda (gen val) (set! (nkssb-modangle gen) val)) zero)
	(test-zero-stability (lambda () (make-nkssb 0.0 1.0 n)) nkssb (lambda (gen val) (set! (nkssb-modangle gen) val) (set! (nkssb-carangle gen) val)) zero)

	(test-zero-stability (lambda () (make-noddsin :n n)) noddsin (lambda (gen val) (set! (noddsin-angle gen) val)) zero)
	(test-zero-stability (lambda () (make-noddcos :n n)) noddcos (lambda (gen val) (set! (noddcos-angle gen) val)) zero)
	(test-zero-stability (lambda () (make-ncos2 :n n)) ncos2 (lambda (gen val) (set! (ncos2-angle gen) val)) zero)
	(test-zero-stability (lambda () (make-npcos :n n)) npcos (lambda (gen val) (set! (npcos-angle gen) val)) zero)
	(test-zero-stability (lambda () (make-nrcos :n n)) nrcos (lambda (gen val) (set! (nrcos-angle gen) val)) zero))
      (list 1 10 3 30))
     
     (test-zero-stability (lambda () (make-krksin :r 0.1)) krksin (lambda (gen val) (set! (krksin-angle gen) val)) zero)
     (test-zero-stability (lambda () (make-k2sin)) k2sin (lambda (gen val) (set! (k2sin-angle gen) val)) zero)
     (test-zero-stability (lambda () (make-k2cos)) k2cos (lambda (gen val) (set! (k2cos-angle gen) val)) zero)
     (test-zero-stability (lambda () (make-k2ssb 0.0 1.0)) k2ssb (lambda (gen val) (set! (k2ssb-modangle gen) val)) zero)
     (test-zero-stability (lambda () (make-abcos :a 1.0 :b 0.5)) abcos (lambda (gen val) (set! (abcos-angle gen) val)) zero)
     (test-zero-stability (lambda () (make-absin :a 1.0 :b 0.5)) absin (lambda (gen val) (set! (absin-angle gen) val)) zero)
     
     (for-each
      (lambda (r)
	(test-zero-stability (lambda () (make-rcos :r r)) rcos (lambda (gen val) (set! (mus-phase (rcos-osc gen)) val)) zero)
	(test-zero-stability (lambda () (make-ercos :r r)) ercos (lambda (gen val) (set! (mus-phase (ercos-osc gen)) val)) zero)
	(test-zero-stability (lambda () (make-r2sin :r r)) r2sin (lambda (gen val) (set! (r2sin-angle gen) val)) zero)
	(test-zero-stability (lambda () (make-r2cos :r r)) r2cos (lambda (gen val) (set! (r2cos-angle gen) val)) zero)
	(test-zero-stability (lambda () (make-eoddcos  :r r)) eoddcos  (lambda (gen val) (set! (mus-phase (eoddcos-osc gen)) val)) zero)
	(test-zero-stability (lambda () (make-rkcos  :r r)) rkcos  (lambda (gen val) (set! (mus-phase (rkcos-osc gen)) val)) zero)
	(test-zero-stability (lambda () (make-rksin :r r)) rksin (lambda (gen val) (set! (rksin-angle gen) val)) zero)
	(test-zero-stability (lambda () (make-rk!cos :r r)) rk!cos (lambda (gen val) (set! (rk!cos-angle gen) val)) zero)
	(test-zero-stability (lambda () (make-r2k!cos :r r)) r2k!cos (lambda (gen val) (set! (mus-phase (r2k!cos-osc gen)) val)) zero)
	(test-zero-stability (lambda () (make-r2k2cos :r r)) r2k2cos (lambda (gen val) (set! (r2k2cos-angle gen) val)) zero)
	
	(test-zero-stability (lambda () (make-rssb 0.0 1.0 :r r)) rssb (lambda (gen val) (set! (rssb-angle2 gen) val)) zero)
	(test-zero-stability (lambda () (make-erssb 0.0 1.0 :r r)) erssb (lambda (gen val) (set! (erssb-modangle gen) val)) zero)
	(test-zero-stability (lambda () (make-rkssb 0.0 1.0 :r r)) rkssb (lambda (gen val) (set! (rkssb-modangle gen) val)) zero)
	(test-zero-stability (lambda () (make-rk!ssb 0.0 1.0 :r r)) rk!ssb (lambda (gen val) (set! (rk!ssb-modangle gen) val)) zero)
	(test-zero-stability (lambda () (make-rkoddssb 0.0 1.0 :r r)) rkoddssb (lambda (gen val) (set! (rkoddssb-modangle gen) val)) zero)
	(test-zero-stability (lambda () (make-r2ssb 0.0 1.0 :r r)) r2ssb (lambda (gen val) (set! (r2ssb-modangle gen) val)) zero)

	(test-zero-stability (lambda () (make-rssb 0.0 1.0 :r r)) rssb (lambda (gen val) (set! (rssb-angle2 gen) val) (set! (rssb-angle1 gen) val)) zero)
	(test-zero-stability (lambda () (make-erssb 0.0 1.0 :r r)) erssb (lambda (gen val) (set! (erssb-modangle gen) val) (set! (erssb-carangle gen) val)) zero)
	(test-zero-stability (lambda () (make-rkssb 0.0 1.0 :r r)) rkssb (lambda (gen val) (set! (rkssb-modangle gen) val) (set! (rkssb-carangle gen) val)) zero)
	(test-zero-stability (lambda () (make-rk!ssb 0.0 1.0 :r r)) rk!ssb (lambda (gen val) (set! (rk!ssb-modangle gen) val) (set! (rk!ssb-carangle gen) val)) zero)
	(test-zero-stability (lambda () (make-rkoddssb 0.0 1.0 :r r)) rkoddssb (lambda (gen val) (set! (rkoddssb-modangle gen) val) (set! (rkoddssb-carangle gen) val)) zero)
	(test-zero-stability (lambda () (make-r2ssb 0.0 1.0 :r r)) r2ssb (lambda (gen val) (set! (r2ssb-modangle gen) val) (set! (r2ssb-carangle gen) val)) zero)
	)
      (list 0.1 0.5 .99)))
   (list 0.0 (* 0.5 pi) pi (* 2.0 pi) (* -0.5 pi) (- pi) (* -2.0 pi) (* 1.5 pi) (* -1.5 pi))))
|#


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

;;; blackman4 as a waveform -- all the other fft windows could be implemented, but I doubt
;;;   they are all that different, or useful as audio sources.

(def-clm-struct (blackman4 
		 :make-wrapper (lambda (g)
				 (set! (blackman4-incr g) (hz->radians (blackman4-frequency g)))
				 (set! (blackman4-coeffs g) (vct .084037 -.29145 .375696 -.20762 .041194))
				 g))
    (frequency 0.0) (coeffs #f :type vct)  ; angle = initial-phase
    (angle 0.0) (incr 0.0))

(define (blackman4 gen fm)
  (declare (gen blackman4) (fm float))
  (let ((x (blackman4-angle gen)))
    (set! (blackman4-angle gen) (+ x fm (blackman4-incr gen)))
    (polynomial (blackman4-coeffs gen) (cos x))))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((black4 (make-blackman4 440.0)))
    (run (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (blackman4 black4 0.0) *output*))))))
|#



;;; --------------------------------------------------------------------------------


;;; we can add the sin(cos) and sin(sin) cases, using -index in the latter to get 
;;;   asymmetric fm since Jn(-B) = (-1)^n Jn(B)
;;;
;;; the same trick would work in the other two cases -- gapped spectra

(def-clm-struct (fmssb
		 :make-wrapper (lambda (g)
				 (set! (fmssb-carincr g) (hz->radians (fmssb-carfreq g)))
				 (set! (fmssb-modincr g) (hz->radians (fmssb-modfreq g)))
				 g))
  (carfreq 0.0) (modfreq 1.0) (index 1.0)
  (carangle 0.0) (carincr 0.0) (modangle 0.0) (modincr 0.0))

(define (fmssb gen fm)
  (declare (gen fmssb) (fm float))
  (let* ((mx (fmssb-modangle gen))
	 (cx (fmssb-carangle gen))
	 (B (fmssb-index gen))
    	 (cfm (* fm (/ (fmssb-carincr gen) (fmssb-modincr gen)))))

    (set! (fmssb-carangle gen) (+ cfm (fmssb-carangle gen) (fmssb-carincr gen)))
    (set! (fmssb-modangle gen) (+ fm (fmssb-modangle gen) (fmssb-modincr gen)))

    (- (* (cos cx)
	  (sin (* B (cos mx))))
       (* (sin cx)
	  (* (sin (* B (sin mx)))))))) ; use -B for the other side

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-fmssb 1000.0 100.0 :index 8.0)))  ; 1 3 7 11 ... -- interesting effect
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (* .3 (fmssb gen 0.0)) *output*))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-fmssb 1000.0 100.0 :index 8.0)) 
	(ampf (make-env '(0 0 1 1 100 0) :base 32 :scaler .3 :end 30000))
	(indf (make-env '(0 1 1 0) :end 30000 :scaler 8)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000)) 
	 (set! (fmssb-index gen) (env indf))
	 (outa i (* (env ampf) (fmssb gen 0.0)) *output*))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-fmssb 1000.0 50.0 :index 1.0)) 
	(ampf (make-env '(0 0 1 1 100 0) :base 32 :scaler .3 :end 30000))
	(indf (make-env '(0 1 1 0) :end 30000 :base 32 :scaler 10)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000)) 
	 (set! (fmssb-index gen) (env indf))
	 (outa i (* (env ampf) (fmssb gen 0.0)) *output*))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-fmssb 100.0 540.0 :index 1.0)) ; also 100 700
	(ampf (make-env '(0 0 1 1 100 0) :base 32 :scaler .3 :end 30000)) ; also 0 0 1 1 3 1 100 0...
	;; '(0 0 1 .75 2 1 3 .95 4 .5 10 0) -> bowed effect, '(0 0 1 .75 2 1 3 .125 4 .25 5 1 6 .8 20 0)
	;; '(0 0 1 .75 2 1 3 .1 4 .7 5 1 6 .8 100 0) -> clickier attack (300 too)
	(indf (make-env '(0 1 1 0) :end 30000 :base 32 :scaler 10)))
        ;; '(0 0 1 1 3 0)
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000)) 
	 (set! (fmssb-index gen) (env indf))
	 (outa i (* (env ampf) (fmssb gen 0.0)) *output*))))))

(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-fmssb 10.0 20.0 :index 1.0)) 
	(ampf (make-env '(0 0 1 1 3 1 100 0) :base 32 :scaler .3 :end 30000))
	(indf (make-env '(0 1 1 0) :end 30000 :base 32 :scaler 10)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000)) 
	 (set! (fmssb-index gen) (env indf))
	 (outa i (* (env ampf) (fmssb gen 0.0)) *output*))))))

;;; imaginary machines (also imaginary beasts)

(definstrument (machine1 beg dur cfreq mfreq amp index gliss)
  (let* ((gen (make-fmssb cfreq mfreq :index 1.0))
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0 0 1 .75 2 1 3 .1 4 .7 5 1 6 .8 100 0) :base 32 :scaler amp :duration dur))
	 (indf (make-env '(0 0 1 1 3 0) :duration dur :base 32 :scaler index))
	 (frqf (make-env (if (> gliss 0.0) '(0 0 1 1) '(0 1 1 0)) :duration dur :scaler (hz->radians (abs gliss)))))
    (run 
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop)) 
	 (set! (fmssb-index gen) (env indf))
	 (outa i (* (env ampf) (fmssb gen (env frqf))) *output*))))))

(with-sound (:statistics #t :play #t)
  (do ((i 0.0 (+ i .5)))
      ((>= i 2.0))
    (machine1 i .3 100 540 0.5 3.0 0.0)
    (machine1 i .1 100 1200 .5 10.0 200.0)
    (machine1 i .3 100 50 .75 10.0 0.0)
    (machine1 (+ i .1) .1 100 1200 .5 20.0 1200.0)
    (machine1 (+ i .3) .1 100 1200 .5 20.0 1200.0)
    (machine1 (+ i .3) .1 100 200 .5 10.0 200.0)
    (machine1 (+ i .36) .1 100 200 .5 10.0 200.0)
    (machine1 (+ i .4) .1 400 300 .5 10.0 -900.0)
    (machine1 (+ i .4) .21 100 50 .75 10.0 1000.0)
    ))

(with-sound (:statistics #t :play #t)
  (do ((i 0.0 (+ i .2)))
      ((>= i 2.0))
    (machine1 i .3 100 540 0.5 4.0 0.0)
    (machine1 (+ i .1) .3 200 540 0.5 3.0 0.0))
  (do ((i 0.0 (+ i .6)))
      ((>= i 2.0))
    (machine1 i .3 1000 540 0.5 6.0 0.0)
    (machine1 (+ i .1) .1 2000 540 0.5 1.0 0.0)
    ))

(with-sound (:statistics #t :play #t :scaled-to .5)
  (let ((gen (make-rkoddssb 1000.0 2000.0 0.875)) 
	(noi (make-rand 15000 .04))
	(gen1 (make-rkoddssb 100.0 10.0 0.9))
	(ampf (make-env '(0 0 1 1 11 1 12 0) :duration 11.0 :scaler .5))
	(frqf (make-env '(0 0 1 1 2 0 10 0 11 1 12 0 20 0) :duration 11.0 :scaler (hz->radians 1.0))))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i (* 12 44100)))
	 (outa i (* (env ampf) 
		    (+ (rkoddssb gen1 (env frqf))
		       (* .2 (sin (rkoddssb gen (rand noi))))))
	       *output*)))))
  (do ((i 0.0 (+ i 2)))
      ((>= i 10.0))
    (machine1 i 3 100 700 0.5 4.0 0.0)
    (machine1 (+ i 1) 3 200 700 0.5 3.0 0.0))
  (do ((i 0.0 (+ i 6)))
      ((>= i 10.0))
    (machine1 i 3 1000 540 0.5 6.0 0.0)
    (machine1 (+ i 1) 1 2000 540 0.5 1.0 0.0)
    ))

(with-sound (:statistics #t :play #t)
  (do ((i 0.0 (+ i .2)))
      ((>= i 2.0))
    (machine1 i .3 1200 540 0.5 40.0 0.0)
    (machine1 (+ i .1) .3 2400 540 0.5 3.0 0.0))
  (do ((i 0.0 (+ i .6)))
      ((>= i 2.0))
    (machine1 i .3 1000 540 0.5 6.0 0.0)
    (machine1 (+ i .1) .1 2000 540 0.5 10.0 100.0)
    ))
|#


(define (fm-cancellation beg dur carfreq modfreq amp index)
  (let* ((cx 0.0)
	 (mx 0.0)
	 (car-incr (hz->radians carfreq))
	 (mod-incr (hz->radians modfreq))
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur))))
    (do ((i start (1+ i)))
	((= i stop))
      (outa i (* amp (- (* (cos cx)
			   (sin (* index (cos mx))))
			(* (sin cx)
			   (* (sin (* index (sin mx))))))) 
	                   ;; use -index for reflection
	    *output*)
      (set! cx (+ cx car-incr))
      (set! mx (+ mx mod-incr)))))

;(with-sound () (fm-cancellation 0 1 1000.0 100.0 0.3 9.0))


;;; --------------------------------------------------------------------------------

;;; k3sin

;;; mostly useful as a test of a vct field 

(def-clm-struct (k3sin
		 :make-wrapper (lambda (g)
				 (set! (k3sin-incr g) (hz->radians (k3sin-frequency g)))
				 (set! (k3sin-coeffs g) (vct 0.0
							     (/ (* pi pi) 6.0)
							     (/ pi -4.0)
							     (/ 1.0 12.0)))
				 g))
  (frequency 0.0)
  (angle 0.0) (incr 0.0) 
  (coeffs #f :type vct))
		   
(define (k3sin gen fm)
  (declare (gen k3sin) (fm float))
  (let ((x (k3sin-angle gen)))
    (set! (k3sin-angle gen) (fmod (+ x fm (k3sin-incr gen)) (* 2 pi)))
    (polynomial (k3sin-coeffs gen) x)))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-k3sin 100.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (k3sin gen 0.0) *output*))))))
|#


;;; --------------------------------------------------------------------------------

;;; I(z) case A&S

(def-clm-struct (izcos
		 :make-wrapper (lambda (g)
				 (set! (izcos-incr g) (hz->radians (izcos-frequency g)))
				 (set! (izcos-dc g) (bes-i0 (izcos-r g)))
				 (set! (izcos-norm g) (- (exp (izcos-r g)) (izcos-dc g)))
				 g)
		 :methods (list
			   (list 'mus-scaler
				 (lambda (g) 
				   (izcos-r g))
				 (lambda (g val)
				   (set! (izcos-r g) val)
				   (set! (izcos-dc g) (bes-i0 val))
				   (set! (izcos-norm g) (- (exp val) (izcos-dc g)))))

			   (list 'mus-offset
				 (lambda (g)
				   (izcos-dc g)))

			   (list 'mus-describe
				 (lambda (g)
				   (format #f ";izcos freq: ~A, r: ~A" (mus-frequency g) (mus-scaler g))))

			   ;; these could be automatically included in a specialization of def-clm-struct
			   (list 'mus-increment
				 (lambda (g)
				   (izcos-incr g))
				 (lambda (g val)
				   (set! (izcos-incr g) val)))

			   (list 'mus-phase
				 (lambda (g)
				   (izcos-angle g))
				 (lambda (g val)
				   (set! (izcos-angle g) val)))

			   (list 'mus-name
				 (lambda (g) "izcos"))

			   (list 'mus-frequency
				 (lambda (g) 
				   (radians->hz (izcos-incr g)))
				 (lambda (g val) 
				   (set! (izcos-incr g) (hz->radians val))))))
  (frequency 0.0) (r 1.0)
  (angle 0.0) (incr 0.0)
  (dc 0.0) (norm 1.0))

(define (izcos gen fm)
  (declare (gen izcos) (fm float))
  (let* ((x (izcos-angle gen))
	 (z (izcos-r gen))
	 (dc (izcos-dc gen))
	 (norm (izcos-norm gen)))
    (set! (izcos-angle gen) (+ x fm (izcos-incr gen)))
    (/ (- (exp (* z (cos x)))
	  dc)
       norm)))

#|
(with-sound (:clipped #f :statistics #t :play #t)
  (let ((gen (make-izcos 100.0 1.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (izcos gen 0.0) *output*))))))
|#



;;; --------------------------------------------------------------------------------

(definstrument (organish beg dur freq amp fm-index amp-env)
  ;; this has a organ-style chiff (better than fm index sweep)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (carriers (make-vector 3 #f))
	 (fmoscs (make-vector 3 #f))
	 (ampfs (make-vector 3 #f))
	 (pervib (make-triangle-wave 5 (hz->radians (* freq .003))))
	 (ranvib (make-rand-interp 6 (hz->radians (* freq .002))))
	 (resc (make-nrssb 340.0 340 5 .5))
	 (resf (make-env (list 0 0 .05 1  .1 0 dur 0) :scaler (* amp .05) :duration dur)))

    (do ((i 0 (1+ i)))
	((= i 3))
      (let* ((frq (* freq (expt 2 i)))
	     (index1 (hz->radians (* fm-index frq (/ 5.0 (log frq)))))
	     (index2 (hz->radians (* fm-index frq 3.0 (/ (- 8.5 (log frq)) (+ 3.0 (* frq .001))))))
	     (index3 (hz->radians (* fm-index frq (/ 4.0 (sqrt frq))))))
	(vector-set! carriers i (make-oscil frq))
	(vector-set! fmoscs i (make-polyshape :frequency frq
					      :coeffs (partials->polynomial 
						       (list 1 index1
							     3 index2
							     4 index3))))))

    (vector-set! ampfs 0 (make-env (or amp-env '(0 0 1 1 2 1 3 0)) :scaler amp :duration dur))
    (vector-set! ampfs 1 (make-env (list 0 0  .04 1  .075 0 dur 0) :scaler (* amp .0125) :duration dur))
    (vector-set! ampfs 2 (make-env (list 0 0  .02 1  .05 0 dur 0) :scaler (* amp .025) :duration dur))

    ;; also good:
    ;(vector-set! ampfs 1 (make-env (list 0 0  .02 1  .05 0  (- dur .1) 0  (- dur .05) 1 dur 0) :scaler (* amp .025) :duration dur))
    ;(vector-set! ampfs 2 (make-env (list 0 0  .01 1 .025 0  (- dur .15) 0 (- dur .1) 1 dur 0) :scaler (* amp .05) :duration dur))

    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let* ((vib (+ (triangle-wave pervib) (rand-interp ranvib)))
		(sum (* (env resf)
			(nrssb resc 0.0))))
	   (do ((k 0 (1+ k))
		(n 1 (* n 2)))
	       ((= k 3))
	     (set! sum (+ sum (* (env (vector-ref ampfs k))
				 (oscil (vector-ref carriers k)
					(+ (* n vib)
					   (polyshape (vector-ref fmoscs k) 1.0 (* n vib))))))))
	   (outa i sum *output*)))))))

#|
(with-sound (:clipped #f :statistics #t :play #t :srate 44100)
  (do ((i 0 (1+ i)))
      ((= i 10))
    (organish (* i .3) .4 (+ 100 (* 50 i)) .5 1.0 #f)))

(with-sound (:clipped #f :statistics #t :play #t :srate 44100)
  (do ((i 0 (1+ i)))
      ((= i 10))
    (organish (* i .3) .4 (+ 100 (* 50 i)) .5 1.0 '(0 0 1 1 2 .5 3 .25 4 .125 10 0))))
|#

;;; TODO: delay + sqr as handler for coupled rand-interp envs
;;; TODO: check the double top env, and talking drum effect
