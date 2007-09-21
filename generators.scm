 (provide 'snd-generators.scm)
(if (not (provided? 'snd-ws.scm)) (load-from-path "ws.scm"))


;;; these try to mimic existing gens (mainly oscil), so "frequency" is placed first.
;;;   Where a factor is involved, I'll try to use "r".
;;;   Where the limit of the sum is settable, I'll use "n".


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

;;; n sinusoids, equal amps: ncos, nsin, nssb

(define ncos sum-of-cosines)
(define make-ncos make-sum-of-cosines)
(define ncos? sum-of-cosines?)

(define nsin sum-of-sines)
(define make-nsin make-sum-of-sines)
(define nsin? sum-of-sines?)

(def-clm-struct (nssb 
		 :make-wrapper
		 (lambda (g)
		   (set! (nssb-carincr g) (hz->radians (nssb-carfreq g)))
		   (set! (nssb-modincr g) (hz->radians (nssb-modfreq g)))
		   g))
  (carfreq 0.0) (modfreq 0.0) (n 1 :type int)
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

    (if (< (abs den) 1.0e-9)
	-1.0
	(/ (- (* (sin cx) 
		 (sin (* mx (/ (+ n 1) 2)))
		 (sin (/ (* n mx) 2)))
	      (* (cos cx) 
		 0.5 (+ den (sin (* mx (+ n 0.5))))))
	   (* (+ n 1) den)))))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-nssb 1000.0 100.0 3)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (nssb gen 0.0) *output*))))))

(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-nssb 1000.0 100.0 3))
	(vib (make-oscil 5.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (nssb gen (* (hz->radians 10.0) (oscil vib))) *output*))))))
|#


;;; --------------------------------------------------------------------------------

;;; n odd sinusoids: noddsin, noddcos, noddssb

;;; sndclm.html (G&R) 1st col 5th row (sum of odd sines)

(def-clm-struct (noddsin 
		 :make-wrapper
		 (lambda (g)
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
    (if (< (abs den) 1.0e-9)
	0.0
	(/ (* norm snx snx) den))))
	   
#|
;;; get normalization:
(do ((i 1 (1+ i))) 
    ((= i 30))
  (let ((v (with-sound (:output (make-vct 1000) :clipped #f :statistics #t)
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
(with-sound (:clipped #f :statistics #t)
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
  (frequency 0.0) (n 1 :type int)
  (angle 0.0) (incr 0.0))

(define (noddcos gen fm)
  (declare (gen noddcos) (fm float))
  (let* ((angle (noddcos-angle gen))
	 (n (noddcos-n gen))
	 (den (* 2 n (sin angle)))) ; "n" here is normalization
    (set! (noddcos-angle gen) (+ angle fm (noddcos-incr gen)))
    (if (< (abs den) 1.0e-9)
	(let ((fang (fmod (abs angle) (* 2 pi))))
	  ;; hopefully this almost never happens...
	  (if (or (< fang 0.001)
		  (< (abs (- fang (* 2 pi))) 0.001))
	      1.0
	      -1.0))
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

(def-clm-struct (noddssb
		 :make-wrapper
		 (lambda (g)
		   (set! (noddssb-carincr g) (hz->radians (- (noddssb-carfreq g) (noddssb-modfreq g))))
		   (set! (noddssb-modincr g) (hz->radians (noddssb-modfreq g)))
		   g))
  (carfreq 0.0) (modfreq 0.0) (n 1 :type int)
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
    (if (< (abs den) 1.0e-9)
	(let ((fang (fmod (abs mx) (* 2 pi))))
	  ;; hopefully this almost never happens...
	  (if (or (< fang 0.001)
		  (< (abs (- fang (* 2 pi))) 0.001))
	      -1.0
	      1.0))
	(- (* (sin cx)
	      (/ (* sinnx sinnx) den))
	   (* (cos cx)
	      (/ (sin (* 2 n mx))
		 (* 2 den)))))))

#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-noddssb 1000.0 100.0 5)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 10000))
	(outa i (noddssb gen 0.0) *output*))))))

(with-sound (:clipped #f :statistics #t)
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
		 :make-wrapper
		 (lambda (g)
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

    (if (< (abs den) 1.0e-9)
	1.0
	(let ((val (/ (sin (* 0.5 (+ n 1) x)) 
		      (* (+ n 1) den))))
	  (* val val)))))

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
		   g))
  (frequency 0.0) (n 1 :type int)
  (angle 0.0) (incr 0.0))

(define (npcos gen fm)
  (declare (gen npcos) (fm float))
  (let* ((angle (npcos-angle gen))
	 (den (sin (* 0.5 angle)))
	 (n (npcos-n gen))
	 (result (if (< (abs den) 1.0e-9)
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
(with-sound (:clipped #f :statistics #t)
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
		 :make-wrapper
		 (lambda (g)
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
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-nrcos 400.0 :n 5 :r 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (nrcos gen 0.0) *output*))))))
|#

;;; G&R 2nd col 1st and 2nd rows

(def-clm-struct (nrssb
		 :make-wrapper
		 (lambda (g)
		   (set! (nrssb-carincr g) (hz->radians (nrssb-carfreq g)))
		   (set! (nrssb-modincr g) (hz->radians (nrssb-modfreq g)))
		   g))
  (carfreq 0.0) (modfreq 0.0) (n 1 :type int) (r 0.0)
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

	  
#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-nrssb 1000 100 5 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (nrssb gen 0.0) *output*))))))
|#


;;; --------------------------------------------------------------------------------

;;; n sinusoids scaled by k: nkssb

;;; G&R 1st col ksinkx cases

(def-clm-struct (nkssb
		 :make-wrapper
		 (lambda (g)
		   (set! (nkssb-carincr g) (hz->radians (- (nkssb-carfreq g) (nkssb-modfreq g))))
		   (set! (nkssb-modincr g) (hz->radians (nkssb-modfreq g)))
		   (set! (nkssb-n g) (+ 1 (nkssb-n g))) ; sum goes 1 to n-1
		   g))
  (carfreq 0.0) (modfreq 0.0) (n 1 :type int)
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

    (if (< (abs sx2) 1.0e-9)
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

    (if (< (abs sx2) 1.0e-9)
	-1.0
	(let* ((s1 (- (/ (sin nx) sxsx)
		      (/ (* n (cos nx2)) sx22)))
	       (c1 (- (/ (* n (sin nx2)) sx22)
		      (/ (- 1.0 (cos nx)) sxsx))))
	  (/ (- (* (cos cx) c1)
		(* interp (* (sin cx) s1)))
	     (* 0.5 n (- n 1))))))) ; normalization, nominal n is off by 1, peak seems to be solid right through the interpolation

	       
#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-nkssb 1000.0 100.0 5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (nkssb gen 0.0) *output*))))))

(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-nkssb 1000.0 100.0 5))
	(vib (make-oscil 5.0))
	(vibamp (hz->radians 5.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 30000))
	 (outa i (nkssb gen (* vibamp (oscil vib))) *output*))))))

(with-sound (:clipped #f :statistics #t)
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
		 :make-wrapper
		 (lambda (g)
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
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-rcos 100.0 :r 0.5)))
    (run (lambda () 
	   (do ((i 0 (1+ i)))
	       ((= i 20000))
	     (outa i (rcos gen 0.0) *output*))))))
|#

(def-clm-struct (rssb 
		 :make-wrapper
		 (lambda (g)
		   (set! (rssb-incr1 g) (hz->radians (rssb-carfreq g)))
		   (set! (rssb-incr2 g) (hz->radians (rssb-modfreq g)))
		   g))
  (carfreq 0.0) (modfreq 0.0) (n 1 :type int) (r 0.0)
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
    (set! (rssb-angle1 gen) (+ cfm (rssb-angle1 gen) (rssb-incr1 gen)))
    (set! (rssb-angle2 gen) (+ fm (rssb-angle2 gen) (rssb-incr2 gen)))
    result))

#|
(with-sound (:clipped #f :statistics #t)
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
		 :make-wrapper 
		 (lambda (g)
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
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-ercos 100 :r 1.0)))
    (run (lambda ()
      (do ((i 0 (1+ i)))
	  ((= i 10000))
	(outa i (ercos gen 0.0) *output*))))))

;; change "t" during note -- smoothly changing sum-of-cosines spectra (damped "lute-stop" effect)
(with-sound (:clipped #f :statistics #t)  
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
		 :make-wrapper
		 (lambda (g)
		   (set! (erssb-carincr g) (hz->radians (- (erssb-carfreq g) (erssb-modfreq g))))
		   (set! (erssb-modincr g) (hz->radians (erssb-modfreq g)))
		   g))
  (carfreq 0.0) (modfreq 0.0) (r 0.0)
  (carangle 0.0) (carincr 0.0) (modangle 0.0) (modincr 0.0))

(define (erssb gen fm)
  (declare (gen erssb) (fm float))
  (let* ((cx (erssb-carangle gen))
	 (mx (erssb-modangle gen))
	 (r (erssb-r gen))
	 (ccmx (- (cosh r) (cos mx))))

    (set! (erssb-carangle gen) (+ (* (/ (erssb-modincr gen) (erssb-carincr gen)) fm) cx (erssb-carincr gen)))
    (set! (erssb-modangle gen) (+ fm mx (erssb-modincr gen)))

    (if (< (abs ccmx) 1.0e-9)
	1.0
	(/ (- (* (cos cx)
		 (- (/ (sinh r) ccmx)
		    1.0))
	      (* (sin cx)
		 (/ (sin mx) ccmx)))
	   (* 2.0 (- (/ 1.0 (- 1.0 (exp (- r)))) 1.0)))))) ; normalization

#|
(with-sound (:clipped #f :statistics #t)
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
		 :make-wrapper
		 (lambda (g)
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

(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-r2sin 100.0 :r 0.5)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 20000))
      (outa i (r2sin gen 0.0) *output*))))))
|#


(def-clm-struct (r2cos
		 :make-wrapper
		 (lambda (g)
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

(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-r2cos 100.0 :r 0.5)))
    (run (lambda () (do ((i 0 (1+ i)))
	((= i 20000))
      (outa i (r2cos gen 0.0) *output*))))))
|#


(def-clm-struct (r2ssb
		 :make-wrapper
		 (lambda (g)
		   (set! (r2ssb-carincr g) (hz->radians (r2ssb-carfreq g)))
		   (set! (r2ssb-modincr g) (hz->radians (r2ssb-modfreq g)))
		   g))
  (carfreq 0.0) (modfreq 0.0) (r 1.0)
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
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-r2ssb 1000.0 100.0 0.5)))
    (run 
     (lambda () 
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (r2ssb gen 0.0) *output*))))))

(with-sound (:clipped #f :statistics #t)
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
		 :make-wrapper
		 (lambda (g)
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

;;; inf odd cosines scaled by complicated mess: koddcos

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

;;; inf cosines scaled by r^k/k: rkcos, rksin, rkssb

;;; G&R 2nd col 6th row
;;; r^k/k -- this sums to ln(1/(1-x)) if x<1 (J 118)

(def-clm-struct (rkcos 
		 :make-wrapper
		 (lambda (g)
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
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-rkcos 440.0 :r 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (rkcos gen 0.0) *output*))))))
|#


(def-clm-struct (rksin
		 :make-wrapper
		 (lambda (g)
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
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-rksin 100.0 :r 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (rksin gen 0.0) *output*))))))
|#

(def-clm-struct (rkssb
		 :make-wrapper
		 (lambda (g)
		   (set! (rkssb-carincr g) (hz->radians (rkssb-carfreq g)))
		   (set! (rkssb-modincr g) (hz->radians (rkssb-modfreq g)))
		   g))
  (carfreq 0.0) (modfreq 0.0) (r 1.0)
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
(with-sound (:clipped #f :statistics #t)
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
		 :make-wrapper
		 (lambda (g)
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
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-rk!cos 440.0 :r 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (rk!cos gen 0.0) *output*))))))
|#

(def-clm-struct (rk!ssb
		 :make-wrapper
		 (lambda (g)
		   (set! (rk!ssb-carincr g) (hz->radians (rk!ssb-carfreq g)))
		   (set! (rk!ssb-modincr g) (hz->radians (rk!ssb-modfreq g)))
		   g))
  (carfreq 0.0) (modfreq 0.0) (r 1.0)
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
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-rk!ssb 1000.0 100.0 :r 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (rk!ssb gen 0.0) *output*))))))
|#
			  


;;; --------------------------------------------------------------------------------

;;; inf cosines scaled by complicated mess: r2k!cos

;;; from Askey "Ramanujan and Hypergeometric Series" in Berndt and Rankin "Ramanujan: Essays and Surveys" p283
;;;
;;; this gives a sum of cosines of decreasing amp where the "k" parameter determines
;;;   the "index" (in FM nomenclature) -- higher k = more cosines; the actual amount
;;;   of the nth cos involves hypergeometric series (looks like r^n/n! (~=e^n?) with a million other terms).

(def-clm-struct (r2k!cos
		 :make-wrapper
		 (lambda (g)
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
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-r2k!cos 440.0 :r 0.5 :k 3.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (r2k!cos gen 0.0) *output*))))))
|#


;;; --------------------------------------------------------------------------------

;;; inf sines scaled by 1/2^k: k2sin

;;; Jolley 1st col 1st row

(def-clm-struct (k2sin
		 :make-wrapper
		 (lambda (g)
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

(define (dblsum gen fm)
  (declare (gen dblsum) (fm float))
  (let* ((x (dblsum-angle gen))
	 (r (dblsum-r gen)))
    (set! (dblsum-angle gen) (+ fm (dblsum-angle gen) (dblsum-incr gen)))
    (/ (* (+ 1 r) (sin (* 0.5 x)))
       (* (- 1 r) (+ 1.0 (* -2.0 r (cos x)) (* r r))))))

#|
(with-sound (:clipped #f :statistics #t)
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
		 :make-wrapper
		 (lambda (g)
		   (set! (rkoddssb-carincr g) (hz->radians (- (rkoddssb-carfreq g) (rkoddssb-modfreq g))))
		   (set! (rkoddssb-modincr g) (hz->radians (rkoddssb-modfreq g)))
		   g))
  (carfreq 0.0) (modfreq 0.0) (r 0.0)
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
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-rkoddssb 1000.0 100.0 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (rkoddssb gen 0.0) *output*))))))
|#


;;; --------------------------------------------------------------------------------

;;; inf sinusoids scaled by kr^k: krksin

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

;;; absolute value of oscil: abssin

;;; Zygmund 2nd -- not actually very useful, but shows sin 2nx of abs

(def-clm-struct (abssin
		 :make-wrapper
		 (lambda (g)
		   (set! (abssin-osc g) (make-oscil (abssin-frequency g)))
		   g))
  (frequency 0.0)
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

;;; inf cosines, scaled by (-a+sqrt(a^2-b^2))^n/b^n: abcos

;;; from Sansone, p182, assumptions: a not 0, b not 0, b/a real, abs(b/a)<1 (b less than a)

(def-clm-struct (abcos
		 :make-wrapper
		 (lambda (g)
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
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-abcos 100.0 0.5 0.25)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (abcos gen 0.0) *output*))))))
|#

;;; --------------------------------------------------------------------------------

;;; inf cosines scaled by 1/(a^2+n^2): r2k2cos

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
(with-sound (:clipped #f :statistics #t)
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
		 :make-wrapper
		 (lambda (g)
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
	 (outa i (blsaw gen 0.0) *output*))))))
|#

;;; needs normalization



;;; --------------------------------------------------------------------------------

;;; asymmetric fm gens

(def-clm-struct (asyfm
		 :make-wrapper 
		 (lambda (gen)
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

;;; Watson "Bessel Functions" p358 127 128 (J0(k sqrt(r^2+a^2- 2ar cos x)) = sum em Jm(ka)Jm(kr) cos mx
;;;   em here is "Neumann's factor" (p22) = 1 if m=0, 2 otherwise

(def-clm-struct (jjcos
		 :make-wrapper
		 (lambda (g)
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
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-jjcos 100.0 :a 1.0 :r 1.0 :k 1)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (jjcos gen 0.0) *output*))))))

;;; example:
(with-sound (:clipped #f :statistics #t)
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


|#


;;; --------------------------------------------------------------------------------

;;; check J0(zsinx) formula 
;;; main difference from FM: index is divided by 2, J value is squared, else just like cos(sin)

(def-clm-struct (j0evencos
		 :make-wrapper
		 (lambda (g)
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
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-j0evencos 100.0 1.0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
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

which is very close to a match

|#


;;; --------------------------------------------------------------------------------

(def-clm-struct (j2cos
		 :make-wrapper
		 (lambda (g)
		   (set! (j2cos-incr g) (hz->radians (j2cos-frequency g)))
		   g))
  (frequency 0.0) (r 0.0) (n 1 :type int)
  (angle 0.0) (incr 0.0))

(define (j2cos gen fm)
  (declare (gen j2cos) (fm float))
  (let* ((x (j2cos-angle gen))
	 (r (j2cos-r gen))
	 (rsinx2 (* r (sin (* 0.5 x))))
	 (n (j2cos-n gen)))

    (set! (j2cos-angle gen) (+ x fm (j2cos-incr gen)))

    (if (< (abs rsinx2) 1.0e-9)
	1.0
	(/ (bes-jn n (* 2.0 rsinx2))
	   rsinx2))))

;;; this goes berserk if n=0, needs normalization, dc omission, doc/test


#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-j2cos 100.0 :r 1.0 :n 0)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (j2cos gen 0.0) *output*))))))
|#


;;; --------------------------------------------------------------------------------

(def-clm-struct (jpcos
		 :make-wrapper
		 (lambda (g)
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
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-jpcos 100.0 :a 1.0 :r 1.0 :k 1)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 210000))
	 (outa i (jpcos gen 0.0) *output*))))))
|#

;;; dc is not right if a!=r, no norm

;;; --------------------------------------------------------------------------------

#|
;;; we can add the sin(cos) and sin(sin) cases, using -index in the latter to get 
;;;   asymmetric fm since Jn(-B) = (-1)^n Jn(B) -- every other side band cancels
;;;
;;; the same trick would work in the other two cases -- gapped spectra

(def-clm-struct (fmtest
		 :make-wrapper
		 (lambda (g)
		   (set! (fmtest-carincr g) (hz->radians (fmtest-carfreq g)))
		   (set! (fmtest-modincr g) (hz->radians (fmtest-modfreq g)))
		   g))
  (carfreq 0.0) (modfreq 0.0) (index 1.0)
  (carangle 0.0) (carincr 0.0) (modangle 0.0) (modincr 0.0))

(define (fmtest gen fm)
  (declare (gen fmtest) (fm float))
  (let* ((mx (fmtest-modangle gen))
	 (cx (fmtest-carangle gen))
	 (B (fmtest-index gen))
    	 (cfm (* fm (/ (fmtest-carincr gen) (fmtest-modincr gen)))))

    (set! (fmtest-carangle gen) (+ cfm (fmtest-carangle gen) (fmtest-carincr gen)))
    (set! (fmtest-modangle gen) (+ fm (fmtest-modangle gen) (fmtest-modincr gen)))

    (- (* (cos cx)
	  (sin (* B (cos mx))))
       (* (sin cx)
	  (* (sin (* (- B) (sin mx))))))))

(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-fmtest 1000.0 100.0 :index 0.5)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 10000))
	 (outa i (fmtest gen 0.0) *output*))))))
|#


;;; --------------------------------------------------------------------------------

;;; G&R 1st col rows 1&2

(def-clm-struct (nxysin
		 :make-wrapper
		 (lambda (g)
		   (set! (nxysin-xincr g) (hz->radians (nxysin-xfrequency g))) ; can be 0 if just x=pi/2 for example
		   (set! (nxysin-yincr g) (hz->radians (nxysin-yfrequency g)))
		   (set! (nxysin-xangle g) (nxysin-x g))
		   g))
  (xfrequency 0.0) (yfrequency 0.0) (n 1 :type int) (x 0.0)
  (xangle 0.0) (xincr 0.0) (yangle 0.0) (yincr 0.0))

(define (nxysin gen fm)
  (declare (gen nxysin) (fm float))
  (let* ((x (nxysin-xangle gen))
	 (y (nxysin-yangle gen))
	 (n (nxysin-n gen))
	 (den (sin (* y 0.5))))
    (set! (nxysin-xangle gen) (+ x (nxysin-xincr gen) (* fm (/ (nxysin-xincr gen) (nxysin-yincr gen)))))
    (set! (nxysin-yangle gen) (+ y (nxysin-yincr gen) fm))
    (if (< (abs den) 1.0e-9)
	0.0
	(/ (* (sin (+ x (* 0.5 (- n 1) y)))
	      (sin (* 0.5 n y)))
	   den))))

;;; normalization here is hard (depends on x and y)
#|
(with-sound (:clipped #f :statistics #t)
  (let ((gen (make-nxysin 300 100 3)))
    (run 
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i 20000))
	 (outa i (nxysin gen 0.0) *output*))))))
|#

(def-clm-struct (nxycos
		 :make-wrapper
		 (lambda (g)
		   (set! (nxycos-xincr g) (hz->radians (nxycos-xfrequency g))) ; can be 0 if just x=pi/2 for example
		   (set! (nxycos-yincr g) (hz->radians (nxycos-yfrequency g)))
		   (set! (nxycos-xangle g) (nxycos-x g))
		   g))
  (xfrequency 0.0) (yfrequency 0.0) (n 1 :type int) (x 0.0)
  (xangle 0.0) (xincr 0.0) (yangle 0.0) (yincr 0.0))

(define (nxycos gen fm)
  (declare (gen nxycos) (fm float))
  (let* ((x (nxycos-xangle gen))
	 (y (nxycos-yangle gen))
	 (n (nxycos-n gen))
	 (den (sin (* y 0.5))))
    (set! (nxycos-xangle gen) (+ x (nxycos-xincr gen) (* fm (/ (nxycos-xincr gen) (nxycos-yincr gen)))))
    (set! (nxycos-yangle gen) (+ y (nxycos-yincr gen) fm))
    (if (< (abs den) 1.0e-9)
	1.0
	(/ (* (cos (+ x (* 0.5 (- n 1) y)))
	      (sin (* 0.5 n y)))
	   (* n den))))) ; n=normalization

#|
(with-sound (:clipped #f :statistics #t)
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

;;; fm2.html: j0sin

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
					;(test-zero-stability (lambda () (make-nkssb 0.0 1.0 n)) nkssb (lambda (gen val) (set! (nkssb-modangle gen) val)) zero)
	
	(test-zero-stability (lambda () (make-nxycos :n n)) nxycos (lambda (gen val) (set! (nxycos-yangle gen) val)) zero)
	(test-zero-stability (lambda () (make-noddsin :n n)) noddsin (lambda (gen val) (set! (noddsin-angle gen) val)) zero)
	(test-zero-stability (lambda () (make-noddcos :n n)) noddcos (lambda (gen val) (set! (noddcos-angle gen) val)) zero)
	(test-zero-stability (lambda () (make-ncos2 :n n)) ncos2 (lambda (gen val) (set! (ncos2-angle gen) val)) zero)
	(test-zero-stability (lambda () (make-npcos :n n)) npcos (lambda (gen val) (set! (npcos-angle gen) val)) zero)
	(test-zero-stability (lambda () (make-nrcos :n n)) nrcos (lambda (gen val) (set! (nrcos-angle gen) val)) zero)
	(test-zero-stability (lambda () (make-nxysin :n n)) nxysin (lambda (gen val) (set! (nxysin-yangle gen) val)) zero))
      (list 1 10 3 30))
     
     (test-zero-stability (lambda () (make-krksin :r 0.1)) krksin (lambda (gen val) (set! (krksin-angle gen) val)) zero)
     (test-zero-stability (lambda () (make-k2sin)) k2sin (lambda (gen val) (set! (k2sin-angle gen) val)) zero)
     (test-zero-stability (lambda () (make-abcos :a 1.0 :b 0.5)) abcos (lambda (gen val) (set! (abcos-angle gen) val)) zero)
     
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
	)
      (list 0.1 0.5 .99)))
   (list 0.0 (* 0.5 pi) pi (* 2.0 pi) (* -0.5 pi) (- pi) (* -2.0 pi))))
|#
