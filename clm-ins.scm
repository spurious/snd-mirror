;;; CLM instruments translated to Snd/Scheme

(if (not (defined? '*output*)) (load-from-path "ws.scm"))
(if (not (defined? 'stretch-envelope)) (load-from-path "env.scm"))

;;; -------- pluck
;;;
;;; The Karplus-Strong algorithm as extended by David Jaffe and Julius Smith -- see 
;;;  Jaffe and Smith, "Extensions of the Karplus-Strong Plucked-String Algorithm"
;;;  CMJ vol 7 no 2 Summer 1983, reprinted in "The Music Machine".
;;;  translated from CLM's pluck.ins

(definstrument (pluck start dur freq amp-1 #:optional (weighting .5) (lossfact .9))
  "(pluck start dur freq amp weighting lossfact) implements the Jaffe-Smith plucked string physical model. \
'weighting' is the ratio of the once-delayed to the twice-delayed samples.  It defaults to .5=shortest decay. \
Anything other than .5 = longer decay.  Must be between 0 and less than 1.0. \
'lossfact' can be used to shorten decays.  Most useful values are between .8 and 1.0. (pluck 0 1 330 .3 .95 .95)"

  (define (getOptimumC S o p)
    (let* ((pa (* (/ 1.0 o) (atan (* S (sin o)) (+ (- 1.0 S) (* S (cos o))))))
	   (tmpInt (inexact->exact (floor (- p pa))))
	   (pc (- p pa tmpInt)))
      (if (< pc .1)
	  (do ()
	      ((>= pc .1))
	    (set! tmpInt (- tmpInt 1))
	    (set! pc (+ pc 1.0))))
      (list tmpInt (/ (- (sin o) (sin (* o pc))) (sin (+ o (* o pc)))))))

  (define (tuneIt f s1)
    (let* ((p (/ (mus-srate) f))	;period as float
	   (s (if (= s1 0.0) 0.5 s1))
	   (o (hz->radians f))
	   (vals (getOptimumC s o p))
	   (T1 (car vals))
	   (C1 (cadr vals))
	   (vals1 (getOptimumC (- 1.0 s) o p))
	   (T2 (car vals1))
	   (C2 (cadr vals1)))
      (if (and (not (= s .5))
	       (< (abs C1) (abs C2)))
	  (list (- 1.0 s) C1 T1)
	(list s C2 T2))))

  (let* ((vals (tuneIt freq weighting))
	 (wt0 (car vals))
	 (c (cadr vals))
	 (dlen (caddr vals))
	 (amp amp-1)
	 (beg (inexact->exact (floor (* start (mus-srate)))))
	 (end (+ beg (inexact->exact (floor (* dur (mus-srate))))))
	 (lf (if (= lossfact 0.0) 1.0 (min 1.0 lossfact)))
	 (wt (if (= wt0 0.0) 0.5 (min 1.0 wt0)))
	 (tab (make-vct dlen))
	 ;; get initial waveform in "tab" -- here we can introduce 0's to simulate different pick
	 ;; positions, and so on -- see the CMJ article for numerous extensions.  The normal case
	 ;; is to load it with white noise (between -1 and 1).
	 (val 0.0)
	 (allp (make-one-zero (* lf (- 1.0 wt)) (* lf wt)))
	 (feedb (make-one-zero c 1.0)) ;or (feedb (make-one-zero 1.0 c))
	 (ctr 0))
    (do ((i 0 (1+ i)))
	((= i dlen))
      (vct-set! tab i (- 1.0 (random 2.0))))
    (run 
     (lambda ()
       (do ((i beg (1+ i)))
	   ((= i end))
	 (let ((val (vct-ref tab ctr)))	;current output value
	   (vct-set! tab ctr (* (- 1.0 c) 
				(one-zero feedb 
					  (one-zero allp val))))
	   (set! ctr (+ ctr 1))
	   (if (>= ctr dlen) (set! ctr 0))
	   (outa i (* amp val) *output*)))))))



;;; -------- mlbvoi
;;;
;;; translation from MUS10 of Marc LeBrun's waveshaping voice instrument (using FM here)
;;; this version translated (and simplified slightly) from CLM's mlbvoi.ins

(definstrument (vox beg dur freq amp ampfun freqfun freqscl voxfun index vibscl)
  "(vox beg dur freq amp ampfun freqfun freqscl voxfun index vibscl) is a version of the waveshaping \
voice: (vox 0 2 110 .4 '(0 0 25 1 75 1 100 0) '(0 0 5 .5 10 0 100 1) .1 '(0 UH 25 UH 35 ER 65 ER 75 UH 100 UH) .025 .1)"

  (let ((formants
	 '((I 390 1990 2550)  (E 530 1840 2480)  (AE 660 1720 2410)
	   (UH 520 1190 2390) (A 730 1090 2440)  (OW 570 840 2410)
	   (U 440 1020 2240)  (OO 300 870 2240)  (ER 490 1350 1690)
	   (W 300 610 2200)   (LL 380 880 2575)  (R 420 1300 1600)
	   (Y 300 2200 3065)  (EE 260 3500 3800) (LH 280 1450 1600)
	   (L 300 1300 3000)  (I2 350 2300 3340) (B 200 800 1750)
	   (D 300 1700 2600)  (G 250 1350 2000)  (M 280 900 2200)
	   (N 280 1700 2600)  (NG 280 2300 2750) (P 300 800 1750)
	   (T 200 1700 2600)  (K 350 1350 2000)  (F 175 900 4400)
	   (TH 200 1400 2200) (S 200 1300 2500)  (SH 200 1800 2000)
	   (V 175 1100 2400)  (THE 200 1600 2200)(Z 200 1300 2500)
	   (ZH 175 1800 2000) (ZZ 900 2400 3800) (VV 565 1045 2400))))
	;;formant center frequencies for a male speaker

    (define (find-phoneme phoneme forms)
      (if (eq? phoneme (car (car forms)))
	  (cdr (car forms))
	(find-phoneme phoneme (cdr forms))))
    
    (let ((f1 '())
	  (f2 '())
	  (f3 '())
	  (len (length voxfun)))
      (do ((i (- len 1) (- i 2)))
	  ((<= i 0))
	(let ((phon (find-phoneme (list-ref voxfun i) formants))
	      (x (list-ref voxfun (- i 1))))
	  (set! f1 (cons (car phon) f1))
	  (set! f1 (cons x f1))
	  (set! f2 (cons (cadr phon) f2))
	  (set! f2 (cons x f2))
	  (set! f3 (cons (caddr phon) f3))
	  (set! f3 (cons x f3))))
      
      (let* ((start (inexact->exact (floor (* (mus-srate) beg))))
	     (end (+ start (inexact->exact (floor (* (mus-srate) dur)))))
	     (car-os (make-oscil :frequency 0))
	     (of0 (make-oscil :frequency 0))
	     (of1 (make-oscil :frequency 0))
	     (of2 (make-oscil :frequency 0))
	     (of3 (make-oscil :frequency 0))
	     (of4 (make-oscil :frequency 0))
	     (of5 (make-oscil :frequency 0))
	     (ampf (make-env :envelope ampfun :scaler amp :duration dur))
	     (frmf1 (make-env :envelope f1 :duration dur))
	     (frmf2 (make-env :envelope f2 :duration dur))
	     (frmf3 (make-env :envelope f3 :duration dur))
	     (freqf (make-env :envelope freqfun :duration dur
			      :scaler (* freqscl freq)
			      :offset freq))
	     (per-vib (make-triangle-wave :frequency 6 :amplitude (* freq vibscl)))
	     (ran-vib (make-rand-interp :frequency 20 :amplitude (* freq .01)))
	     (car 0.0)
	     (frq 0.0)
	     (frm-int 0)
	     (frm0 0.0)
	     (frm 0.0)
	     (frq0 0.0) (frq1 0.0) (frq2 0.0) (frq3 0.0) (frq4 0.0) (frq5 0.0)
	     (amp0 0.0) (amp1 0.0) (amp2 0.0) (amp3 0.0) (amp4 0.0) (amp5 0.0))
	(run
	 (lambda ()
	   (do ((i start (1+ i)))
	       ((= i end))
	     (set! frq (+ (env freqf) (triangle-wave per-vib) (rand-interp ran-vib)))
	     (set! car (* index (oscil car-os (hz->radians frq))))
	     (set! frm (env frmf1))
	     (set! frm0 (/ frm frq))
	     (set! frm-int (inexact->exact (floor frm0)))
	     (if (even? frm-int)
		 (begin
		   (set! frq0 (hz->radians (* frm-int frq)))
		   (set! frq1 (hz->radians (* (+ frm-int 1) frq)))
		   (set! amp1 (- frm0 frm-int))
		   (set! amp0 (- 1.0 amp1)))
		 (begin
		   (set! frq1 (hz->radians (* frm-int frq)))
		   (set! frq0 (hz->radians (* (+ frm-int 1) frq)))
		   (set! amp0 (- frm0 frm-int))
		   (set! amp1 (- 1.0 amp0))))
	     (set! frm (env frmf2))
	     (set! frm0 (/ frm frq))
	     (set! frm-int (inexact->exact (floor frm0)))
	     (if (even? frm-int)
		 (begin
		   (set! frq2 (hz->radians (* frm-int frq)))
		   (set! frq3 (hz->radians (* (+ frm-int 1) frq)))
		   (set! amp3 (- frm0 frm-int))
		   (set! amp2 (- 1.0 amp3)))
		 (begin
		   (set! frq3 (hz->radians (* frm-int frq)))
		   (set! frq2 (hz->radians (* (+ frm-int 1) frq)))
		   (set! amp2 (- frm0 frm-int))
		   (set! amp3 (- 1.0 amp2))))
	     (set! frm (env frmf3))
	     (set! frm0 (/ frm frq))
	     (set! frm-int (inexact->exact (floor frm0)))
	     (if (even? frm-int)
		 (begin
		   (set! frq4 (hz->radians (* frm-int frq)))
		   (set! frq5 (hz->radians (* (+ frm-int 1) frq)))
		   (set! amp5 (- frm0 frm-int))
		   (set! amp4 (- 1.0 amp5)))
		 (begin
		   (set! frq5 (hz->radians (* frm-int frq)))
		   (set! frq4 (hz->radians (* (+ frm-int 1) frq)))
		   (set! amp4 (- frm0 frm-int))
		   (set! amp5 (- 1.0 amp4))))
	     (outa i (* (env ampf)
			(+ (* .8 (+ (* amp0 (oscil of0 (+ frq0 (* .2 car))))
				    (* amp1 (oscil of1 (+ frq1 (* .2 car))))))
			   (* .15 (+ (* amp2 (oscil of2 (+ frq2 (* .5 car))))
				     (* amp3 (oscil of3 (+ frq3 (* .5 car))))))
			   (* .05 (+ (* amp4 (oscil of4 (+ frq4 car)))
				     (* amp5 (oscil of5 (+ frq5 car)))))))
		   *output*))))))))
  
;;; (vox 0 2 170 .4 '(0 0 25 1 75 1 100 0) '(0 0 5 .5 10 0 100 1) .1 '(0 E 25 AE 35 ER 65 ER 75 I 100 UH) .05 .1)
;;; (vox 0 2 300 .4 '(0 0 25 1 75 1 100 0) '(0 0 5 .5 10 0 100 1) .1 '(0 I 5 OW 10 I 50 AE 100 OO) .02 .1)
;;; (vox 0 5 600 .4 '(0 0 25 1 75 1 100 0) '(0 0 5 .5 10 0 100 1) .1 '(0 I 5 OW 10 I 50 AE 100 OO) .01 .1)
  

;;; -------- FOF example

(definstrument (fofins beg dur frq amp uvib f0 a0 f1 a1 f2 a2 #:optional (ae '(0 0 25 1 75 1 100 0)))
  "(fofins beg dur frq amp vib f0 a0 f1 a1 f2 a2 #:optional (ae '(0 0 25 1 75 1 100 0))) produces FOF \
synthesis: (fofins 0 1 270 .2 .001 730 .6 1090 .3 2440 .1)"
    (let* ((two-pi (* 2 3.141592653589793))
	   (start (inexact->exact (* beg (mus-srate))))
	   (len (inexact->exact (* dur (mus-srate))))
	   (end (+ start len))
	   (vib uvib) ; for the optimizer (it can't find define* args sometimes)
	   (ampf (make-env :envelope ae :scaler amp :duration dur))
	   (frq0 (hz->radians f0))
	   (frq1 (hz->radians f1))
	   (frq2 (hz->radians f2))
	   (foflen (if (= (mus-srate) 22050) 100 200))
	   (vibr (make-oscil :frequency 6))
	   (win-freq (/ two-pi foflen))
	   (wt0 (make-wave-train :wave (make-vct foflen) :frequency frq))
	   (foftab (mus-data wt0)))
      (do ((i 0 (1+ i)))
	  ((= i foflen))
	(vct-set! foftab i (* (+ (* a0 (sin (* i frq0)))
				 (* a1 (sin (* i frq1)))
				 (* a2 (sin (* i frq2))))
			      .5 (- 1.0 (cos (* i win-freq))))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i end))
	   (outa i (* (env ampf) (wave-train wt0 (* vib (oscil vibr)))) *output*))))))


;;; -------- bes-fm

(define (j0 x)				;returns J0(x) for any real x
  (if (< (abs x) 8.0)			;direct rational function fit
      (let* ((y (* x x))
	     (ans1 (+ 57568490574.0
		      (* y (+ -13362590354.0 
			      (* y  (+ 651619640.7
				       (* y (+ -11214424.18 
					       (* y (+ 77392.33017
						       (* y -184.9052456)))))))))))
	     (ans2 (+ 57568490411.0 
		      (* y (+ 1029532985.0 
			      (* y (+ 9494680.718
				      (* y (+ 59272.64853
					      (* y (+ 267.8532712 y)))))))))))
	(/ ans1 ans2))
    (let* ((ax (abs x))
	   (z (/ 8.0 ax))
	   (y (* z z))
	   (xx (- ax 0.785398164))
	   (ans1 (+ 1.0 
		    (* y (+ -0.1098628627e-2 
			    (* y (+ 0.2734510407e-4
				    (* y (+ -0.2073370639e-5
					    (* y 0.2093887211e-6)))))))))
	   (ans2 (+ -0.1562499995e-1
		    (* y (+ 0.1430488765e-3
			    (* y (+ -0.6911147651e-5
				    (* y (+ 0.7621095161e-6
					    (* y -0.934945152e-7))))))))))
      (* (sqrt (/ 0.636619772 ax))
	 (- (* (cos xx) ans1)
	    (* z (sin xx) ans2))))))

(definstrument (bes-fm start len freq amp ratio index)
  (let* ((car-ph 0.0)
	 (mod-ph 0.0)
	 (beg (inexact->exact (floor (* (mus-srate) start))))
	 (dur (inexact->exact (floor (* (mus-srate) len))))
	 (end (+ beg dur))
	 (car-incr (hz->radians freq))
	 (mod-incr (* ratio car-incr))
	 (ampenv (make-env :envelope '(0 0 25 1 75 1 100 0) :scaler amp :end dur)))
    (run
     (lambda ()
       (do ((i beg (1+ i)))
	   ((= i end))
	 (outa i (* (env ampenv) (j0 car-ph)) *output*)
	 (set! car-ph (+ car-ph car-incr (* index (j0 mod-ph))))
	 (set! mod-ph (+ mod-ph mod-incr)))))))

;;; (with-sound () (bes-fm 0 .5 440 5.0 1.0 8.0))


;;; FM TRUMPET ---------------------------------------------------
;;; Dexter Morrill's FM-trumpet:
;;; from CMJ feb 77 p51

(definstrument (fm-trumpet startime dur
			   #:key (frq1 250.0)
			   (frq2 1500.0)
			   (amp1 0.5)
			   (amp2 0.1)
			   (ampatt1 0.03)
			   (ampdec1 0.35)
			   (ampatt2 0.03)
			   (ampdec2 0.3)
			   (modfrq1 250.0)
			   (modind11 0.0)
			   (modind12 2.66)
			   (modfrq2 250.0)
			   (modind21 0.0)
			   (modind22 1.8)
			   (rvibamp 0.007)
			   (rvibfrq 125.0)
			   (vibamp 0.007)
			   (vibfrq 7.0)
			   (vibatt 0.6)
			   (vibdec 0.2)
			   (frqskw 0.03)
			   (frqatt 0.06)
			   (ampenv1 '(0 0  25 1  75 .9  100 0))
			   (ampenv2 '(0 0  25 1  75 .9  100 0))
			   (indenv1 '(0 0  25 1  75 .9  100 0))
			   (indenv2 '(0 0  25 1  75 .9  100 0))
			   (degree 0.0)
			   (distance 1.0)
			   (reverb-amount 0.005))
  (let* ((beg (inexact->exact (floor (* startime (mus-srate)))))
	 (end (+ beg (inexact->exact (floor (* dur (mus-srate))))))
	 (loc (make-locsig degree distance reverb-amount *output* *reverb* (mus-channels *output*)))
	 (per-vib-f (make-env :envelope (stretch-envelope '(0 1  25 .1  75 0  100 0)
							  25 (min (* 100 (/ vibatt dur)) 45)
							  75 (max (* 100 (- 1.0 (/ vibdec dur))) 55))
			      :scaler vibamp :duration dur))
	 (ran-vib (make-rand-interp :frequency rvibfrq :amplitude rvibamp))
	 (per-vib (make-oscil :frequency vibfrq))
	 (dec-01 (max 75 (* 100 (- 1.0 (/ .01 dur)))))
	 (frq-f (make-env :envelope (stretch-envelope '(0 0  25 1  75 1  100 0)
						      25 (min 25 (* 100 (/ frqatt dur)))
						      75 dec-01)
			  :scaler frqskw :duration dur))
	 (ampattpt1 (min 25 (* 100 (/ ampatt1 dur))))
	 (ampdecpt1 (max 75 (* 100 (- 1.0 (/ ampdec1 dur)))))
	 (ampattpt2 (min 25 (* 100 (/ ampatt2 dur))))
	 (ampdecpt2 (max 75 (* 100 (- 1.0 (/ ampdec2 dur)))))
	 
	 (mod1-f (make-env :envelope (stretch-envelope indenv1 25 ampattpt1 75 dec-01)
			   :scaler (* modfrq1 (- modind12 modind11)) :duration dur))
	 (mod1 (make-oscil :frequency 0.0))
	 (car1 (make-oscil :frequency 0.0))
	 ;; set frequency to zero here because it is handled multiplicatively below
	 (car1-f (make-env :envelope (stretch-envelope ampenv1 25 ampattpt1 75 ampdecpt1)
			   :scaler amp1 :duration dur))
	 
	 (mod2-f (make-env :envelope (stretch-envelope indenv2 25 ampattpt2 75 dec-01)
			   :scaler (* modfrq2 (- modind22 modind21)) :duration dur))
	 (mod2 (make-oscil :frequency 0.0))
	 (car2 (make-oscil :frequency 0.0))
	 (car2-f (make-env :envelope (stretch-envelope ampenv2 25 ampattpt2 75 ampdecpt2)
			   :scaler amp2 :duration dur)))
    
    (run
     (lambda ()
       (do ((i beg (1+ i)))
	   ((= i end))
	 (let ((frq-change (hz->radians (* (+ 1.0 (rand-interp ran-vib))
					   (+ 1.0 (* (env per-vib-f) (oscil per-vib)))
					   (+ 1.0 (env frq-f))))))
	   (locsig loc i (+ (* (env car1-f) 
			       (oscil car1 (* frq-change 
					      (+ frq1 (* (env mod1-f) 
							 (oscil mod1 (* modfrq1 frq-change)))))))
			    (* (env car2-f) 
			       (oscil car2 (* frq-change 
					      (+ frq2 (* (env mod2-f) 
							 (oscil mod2 (* modfrq2 frq-change)))))))))))))))





