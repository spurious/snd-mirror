;;; translation of CLM pqwvox.ins (itself translated from MUS10 of MLB's waveshaping voice instrument (using phase quadrature waveshaping))

(define formant-list ; "formants" already used in examp.scm
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
      (ZH 175 1800 2000) (ZZ 900 2400 3800) (VV 565 1045 2400)))
                   ;;formant center frequencies for a male speaker

(define find-phoneme
  (lambda (phoneme form)
    (if (eq? (car (car form)) phoneme)
	(cdr (car form))
      (find-phoneme phoneme (cdr form)))))

(define vox-fun
  (lambda (phons which newenv)
    ;; make an envelope from which-th entry of phoneme data referred to by phons
    (if (null? phons)
	newenv
      (vox-fun (cddr phons) which
	       (append newenv
		       (list (car phons)
			     (list-ref (find-phoneme (cadr phons) formant-list) which)))))))

(define normalize-partials
  (lambda (partials)
    (let ((sum 0.0)
	  (len (length partials)))
      (do ((i 1 (+ i 2)))
	  ((>= i len))
	(set! sum (+ sum (abs (list-ref partials i)))))
      (do ((i 1 (+ i 2)))
	  ((>= i len) partials)
	(list-set! partials i (/ (list-ref partials i) sum))))))

(define pqw-vox
  (lambda (beg dur freq spacing-freq amp ampfun freqfun freqscl phonemes formant-amps formant-shapes)
    (let* ((start (inexact->exact (* (srate) beg)))
	   (samps (inexact->exact (* (srate) dur)))
	   (car-sin (make-oscil :frequency 0))
	   (car-cos (make-oscil :frequency 0 :initial-phase (/ pi 2.0)))
	   (frq-ratio (/ spacing-freq freq))
	   (fs (length formant-amps))
	   (sin-evens (make-vector fs))
	   (cos-evens (make-vector fs))
	   (sin-odds (make-vector fs))
	   (cos-odds (make-vector fs))
	   (amps (make-vector fs))
	   (frmfs (make-vector fs))
	   (sin-coeffs (make-vector fs))
	   (cos-coeffs (make-vector fs))
	   (ampf (make-env :envelope ampfun :scaler amp :duration dur))
	   (freqf (make-env :envelope freqfun :duration dur :scaler (* freqscl freq) :offset freq))
	   (per-vib (make-triangle-wave :frequency 6.0 :amplitude (* freq .1)))
	   (ran-vib (make-rand-interp :frequency 20.0 :amplitude (* freq .05)))
	   (out-data (make-vct samps)))
      (do ((i 0 (1+ i)))
	  ((= i fs))
	(let* ((amp (list-ref formant-amps i))
	       (fshape (list-ref formant-shapes i))
	       (shape (normalize-partials fshape)))
	  (vector-set! sin-evens i (make-oscil :frequency 0))
	  (vector-set! sin-odds i (make-oscil :frequency 0))
	  (vector-set! cos-evens i (make-oscil :frequency 0 :initial-phase (/ pi 2.0)))
	  (vector-set! cos-odds i (make-oscil :frequency 0 :initial-phase (/ pi 2.0)))
	  (vector-set! amps i amp)
	  (vector-set! cos-coeffs i (partials->polynomial shape 1))
	  (vector-set! sin-coeffs i (partials->polynomial shape 0))
	  (vector-set! frmfs i (make-env :envelope (vox-fun phonemes i '()) :duration dur))))
      (vct-map! out-data
		(lambda ()
		  (let* ((frq (+ (env freqf) (triangle-wave per-vib) (rand-interp ran-vib)))
			 (frqscl (hz->radians (* frq frq-ratio)))
			 (carsin (oscil car-sin frqscl))
			 (carcos (oscil car-cos frqscl))
			 (even-amp 0.0)
			 (odd-amp 0.0)
			 (even-freq 0.0)
			 (odd-freq 0.0)
			 (sum 0.0))
		    (do ((k 0 (1+ k)))
			((= k fs))
		      (let* ((frm (env (vector-ref frmfs k)))
			     (frm0 (/ frm frq))
			     (frm-int (inexact->exact (floor frm0))))
			(if (even? frm-int)
			    (begin
			     (set! even-freq (hz->radians (* frm-int frq)))
			     (set! odd-freq (hz->radians (* (+ frm-int 1) frq)))
			     (set! odd-amp (- frm0 frm-int))
			     (set! even-amp (- 1.0 odd-amp)))
			  (begin
			   (set! odd-freq (hz->radians (* frm-int frq)))
			   (set! even-freq (hz->radians (* (+ frm-int 1) frq)))
			   (set! even-amp (- frm0 frm-int))
			   (set! odd-amp (- 1.0 even-amp))))
			(let* ((fax (polynomial (vector-ref cos-coeffs k) carcos))
			       (yfax (* carsin (polynomial (vector-ref sin-coeffs k) carcos))))
			  (set! sum (+ sum (* (vector-ref amps k)
					      (+ (* even-amp (- (* yfax (oscil (vector-ref sin-evens k) even-freq))
								(* fax (oscil (vector-ref cos-evens k) even-freq))))
						 (* odd-amp (- (* yfax (oscil (vector-ref sin-odds k) odd-freq))
							       (* fax (oscil (vector-ref cos-odds k) odd-freq)))))))))))
		    (* (env ampf) sum))))
      (mix-vct out-data start #f 0 #f))))


;;; (pqw-vox 0 1 300 300 .1 '(0 0 50 1 100 0) '(0 0 100 0) 0 '(0 L 100 L) '(.33 .33 .33) '((1 1 2 .5) (1 .5 2 .5 3 1) (1 1 4 .5)))
;;; (a test to see if the cancellation is working -- sounds like a mosquito)

;;; (pqw-vox 0 2 100 100 .1 '(0 0 50 1 100 0) '(0 0 100 1) .1 '(0 UH 100 ER) '(.8 .15 .05) '((1 1 2 .5) (1 1 2 .5 3 .2 4 .1) (1 1 3 .1 4 .5)))
;;; (pqw-vox 0 2 100 314 .1 '(0 0 50 1 100 0) '(0 0 100 1) .1 '(0 UH 100 ER) '(.8 .15 .05) '((1 1 2 .5) (1 1 2 .5 3 .2 4 .1) (1 1 3 .1 4 .5)))
;;; (pqw-vox 0 2 200 314 .1 '(0 0 50 1 100 0) '(0 0 100 1) .01 '(0 UH 100 ER) '(.8 .15 .05) '((1 1 2 .5) (1 1 4 .1) (1 1 2 .1 4 .05)))
;;; (pqw-vox 0 2 100 414 .2 '(0 0 50 1 100 0) '(0 0 100 1) .01 '(0 OW 50 E 100 ER) '(.8 .15 .05) '((1 1 2 .5 3 .1 4 .01) (1 1 4 .1) (1 1 2 .1 4 .05)))
