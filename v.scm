(use-modules (ice-9 optargs))
(if (not (defined? '*output*)) (load-from-path "ws.scm"))

;;; this version of the fm-violin assumes it is running within with-sound (where *output* and *reverb* are defined)
;;; see fmv.scm for a version that runs more easily in Snd

(definstrument (fm-violin startime dur frequency amplitude #:key
	    (fm-index 1.0)
	    (amp-env '(0 0  25 1  75 1  100 0))
	    (periodic-vibrato-rate 5.0) 
	    (random-vibrato-rate 16.0)
	    (periodic-vibrato-amplitude 0.0025) 
	    (random-vibrato-amplitude 0.005)
	    (noise-amount 0.0) 
	    (noise-freq 1000.0)
	    (ind-noise-freq 10.0) 
	    (ind-noise-amount 0.0)
	    (amp-noise-freq 20.0) 
	    (amp-noise-amount 0.0)
	    (gliss-env '(0 0  100 0)) 
	    (glissando-amount 0.0) 
	    (fm1-env '(0 1  25 .4  75 .6  100 0))  
	    (fm2-env '(0 1  25 .4  75 .6  100 0)) 
	    (fm3-env '(0 1  25 .4  75 .6  100 0))
	    (fm1-rat 1.0) 
	    (fm2-rat 3.0)	 
	    (fm3-rat 4.0)                    
	    (fm1-index #f) 
	    (fm2-index #f) 
	    (fm3-index #f)
	    (degree 0)
	    (distance 1.0)
	    (reverb-amount 0.01)
	    (base 1.0)
	    #:allow-other-keys)

    "(fm-violin startime dur frequency amplitude #:key \n\
   (fm-index 1.0) (amp-env '(0 0  25 1  75 1  100 0)) 
   (periodic-vibrato-rate 5.0) (random-vibrato-rate 16.0) \n\
   (periodic-vibrato-amplitude 0.0025) (random-vibrato-amplitude 0.005) \n\
   (noise-amount 0.0) (noise-freq 1000.0) (ind-noise-freq 10.0) \n\
   (ind-noise-amount 0.0) (amp-noise-freq 20.0) \n\
   (amp-noise-amount 0.0) (gliss-env '(0 0  100 0)) \n\
   (glissando-amount 0.0) (fm1-env '(0 1  25 .4  75 .6  100 0)) \n\
   (fm2-env '(0 1  25 .4  75 .6  100 0)) (fm3-rat 4.0) \n\
   (fm3-env '(0 1  25 .4  75 .6  100 0)) (fm1-rat 1.0) \n\
   (fm2-rat 3.0) (fm1-index #f) (fm2-index #f) \n\
   (fm3-index #f) (degree 0) (distance 1.0) \n\
   (reverb-amount 0.01) (base 1.0)) \n\
This version of the fm-violin assumes it is running within with-sound (where *output* and *reverb* are defined).\n\
  (with-sound () (fm-violin 0 1 440 .1))"

    (let* ((pi 3.141592653589793)
	   (beg (inexact->exact (floor (* startime (mus-srate)))))
	   (len (inexact->exact (floor (* dur (mus-srate)))))
	   (end (+ beg len))
	   (frq-scl (hz->radians frequency))
	   (modulate (not (zero? fm-index)))
	   (maxdev (* frq-scl fm-index))
	   (logfreq (log frequency))
	   (sqrtfreq (sqrt frequency))
	   (index1 (or fm1-index (min pi (* maxdev (/ 5.0 logfreq)))))
	   (index2 (or fm2-index (min pi (* maxdev 3.0 (/ (- 8.5 logfreq) (+ 3.0 (* frequency .001)))))))
	   (index3 (or fm3-index (min pi (* maxdev (/ 4.0 sqrtfreq)))))
	   (easy-case (and (zero? noise-amount)
			   (equal? fm1-env fm2-env)
			   (equal? fm1-env fm3-env)
			   (= fm1-rat (floor fm1-rat))
			   (= fm2-rat (floor fm2-rat))
			   (= fm3-rat (floor fm3-rat))))
	   (coeffs (and easy-case modulate
			(partials->polynomial
			 (list fm1-rat index1
			       (inexact->exact (floor (/ fm2-rat fm1-rat))) index2
			       (inexact->exact (floor (/ fm3-rat fm1-rat))) index3))))
	   (norm (or (and easy-case modulate 1.0) index1))
	   (carrier (make-oscil frequency))
	   (fmosc1  (and modulate (make-oscil (* fm1-rat frequency))))
	   (fmosc2  (and modulate (or easy-case (make-oscil (* fm2-rat frequency)))))
	   (fmosc3  (and modulate (or easy-case (make-oscil (* fm3-rat frequency)))))
	   (ampf  (make-env amp-env :scaler amplitude :base base :duration dur))
	   (indf1 (and modulate (make-env fm1-env norm :duration dur)))
	   (indf2 (and modulate (or easy-case (make-env fm2-env index2 :duration dur))))
	   (indf3 (and modulate (or easy-case (make-env fm3-env index3 :duration dur))))
	   (frqf (make-env gliss-env (* glissando-amount frq-scl) :duration dur))
	   (pervib (make-triangle-wave periodic-vibrato-rate (* periodic-vibrato-amplitude frq-scl)))
	   (ranvib (make-rand-interp random-vibrato-rate (* random-vibrato-amplitude frq-scl)))
	   (fm-noi (if (not (= 0.0 noise-amount))
		       (make-rand noise-freq (* pi noise-amount))
		       #f))
	   (ind-noi (if (and (not (= 0.0 ind-noise-amount)) (not (= 0.0 ind-noise-freq)))
			(make-rand-interp ind-noise-freq ind-noise-amount)
			#f))
	   (amp-noi (if (and (not (= 0.0 amp-noise-amount)) (not (= 0.0 amp-noise-freq)))
			(make-rand-interp amp-noise-freq amp-noise-amount)
			#f))
	   (locs (make-locsig degree distance reverb-amount *output* *reverb* (mus-channels *output*)))
	   (vib 0.0) 
	   (modulation 0.0)
	   (fuzz 0.0)
	   (ind-fuzz 1.0)
	   (amp-fuzz 1.0))
      (if (c-g?)
	  (let ((stack (make-stack #t)))
	    (call-with-current-continuation
	     (lambda (continue)
	       (throw 'with-sound-interrupt continue stack)))))
      (if (or (not easy-case) ind-noi amp-noi (> noise-amount 0.0))
	  (run
	   (lambda ()
	     (do ((i beg (1+ i)))
		 ((= i end))
	       (if (not (= 0.0 noise-amount))
		   (set! fuzz (rand fm-noi)))
	       (set! vib (+ (env frqf) (triangle-wave pervib) (rand-interp ranvib)))
	       (if ind-noi (set! ind-fuzz (+ 1.0 (rand-interp ind-noi))))
	       (if amp-noi (set! amp-fuzz (+ 1.0 (rand-interp amp-noi))))
	       (if modulate
		   (if easy-case
		       (set! modulation
			     (* (env indf1) 
				(polynomial coeffs (oscil fmosc1 vib)))) ;(* vib fm1-rat)??
		       (set! modulation
			     (+ (* (env indf1) (oscil fmosc1 (+ (* fm1-rat vib) fuzz)))
				(* (env indf2) (oscil fmosc2 (+ (* fm2-rat vib) fuzz)))
				(* (env indf3) (oscil fmosc3 (+ (* fm3-rat vib) fuzz)))))))
	       (locsig locs i (* (env ampf) amp-fuzz
				 (oscil carrier (+ vib (* ind-fuzz modulation))))))))
	  (run
	   (lambda () 
	     (do ((i beg (1+ i)))
		 ((= i end))
	       (let* ((vib (+ (env frqf) (triangle-wave pervib) (rand-interp ranvib)))
		      (modulation (* (env indf1) (polynomial coeffs (oscil fmosc1 vib)))))
		 (locsig locs i (* (env ampf) (oscil carrier (+ vib modulation)))))))))))



; (fm-violin 0 1 440 .1 :fm-index 2.0)
