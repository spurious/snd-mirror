;;; fm-violin as a generator (and at end, original instrument using this generator)
;;;
;;; make-fm-violin takes the same args as the instrument version with the following changes
;;;   beg and dur are omitted, also degree, reverb-amount, distance
;;;   all envelopes default to constants (rather than envelopes)
;;;   from the generator's point of view, each envelope is a function called at run time to get its next value,
;;;     very much like "as-needed" input in src or granulate, so the envelopes could actually be any
;;;     arbitrary function you like (see examples at end).
;;;   returns a violin structure (actually a list)
;;; fm-violin takes the value returned by make-fm-violin and returns a new sample each time it is called
;;; see snd-gtk.scm for a dialog that runs this instrument in "real-time" with a slider controlling its amplitude

(use-modules (ice-9 optargs))

(define pi 3.141592653589793)

(define make-fm-violin 
  (lambda* (frequency amplitude #&key
	    (fm-index 1.0)
	    (amp-env #f)
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
	    (gliss-env #f)
	    (fm1-env #f)
	    (fm2-env #f)
	    (fm3-env #f)
	    (fm1-rat 1.0) 
	    (fm2-rat 3.0)	 
	    (fm3-rat 4.0)                    
	    (fm1-index #f) 
	    (fm2-index #f) 
	    (fm3-index #f)
	    (base 1.0)
	    #&allow-other-keys)
    (let* ((frq-scl (hz->radians frequency))
	   (modulate (not (zero? fm-index)))
	   (maxdev (* frq-scl fm-index))
	   (logfreq (log frequency))
	   (index1 (or fm1-index (min pi (* maxdev (/ 5.0 logfreq)))))
	   (index2 (or fm2-index (min pi (* maxdev 3.0 (/ (- 8.5 logfreq) (+ 3.0 (* frequency .001)))))))
	   (index3 (or fm3-index (min pi (* maxdev (/ 4.0 (sqrt frequency))))))
	   (easy-case (and (zero? noise-amount)
			   (or (not fm2-env) (equal? fm1-env fm2-env))
			   (or (not fm3-env) (equal? fm1-env fm3-env))
			   (= fm1-rat (floor fm1-rat))
			   (= fm2-rat (floor fm2-rat))
			   (= fm3-rat (floor fm3-rat)))))
      (list
       (make-oscil frequency)                                                            ;carrier
       (and modulate (make-oscil (* fm1-rat frequency)))                                 ;fmosc1
       (and modulate (or easy-case (make-oscil (* fm2-rat frequency))))                  ;fmosc2
       (and modulate (or easy-case (make-oscil (* fm3-rat frequency))))                  ;fmosc3
       (and easy-case modulate
	    (partials->polynomial
	     (list fm1-rat index1
		   (floor (/ fm2-rat fm1-rat)) index2
		   (floor (/ fm3-rat fm1-rat)) index3)))                                 ;coeffs
       (or amp-env (lambda () amplitude))                                                ;ampf
       (or fm1-env (lambda () (or (and easy-case modulate 1.0) index1)))                 ;indf1
       (or fm2-env (lambda () index2))                                                   ;indf2
       (or fm3-env (lambda () index3))                                                   ;indf3
       (make-triangle-wave periodic-vibrato-rate (* periodic-vibrato-amplitude frq-scl)) ;pervib
       (make-rand-interp random-vibrato-rate (* random-vibrato-amplitude frq-scl))       ;ranvib
       (if (not (= 0.0 noise-amount))
	   (make-rand noise-freq (* pi noise-amount))
	   #f)                                                                           ;fm-noi
       fm1-rat
       fm2-rat
       fm3-rat
       (if (and (not (= 0.0 amp-noise-amount)) (not (= 0.0 amp-noise-freq)))
	   (make-rand-interp amp-noise-freq amp-noise-amount)
	   #f)                                                                           ;amp-noi
       (if (and (not (= 0.0 ind-noise-amount)) (not (= 0.0 ind-noise-freq)))
	   (make-rand-interp ind-noise-freq ind-noise-amount)
	   #f)                                                                           ;ind-noi
       (or gliss-env (lambda () 0.0))))))

(define v-carrier (lambda (v) (list-ref v 0)))
(define v-fmosc1 (lambda (v) (list-ref v 1)))
(define v-fmosc2 (lambda (v) (list-ref v 2)))
(define v-fmosc3 (lambda (v) (list-ref v 3)))
(define v-coeffs (lambda (v) (list-ref v 4)))
(define v-ampf (lambda (v) (list-ref v 5)))
(define v-indf1 (lambda (v) (list-ref v 6)))
(define v-indf2 (lambda (v) (list-ref v 7)))
(define v-indf3 (lambda (v) (list-ref v 8)))
(define v-pervib (lambda (v) (list-ref v 9)))
(define v-ranvib (lambda (v) (list-ref v 10)))
(define v-fm-noi (lambda (v) (list-ref v 11)))
(define v-fm1-rat (lambda (v) (list-ref v 12)))
(define v-fm2-rat (lambda (v) (list-ref v 13)))
(define v-fm3-rat (lambda (v) (list-ref v 14)))
(define v-amp-noi (lambda (v) (list-ref v 15)))
(define v-ind-noi (lambda (v) (list-ref v 16)))
(define v-frqf (lambda (v) (list-ref v 17)))

(define fm-violin
  (lambda (v)
    (let ((vib (+ ((v-frqf v)) (triangle-wave (v-pervib v)) (rand-interp (v-ranvib v))))
	  (fuzz (if (v-fm-noi v) (rand (v-fm-noi v)) 0.0)))
      (* ((v-ampf v))
	 (if (v-amp-noi v) (+ 1.0 (rand-interp (v-amp-noi v))) 1.0)
	 (oscil (v-carrier v) 
		(+ vib 
		   (* (if (v-ind-noi v) (+ 1.0 (rand-interp (v-ind-noi v))) 1.0)
		      (if (v-fmosc1 v)
			  (if (v-coeffs v)
			      (* ((v-indf1 v))
				 (polynomial (v-coeffs v) (oscil (v-fmosc1 v) vib)))
			      (+ (* ((v-indf1 v)) (oscil (v-fmosc1 v) (+ (* (v-fm1-rat v) vib) fuzz)))
				 (* ((v-indf2 v)) (oscil (v-fmosc2 v) (+ (* (v-fm2-rat v) vib) fuzz)))
				 (* ((v-indf3 v)) (oscil (v-fmosc3 v) (+ (* (v-fm3-rat v) vib) fuzz)))))
			  0.0))))))))


(define test-v 
  (lambda (beg dur freq amp amp-env)
    (let ((v (make-fm-violin 
	      freq amp 
	      :amp-env (let ((e (make-env :envelope (or amp-env '(0 0 1 1 2 0)) 
					  :scaler amp 
					  :end dur)))
			 (lambda () (env e)))))
	  (data (samples->vct beg dur)))
      (do ((i 0 (1+ i)))
	  ((= i dur))
	(vct-set! data i (+ (vct-ref data i)
			    (fm-violin v))))
      (set-samples beg dur data))))

(define test-v1
  ;; use oscil as index envelope
  (lambda (beg dur freq amp amp-env)
    (let ((v (make-fm-violin 
	      freq amp 
	      :amp-env (let ((e (make-env :envelope (or amp-env '(0 0 1 1 2 0)) 
					  :scaler amp 
					  :end dur)))
			 (lambda () (env e)))
	      :fm1-env (let ((osc (make-oscil 100.0)))
			 (lambda () (oscil osc)))))
	  (data (samples->vct beg dur)))
      (do ((i 0 (1+ i)))
	  ((= i dur))
	(vct-set! data i (+ (vct-ref data i)
			    (fm-violin v))))
      (set-samples beg dur data))))

;;; here's the original instrument using this new generator (it's tedious to repeat the args -- there must be a way around this...)
(define fm-violin-ins
  (lambda* (startime dur frequency amplitude #&key
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
	    (base 1.0)
	    (reverb-amount 0.01)
	    (degree #f) (distance 1.0) (degrees #f)
	    #&allow-other-keys)
    (let* ((beg (floor (* startime (srate))))
	   (len (floor (* dur (srate))))
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
	   (norm (or (and easy-case modulate 1.0) index1))
	   (loc (make-locsig :channels (channels) :degree (or degree degrees (random 90.0)) :reverb reverb-amount :distance distance))
	   (out-data (make-vct len))
	   (v (make-fm-violin 
	       frequency amplitude
	       :fm-index fm-index
	       :amp-env (let ((e (make-env amp-env :scaler amplitude :base base :duration dur))) (lambda () (env e)))
	       :fm1-env (and modulate (let ((e (make-env fm1-env norm :duration dur))) (lambda () (env e))))
	       :fm2-env (and modulate (not easy-case) (let ((e (make-env fm2-env index2 :duration dur))) (lambda () (env e))))
	       :fm3-env (and modulate (not easy-case) (let ((e (make-env fm3-env index3 :duration dur))) (lambda () (env e))))
	       :gliss-env (let ((e (make-env gliss-env (* glissando-amount frq-scl) :duration dur))) (lambda () (env e)))
	       :periodic-vibrato-rate periodic-vibrato-rate
	       :random-vibrato-rate random-vibrato-rate
	       :periodic-vibrato-amplitude periodic-vibrato-amplitude
	       :random-vibrato-amplitude random-vibrato-amplitude
	       :noise-amount noise-amount
	       :noise-freq noise-freq
	       :ind-noise-freq ind-noise-freq
	       :ind-noise-amount ind-noise-amount
	       :amp-noise-freq amp-noise-freq
	       :amp-noise-amount amp-noise-amount
	       :fm1-rat fm1-rat :fm2-rat fm2-rat :fm3-rat fm3-rat
	       :fm1-index fm1-index :fm2-index fm2-index :fm3-index fm3-index :base base)))
      (do ((i 0 (1+ i)))
	  ((= i len))
	(vct-set! out-data i (fm-violin v)))
      (if (= (channels) 2)
	  (let ((bsamps (vct-copy out-data)))
	    (mix-vct (vct-scale! bsamps (locsig-ref loc 1)) beg #f 1 #f)
	    (mix-vct (vct-scale! out-data (locsig-ref loc 0)) beg #f 0 #f))
	  (mix-vct out-data beg #f 0 #f)))))


			  
