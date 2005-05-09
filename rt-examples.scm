

;; Instructions:
;; 1. Start the jack sound server.
;; 2. (load-from-path "rt-compiler.scm")
;; 3. (load-from-path "rt-examples.scm")
;; 4. Browse the file and evaluate commented blocks.
;;
;; In case of stuck sounds, evaluate (rte-reset)
;; 
;; K.Matheussen 2005.




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Realtime version of bird.scm.

(load-from-path "bird.scm")

(define bigbird-org bigbird)
(definstrument (bigbird-new start dur frequency freqskew amplitude freq-envelope amp-envelope partials)
  "(bigbird start dur frequency freqskew amplitude freq-envelope amp-envelope partials)"

  (define (sum-partials lst sum)
    (if (null? lst)
	sum
	(sum-partials (cddr lst) (+ sum (cadr lst)))))
    
  (define (scale-partials lst scl newlst)
    (if (null? lst)
	newlst
	(scale-partials (cddr lst) scl (append newlst (list (car lst) (* scl (cadr lst)))))))
    
  (define (normalize-partials lst)
    (scale-partials lst (/ 1.0 (sum-partials lst 0.0)) '()))
    
  (let* ((gls-env (make-env freq-envelope (hz->radians freqskew) dur))
	 (os (make-polyshape frequency :coeffs (partials->polynomial (normalize-partials partials))))
	 (amp-env (make-env amp-envelope amplitude dur))
	 (beg (inexact->exact (round (* (mus-srate) start))))
	 (len (inexact->exact (round (* (mus-srate) dur))))
	 (end (+ beg len)))
    (ws-interrupt?)
    (rt-run start dur
	    (lambda ()
	      (out (* (env amp-env)
		      (polyshape os 1.0 (env gls-env))))))))

(define bird-org bird)
(definstrument (bird-new start dur frequency freqskew amplitude freq-envelope amp-envelope)
  "(bird start dur frequency freqskew amplitude freq-envelope amp-envelope)"
  (let* ((gls-env (make-env freq-envelope (hz->radians freqskew) dur))
	 (os (make-oscil :frequency frequency))
	 (amp-env (make-env amp-envelope amplitude dur))
	 (len (inexact->exact (round (* (mus-srate) dur))))
	 (beg (inexact->exact (round (* (mus-srate) start))))
	 (end (+ beg len)))
    (ws-interrupt?)
    (rt-run start dur
	    (lambda ()
	      (out (* (env amp-env)
		      (oscil os (env gls-env))))))))
  

(define with-sound-org with-sound)
(defmacro with-sound-new (args . body)
  `(begin
     ,@body))
  
(define (make-rt-birds)
  (set! bird bird-new)
  (set! bigbird bigbird-new)
  (set! with-sound with-sound-new)

  (make-birds)
  
  (set! with-sound with-sound-org)
  (set! bird bird-org)
  (set! bigbird bigbird-org))



#!
(make-rt-birds)
(rte-reset)
(rte-info)
!#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GUI examples

(definstrument (make-osc-gui)
  (letrec* ((osc (make-oscil #:frequency 440))
	    (vol 0.4)
	    (instrument (rt-play (lambda ()
				   (out (* vol (oscil osc))))))
	    (exit (lambda ()
		    (-> instrument stop)
		    (-> d hide)))
	    (d (<dialog> "Hard Realtime Common Lisp Music!"  exit
			 "Close" exit
			 "Stop" (<- instrument stop)
			 "Start" (<- instrument play))))
    (<slider> d "Frequency" 50 440 20000 (lambda (val) 
					   (set! (mus-frequency osc) val))
	      1)
    (<slider> d "Amplitude" 0 vol 2.0 (lambda (val) 
					(set! (-> instrument vol) val))
	      1000)
    (-> d show)))


(definstrument (make-fm-gui i)
  (letrec* ((freq i)
	    (amp 0.6)
	    (mc-ratio 0.1)
	    (index 4)
	    
	    (fm (make-oscil (* freq mc-ratio) :initial-phase (/ 3.14159 2.0)))
	    (carrier (make-oscil freq))
	    (fm_index (* (hz->radians freq) mc-ratio index))
	    
	    (instrument (rt-play (lambda ()
				   (out (* amp
					   (oscil carrier (* fm_index
							     (oscil fm))))))))
	    (exit (lambda ()
		    (-> instrument stop)
		    (-> d hide)))
	    
	    (d (<dialog> "Hard Realtime Common Lisp Music!"  exit
			 "Close" exit
				"Stop" (<- instrument stop)
				"Start" (<- instrument play))))
    (c-display "amp/fm_index" amp fm_index)
    (<slider> d "Fm Frequency" 2 (mus-frequency fm) 1200 (lambda (val) 
							   (set! (mus-frequency fm) (* mc-ratio val)))
	      10)
    (<slider> d "Carrier Frequency" 50 (mus-frequency carrier) 1200 (lambda (val) 
								      (set! (mus-frequency carrier) val))
	      10)
    (<slider> d "Index" 0 (-> instrument fm_index) 50.0 (lambda (val) 
							  (set! (-> instrument fm_index) (* (hz->radians freq) mc-ratio val)))
	      100)
    (<slider> d "Amplitude" 0 (-> instrument amp) 1.0 (lambda (val) 
							(set! (-> instrument amp) val))
	      1000)
    (-> d show)))


#!
;; gui.scm requires gtk:
(c-load-from-path gui)

(make-osc-gui)
(make-fm-gui 200)
!#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Oscilator

(definstrument (oscilator)
  (let ((osc (make-oscil))
	(vol 0.8)
	(fm 0))
    (rt-play (lambda ()
	       (out (* (oscil osc)
		       vol))))))


#!
(define i (oscilator))
(set! (-> i vol) 0.3)
(set! (mus-frequency (-> i osc)) 200)
(-> i stop)
!#




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fileplayers

(definstrument (loopplay filename)
  (let* ((rs (make-readin filename)))
    (rt-play (lambda ()
	       (if (>= (mus-location rs) (mus-length rs))
		   (set! (mus-location rs) 0))
	       (out (readin rs))))))


(definstrument (backandforth-stereo filename pan)
  (let* ((read0 (make-readin filename #:channel 0))
	 (read1 (make-readin filename #:channel 1)))
    (rt-play (lambda ()
	       (let ((readfunc (lambda (read)
				 (if (>= (mus-location read) (mus-length read))
				     (set! (mus-increment read) -1)
				     (if (<= (mus-location read) 0)
					 (set! (mus-increment read) 1)))
				 (readin read))))
		 ;; (Very stupid panner)
		 (out 0 (* (readfunc read0)
			   (if (< pan 0)
			       1
			       (- 1 pan))))
		 (out 1 (* (readfunc read1)
			   (if (> pan 0)
			       1
			       (+ 1 pan)))))))))



#!
(define filename "/home/kjetil/t1.wav")

(define p (loopplay filename))
(-> p stop)

(define p (backandforth-stereo filename 0))
(set! (-> p pan) -1)
(set! (-> p pan) 0)
(set! (-> p pan) 1)
(-> p stop)

!#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The fm violin.
;;
;; Copied from v.scm.

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

    "(fm-violin startime dur frequency amplitude #:key 
   (fm-index 1.0) (amp-env '(0 0  25 1  75 1  100 0)) 
   (periodic-vibrato-rate 5.0) (random-vibrato-rate 16.0) 
   (periodic-vibrato-amplitude 0.0025) (random-vibrato-amplitude 0.005) 
   (noise-amount 0.0) (noise-freq 1000.0) (ind-noise-freq 10.0) 
   (ind-noise-amount 0.0) (amp-noise-freq 20.0) 
   (amp-noise-amount 0.0) (gliss-env '(0 0  100 0)) 
   (glissando-amount 0.0) (fm1-env '(0 1  25 .4  75 .6  100 0)) 
   (fm2-env '(0 1  25 .4  75 .6  100 0)) (fm3-rat 4.0) 
   (fm3-env '(0 1  25 .4  75 .6  100 0)) (fm1-rat 1.0) 
   (fm2-rat 3.0) (fm1-index #f) (fm2-index #f) 
   (fm3-index #f) (degree 0) (distance 1.0) 
   (reverb-amount 0.01) (base 1.0)) 
This version of the fm-violin assumes it is running within with-sound (where *output* and *reverb* are defined).
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
			   (= fm3-rat (floor fm3-rat))
			   (integer? (inexact->exact (/ fm2-rat fm1-rat))) ; might be 2=2 but 1=3 or whatever
			   (integer? (inexact->exact (/ fm3-rat fm1-rat)))))
	   (norm (or (and easy-case modulate 1.0) index1))
	   (carrier (make-oscil frequency))
	   (fmosc1  (if modulate 
			(if easy-case 
			    (make-polyshape :frequency (* fm1-rat frequency) 
					    :coeffs (partials->polynomial (list (inexact->exact fm1-rat) index1
										(inexact->exact (floor (/ fm2-rat fm1-rat))) index2
										(inexact->exact (floor (/ fm3-rat fm1-rat))) index3)))
			    (make-oscil (* fm1-rat frequency)))
			#f))
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
	   ;;(locs (make-locsig degree distance reverb-amount *output* *reverb* (mus-channels *output*)))
	   (locs (make-locsig degree distance reverb-amount *output* *reverb* 2))
	   (vib 0.0) 
	   (modulation 0.0)
	   (fuzz 0.0)
	   (ind-fuzz 1.0)
	   (amp-fuzz 1.0))
      (ws-interrupt?)
      (if (or (not easy-case) ind-noi amp-noi (> noise-amount 0.0) (not modulate))
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
				(polyshape fmosc1 1.0 vib)))
		       (set! modulation
			     (+ (* (env indf1) (oscil fmosc1 (+ (* fm1-rat vib) fuzz)))
				(* (env indf2) (oscil fmosc2 (+ (* fm2-rat vib) fuzz)))
				(* (env indf3) (oscil fmosc3 (+ (* fm3-rat vib) fuzz)))))))
	       (locsig locs i (* (env ampf) amp-fuzz
				 (oscil carrier (+ vib (* ind-fuzz modulation))))))))
	  (rt-run startime dur
		  (lambda () 
		    (let* ((vib (+ (env frqf) (triangle-wave pervib) (rand-interp ranvib))))
		      (locsig locs (* (env ampf) 
				      (oscil carrier (+ vib (* (env indf1) 
							       (polyshape fmosc1 1.0 vib))))))))))))


#!
(c-for 0 < 60 1
       (lambda (i)
	 (c-display i)
	 (fm-violin i (max 0.5 (random 5)) (+ (* i 100) (random 840)) .1
		    :degree (random 90)
		    :fm-index (+ i 1))))
(rte-reset)
(rte-info)

(fm-violin 0 5 200 90 .1)

!#
