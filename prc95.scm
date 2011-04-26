;;; this is a translation to Snd (from CLM's prc-toolkit95.lisp)
;;;  of Perry Cook's Physical Modelling Toolkit.

(provide 'snd-prc95.scm)
(if (not (provided? 'snd-ws.scm)) (load "ws.scm"))


(define* (make-reed (offset 0.6) (slope -0.8))
  (vct offset slope))

(define (reedtable r sample)
  (min 1.0 (+ (r 0) (* (r 1) sample))))

(define* (make-bowtable (offset 0.0) (slope 1.0))
  (vct offset slope))

(define (bowtable b sample)
  (max 0.0 (- 1.0 (abs (* (b 1) (+ sample (b 0)))))))

(define (jettable sample) 
  (max -1.0 (min 1.0 (* sample (- (* sample sample) 1.0)))))

(define* (make-onezero (gain 0.5) (zerocoeff 1.0))
  (make-one-zero gain (* gain zerocoeff)))

(define* (make-onep (polecoeff 0.9))
  (make-one-pole (- 1.0 polecoeff) (- polecoeff)))

(define (set-pole p val) 
  (set! (mus-ycoeff p 1) (- val))
  (set! (mus-xcoeff p 0) (- 1.0 val)))

(define (set-gain p val)
  (set! (mus-xcoeff p 0) (* (mus-xcoeff p 0) val)))


(define (lip-set-freq b freq)
  (set! (mus-frequency b) freq))

(define (lip b mouthsample boresample)
  (let ((temp (formant b (- mouthsample boresample))))
    (set! temp (min 1.0 (* temp temp)))
    (+ (* temp mouthsample) (* (- 1.0 temp) boresample))))


(define (make-dc-block)
  (vct 0.0 0.0))

(define (dc-block b sample)
  (set! (b 1) (+ sample (- (* 0.99 (b 1)) (b 0))))
  (set! (b 0) sample)
  (b 1))
;; we could also use a filter generator here: (make-filter 2 (vct 1 -1) (vct 0 -0.99))


;;; these are 0-based versions of the clm delays
(defgenerator dlya (outp 0 :type float) (input #f :type clm))

(define (make-delaya len lag) 
  (make-dlya :input (make-delay len :type mus-interp-all-pass :max-size (ceiling (+ len lag 1)))
	     :outp (- lag (+ len 1))))

(define (delaya d sample)
  (delay-tick (dlya-input d) sample)
  (tap (dlya-input d) (dlya-outp d)))

(define (make-delayl len lag)
  ;; Perry's original had linear interp bug, I think -- this form is more in tune
  (make-dlya :input (make-delay len :max-size (ceiling (+ len lag 1)))
	     :outp (- lag len)))

(define (delayl d sample)
  (delay-tick (dlya-input d) sample)
  (tap (dlya-input d) (dlya-outp d)))



;;; now some example instruments

(definstrument (plucky beg dur freq amplitude maxa)
  (let* ((lowestfreq 100.0)
	 (len (+ 1 (floor (/ (mus-srate) lowestfreq))))
	 (delayline (make-delaya len (- (/ (mus-srate) freq) 0.5)))
	 (filter (make-onezero))
	 (start (seconds->samples beg))
	 (durlen (seconds->samples dur))
	 (end (+ start durlen))
	 (dout 0.0))
    (do ((i 0 (+ 1 i)))
	((= i len))
      (set! dout (delaya delayline (+ (* 0.99 dout)
				      (* maxa (- 1.0 (random 2.0)))))))
    (run
     (do ((i start (+ 1 i)))
	 ((= i end))
       (set! dout (delaya delayline (one-zero filter dout)))
       (outa i (* amplitude dout))))))


;;; freq is off in this one (in prc's original also)
(definstrument (bowstr beg dur frq amplitude maxa)
  (let* ((lowestfreq 100.0)
	 (len (+ 1 (floor (/ (mus-srate) lowestfreq))))
	 (ratio 0.8317)
	 (temp (- (/ (mus-srate) frq) 4.0))
	 (neckdelay (make-delayl len (* temp ratio)))
	 (bridgedelay (make-delayl (floor (/ len 2)) (* temp (- 1.0 ratio))))
	 (bowtab (make-bowtable :slope 3.0))
	 (filt (make-onep))
	 (rate .001)
	 (bowing #t)
	 (bowvelocity rate)
	 (maxvelocity maxa)
	 (attackrate rate)
	 (st (seconds->samples beg))
	 (durlen (seconds->samples dur))
	 (end (+ st durlen))
	 (ctr 0)
	 (release (floor (* .8 durlen)))
	 (bridgeout 0.0)
	 (neckout 0.0))
    (set-pole filt 0.6)
    (set-gain filt 0.3)
    (run
     (do ((i st (+ 1 i)))
	 ((= i end))
       (let ((bridgerefl 0.0)
	     (nutrefl 0.0) 
	     (veldiff 0.0) 
	     (stringvel 0.0) 
	     (bowtemp 0.0))
	 (if bowing
	     (if (not (= maxvelocity bowvelocity))
		 (if (< bowvelocity maxvelocity)
		     (set! bowvelocity (+ bowvelocity attackrate))
		     (set! bowvelocity (- bowvelocity attackrate))))
	     (if (> bowvelocity 0.0) 
		 (set! bowvelocity (- bowvelocity attackrate))))
	 (set! bowtemp (* 0.3 bowvelocity))
	 (let ((filt-output (one-pole filt bridgeout)))
	   (set! bridgerefl (- filt-output))
	   (set! nutrefl (- neckout))
	   (set! stringvel (+ bridgerefl nutrefl))
	   (set! veldiff (- bowtemp stringvel))
	   (set! veldiff (* veldiff (bowtable bowtab veldiff)))
	   (set! neckout (delayl neckdelay (+ bridgerefl veldiff)))
	   (set! bridgeout (delayl bridgedelay (+ nutrefl veldiff)))
	   (let ((result (* amplitude 10.0 filt-output)))
	     (if (= ctr release)
		 (begin
		   (set! bowing #f)
		   (set! attackrate .0005)))
	     (set! ctr (+ ctr 1))
	     (outa i result))))))))


(definstrument (brass beg dur freq amplitude maxa)
  (let* ((lowestfreq 100.0)
	 (len (+ 1 (floor (/ (mus-srate) lowestfreq))))
	 (delayline (make-delaya len (+ 1.0 (/ (mus-srate) freq))))
	 (lipfilter (make-formant freq))
	 (dcblocker (make-dc-block))
	 (blowing #t)
	 (rate .001)
	 (breathpressure 0.0)  ; 0.1 ?
	 (maxpressure maxa)
	 (attackrate rate)
	 (st (seconds->samples beg))
	 (durlen (seconds->samples dur))
	 (end (+ st durlen))
	 (release (floor (* .8 durlen)))
	 (ctr 0)
	 (dout 0.0))
    (run
     (do ((i st (+ 1 i)))
	 ((= i end))
       (if blowing
	   (if (not (= maxpressure breathpressure))
	       (if (< breathpressure maxpressure)
		   (set! breathpressure (+ breathpressure attackrate))
		   (set! breathpressure (- breathpressure attackrate))))
	   (if (> breathpressure 0.0)
	       (set! breathpressure (- breathpressure attackrate))))
       (set! dout (delaya delayline (dc-block dcblocker
					      (lip lipfilter
						   (* 0.3 breathpressure)
						   (* 0.9 dout)))))
       (let ((result (* amplitude dout)))
	 (if (= ctr release) 
	     (begin
	       (set! blowing #f)
	       (set! attackrate .0005)))
	 (set! ctr (+ ctr 1))
	 (outa i result))))))


(definstrument (clarinet beg dur freq amplitude maxa)
  (let* ((lowestfreq 100.0)
	 (len (+ 1 (floor (/ (mus-srate) lowestfreq))))
	 (delayline (make-delayl len (- (* 0.5 (/ (mus-srate) freq)) 1.0)))
	 (rtable (make-reed :offset 0.7 :slope -0.3))
	 (filter (make-onezero))
	 (blowing #t)
	 (breathpressure 0.0) ; 0.1 ?
	 (rate .001)
	 (maxpressure maxa)
	 (attackrate rate)
	 (st (seconds->samples beg))
	 (durlen (seconds->samples dur))
	 (end (+ st durlen))
	 (ctr 0)
	 (release (floor (* .8 durlen)))
	 (dlyout 0.0))
    (run
     (do ((i st (+ 1 i)))
	 ((= i end))
       (let ((pressurediff 0.0))
	 (if blowing
	     (if (not (= maxpressure breathpressure))
		 (if (< breathpressure maxpressure)
		     (set! breathpressure (+ breathpressure attackrate))
		     (set! breathpressure (- breathpressure attackrate))))
	     (if (> breathpressure 0.0)
		 (set! breathpressure (- breathpressure attackrate))))
	 (set! pressurediff (- (one-zero filter (* -0.95 dlyout)) breathpressure))
	 (set! dlyout (delayl delayline 
			      (+ breathpressure 
				 (* pressurediff 
				    (reedtable rtable pressurediff)))))
	 (let ((result (* amplitude dlyout)))
	   (if (= ctr release)
	       (begin
		 (set! blowing #f)
		 (set! attackrate .0005)))
	   (set! ctr (+ ctr 1))
	   (outa i result)))))))


(definstrument (flute beg dur freq amplitude maxa)
  (let* ((lowestfreq 100.0)
	 (len (+ 1 (floor (/ (mus-srate) lowestfreq))))
	 (ratio 0.8)
	 (temp (- (/ (mus-srate) freq) 5.0))
	 (jetdelay (make-delayl (floor (/ len 2)) (* temp (- 1.0 ratio))))
	 (boredelay (make-delayl len (* ratio temp)))
	 (filter (make-onep))
	 (dcblocker (make-dc-block))
	 (jetrefl 0.6)
	 (endrefl 0.6)
	 (sinphase 0.0)
	 (blowing #t)
	 (rate .0005)
	 (breathpressure 0.0) ; 0.1 ?
	 (maxpressure maxa)
	 (attackrate rate)
	 (st (seconds->samples beg))
	 (durlen (seconds->samples dur))
	 (end (+ st durlen))
	 (ctr 0)
	 (release (floor (* .8 durlen)))
	 (boreout 0.0))
    (set-pole filter 0.8)
    (set-gain filter -1.0)
    (run
     (do ((i st (+ 1 i)))
	 ((= i end))
       (let ((randpressure (* 0.1 breathpressure (random 1.0)))
	     (temp 0.0) 
	     (pressurediff 0.0))
	 (set! sinphase (+ sinphase 0.0007))		;5 hz vibrato?
	 (if (> sinphase 6.28) (set! sinphase (- sinphase 6.28)))
	 (set! randpressure (+ randpressure (* 0.05 breathpressure (sin sinphase))))
	 (if blowing
	     (if (not (= maxpressure breathpressure))
		 (if (< breathpressure maxpressure)
		     (set! breathpressure (+ breathpressure attackrate))
		     (set! breathpressure (- breathpressure attackrate))))
	     (if (> breathpressure 0.0) 
		 (set! breathpressure (- breathpressure attackrate))))
	 (set! temp (dc-block dcblocker (one-pole filter boreout)))
	 (set! pressurediff (+ (jettable 
				(delayl jetdelay 
					(+ breathpressure 
					   (- randpressure (* jetrefl temp))))) 
			       (* endrefl temp)))
	 (set! boreout (delayl boredelay pressurediff))
	 (let ((result (* 0.3 amplitude boreout)))
	   (if (= ctr release)
	       (begin
		 (set! blowing #f)
		 (set! attackrate .0005)))
	   (set! ctr (+ ctr 1))
	   (outa i result)))))))

#|
(with-sound ()
	    (plucky 0 .3 440 .2 1.0)
	    (bowstr .5 .3 220 .2 1.0)
	    (brass 1 .3 440 .2 1.0)
	    (clarinet 1.5 .3 440 .2 1.0)
	    (flute 2 .3 440 .2 1.0))
|#

