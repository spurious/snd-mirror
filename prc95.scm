;;; this is a (somewhat verbose and slow) translation to Snd (from CLM's prc-toolkit95.lisp)
;;;  of Perry Cook's Physical Modelling Toolkit.

(use-modules (ice-9 optargs))
(use-modules (ice-9 format))
(define pi 3.141592653589793)

;;; reedtable
(define (reed-offset reed) (list-ref reed 0))
(define (reed-slope reed) (list-ref reed 1)) 
(define* (make-reed #&key (offset 0.6) (slope -0.8)) (list offset slope))

(define (reedtable r sample) 
  (min 1.0 (+ (reed-offset r)
	      (* (reed-slope r) sample))))

;;; bowtable
(define (bowt-offset bowt) (list-ref bowt 0))
(define (bowt-slope bowt) (list-ref bowt 1))
(define* (make-bowt #&key (offset 0.0) (slope 1.0)) (list offset slope))

(define (bowtable b sample) 
  (max 0.0 (- 1.0 (abs (* (bowt-slope b)
			  (+ sample (bowt-offset b)))))))

;;;jettable
(define (jettable sample) 
  (max -1.0 (min 1.0 (* sample (- (* sample sample) 1.0)))))

;;; one-zero filter (slightly different from clm's)
(define (onez-gain onez) (list-ref onez 0))
(define (onez-zerocoeff onez) (list-ref onez 1))
(define (onez-input onez) (list-ref onez 2))
(define (onez-set-input onez val) (list-set! onez 2 val))
(define* (make-onez #&key (gain 0.5) (zerocoeff 1.0) (input 0.0)) (list gain zerocoeff input))

(define (onezero b sample)
  (let ((result (* (onez-gain b) (+ sample (* (onez-zerocoeff b) (onez-input b))))))
    (onez-set-input b sample)
    result))

;;; one-pole filter (also slightly different)
(define (onep-polecoeff onep) (list-ref onep 0))
(define (onep-gain onep) (list-ref onep 1))
(define (onep-sgain onep) (list-ref onep 2))
(define (onep-set-sgain onep val) (list-set! onep 2 val))
(define (onep-output onep) (list-ref onep 3))
(define (onep-set-output onep val) (list-set! onep 3 val))
(define* (make-onep #&key (polecoeff 0.9) (gain 1.0) (sgain 0.1) (output 0.0)) (list polecoeff gain sgain output))

(define (fixup-sgain p)
  (if (> (onep-polecoeff p) 0.0) 
      (onep-set-sgain p (* (onep-gain p) (- 1.0 (onep-polecoeff p))))
    (onep-set-sgain p (* (onep-gain p) (+ 1.0 (onep-polecoeff p))))))

(define (set-pole p aval) (list-set! p 0 aval) (fixup-sgain p))
(define (set-gain p aval) (list-set! p 1 aval) (fixup-sgain p))

(define (onepole p sample)
  (let ((result (+ (* sample (onep-sgain p)) 
		   (* (onep-polecoeff p) (onep-output p)))))
    (onep-set-output p result)
    result))
	
;;; biquadfilter (a version of clm's formant generator)
(define (biq-pc0 biq) (list-ref biq 0))
(define (biq-set-pc0 biq val) (list-set! biq 0 val))
(define (biq-pc1 biq) (list-ref biq 1))
(define (biq-set-pc1 biq val) (list-set! biq 1 val))
(define (biq-zc0 biq) (list-ref biq 2))
(define (biq-set-zc0 biq val) (list-set! biq 2 val))
(define (biq-zc1 biq) (list-ref biq 3))
(define (biq-set-zc1 biq val) (list-set! biq 3 val))
(define (biq-gain biq) (list-ref biq 4))
(define (biq-set-gain biq val) (list-set! biq 4 val))
(define (biq-out0 biq) (list-ref biq 5))
(define (biq-set-out0 biq val) (list-set! biq 5 val))
(define (biq-out1 biq) (list-ref biq 6))
(define (biq-set-out1 biq val) (list-set! biq 6 val))
(define (biq-in0 biq) (list-ref biq 7))
(define (biq-set-in0 biq val) (list-set! biq 7 val))
(define (biq-in1 biq) (list-ref biq 8))
(define (biq-set-in1 biq val) (list-set! biq 8 val))
(define (make-biq) (list 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0))

(define (biquad b sample)
  (let ((temp (+ (* (biq-zc0 b) (biq-in0 b))
		 (* (biq-zc1 b) (biq-in1 b)))))
    (biq-set-in1 b (biq-in0 b))
    (biq-set-in0 b (* (biq-gain b) sample))
    (set! temp (+ temp
		  (+ (biq-in0 b)
		     (* (biq-pc0 b) (biq-out0 b))
		     (* (biq-pc1 b) (biq-out1 b)))))
    (biq-set-out1 b (biq-out0 b))
    (biq-set-out0 b temp)
    temp))

;;;lipfilter (a biquad filter)
(define (lip-set-freq b freq) 
  (biq-set-pc0 b (* 2.0 0.999 (cos (/ (* pi 2 freq) (srate)))))
  (biq-set-pc1 b (* -0.999 0.999))
  (biq-set-gain b 0.02))

(define (lip b mouthsample boresample)
  (let ((temp (biquad b (- mouthsample boresample))))
    (set! temp (min 1.0 (* temp temp)))
    (+ (* temp mouthsample) (* (- 1.0 temp) boresample))))


(define (dcb-input dcb) (list-ref dcb 0))
(define (dcb-set-input dcb val) (list-set! dcb 0 val))
(define (dcb-output dcb) (list-ref dcb 1))
(define (dcb-set-output dcb val) (list-set! dcb 1 val))
(define* (make-dcb #&key (input 0.0) (output 0.0)) (list input output))

(define (dcblock b sample) 
  (let ((result (+ sample (- (* 0.99 (dcb-output b)) (dcb-input b)))))
    (dcb-set-output b result)
    (dcb-set-input b sample)
    result))
;;; in the sndtools program sndblockdc, the 0.99 is replaced by (- 1.0 (/ 7.0 adaption_time))

;;;delaylinea
(define (dla-inpoint d) (list-ref d 0))
(define (dla-set-inpoint d val) (list-set! d 0 val))
(define (dla-outpoint d) (list-ref d 1))
(define (dla-set-outpoint d val) (list-set! d 1 val))
(define (dla-lastin d) (list-ref d 2))
(define (dla-set-lastin d val) (list-set! d 2 val))
(define (dla-length d) (list-ref d 3))
(define (dla-set-length d val) (list-set! d 3 val))
(define (dla-output d) (list-ref d 4))
(define (dla-set-output d val) (list-set! d 4 val))
(define (dla-input d) (list-ref d 5))
(define (dla-set-input d i val) (vct-set! (list-ref d 5) i val))
(define (dla-alpha d) (list-ref d 6))
(define (dla-set-alpha d val) (list-set! d 6 val))
(define (dla-coeff d) (list-ref d 7))
(define (dla-set-coeff d val) (list-set! d 7 val))
(define* (make-dla #&key inpoint outpoint (lastin 0.0) length (output 0.0) input alpha coeff)
  (list inpoint outpoint lastin length output input alpha coeff))

(define (set-delaya d lag)
  (let ((outpointer (+ (dla-inpoint d) (- 2.0 lag))))
    (if (< outpointer 0.0)
	(do ()
	    ((>= outpointer 0.0))
	  (set! outpointer (+ outpointer (dla-length d)))))
    (dla-set-outpoint d (inexact->exact (floor outpointer)))
    (dla-set-alpha d (- outpointer (dla-outpoint d)))
    (dla-set-coeff d (/ (- 1.0 (dla-alpha d)) (+ 1.0 (dla-alpha d))))))

(define (make-delaya len) 
  (let ((nd (make-dla :length len :input (make-vct len) :inpoint 0 :outpoint 0)))
    (set-delaya nd (* 0.5 len))
    nd))

(define (delaya d sample)
  (let ((temp 0.0))
    (dla-set-input d (dla-inpoint d) sample)
    (dla-set-inpoint d (+ 1 (dla-inpoint d)))
    (if (= (dla-inpoint d) (dla-length d)) 
	(dla-set-inpoint d 0))
    (set! temp (vct-ref (dla-input d) (dla-outpoint d)))
    (dla-set-outpoint d (+ 1 (dla-outpoint d)))
    (if (>= (dla-outpoint d) (dla-length d)) 
	(dla-set-outpoint d (- (dla-outpoint d) (dla-length d))))
    (dla-set-output d (+ (* (- (dla-coeff d)) (dla-output d)) 
			 (dla-lastin d) 
			 (* temp (dla-coeff d))))
    (dla-set-lastin d temp)
    (dla-output d)))

;;; delaylinel
(define (dll-inpoint d) (list-ref d 0))
(define (dll-set-inpoint d val) (list-set! d 0 val))
(define (dll-outpoint d) (list-ref d 1))
(define (dll-set-outpoint d val) (list-set! d 1 val))
(define (dll-length d) (list-ref d 2))
(define (dll-set-length d val) (list-set! d 2 val))
(define (dll-output d) (list-ref d 3))
(define (dll-set-output d val) (list-set! d 3 val))
(define (dll-input d) (list-ref d 4))
(define (dll-set-input d i val) (vct-set! (list-ref d 4) i val))
(define (dll-alpha d) (list-ref d 5))
(define (dll-set-alpha d val) (list-set! d 5 val))
(define (dll-omalpha d) (list-ref d 6))
(define (dll-set-omalpha d val) (list-set! d 6 val))
(define* (make-dll #&key inpoint outpoint length (output 0.0) input alpha omalpha)
  (list inpoint outpoint length output input alpha omalpha))

(define (make-delayl len) 
  (let ((nd (make-dll :length len :input (make-vct len) :inpoint 0 :outpoint 0)))
    (set-delayl nd (* 0.5 len))
    nd))

(define (set-delayl d lag)
  (let ((outpointer (+ (dll-inpoint d) (- 1 lag))))
    (if (< outpointer 0.0)
	(do ()
	    ((>= outpointer 0.0))
	  (set! outpointer (+ outpointer (dll-length d)))))
    (dll-set-outpoint d (inexact->exact (floor outpointer)))
    (dll-set-alpha d (- outpointer (dll-outpoint d)))
    (dll-set-omalpha d (- 1.0 (dll-alpha d)))))

(define (delayl d sample)
  (dll-set-input d (dll-inpoint d) sample)
  (dll-set-inpoint d (+ 1 (dll-inpoint d)))
  (if (= (dll-inpoint d) (dll-length d)) 
      (dll-set-inpoint d 0))
  (dll-set-output d (* (vct-ref (dll-input d) (dll-outpoint d)) (dll-omalpha d)))
  (dll-set-outpoint d (+ 1 (dll-outpoint d)))
  (if (= (dll-outpoint d) (dll-length d)) 
      (dll-set-outpoint d 0))
  (dll-set-output d (+ (dll-output d) 
		       (* (vct-ref (dll-input d) (dll-outpoint d)) 
			  (dll-alpha d))))
  (dll-output d))


;;; now some example instruments

(define (plucky beg dur freq amplitude maxa)
  (let* ((lowestfreq 100.0)
	 (len (1+ (inexact->exact (floor (/ (srate) lowestfreq)))))
	 (delayline (make-delaya len))
	 (filter (make-onez))
	 (st (inexact->exact (floor (* (srate) beg))))
	 (durlen (inexact->exact (floor (* (srate) dur))))
	 (out-data (make-vct durlen)))
    (set-delaya delayline (- (/ (srate) freq) 0.5))
    (do ((i 0 (1+ i)))
	((= i len))
      (delaya delayline (+ (* 0.99 (dla-output delayline)) 
			   (* maxa (- 1.0 (random 2.0))))))
    (vct-map! out-data
	      (lambda ()
		(* amplitude 
		   (delaya delayline 
			   (onezero filter (dla-output delayline))))))
    (mix-vct out-data st #f 0 #f)))


;;; freq is off in this one (in prc's original also)
(define (bow beg dur frq amplitude maxa)
  (let* ((lowestfreq 100.0)
	 (len (1+ (inexact->exact (floor (/ (srate) lowestfreq)))))
	 (neckdelay (make-delayl len))
	 (bridgedelay (make-delayl (inexact->exact (floor (/ len 2)))))
	 (bowtab (make-bowt :slope 3.0))
	 (filt (make-onep))
	 (rate .001)
	 (bowing #t)
	 (bowvelocity rate)
	 (maxvelocity maxa)
	 (attackrate rate)
	 (st (inexact->exact (floor (* (srate) beg))))
	 (durlen (inexact->exact (floor (* (srate) dur))))
	 (out-data (make-vct durlen))
	 (ctr 0)
	 (release (inexact->exact (floor (* .8 durlen)))))
    (set-pole filt 0.6)
    (set-gain filt 0.3)
    (let ((ratio 0.8317)
	  (temp (- (/ (srate) frq) 4.0)))
      (set-delayl neckdelay (* temp ratio))
      (set-delayl bridgedelay (* temp (- 1.0 ratio))))
    (vct-map! out-data
	      (lambda ()
		(let* ((bridgerefl 0.0)
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
		  (set! bridgerefl (- (onepole filt (dll-output bridgedelay))))
		  (set! nutrefl (- (dll-output neckdelay)))
		  (set! stringvel (+ bridgerefl nutrefl))
		  (set! veldiff (- bowtemp stringvel))
		  (set! veldiff (* veldiff (bowtable bowtab veldiff)))
		  (delayl neckdelay (+ bridgerefl veldiff))
		  (delayl bridgedelay (+ nutrefl veldiff))
		  (let ((result (* amplitude 10.0 (onep-output filt))))
		    (if (= ctr release)
			(begin
			  (set! bowing #f)
			  (set! attackrate .0005)))
		    (set! ctr (+ ctr 1))
		    result))))
    (mix-vct out-data st #f 0 #f)))


(define (brass beg dur freq amplitude maxa)
  (let* ((lowestfreq 100.0)
	 (len (1+ (inexact->exact (floor (/ (srate) lowestfreq)))))
	 (delayline (make-delaya len))
	 (lipfilter (make-biq))
	 (dcblocker (make-dcb))
	 (blowing #t)
	 (rate .001)
	 (breathpressure 0.0)  ; 0.1 ?
	 (maxpressure maxa)
	 (attackrate rate)
	 (st (inexact->exact (floor (* (srate) beg))))
	 (durlen (inexact->exact (floor (* (srate) dur))))
	 (out-data (make-vct durlen))
	 (release (inexact->exact (floor (* .8 durlen))))
	 (ctr 0))
    (set-delaya delayline (+ 1.0 (/ (srate) freq)))
    (lip-set-freq lipfilter freq)
    (vct-map! out-data
	      (lambda ()
		(if blowing
		    (if (not (= maxpressure breathpressure))
			(if (< breathpressure maxpressure)
			    (set! breathpressure (+ breathpressure attackrate))
			    (set! breathpressure (- breathpressure attackrate))))
		    (if (> breathpressure 0.0)
			(set! breathpressure (- breathpressure attackrate))))
		(let ((result (* amplitude (delaya delayline
						   (dcblock dcblocker
							    (lip lipfilter
								 (* 0.3 breathpressure)
								 (* 0.9 (dla-output delayline))))))))
		  (if (= ctr release) 
		      (begin
			(set! blowing #f)
			(set! attackrate .0005)))
		  (set! ctr (+ ctr 1))
		  result)))
    (mix-vct out-data st #f 0 #f)))


(define (clarinet beg dur freq amplitude maxa)
  (let* ((lowestfreq 100.0)
	 (len (1+ (inexact->exact (floor (/ (srate) lowestfreq)))))
	 (delayline (make-delayl len))
	 (rtable (make-reed :offset 0.7 :slope -0.3))
	 (filter (make-onez))
	 (blowing #t)
	 (breathpressure 0.0) ; 0.1 ?
	 (rate .001)
	 (maxpressure maxa)
	 (attackrate rate)
	 (st (inexact->exact (floor (* (srate) beg))))
	 (durlen (inexact->exact (floor (* (srate) dur))))
	 (out-data (make-vct durlen))
	 (ctr 0)
	 (release (inexact->exact (floor (* .8 durlen)))))
    (set-delayl delayline (- (* 0.5 (/ (srate) freq)) 1.0))
    (vct-map! out-data
	      (lambda ()
		(let ((pressurediff 0.0))
		  (if blowing
		      (if (not (= maxpressure breathpressure))
			  (if (< breathpressure maxpressure)
			      (set! breathpressure (+ breathpressure attackrate))
			      (set! breathpressure (- breathpressure attackrate))))
		      (if (> breathpressure 0.0)
			  (set! breathpressure (- breathpressure attackrate))))
		  (set! pressurediff (- (onezero filter (* -0.95 (dll-output delayline))) breathpressure))
		  (let ((result (* amplitude 
				   (delayl delayline 
					   (+ breathpressure 
					      (* pressurediff 
						 (reedtable rtable pressurediff)))))))
		    (if (= ctr release)
			(begin
			  (set! blowing #f)
			  (set! attackrate .0005)))
		    (set! ctr (+ ctr 1))
		    result))))
    (mix-vct out-data st #f 0 #f)))


(define (flute beg dur freq amplitude maxa)
  (let* ((lowestfreq 100.0)
	 (len (1+ (inexact->exact (floor (/ (srate) lowestfreq)))))
	 (jetdelay (make-delayl (inexact->exact (floor (/ len 2)))))
	 (boredelay (make-delayl len))
	 (filter (make-onep))
	 (dcblocker (make-dcb))
	 (jetrefl 0.6)
	 (endrefl 0.6)
	 (sinphase 0.0)
	 (blowing #t)
	 (rate .0005)
	 (breathpressure 0.0) ; 0.1 ?
	 (maxpressure maxa)
	 (attackrate rate)
	 (st (inexact->exact (floor (* (srate) beg))))
	 (durlen (inexact->exact (floor (* (srate) dur))))
	 (out-data (make-vct durlen))
	 (ctr 0)
	 (release (inexact->exact (floor (* .8 durlen)))))
    (set-pole filter 0.8)
    (set-gain filter -1.0)
    (let ((ratio 0.8)
	  (temp (- (/ (srate) freq) 5.0)))
      (set-delayl boredelay (* ratio temp))
      (set-delayl jetdelay (* temp (- 1.0 ratio))))
    (vct-map! out-data
	      (lambda ()
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
		  (set! temp (dcblock dcblocker (onepole filter (dll-output boredelay))))
		  (set! pressurediff (+ (jettable 
					 (delayl jetdelay 
						 (+ breathpressure 
						    (- randpressure (* jetrefl temp))))) 
					(* endrefl temp)))
		  (let ((result (* 0.3 amplitude (delayl boredelay pressurediff))))
		    (if (= ctr release)
			(begin
			  (set! blowing #f)
			  (set! attackrate .0005)))
		    (set! ctr (+ ctr 1))
		    result))))
    (mix-vct out-data st #f 0 #f)))



(define (test-prc95)
  (plucky 0 .3 440 .2 1.0)
  (bow .5 .3 220 .2 1.0)
  (brass 1 .3 440 .2 1.0)
  (clarinet 1.5 .3 440 .2 1.0)
  (flute 2 .3 440 .2 1.0))

