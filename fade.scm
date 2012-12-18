;;; cross fade instruments
;;;
;;; cross-fade sweeps up, down, or from mid-spectrum outwards,
;;; dissolve-fade chooses randomly -- like a graphical dissolve
;;; neither is exactly spectacular, but they work -- use similar sounds if possible (speech is problematic)
;;;
;;; translated from fade.ins

(definstrument (cross-fade beg dur amp file1 file2 ramp-beg ramp-dur ramp-type bank-dur fs fwidth)
  ;; ramp-type 0=sweep up, 1=sweep down, 2=split from middle
  (let ((fil1 (make-sampler 0 file1))
	(fil2 (make-sampler 0 file2))
	(start (seconds->samples beg))
	(ramp-samps (seconds->samples ramp-dur))
	(bank-samps (seconds->samples bank-dur))
	(fs1 (make-vector fs)))

    (let ((bin (/ (mus-srate) (* 2 fs)))
	  (radius (- 1.0 (/ fwidth (* 2 fs)))))
      (do ((k 0 (+ k 1)))
	  ((= k fs))
	(set! (fs1 k) (make-formant (* k bin) radius))))
	  
    (let ((end (+ start (seconds->samples dur)))
	  (bank-incr (/ 1.0 bank-samps))
	  (ramp-incr (/ 1.0 ramp-samps))
	  (ramp-start (+ start (seconds->samples ramp-beg))))
      (let ((bank1-start (- ramp-start bank-samps))
	    (ramp-end (+ ramp-start ramp-samps))
	    (bank2-start (+ ramp-start ramp-samps)))

	(do ((i start (+ i 1)))
	    ((= i bank1-start))
	  ;; in first section -- just mix in file1
	  (outa i (* amp (read-sample fil1))))

	(let ((bank2-end (+ bank2-start bank-samps))
	      (ramp 0.0)
	      (bank1 0.0)
	      (bank2 1.0)
	      (outval 0.0)
	      (inputs (make-vct fs 0.0))
	      (amps (make-vct fs 1.0))
	      (ifs (/ 1.0 fs))
	      (mid 0))

	  (do ((i bank1-start (+ i 1)))
	      ((= i bank2-end))
	    (if (< i ramp-start)
		;; in bank1 section -- fire up the resonators
		(let ((inval (read-sample fil1)))
		  (set! bank1 (+ bank1 bank-incr))
		  (set! outval (formant-bank amps fs1 inval))
		  (outa i (* amp (+ (* bank1 outval) (* (- 1.0 bank1) inval)))))
		
		(if (> i ramp-end)
		    ;; in bank2 section -- ramp out resonators
		    (let ((inval (read-sample fil2)))
		      (set! bank2 (- bank2 bank-incr))
		      (set! outval (formant-bank amps fs1 inval))
		      (outa i (* amp (+ (* bank2 outval) (* (- 1.0 bank2) inval)))))
		    
		    ;; in the fade section
		    (let ((inval1 (read-sample fil1))
			  (inval2 (read-sample fil2)))
		      ;; now the choice of spectral fade -- we should end with all bank1 0.0 and all bank2 1.0
		      (set! ramp (+ ramp ramp-incr))
		      
		      (case ramp-type
			((0)
			 ;; low freqs go first
			 (if (>= ramp 0.5)
			     (begin
			       (set! mid (min fs (floor (/ (- (* 2.0 ramp) 1.0) ifs))))
			       (do ((k 0 (+ k 1)))
				   ((= k mid))
				 (set! (inputs k) inval2))
			       (do ((k mid (+ k 1))
				    (ks 1.0 (- ks ifs)))
				   ((= k fs))
				 (set! (inputs k) (+ (* ks inval2) (* (- 1.0 ks) inval1)))))
			     (begin
			       (set! mid (min fs (floor (/ (* 2.0 ramp) ifs))))
			       (do ((k 0 (+ k 1))
				    (ks (* 2.0 ramp) (- ks ifs)))
				   ((= k mid))
				 (set! (inputs k) (+ (* ks inval2) (* (- 1.0 ks) inval1))))
			       (do ((k mid (+ k 1)))
				   ((= k fs))
				 (set! (inputs k) inval1))))
			 (outa i (* amp (formant-bank amps fs1 inputs))))
			
			((1)
			 ;; high freqs go first
			 (if (>= ramp 0.5)
			     (let ((r2 (- (* 2.0 ramp) 1.0)))
			       (set! mid (min fs (ceiling (/ (* 2.0 (- 1.0 ramp)) ifs))))
			       (do ((k 0 (+ k 1))
				    (ks r2 (+ ks ifs)))
				   ((= k mid))
				 (set! (inputs k) (+ (* ks inval2) (* (- 1.0 ks) inval1))))
			       (do ((k mid (+ k 1)))
				   ((= k fs))
				 (set! (inputs k) inval2)))
			     (begin
			       (set! mid (min fs (ceiling (/ (- 1.0 (* 2 ramp)) ifs))))
			       (do ((k 0 (+ k 1)))
				   ((= k mid))
				 (set! (inputs k) inval1))
			       (do ((k mid (+ k 1))
				    (ks 0.0 (+ ks ifs)))
				   ((= k fs))
				 (set! (inputs k) (+ (* ks inval2) (* (- 1.0 ks) inval1))))))
			 (outa i (* amp (formant-bank amps fs1 inputs))))
			
			;; sweep from midpoint out
			(else
			 (let ((r2 (* 2 ramp))
			       (half-fs (/ fs 2)))
			   (do ((k 0 (+ k 1)))
			       ((= k half-fs))
			     (let ((rfs (max 0.0 (min 1.0 (- (+ r2 0.5) (* (- fs k) ifs))))))
			       (set! (inputs (+ k 1)) (+ (* rfs inval2) (* (- 1.0 rfs) inval1)))))
			   (do ((k 0 (+ k 1)))
			       ((= k (- half-fs 1)))
			     (let ((rfs (max 0.0 (min 1.0 (- r2 (/ k half-fs))))))
			       (set! (inputs (+ k 1 half-fs)) (+ (* rfs inval2) (* (- 1.0 rfs) inval1)))))
			   (outa i (* amp (formant-bank amps fs1 inputs))))))))))

	  (do ((i bank2-end (+ i 1)))
	      ((= i end))
	    ;; in last section -- just mix file2
	    (outa i (* amp (read-sample fil2))))
	  )))))



;;; (vct->channel (with-sound (:output (make-vct 22050)) (cross-fade 0 .1 1 0 1 .01 .01 0 .1 256 2)))
;;; (vct->channel (with-sound (:output (make-vct 44100)) (cross-fade 0 2 1.0 "oboe.snd" "trumpet.snd" 0.5 1.0 0 .1 256 2)))
;;; (with-sound (:statistics #t) (cross-fade 0 2 1.0 "oboe.snd" "trumpet.snd" 0.5 1.0 0 .1 256 2))
;;; (with-sound () (cross-fade 0 2 1.0 "oboe.snd" "trumpet.snd" 0.5 1.0 0 .1 256 2))
;;; these fades seem more successful to me when done relatively quickly (the opposite of the dissolve below
;;; which is best if done as slowly as possible).  I like the sweep up best -- a sort of "evaporation" effect.


(definstrument (dissolve-fade beg dur amp file1 file2 fsize r lo hi)
  (let ((fil1 (make-sampler 0 file1))
	(fil2 (make-sampler 0 file2))
	(start (seconds->samples beg))
	(freq-inc (floor (/ fsize 2)))
	(ramp-inc (/ 1.0 1024.0)))
    (let ((end (+ start (seconds->samples dur)))
	  (spectr (make-vector freq-inc 1.0))
	  (trigger (floor (/ (* dur (mus-srate)) freq-inc)))
	  (fs (make-vector freq-inc #f))
	  (ramping (make-vector freq-inc #f))
	  (ctr 0)
	  (amps (make-vct freq-inc amp))
	  (inputs (make-vct freq-inc 0.0)))
    
      (if (not (number? hi)) (set! hi freq-inc))
      (let ((bin (floor (/ (mus-srate) fsize)))
	    (radius (- 1.0 (/ r fsize)))) 
	(do ((k lo (+ k 1)))
	    ((= k hi))
	  (set! (fs k) (make-formant (* k bin) radius))))
      
      (do ((i start (+ i 1)))
	  ((= i end))
	;; once a ramp is set in motion, it takes care of itself -- we need only choose which to trigger
	(set! ctr (+ 1 ctr))
	(if (> ctr trigger)
	    (let ((next (floor (random freq-inc))))
	      ;; find next randomly chosen resonator to flip
	      (if (not (= (spectr next) 1.0))
		  (call-with-exit
		   (lambda (bbreak)
		     (do ((j next (+ 1 j))
			  (k next (- k 1)))
			 (#t)
		       (if (and (< j freq-inc) 
				(= (spectr j) 1.0))
			   (begin 
			     (set! next j)
			     (bbreak)))
		       (if (and (>= k 0) 
				(= (spectr k) 1.0))
			   (begin 
			     (set! next k)
			     (bbreak)))))))
	      ;(set! (spectr next) (- (spectr next) ramp-inc))
	      (set! (ramping next) #t)
	      (set! ctr 0)))
	
	(let ((inval1 (read-sample fil1))
	      (inval2 (read-sample fil2))
	      (sp 0.0))
	  (do ((k lo (+ k 1)))
	      ((= k hi))
	    (set! sp (vector-ref spectr k))
	    (set! (inputs k) (+ (* sp inval1) (* (- 1.0 sp) inval2)))
	    (if (vector-ref ramping k)
		(if (positive? sp)
		    (vector-set! spectr k (- sp ramp-inc))
		    (set! (ramping k) #f))))
	  
	  (outa i (formant-bank amps fs inputs)))))))


;;; (with-sound () (dissolve-fade 0 1 1.0 "oboe.snd" "trumpet.snd" 256 2 0 128))
;;; (vct->channel (with-sound (:output (make-vct 44100)) (dissolve-fade 0 2 1 0 1 4096 2 2 #f)))
;;;
;;; another neat effect here is to simply let the random changes float along with no
;;; direction -- if the hit is 1.0 send it toward 0.0 and vice versa -- strange
;;; pitches emerge from noises etc



#|
;;; make it easy to see and hear:

(with-sound (:output "p1.snd") 
  (let ((g (make-ncos 200 100)))
    (do ((i 0 (+ i 1)))
	((= i 100000))
      (outa i (ncos g)))))

(with-sound (:output "p2.snd") 
  (let ((g (make-ncos 1234 10)))
    (do ((i 0 (+ i 1)))
	((= i 100000))
      (outa i (ncos g)))))

(with-sound (:statistics #t) 
  (cross-fade 0 2 1.0 "p1.snd" "p2.snd" 0.5 1.0 0 .1 256 2))

(with-sound (:statistics #t) 
  (dissolve-fade 0 2 1.0 "p1.snd" "p2.snd" 256 2 0 128))
|#
