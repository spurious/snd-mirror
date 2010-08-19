;;; cross fade instruments
;;;
;;; cross-fade sweeps up, down, or from mid-spectrum outwards,
;;; dissolve-fade chooses randomly -- like a graphical dissolve
;;; neither is exactly spectacular, but they work -- use similar sounds if possible (speech is problematic)
;;;
;;; translated from fade.ins

(definstrument (cross-fade beg dur amp file1 file2 ramp-beg ramp-dur ramp-type bank-dur fs fwidth)
  ;; ramp-type 0=sweep up, 1=sweep down, 2=split from middle
  (let* ((fil1 (make-sampler 0 file1))
         (fil2 (make-sampler 0 file2))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (ramp 0.0)
	 (bank1 0.0)
	 (bank2 1.0)
	 (half-fs (/ fs 2))
	 (ramp-samps (seconds->samples ramp-dur))
	 (bank-samps (seconds->samples bank-dur))
	 (ramp-incr (/ 1.0 ramp-samps))
	 (ramp-start (+ start (seconds->samples ramp-beg)))
	 (bank1-start (- ramp-start bank-samps))
	 (bank-incr (/ 1.0 bank-samps))
	 (ramp-end (+ ramp-start ramp-samps))
	 (bank2-start ramp-end)
	 (bank2-end (+ bank2-start bank-samps))
	 (bin (/ (mus-srate) (* 2 fs)))
	 (radius (- 1.0 (/ fwidth (* 2 fs))))
	 (fs1 (make-vector fs))
	 (val 0.0)
	 (ifs (/ 1.0 fs)))

    (do ((k 0 (+ 1 k)))
	((= k fs))
      (set! (fs1 k) (make-formant (* k bin) radius)))

    (run
     (do ((i start (+ 1 i)))
	 ((= i end))
       
       (if (< i bank1-start)
	   ;; in first section -- just mix in file1
	   (set! val (read-sample fil1))
	   
	   (if (> i bank2-end)
	       ;; in last section -- just mix file2
	       (set! val (read-sample fil2))
	       
	       (if (< i ramp-start)
		   ;; in bank1 section -- fire up the resonators
		   (let ((inval (read-sample fil1))
			 (outval 0.0))
		     (set! bank1 (+ bank1 bank-incr))
		     (do ((k 0 (+ 1 k)))
			 ((= k (- fs 1)))
		       (set! outval (+ outval (formant (fs1 (+ 1 k)) inval))))
		     (set! val (+ (* bank1 outval) (* (- 1.0 bank1) inval))))
		   
		   (if (> i ramp-end)
		       ;; in bank2 section -- ramp out resonators
		       (let ((inval (read-sample fil2))
			     (outval 0.0))
			 (set! bank2 (- bank2 bank-incr))
			 (do ((k 0 (+ 1 k)))
			     ((= k (- fs 1)))
			   (set! outval (+ outval (formant (fs1 (+ 1 k)) inval))))
			 (set! val (+ (* bank2 outval) (* (- 1.0 bank2) inval))))
		       
		       ;; in the fade section
		       (let ((inval1 (read-sample fil1))
			     (inval2 (read-sample fil2))
			     (outval 0.0))
			 ;; now the choice of spectral fade -- we should end with all bank1 0.0 and all bank2 1.0
			 (set! ramp (+ ramp ramp-incr))
			 
			 (if (= ramp-type 0)
			     (let ((r2 (* 2 ramp)))
			       ;; sweep up so low freqs go first
			       (do ((k 0 (+ 1 k)))
				   ((= k (- fs 1)))
				 (let ((rfs (max 0.0 (min 1.0 (- r2 (* k ifs))))))
				   (set! outval (+ outval (formant (fs1 (+ 1 k)) 
								   (+ (* rfs inval2) (* (- 1.0 rfs) inval1)))))))
			       (set! val outval))
			     
			     (if (= ramp-type 1)
				 (let ((r2 (* 2 ramp)))
				   ;; sweep up so high freqs go first
				   (do ((k 0 (+ 1 k)))
				       ((= k (- fs 1)))
				     (let ((rfs (max 0.0 (min 1.0 (- r2 (* (- fs k) ifs))))))
				       (set! outval (+ outval (formant (fs1 (+ 1 k)) 
								       (+ (* rfs inval2) (* (- 1.0 rfs) inval1)))))))
				   (set! val outval))
				 
				 ;; sweep from midpoint out
				 (let ((r2 (* 2 ramp)))
				   (do ((k 0 (+ 1 k)))
				       ((= k half-fs))
				     (let ((rfs (max 0.0 (min 1.0 (- (+ r2 0.5) (* (- fs k) ifs))))))
				       (set! outval (+ outval (formant (fs1 (+ 1 k)) 
								       (+ (* rfs inval2) (* (- 1.0 rfs) inval1)))))))
				   (do ((k 0 (+ 1 k)))
				       ((= k (- half-fs 1)))
				     (let ((rfs (max 0.0 (min 1.0 (- r2 (/ k half-fs))))))
				       (set! outval (+ outval (formant (fs1 (+ k 1 half-fs)) 
								       (+ (* rfs inval2) (* (- 1.0 rfs) inval1)))))))
				   (set! val outval)))))))))
       (outa i (* amp val))))))


;;; (vct->channel (with-sound (:output (make-vct 22050)) (cross-fade 0 .1 1 0 1 .01 .01 0 .1 256 2)))
;;; (vct->channel (with-sound (:output (make-vct 44100)) (cross-fade 0 2 1.0 "oboe.snd" "trumpet.snd" 0.5 1.0 0 .1 256 2)))
;;;
;;; these fades seem more successful to me when done relatively quickly (the opposite of the dissolve below
;;; which is best if done as slowly as possible).  I like the sweep up best -- a sort of "evaporation" effect.


(definstrument (dissolve-fade beg dur amp file1 file2 fsize r lo hi)
  (let* ((fil1 (make-sampler 0 file1))
         (fil2 (make-sampler 0 file2))
	 (start (seconds->samples beg))
	 (end (+ start (seconds->samples dur)))
	 (freq-inc (floor (/ fsize 2)))
	 (bin (floor (/ (mus-srate) fsize)))
	 (radius (- 1.0 (/ r fsize)))
	 (spectr (make-vct freq-inc 1.0))
	 (ramp-inc (/ 1.0 1024.0))
	 (trigger (floor (/ (* dur (mus-srate)) freq-inc)))
	 (fs (make-vector freq-inc))
	 (ctr 0))
    
    (if (not (number? hi)) (set! hi freq-inc))
    (do ((k 0 (+ 1 k)))
	((= k hi))
      (set! (fs k) (make-formant (* k bin) radius)))
    
    (run
     (do ((i start (+ 1 i)))
	 ((= i end))
       (let ((outval 0.0)
	     (inval1 (read-sample fil1))
	     (inval2 (read-sample fil2)))
	 ;; once a ramp is set in motion, it takes care of itself -- we need only choose which to trigger
	 (set! ctr (+ 1 ctr))
	 (if (> ctr trigger)
	     (begin
	       ;; find next randomly chosen resonator to flip
	       (let ((next (floor (random freq-inc))))
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
		 (set! (spectr next) (- (spectr next) ramp-inc))
		 (set! ctr 0))))
	 (do ((k lo (+ 1 k)))
	     ((= k hi))
	   (let ((sp (spectr k)))
	     (set! outval (+ outval (formant (fs k) (+ (* sp inval1) (* (- 1.0 sp) inval2)))))
	     (if (> 1.0 sp 0.0)
		 (set! (spectr k) (- (spectr k) ramp-inc)))))
	 (outa i (* amp outval)))))))


;;; (with-sound () (dissolve-fade 0 1 1.0 "oboe.snd" "trumpet.snd" 256 2 0 128))
;;; (vct->channel (with-sound (:output (make-vct 44100)) (dissolve-fade 0 2 1 0 1 4096 2 2 #f)))
;;;
;;; another neat effect here is to simply let the random changes float along with no
;;; direction -- if the hit is 1.0 send it toward 0.0 and vice versa -- strange
;;; pitches emerge from noises etc

