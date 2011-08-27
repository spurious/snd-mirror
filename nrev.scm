;;; NREV (the most popular Samson box reverb)

(provide 'snd-nrev.scm)

(if (and (not (provided? 'snd-ws.scm)) 
	 (not (provided? 'sndlib-ws.scm)))
    (load "ws.scm"))


(definstrument (nrev (reverb-factor 1.09) (lp-coeff 0.7) (volume 1.0))
  ;; reverb-factor controls the length of the decay -- it should not exceed (/ 1.0 .823)
  ;; lp-coeff controls the strength of the low pass filter inserted in the feedback loop
  ;; output-scale can be used to boost the reverb output
  (define (prime? val)
    (or (= val 2)
	(and (odd? val)
	     (do ((i 3 (+ i 2))
		  (lim (sqrt val)))
		 ((or (= 0 (modulo val i)) (> i lim))
		  (> i lim))))))
  (define (next-prime val)
    (if (prime? val)
	val
	(next-prime (+ val 2))))
       
  (let ((srscale (/ (mus-srate) 25641))
	(dly-len (list 1433 1601 1867 2053 2251 2399 347 113 37 59 53 43 37 29 19)))
    (do ((i 0 (+ i 1)))
	((= i 15))
      (let ((val (floor (* srscale (dly-len i)))))
	(if (even? val) (set! val (+ 1 val)))
	(set! (dly-len i) (next-prime val))))

    (let* ((len (+ (floor (mus-srate)) (frames *reverb*)))
	   (comb1 (make-comb (* .822 reverb-factor) (dly-len 0)))
	   (comb2 (make-comb (* .802 reverb-factor) (dly-len 1)))
	   (comb3 (make-comb (* .773 reverb-factor) (dly-len 2)))
	   (comb4 (make-comb (* .753 reverb-factor) (dly-len 3)))
	   (comb5 (make-comb (* .753 reverb-factor) (dly-len 4)))
	   (comb6 (make-comb (* .733 reverb-factor) (dly-len 5)))
	   (low (make-one-pole lp-coeff (- lp-coeff 1.0)))
	   (chan2 (> (channels *output*) 1))
	   (chan4 (= (channels *output*) 4))
	   (allpass1 (make-all-pass -0.700 0.700 (dly-len 6)))
	   (allpass2 (make-all-pass -0.700 0.700 (dly-len 7)))
	   (allpass3 (make-all-pass -0.700 0.700 (dly-len 8)))
	   (allpass4 (make-all-pass -0.700 0.700 (dly-len 9))) ; 10 for quad
	   (allpass5 (make-all-pass -0.700 0.700 (dly-len 11)))
	   (allpass6 (if chan2 (make-all-pass -0.700 0.700 (dly-len 12)) #f))
	   (allpass7 (if chan4 (make-all-pass -0.700 0.700 (dly-len 13)) #f))
	   (allpass8 (if chan4 (make-all-pass -0.700 0.700 (dly-len 14)) #f)))
      (if (not chan2)
	  (run
	   (do ((i 0 (+ i 1)))
	       ((= i len))
	     (let ((rev (* volume (ina i *reverb*))))
		(outa i (all-pass allpass5
				  (all-pass allpass4
					    (one-pole low
						      (all-pass allpass3
								(all-pass allpass2
									  (all-pass allpass1
										    (+ (comb comb1 rev)
										       (comb comb2 rev)
										       (comb comb3 rev)
										       (comb comb4 rev)
										       (comb comb5 rev)
										       (comb comb6 rev))))))))))))
	  (run
	   (do ((i 0 (+ i 1)))
	       ((= i len))
	     (let* ((rev (* volume (ina i *reverb*)))
		    (outrev (all-pass allpass4
				      (one-pole low
						(all-pass allpass3
							  (all-pass allpass2
								    (all-pass allpass1
									      (+ (comb comb1 rev)
										 (comb comb2 rev)
										 (comb comb3 rev)
										 (comb comb4 rev)
										 (comb comb5 rev)
										 (comb comb6 rev)))))))))
	       (outa i (all-pass allpass5 outrev))
	       (outb i (all-pass allpass6 outrev))
	       (if chan4 (outc i (all-pass allpass7 outrev)))
	       (if chan4 (outd i (all-pass allpass8 outrev))))))))))


