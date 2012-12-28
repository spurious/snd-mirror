(provide 'snd-jcrev.scm)

(if (and (not (provided? 'snd-ws.scm)) 
	 (not (provided? 'sndlib-ws.scm)))
    (load "ws.scm"))


(definstrument (jc-reverb (low-pass #f) (volume 1.0) (amp-env #f))
  "(jc-reverb (low-pass #f) (volume 1.0) (amp-env #f)) -- Chowning reverb"
  (let ((allpass1 (make-all-pass -0.700 0.700 1051))
	(allpass2 (make-all-pass -0.700 0.700  337))
	(allpass3 (make-all-pass -0.700 0.700  113))
	(comb1 (make-comb 0.742 4799))
	(comb2 (make-comb 0.733 4999))
	(comb3 (make-comb 0.715 5399))
	(comb4 (make-comb 0.697 5801))
	(decay-dur (mus-srate))
	(chns (channels *output*))
	(file-dur (frames *reverb*))
	(outdel1 (make-delay (seconds->samples .013))))

    (let ((len (floor (+ decay-dur file-dur)))
	  (outdel2 (if (> chns 1) (make-delay (seconds->samples .011)) #f)))

      (if (or amp-env low-pass)
	  (let ((comb-sum-1 0.0)
		(comb-sum-2 0.0)
		(comb-sum 0.0)
		(all-sums 0.0)
		(envA (if amp-env (make-env :envelope amp-env :scaler volume :duration (/ len (mus-srate))) #f)))
	    (do ((i 0 (+ i 1)))
		((= i len))
	      (let* ((inval (ina i *reverb*))
		     (allpass-sum (all-pass allpass3 (all-pass allpass2 (all-pass allpass1 inval))))
		     (amp (if envA (env envA) 1.0)))
		(set! comb-sum-2 comb-sum-1)
		(set! comb-sum-1 comb-sum)
		(set! comb-sum 
		      (+ (comb comb1 allpass-sum)
			 (comb comb2 allpass-sum)
			 (comb comb3 allpass-sum)
			 (comb comb4 allpass-sum)))
		(if low-pass
		    (set! all-sums (+ (* .25 (+ comb-sum comb-sum-2)) (* .5 comb-sum-1)))
		    (set! all-sums comb-sum))
		(outa i (* amp (delay outdel1 all-sums)))
		(if (> chns 1) (outb i (* amp (delay outdel2 all-sums)))))))

	  (let ((filts (if (= chns 1) (vector outdel1) (vector outdel1 outdel2))))
	    (do ((i 0 (+ i 1)))
		((= i len))
	      (let ((allpass-sum (all-pass allpass3 (all-pass allpass2 (all-pass allpass1 (ina i *reverb*))))))
		(out-bank i filts (* volume (+ (comb comb1 allpass-sum)
					       (comb comb2 allpass-sum)
					       (comb comb3 allpass-sum)
					       (comb comb4 allpass-sum)))))))))))
  
;;; (with-sound (:reverb jc-reverb) (fm-violin 0 .1 440 .1 :reverb-amount .3))
  