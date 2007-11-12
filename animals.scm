;;; animals.scm
;;;
;;; mosquito
;;; Knudsen's frog
;;; Oak toad
;;; Broad-winged tree-cricket
;;; Southern cricket frog
;;; Long-spurred meadow katydid
;;; Northern leopard frog
;;; Southern mole cricket
;;; Green tree-frog
;;; Spring peeper
;;; Crawfish frog
;;; River frog


(use-modules (ice-9 optargs) (ice-9 format))
(provide 'snd-animals.scm)

(if (not (provided? 'snd-generators.scm)) (load "generators.scm"))
(if (not (provided? 'snd-ws.scm)) (load "ws.scm"))


;;; this mosquito taken from Richard Mankin, Reference Library of Digitized Insect Sounds, http://www.ars.usda.gov/sp2UserFiles/person/3559/soundlibrary.html

(definstrument (mosquito beg dur freq amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (carrier (make-oscil freq))
	 (modulator1 (make-oscil (* freq 2))) ; or 1 (but leave lower mult at 2??)
	 (modulator3 (make-oscil (* freq 3)))
	 (modulator2 (make-oscil (* freq 8))) ; or 9
	 (ampf (make-env '(0 0 .2 .5 1 .5 2 .5 3 1 4 .5 5 .5) :scaler amp :duration dur :base 32))
	 (frqf (make-env '(0 0  1 0  1.01 0.1  1.03 -0.1  1.05 0.0  3 0 3.02 .05 3.03 -0.1 3.1 0 5 0.0) :duration dur :scaler (hz->radians freq)))
	 ;; TODO: make freq env flicks at call time (is there a comb-filter effect as it gets near?)
	 (vib (make-rand-interp 10.0 (hz->radians 10)))
	 (indf (make-rand-interp 1 .5))
	 (index2 (hz->radians (* freq 1.0)))
	 (index3 (hz->radians (* freq 0.5))))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let* ((amp (env ampf))
		(frq (+ (env frqf) (rand-interp vib)))
		(pitch (oscil carrier frq))
		(index1 (hz->radians (* freq (+ 1.0 (rand-interp indf))))))
	   (outa i (* amp
		      (+ (* 0.6 (oscil modulator1 (+ (* 2 frq) (* index1 pitch))))
			 (* 0.3 (oscil modulator3 (+ (* 3 frq) (* index3 pitch))))
			 (* 0.1 (oscil modulator2 (+ (* 8 frq) (* index2 pitch))))))
		 *output*)))))))

#|
(with-sound (:play #t)
  (mosquito 0 5 560 .2)
  (mosquito 1 3 880 .05))
|#



;;; "The Diversity of Animal Sounds", Cornell Lab of Ornithology

(definstrument (knudsens-frog beg amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples .25)))
	 (base (make-oscil 480))
	 (modm (make-oscil 40))
	 (frqf (make-env '(0 0 .5 .2 .8 1 1 1) :duration .25 :base 32 :scaler (hz->radians 50)))
	 (ampm (make-env '(0 .1  .5 .4  .6 .75  1 .9  1.5 1  2 .9 2.3 .1) :duration (/ .25 7)))
	 (ampp (make-pulse-train (/ 7 .25)))
	 (ampf (make-env '(0 0 1 1 3 1 4 0) :duration .25 :scaler amp)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (if (> (pulse-train ampp) 0.1)
	     (mus-reset ampm))
	 (outa i (* (env ampf)
		    (oscil base (+ (env frqf) (* .01 (oscil modm))))
		    (env ampm))
	       *output*))))))
#|
(with-sound (:play #t :statistics #t) 
  (knudsens-frog 0 .5))
|#


(definstrument (a-frog beg dur freq amp amp-env gliss gliss-env pulse-dur pulse-env fm-index fm-freq)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (base (make-oscil freq))
	 (modm (if (and fm-index (> fm-index 0.0)) (make-oscil fm-freq) #f))
	 (frqf (if gliss-env (make-env gliss-env :duration dur :base 32 :scaler (hz->radians gliss)) #f))
	 (pulse (make-pulsed-env pulse-env pulse-dur (/ 1.0 pulse-dur)))
	 (ampf (make-env amp-env :duration dur :scaler amp))
	 (index (hz->radians (* fm-freq fm-index))))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (pulsed-env pulse 0.0)
		    (oscil base (+ (if frqf (env frqf) 0.0) 
				   (if modm (* index (oscil modm)) 0.0))))
	       *output*))))))

(define (knudsens-frog beg amp)
  (a-frog beg .25 480 amp '(0 0 1 1 3 1 4 0) 
	  50 '(0 0 .5 .2 .8 1 1 1) 
	  (/ .25 7) '(0 .1  .5 .4  .6 .75  1 .9  1.5 1  2 .9 2.3 .1) 
	  1.75 40))  ; 0.01 here is about 1.75 as an fm index: (/ (radians->hz .01) 40)

#|
;;; cricket-like:
(with-sound (:play #t) 
    (a-frog 0 .25 2000 .5 '(0 0 1 1 3 1 4 0) ; or 3000 6000 etc
                        50 '(0 0 .5 .2 .8 1 1 1) 
                        (/ .25 5) '(0 0 1 0 5 1 8 0 20 0) 
                        0.01 40))

(with-sound (:play #t) 
    (a-frog 0 .25 4000 .5 '(0 0 1 1 3 1 4 0) 
                        0 #f
                        (/ .25 10) '(0 0 1 1 2 1 4 0 10 0) 
                        0.0 10))

;;; frog-like
(with-sound (:play #t) 
    (a-frog 0 .25 2000 .5 '(0 0 1 1 3 1 4 0) 
                        50 '(0 0 .5 .2 .8 1 1 1) 
                        (/ .25 10) '(0 0 1 1 2 1 3 0 4 0 5 1 7 1 8 0 20 0) 
                        0.0 10))
|#

(definstrument (a-cricket beg dur freq freq1 amp amp-env pulse-dur pulse-env)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (base (make-oscil freq))
	 (base1 (make-oscil freq1))
	 (pulse (make-pulsed-env pulse-env pulse-dur (/ 1.0 pulse-dur)))
	 (ampf (make-env amp-env :duration dur :scaler amp)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (pulsed-env pulse 0.0)
		    (+ (* .8 (oscil base 0.0))
		       (* .2 (oscil base1 0.0))))
	       *output*))))))

#|
(with-sound (:play #t) 
    (a-cricket 0 .12 4500 5400 .5 '(0 0 1 1 3 1 4 0)
                        (/ .11 3) '(0 0 1 .8 5 1 6 0 15 0)))
|#

;;; TODO: delay + sqr as handler for coupled rand-interp envs
;;; TODO: 8bat, indri?, insect trill in rail is at 8.5KHz? rail, midship (start), capuchin, allig



#|
;;; Oak Toad (L Elliott "The Calls of Frogs and Toads" #38)
;;;   might be slightly too much noise (the peep I worked on turned out to be a raspy one)
;;;
;;; L Elliott and W Herschberger "The Songs of the Insects"
;;; L Elliott "Music of the Birds"

(with-sound (:play #t :clipped #f :statistics #t)
  (let* ((dur .43)
	 (stop (seconds->samples dur))
	 (ampf (make-env '(0 0 10 1 15 0 43 0) :base .3 :duration dur :scaler .25))
	 (gen1 (make-polyshape 2150 :partials (list 1 .01  2 1.0  3 .001 4 .005  6 .02)))
	 (frqf (make-env '(0 -.5 1 1 5 -1) :duration .15 :scaler (hz->radians 70)))
	 (noise (make-rand-interp 1000)))
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (+ (env frqf)
					   (* .014 (rand-interp noise)))))
	       *output*))))))

(with-sound (:play #t :clipped #f :statistics #t)
  (let ((last-beg 0.0))
    (do ((k 0 (1+ k)))
	((= k 12))
      (let* ((beg (+ last-beg .37 (random .08)))
	     (start (seconds->samples beg))
	     (dur .43)
	     (stop (+ start (seconds->samples dur)))
	     (ampf (make-env '(0 0 10 1 15 0 43 0) :base .3 :duration dur :scaler .25))
	     (gen1 (make-polyshape 2150 :partials (list 1 .01  2 1.0  3 .001 4 .005  6 .02))) ; this is at or past the limit of my hearing, sad to say
	     (frqf (make-env '(0 -.5 1 1 5 -1) :duration .15 :scaler (hz->radians (+ 50 (random 40)))))
	     (noise (make-rand-interp 1000))
	     (noise-amount (+ .01 (random .005))))
	(set! last-beg beg)
	(run
	 (lambda ()
	   (do ((i start (1+ i)))
	       ((= i stop))
	     (outa i (* (env ampf)
			(polyshape gen1 1.0 (+ (env frqf)
					       (* noise-amount (rand-interp noise)))))
		   *output*))))))))
|#
(definstrument (broad-winged-tree-cricket beg dur amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (freq 1700)
	 (base (make-oscil freq))
	 (base1 (make-oscil (* 2 freq)))
	 (base2 (make-oscil (* 3 freq)))
	 (base3 (make-oscil (* 4 freq)))
	 (base4 (make-oscil (* 5 freq)))
	 (ampmod (make-triangle-wave 155))
	 (pulse (make-pulsed-env '(0 0 1 .1 6 .2 7 0 8 .3 10 1 18 .9 21 .1 23 .3 28 .1 30 0) .06 (/ 1.0 .06))) ; base of about .1 would be better here
	 (ampf (make-env '(0 0 8 1 20 1 21 0) :duration dur :scaler amp))
	 (frqf (make-pulsed-env '(0 1 1 -1 2 -1) .06 (/ 1.0 .06))) ; base of about 10 here -- TODO: need base arg to pulsed-env (or pass env itself)
	 (indf (make-pulsed-env '(0 0 10 0 18 1 23 1 26 0 30 0) .06 (/ 1.0 .06))) ; TODO: all of these pulses need to march together
	 (noise (make-rand-interp 1000)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((buzz (+ (* (hz->radians 40) (pulsed-env frqf 0.0))
			(* .005 (rand-interp noise)))))
	   (outa i (* (env ampf)
		      (pulsed-env pulse 0.0)
		      (+ .93 (* .07 (triangle-wave ampmod)))
		      (+ (* .7 (oscil base buzz))
			 (* .05 (oscil base1 (* 2 buzz)))
			 (* .04 (oscil base2 (* 3 buzz)))
			 (* (pulsed-env indf 0.0)
			    (+ 	(* .02 (oscil base3 (* 4 buzz)))
				(* .02 (oscil base4 (* 5 buzz)))))))
	       *output*)))))))
#|
;(with-sound (:play #t) (broad-winged-tree-cricket 0 1.0 0.3))


;;; TODO: frogs/crickets if ins dur>base song, repeat at correct interval


;;; southern cricket frog

(with-sound (:play #t :clipped #f :statistics #t)
  (let* ((dur1 .03)
	 (stop (seconds->samples dur1))
	 (ampf (make-env '(0 0 .75 1 5 1 10 0) :scaler .5 :duration dur1))
	 (gen1 (make-oscil 3500))
	 (gen2 (make-oscil 6400))
	 (pulse (make-pulsed-env '(0 .1 1 .6 2 .8 3 1 6 .1 8 .1) (/ dur1 8) (/ 8 dur1)))
	 (index (hz->radians (* 150 2)))
	 (f1 (make-env '(0 .9 9 .9 10 0) :duration dur1))
	 (f2 (make-env '(0 .05 8 .1 10 .8 .1) :duration dur1))
	 (fm (make-oscil 150)))
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i stop))
	 (let ((fm (* index (oscil fm))))
	   (outa i (* (env ampf)
		      (pulsed-env pulse 0.0)
		      (+ (* (env f1) (oscil gen1 fm))
			 (* (env f2) (oscil gen2 (* 2 fm)))))
	       *output*)))))))


;;; long-spurred meadow katydid -- I can barely hear this at its true pitch, so the match was
;;;    done down one or two octaves -- I think the recording has cut off high info (above 20Khz) --
;;;    need much higher srate to see what this guy is really doing.  This is not very good...

(with-sound (:clipped #f :statistics #t)
  (let* ((beg 0.0)
	 (dur 10.1) ; overall duration
	 (slow-start 2.2) ; buzz at start
	 (soft-end (+ slow-start 4.0)) ; softer section, rest is full vol
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 ;; looks like the same basic pulse throughout, almost same speed at start but every other one is squelched
	 ;; slow startup pulse starts much faster (.06 mid-pulse duration, .0013 base pulse)
	 (carrier (make-sine-summation 13200 0 4 .7 (/ 800 13200))) 
	 (modulator (make-oscil 1500 (* 0.5 pi)))
	 (noise (make-rand-interp 5000))
	 (peep (make-pulsed-env '(0 0 1 0 2 .2 3 0 5 .75 8 1 10 0 11 0) .06 (/ 1.0 .06)))
	 (amp .4)
	 (ampf (make-env (list 0 0 .5 .5 slow-start .4 soft-end .4 (+ soft-end .5) 1 (- dur 1) 1 dur 0.0) :duration dur :scaler amp))
	 (pulsef (make-env (list 0 -1 slow-start -1 (+ slow-start .03) 0 dur 0) :duration dur :scaler (hz->radians 8)))
	 )
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let* ((frq (env pulsef))
		(md (oscil modulator)))
	   (outa i (* .2 (env ampf)
		      (pulsed-env peep frq)
		      md md
		      (sine-summation carrier (+ (* frq (/ 13200 16))
					       (* .1 (rand-interp noise))
					       (* .1 md))))
		 *output*)))))))



;;; northern leopard frog (1)

;;; TODO: this is slightly low-passed, and I don't quite have the vowel right at the end

(with-sound (:statistics #t :clipped #f :play #t :scaled-to .5)
  (let* ((beg 0.0)
	 (dur 4.2)
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (gen1 (make-oscil 440))
	 (gen2 (make-oscil 1030)) ; there's also a 1500 formant that follows the 1000 case -- looks a lot like FM index 1 ca 600Hz
	 (gen3 (make-oscil 2600))
	 (pulse (make-pulse-train (/ 1.0 0.09)))
	 (pulsef1 (make-env '(0 0 .1 1 10 0) :duration .013 :base 32.0))
	 (pulsef2 (make-env '(0 0 4 1 10 0) :duration .013 :base 3.0))
	 (interpf (make-env '(0 0 6 1 8 1 10 .5) :duration dur))
	 (amp .5)
	 (ampf (make-env '(0 0 3 1 9.5 1 10 0) :base .2 :duration dur :scaler amp))
	 (gen1f (make-env '(0 1 8 1 10 0) :duration dur :scaler .65 :base 3))
	 (gen2f (make-env '(0 0 8 0 10 1) :duration dur :scaler (hz->radians 90)))
	 (gen3f (make-env '(0 1 6 1 10 0) :duration dur :offset (hz->radians 2200) :scaler (hz->radians 400)))
	 (gen4f (make-env '(0 0 8 0 10 .02) :duration dur))
	 (gen5f (make-env '(0 0 5 0 10 -1) :duration dur :scaler (hz->radians 200)))
	 (pulf (make-env '(0 1 2 0 10 0) :scaler (hz->radians 3) :duration dur))
	 (gen6 (make-oscil 170)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (if (> (pulse-train pulse (env pulf)) 0.1)
	     (begin
	       (mus-reset pulsef1)
	       (mus-reset pulsef2)
	       (mus-reset gen1)
	       (mus-reset gen2)))
	 (let* ((intrp (env interpf))
		(gentrp (env gen1f))
		(gen4trp (env gen4f))
		(result (* (env ampf)
			   (+ (* intrp (env pulsef1))
			      (* (- 1.0 intrp) (env pulsef2)))
			   (+ (* gentrp (oscil gen2 (env gen5f)))
			      (* (- 1.0 gentrp) (+ (* (- 1.0 gen4trp) (oscil gen1 (env gen2f)))
						   (* gen4trp (oscil gen3 (+ (env gen3f)
									     (* .075 (oscil gen6)))))))))))
	   (outa i result *output*)))))))



;;; southern mole cricket

(with-sound (:play #t :clipped #f :statistics #t)
  (let* ((dur 3.0) ; goes on indefinitely
	 (stop (seconds->samples dur))
	 (ampf (make-env '(0 0 1 1 20 1 21 0) :scaler .25 :duration dur))
	 (gen1 (make-oscil 2700))
	 (gen2 (make-oscil (* 2700 2.4)))
	 (gen3 (make-oscil 60))
	 (gen5 (make-oscil 360))
	 (gen4 (make-oscil (* 2700 3.2)))
	 (gargle (make-rand-interp 360 (hz->radians (* .25 360)))))
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i stop))
	 (let* ((pval (oscil gen3))
		(noise (rand-interp gargle))
		(aval (+ .7 (* .3 (oscil gen5))))
		)
	   (outa i (* (env ampf)
		      (+ (* (max pval 0.0)
			    (+ (* .95 (oscil gen1 noise))
			       (* .05 (oscil gen4 noise))))
			 (* (max (- 1.0 pval) 0.0)
			    (* .05 aval (oscil gen2 (* 2.4 noise))))))
		 *output*)))))))



;;; TODO: green treefrog but needs formant movement, speedup at start, slow down at end, other main section to the call

(with-sound (:play #t :clipped #f :statistics #t)
  (let* ((dur 0.2)
	 (stop (seconds->samples dur))
	 (ampf (make-env '(0 0 1 1 8 1 12 0) :scaler .25 :duration dur))
	 (pitch 277)
	 (gen2770 (make-oscil (* 10 pitch)))
	 (mod277 (make-oscil pitch))
	 (gen7479 (make-oscil (* pitch 27)))
	 (poly (make-polyshape pitch :coeffs (partials->polynomial (list 3 .3  8 .2  9 .2  10 .9  11 1.0  12 .5))))
	 (poly2 (make-polyshape 860 :coeffs (partials->polynomial (list  1 .4  2 .1  3 .03  4 .3  5 .03))))
	 (index (hz->radians 277))
	 (frqf (make-env '(0 -.3  1 .3  2 0  5 0  6 -1) :duration dur :scaler (hz->radians 70)))
	 (pulsef (make-pulsed-env '(0 .2 1 1 3 .7 5 .2) (/ 1.0 pitch) pitch))
	 )
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i stop))
	 (let* ((md (* index (oscil mod277)))
		(frq (env frqf)))
	   (outa i (* (env ampf)
		      (pulsed-env pulsef frq)
		      (+ (* .78 (polyshape poly 1.0 frq))
			 (* .2 (oscil gen2770 (* 10 (+ frq md))))
			 (* .02 (oscil gen7479 (* 27 (+ frq md))))
			 (* .25 (polyshape poly2 1.0 (* 3 frq)))
			 ))
		 *output*)))))))


;;; spring peeper

(with-sound (:play #t :clipped #f :statistics #t)
  (let* (;; first note
	 (dur 0.17)
	 (stop (seconds->samples dur))
	 (ampf (make-env '(0 0 .25 .6 8 1 10 .8 10.5 0) :scaler .25 :duration dur :base .03))
	 (gen1 (make-oscil 2400))
	 (gen2 (make-oscil (/ 2400 2)))
	 (gen2a (make-oscil 2400))
	 (frqf (make-env '(0 0 1 1) :scaler (hz->radians 600) :duration dur :base 30.0))
	 
	 ;; second note
	 (pause 0.23)
	 (start2 (+ stop (seconds->samples pause)))
	 (dur2 .13)
	 (stop2 (+ start2 (seconds->samples dur2)))
	 (ampf2 (make-env '(0 0 .125 .8 1 .9 2 .7 4 1 10 0) :base .1 :duration dur2 :scaler .1))
	 (frqf2 (make-env '(0 0 2 1 3 .75) :duration dur2 :base .03 :scaler (hz->radians 300)))
	 (gen3 (make-oscil 2900))
	 (gen4 (make-oscil (/ 2900 2)))
	 (index (hz->radians (* 0.1 2900)))
	 )
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i stop))
	 (let* ((frq (env frqf)))
	   (outa i (* (env ampf)
		      (oscil gen1 (+ frq 
				     (* index
					(+ (* 0.2 (oscil gen2 (* 0.5 frq))) 
					   (* 1.5 (oscil gen2a frq)))))))
		 *output*))) ; end is not quite right (original has a catch)
       (do ((i start2 (1+ i)))
	   ((= i stop2))
	 (let* ((frq (env frqf2)))
	   (outa i (* (env ampf2)
		      (oscil gen3 (+ frq (* index (oscil gen4 (* 0.5 frq))))))
		 *output*)))))))



;;; crawfish frog
(with-sound (:play #t :clipped #f :statistics #t)
  (let* ((dur 0.6)
	 (pitch 58)
	 (stop (seconds->samples dur))
	 (ampf (make-env '(0 0 4 1 8 1 9 .7 10 0) :scaler .5 :duration dur))
	 (pulser (make-pulse-train pitch))
	 (pulsef (make-env '(0 0 1 1 10 0) :base 32.0 :duration (/ 1.0 pitch)))
	 (fmd (make-oscil pitch))
	 (gen1 (make-oscil (* pitch 15)))
	 (frqf (make-env '(0 .5  .2 0  1 1) :scaler (hz->radians pitch) :base 10.0 :duration dur))
	 (index (hz->radians pitch))
	 (poly1 (make-polyshape (* pitch 6) :coeffs (partials->polynomial '(1 .5  2 1  5 .5))))
	 (poly2 (make-polyshape (* pitch 6) :coeffs (partials->polynomial '(2 .5  3 1  7 .25))))
	 (intrp (make-env '(0 1 1 0) :duration dur :scaler .2 :base 4))
	 )
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i stop))
	 (let* ((frq (env frqf))
		(wha (env intrp)))
	   (if (> (pulse-train pulser) 0.1)
	       (begin
		 (mus-reset pulsef)
		 (mus-reset gen1)
		 (mus-reset fmd)))
	   (outa i (* (env ampf)
		      (env pulsef)
		      (+ (* .5 (oscil gen1 (+ frq 
					      (* index (oscil fmd (/ frq 15))))))
			 (* wha (polyshape poly1 1.0 frq))
			 (* (- 0.2 wha) (polyshape poly2 1.0 frq))))
		 *output*)))))))


;;; river frog
(with-sound (:play #t :clipped #f :statistics #t)
  (let* ((dur 1.85)
	 (stop (seconds->samples dur))
	 (ampf (make-env '(0 0 2 1  7 .9  10 0) :scaler .5 :duration dur :base .1))
	 (pulse-pitch 42)
	 (pulsef (make-pulsed-env '(0 .1 3 .1 3.1 1 4 1 6 .1 9 .1) (/ 1.0 pulse-pitch) pulse-pitch))
	 (mid-pitch 185)
	 (mid-pitch-change 10)
	 (frqf (make-env '(0 .1 .2 -.02 .5 0 .65 0 1 1) :scaler (hz->radians mid-pitch-change) :duration dur))

	 (vib (make-rand-interp 100 (hz->radians 10.0)))
	 
	 (fm (make-oscil pulse-pitch))
	 (index (hz->radians (* 1.0 pulse-pitch)))

	 (poly1 (make-polyshape mid-pitch :coeffs (partials->polynomial (normalize-partials (list 2 1.2  4 .1  7 0.75  8 .1   10 .5)))))
	 (poly2 (make-polyshape mid-pitch :coeffs (partials->polynomial (normalize-partials (list 2 1.0        7 .5  9 .7  12 .01)))))

	 (interpf (make-env '(0 0 2 0 5 1 7 1) :duration dur))
	 )
    (run
     (lambda ()
       (do ((i 0 (1+ i)))
	   ((= i stop))
	 (let* ((frq (+ (env frqf)
			(rand-interp vib)
			(* index (oscil fm))))
		(intrp (env interpf)))
	   (outa i (* (env ampf)
		      (pulsed-env pulsef 0.0)
		      (+ (* (- 1.0 intrp)
			    (polyshape poly1 1.0 frq))
			 (* intrp 
			    (polyshape poly2 1.0 frq)))
		      
		      )
		 *output*)))))))|#


