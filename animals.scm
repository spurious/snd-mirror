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
;;; Indri
;;; Handsome trig
;;; Fast-calling tree cricket
;;; Dog-day cicada
;;; Linnaeus' cicada
;;; Lyric cicada


(use-modules (ice-9 optargs) (ice-9 format))
(provide 'snd-animals.scm)

(if (not (provided? 'snd-generators.scm)) (load "generators.scm"))
(if (not (provided? 'snd-ws.scm)) (load "ws.scm"))


;;; bat recording has a steady sine wave at 18755 Hz -- very loud if I could hear that high
;;; TODO: delay + sqr as handler for coupled rand-interp envs
;;; TODO: 8bat, insect trill in rail is at 8.5KHz? rail, midship (start), capuchin, allig
;;; TODO: frogs/crickets if ins dur>base song, repeat at correct interval



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
	 ;; SOMEDAY: make freq env flicks at call time (is there a comb-filter effect as it gets near?)
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


;;; L Elliott "The Calls of Frogs and Toads"
;;; L Elliott and W Herschberger "The Songs of the Insects"
;;; L Elliott "Music of the Birds"


;;; --------------------------------------------------------------------------------
;;; Oak Toad
;;;   might be slightly too much noise (the peep I worked on turned out to be a raspy one)

(definstrument (oak-toad beg amp)
  (let* ((dur .43)
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0 0 10 1 15 0 43 0) :base .3 :duration dur :scaler amp))
	 (gen1 (make-polyshape 2150 :partials (list 1 .01  2 1.0  3 .001 4 .005  6 .02)))
	 (frqf (make-env '(0 -.5 1 1 5 -1) :duration .15 :scaler (hz->radians (+ 50 (random 40)))))
	 (noise (make-rand-interp 1000 (+ .01 (random .005)))))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (+ (env frqf)
					   (rand-interp noise))))
	       *output*))))))
#|
(with-sound (:play #t)
  (let ((last-beg 0.0))
    (do ((k 0 (1+ k)))
	((= k 12))
      (let ((beg (+ last-beg .37 (random .08))))
	(oak-toad beg (+ .25 (random .3)))
	(set! last-beg beg)))))
|#


;;; --------------------------------------------------------------------------------
;;; Broad-winged Tree-cricket

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
	 (ampf (make-env '(0 0 8 1 20 1 21 0) :duration dur :scaler amp))
	 (frqf (make-env '(0 1  1 -1  2 -1) :duration .06 :base 10.0))
	 (pulsef (make-env '(0 0 1 .1 6 .2 7 0 8 .3 10 1 18 .9 21 .1 23 .3 28 .1 30 0) :duration .06 :base .1))
	 (pulser (make-pulse-train (/ 1.0 .06)))
	 (indf (make-env '(0 0 10 0 18 1 23 1 26 0 30 0) :duration .06))
	 (noise (make-rand-interp 1000)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let* ((pulse (pulse-train pulser)))
	   (if (> pulse .1)
	       (begin
		 (mus-reset pulsef)
		 (mus-reset frqf)
		 (mus-reset indf)))
	   (let* ((buzz (+ (* (hz->radians 40) (env frqf))
			   (* .005 (rand-interp noise)))))
	     (outa i (* (env ampf)
			(env pulsef)
			(+ .93 (* .07 (triangle-wave ampmod)))
			(+ (* .7 (oscil base buzz))
			   (* .05 (oscil base1 (* 2 buzz)))
			   (* .04 (oscil base2 (* 3 buzz)))
			   (* (env indf)
			      (+ (* .02 (oscil base3 (* 4 buzz)))
				 (* .02 (oscil base4 (* 5 buzz)))))))
		   *output*))))))))

;(with-sound (:play #t) (broad-winged-tree-cricket 0 1.0 0.3))



;;; --------------------------------------------------------------------------------
;;; southern cricket frog

(definstrument (southern-cricket-frog beg amp)
  (let* ((dur1 .03)
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur1)))
	 (ampf (make-env '(0 0 .75 1 5 1 10 0) :scaler amp :duration dur1))
	 (gen1 (make-oscil 3500))
	 (gen2 (make-oscil 6400))
	 (pulse (make-pulsed-env '(0 .1 1 .6 2 .8 3 1 6 .1 8 .1) (/ dur1 8) (/ 8 dur1)))
	 (index (hz->radians (* 150 2)))
	 (f1 (make-env '(0 .9 9 .9 10 0) :duration dur1))
	 (f2 (make-env '(0 .05 8 .1 10 .8 .1) :duration dur1))
	 (fm (make-oscil 150)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((fm (* index (oscil fm))))
	   (outa i (* (env ampf)
		      (pulsed-env pulse 0.0)
		      (+ (* (env f1) (oscil gen1 fm))
			 (* (env f2) (oscil gen2 (* 2 fm)))))
	       *output*)))))))

;(with-sound () (southern-cricket-frog 0 0.5))


;;; --------------------------------------------------------------------------------
;;; long-spurred meadow katydid
;;;    I can barely hear this at its true pitch, so the match was
;;;    done down one or two octaves -- I think the recording has cut off high info (above 20Khz) --
;;;    need much higher srate to see what this guy is really doing.  This is not very good...

(definstrument (long-spurred-meadow-katydid beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 10.1) ; overall duration
	 (slow-start 2.2) ; buzz at start
	 (soft-end (+ slow-start 4.0)) ; softer section, rest is full vol
	 (stop (+ start (seconds->samples dur)))
	 ;; looks like the same basic pulse throughout, almost same speed at start but every other one is squelched
	 ;; slow startup pulse starts much faster (.06 mid-pulse duration, .0013 base pulse)
	 (carrier (make-sine-summation 13200 0 4 .7 (/ 800 13200))) 
	 (modulator (make-oscil 1500 (* 0.5 pi)))
	 (noise (make-rand-interp 5000))
	 (peep (make-pulsed-env '(0 0 1 0 2 .2 3 0 5 .75 8 1 10 0 11 0) .06 (/ 1.0 .06)))
	 (ampf (make-env (list 0 0 .5 .5 slow-start .4 soft-end .4 (+ soft-end .5) 1 (- dur 1) 1 dur 0.0) :duration dur :scaler amp))
	 (pulsef (make-env (list 0 -1 slow-start -1 (+ slow-start .03) 0 dur 0) :duration dur :scaler (hz->radians 8))))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let* ((frq (env pulsef))
		(md (oscil modulator)))
	   (outa i (* (env ampf)
		      (pulsed-env peep frq)
		      md md
		      (sine-summation carrier (+ (* frq (/ 13200 16))
					       (* .1 (rand-interp noise))
					       (* .1 md))))
		 *output*)))))))

;(with-sound () (long-spurred-meadow-katydid 0 .5))


;;; --------------------------------------------------------------------------------
;;; northern leopard frog (1)

;;; TODO: this is slightly low-passed, and I don't quite have the vowel right at the end

(definstrument (northern-leopard-frog beg amp)
  (let* ((dur 4.2)
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (gen1 (make-oscil 440))
	 (gen2 (make-oscil 1030)) ; there's also a 1500 formant that follows the 1000 case -- looks a lot like FM index 1 ca 600Hz
	 (gen3 (make-oscil 2600))
	 (pulse (make-pulse-train (/ 1.0 0.09)))
	 (pulsef1 (make-env '(0 0 .1 1 10 0) :duration .013 :base 32.0))
	 (pulsef2 (make-env '(0 0 4 1 10 0) :duration .013 :base 3.0))
	 (interpf (make-env '(0 0 6 1 8 1 10 .5) :duration dur))
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

;(with-sound (:statistics #t :play #t) (northern-leopard-frog 0 .5))


;;; --------------------------------------------------------------------------------
;;; southern mole cricket

(definstrument (southern-mole-cricket beg dur amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0 0 1 1 20 1 21 0) :scaler amp :duration dur))
	 (gen1 (make-oscil 2700))
	 (gen2 (make-oscil (* 2700 2.4)))
	 (gen3 (make-oscil 60))
	 (gen5 (make-oscil 360))
	 (gen4 (make-oscil (* 2700 3.2)))
	 (gargle (make-rand-interp 360 (hz->radians (* .25 360)))))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let* ((pval (oscil gen3))
		(noise (rand-interp gargle))
		(aval (+ .7 (* .3 (oscil gen5)))))
	   (outa i (* (env ampf)
		      (+ (* (max pval 0.0)
			    (+ (* .95 (oscil gen1 noise))
			       (* .05 (oscil gen4 noise))))
			 (* (max (- 1.0 pval) 0.0)
			    (* .05 aval (oscil gen2 (* 2.4 noise))))))
		 *output*)))))))

;(with-sound () (southern-mole-cricket 0 3 .5))


;;; --------------------------------------------------------------------------------
;;; green tree-frog

(definstrument (green-tree-frog beg amp)
  (let* ((dur 0.2)
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0 0 1 1 8 1 12 0) :scaler amp :duration dur))
	 (pitch 277)
	 (gen2770 (make-oscil (* 10 pitch) (* 0.5 pi)))
	 (mod277 (make-oscil pitch (* 0.5 pi)))
	 (gen7479 (make-oscil (* pitch 27)))
	 (poly (make-polyshape pitch :coeffs (partials->polynomial (list 3 .3  8 .2  9 .2  10 .9  11 1.0  12 .5))))
	 (poly2 (make-polyshape 860 :coeffs (partials->polynomial (list  1 .4  2 .1  3 .03  4 .3  5 .03))))
	 (index (hz->radians 277))
	 (frqf (make-env '(0 -.3  1 .3  2 0  5 0  6 -1) :duration dur :scaler (hz->radians 70)))
	 (pulsef (make-pulsed-env '(0 .2 1 1 3 .7 5 .2) (/ 1.0 pitch) pitch)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let* ((md (* index (oscil mod277)))
		(frq (env frqf)))
	   (outa i (* (env ampf)
		      (* .333 (pulsed-env pulsef frq))
		      (+ (* .78 (polyshape poly 1.0 frq))
			 (* .2 (oscil gen2770 (* 10 (+ frq md))))
			 (* .02 (oscil gen7479 (* 27 (+ frq md))))
			 (* .25 (polyshape poly2 1.0 (* 3 frq)))))
		 *output*)))))))

;(with-sound () (green-tree-frog 0 .5))


;;; --------------------------------------------------------------------------------
;;; spring peeper

(definstrument (spring-peeper beg amp)
  (let* (;; first note
	 (dur 0.17)
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0 0 .25 .6 8 1 10 .8 10.5 0) :scaler amp :duration dur :base .03))
	 (gen1 (make-oscil 2400))
	 (gen2 (make-oscil (/ 2400 2)))
	 (gen2a (make-oscil 2400))
	 (frqf (make-env '(0 0 1 1) :scaler (hz->radians 600) :duration dur :base 30.0))
	 
	 ;; second note
	 (pause 0.23)
	 (start2 (+ stop (seconds->samples pause)))
	 (dur2 .13)
	 (stop2 (+ start2 (seconds->samples dur2)))
	 (ampf2 (make-env '(0 0 .125 .8 1 .9 2 .7 4 1 10 0) :base .1 :duration dur2 :scaler (* .4 amp)))
	 (frqf2 (make-env '(0 0 2 1 3 .75) :duration dur2 :base .03 :scaler (hz->radians 300)))
	 (gen3 (make-oscil 2900))
	 (gen4 (make-oscil (/ 2900 2)))
	 (index (hz->radians (* 0.1 2900))))
    (run
     (lambda ()
       (do ((i start (1+ i)))
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

;(with-sound () (spring-peeper 0 .5))


;;;--------------------------------------------------------------------------------
;;; crawfish frog

(definstrument (crawfish-frog beg amp)
  (let* ((dur 0.6)
	 (pitch 58)
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0 0 4 1 8 1 9 .7 10 0) :scaler amp :duration dur))
	 (pulser (make-pulse-train pitch))
	 (pulsef (make-env '(0 0 1 1 10 0) :base 32.0 :duration (/ 1.0 pitch)))
	 (fmd (make-oscil pitch))
	 (gen1 (make-oscil (* pitch 15)))
	 (frqf (make-env '(0 .5  .2 0  1 1) :scaler (hz->radians pitch) :base 10.0 :duration dur))
	 (index (hz->radians pitch))
	 (poly1 (make-polyshape (* pitch 6) :coeffs (partials->polynomial '(1 .5  2 1  5 .5))))
	 (poly2 (make-polyshape (* pitch 6) :coeffs (partials->polynomial '(2 .5  3 1  7 .25))))
	 (intrp (make-env '(0 1 1 0) :duration dur :scaler .2 :base 4)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
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

;(with-sound () (crawfish-frog 0 .5))


;;; --------------------------------------------------------------------------------
;;; river frog
;;;
;;; original formants were much sharper, but using rxyk!cos to sharpen ours didn't seem to help
;;; animal seems to group these in 3's

(definstrument (river-frog beg amp)
  (let* ((dur 1.85)
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0 0 2 1  7 .9  10 0) :scaler amp :duration dur :base .1))
	 (pulse-pitch 42)
	 (pulsef (make-pulsed-env '(0 .1 3 .1 3.1 1 4 1 6 .1 9 .1) (/ 1.0 pulse-pitch) pulse-pitch))
	 (mid-pitch 185)
	 (mid-pitch-change 10)
	 (frqf (make-env '(0 .1 .2 -.02 .5 0 .65 0 1 1) :scaler (hz->radians mid-pitch-change) :duration dur))
	 (vib (make-rand-interp 100 (hz->radians 10.0)))
	 (fm (make-oscil pulse-pitch))
	 (index (hz->radians (* 1.0 pulse-pitch)))
	 (poly1 (make-polyshape mid-pitch :coeffs (partials->polynomial (normalize-partials (list 2 1.2  4 .1  7 0.75  8 .1       10 .5)))))
	 (poly2 (make-polyshape mid-pitch :coeffs (partials->polynomial (normalize-partials (list 2 1.0        7 .5         9 .7        12 .01)))))
	 (interpf (make-env '(0 0 2 0 5 1 7 1) :duration dur)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
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
			    (polyshape poly2 1.0 frq))))
		 *output*)))))))

;(with-sound () (river-frog 0 .5))


;;; --------------------------------------------------------------------------------
;;; indri
;;; close in spectrum, amp, freq, but the original has what sounds like a ton of reverb
;;;   if I add the reverb, it's close -- a "field recording" at the local zoo?

(definstrument (indri beg amp)
  (let* ((pitch 900)
	 (dur 1.5)
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (gen1 (make-oscil (* 2 pitch))) 
	 (gen4 (make-oscil pitch))
	 (gen2 (make-oscil (* 8 pitch))) 
	 (gen3 (make-oscil pitch))
	 (ampf (make-env '(0.0 0.0  0.05 0.4  0.1 0.65  0.2 0.5  0.27 1.0  0.4 0.43  0.43 0.53 
			   0.5 0.5  0.53 0.36  0.6 1.0  0.64 0.99  0.69 0.6  0.7 0.3  0.77 0.15 
			   0.8 0.04  1.0 0.0) :duration dur :scaler amp))
	 (frqf (make-env '(0 1  2 0  6 0 7 3 9 3) :scaler (hz->radians 60) :duration dur))
	 (vib (make-oscil 2)))
    (run 
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let* ((frq (+ (env frqf)
			(* (hz->radians 10) (oscil vib))))
		(md (+ (* 2 frq)
		       (* .125 (oscil gen4 frq)))))
	   (outa i (* (env ampf)
		      (+ (* .9 (oscil gen1 md))
			 (* .1 (oscil gen2 (+ (* 8 frq) 
					      (* .2 (oscil gen3 frq)))))))
		 *output*)))))))

;(with-sound () (indri 0 .5))



;;; --------------------------------------------------------------------------------
;;; Handsome trig

(definstrument (handsome-trig beg dur amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (freqs (list 6439 6585 6860 6940 7090 7266 7362))
	 (num (length freqs))
	 (amps (let* ((v (make-vct num))
		      (lst (list 1.0  1.0  .25  .17  .2   .1   .1))
		      (scl (apply + lst)))
		 (do ((i 0 (1+ i)))
		     ((= i num))
		   (vct-set! v i (/ (list-ref lst i) scl)))
		 v))
	 (gens (let ((v (make-vector num #f)))
		 (do ((i 0 (1+ i)))
		     ((= i num))
		   (vector-set! v i (make-oscil (list-ref freqs i))))
		 v))
	 (pulse-dur .02)
	 (pulse-count 0)
	 (pulse-length (seconds->samples pulse-dur))
	 (ampf (make-env '(0 0 1 1 20 1 21 0) :duration dur :scaler amp)) ; continuous call
	 (pulsef (make-env '(0.0 0.0  0.05 1.0  0.13 1.0  0.4 0.1  1.0 0.0) :duration pulse-dur))
	 (pulses 0))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (set! pulse-count (1+ pulse-count))
	 (if (>= pulse-count pulse-length)
	     (let ((dont-click
		    ;; count pulses in current group -- at least 2, at most 4, if 2 or 3, possibly and if 4 definitely insert a space
		    (or (= pulses 4)
			(and (or (and (= pulses 2)
				      (> (random 1.0) 0.6))
				 (and (= pulses 3)
				      (> (random 1.0) 0.3)))))))
	       (set! pulse-count 0)
	       (if dont-click
		   (begin
		     (set! pulses 0)
		     (set! pulse-length (seconds->samples (+ .015 (random .005)))))
		   (begin
		     (set! pulses (1+ pulses))
		     (set! pulse-length (seconds->samples pulse-dur))
		     (mus-reset pulsef)
		     (do ((k 0 (1+ k)))
			 ((= k num))
		       (mus-reset (vector-ref gens k)))))))
	 (let ((sum 0.0))
	   (do ((k 0 (1+ k)))
	       ((= k num))
	     (set! sum (+ sum (* (vct-ref amps k) (oscil (vector-ref gens k))))))
	   (outa i (* (env ampf) 
		      (env pulsef)
		      sum) 
		 *output*)))))))

;(with-sound () (handsome-trig 0 2 .5))


;;; --------------------------------------------------------------------------------
;;; Fast calling tree cricket
;;;
;;; (use fm for noise to get the burble right, and pm for spectrum)

(definstrument (fast-calling-tree-cricket beg dur amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (pulse-dur .0167)
	 (pulsef (make-env '(0.0 0.0  0.05 0.07  0.08 0.4  0.2 0.7  0.37 0.93   0.52 1.0   0.6 0.4  0.67 0.2  0.84 0.16  0.88 0.06  0.96 0.03  1.0 0.0)
			   :duration pulse-dur))
	 (gen1 (make-oscil 4100))
	 (md (make-oscil 4100))
	 (md1 (make-oscil 205))
	 (rnd (make-rand-interp 180 .01))
	 ;; 1.0 .04 .007
	 (pulser (make-pulse-train 55.0))
	 (ampf (make-env '(0 0 1 1 20 1 21 0) :duration dur :scaler amp)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((pulse (pulse-train pulser)))
	   (if (> pulse .1)
	       (mus-reset pulsef))
	   (outa i (* (env ampf)
		      (env pulsef)
		      (oscil gen1 
			     (rand-interp rnd)
			     (+ (* .1 (oscil md))
				(* .2 (oscil md1)))))

		 *output*)))))))

;(with-sound () (fast-calling-tree-cricket 0 2 .5))


;;; --------------------------------------------------------------------------------
;;; Dog-day cicada

(definstrument (dog-day-cicada beg dur amp) ; dur ca 10 ..15 secs
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.0 0.0    0.1 0.03   0.2 0.2   0.3 0.6   0.34 0.8  0.44 1.0 
			   0.5 0.83   0.55 0.92  0.6 0.86  0.66 1.0  0.7 0.7   0.88 0.25  
			   0.91 0.24  0.93 0.02  1.0 0.0)
			 :duration dur :scaler amp))
	 (gen1 (make-oscil 7500))
	 (gen2 (make-oscil 200))
	 (rnd (make-rand-interp 200))
	 (rndf (make-env '(0 .3  .7 .3  .8 1  1 0) :scaler (hz->radians 120)))
	 (frqf (make-env '(0 -.5  .2 0  .85 0  1 -1) :scaler (hz->radians 400) :duration dur))
	 (rx (make-rxyk!cos 4000 600 8.0)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((frq (env frqf)))
	   (outa i (* (env ampf)
		      (* .5 (+ (oscil gen1 (+ frq
					      (* (env rndf) (rand-interp rnd))
					      (* .15 (oscil gen2))))
			       (rxyk!cos rx 0.0))))
	       *output*)))))))
		     


;;; --------------------------------------------------------------------------------
;;;
;;; Linnaeus' Cicada

(definstrument (linnaeus-cicada beg dur amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0 0 1 .8 10 1 11 0) :duration dur :scaler amp))
	 (frqf (make-env '(0 0  3 .02 6 0 8 .03 9.8 0  10.6 -1 11 -1) :scaler (hz->radians 450) :duration dur :base 32))
	 (gen1 (make-oscil 1280))
	 (gen2 (make-oscil 1100))
	 (gen3 (make-oscil 1460))
	 (gen4 (make-oscil (* 2 1280)))
	 (gen5 (make-oscil (* 3 1280)))
	 (rnd (make-rand-interp 1280))
	 (saw (make-sawtooth-wave 180)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let* ((frq (env frqf))
		(md (+ frq
		       (* .014 (rand-interp rnd))))
		(sw (sawtooth-wave saw)))
	   (outa i (* (env ampf)
		      (+ .75 (* .25 sw sw))
		      (+ (* .8 (oscil gen1 md))
			 (* .15 (oscil gen2 (* md (/ 1100 1280))))
			 (* .07 (oscil gen3 (* md (/ 1460 1280))))
			 (* .02 (oscil gen4 (* 2 md)))
			 (* .02 (oscil gen5 (* 3 md)))
			 ))
		 *output*)))))))


;;; --------------------------------------------------------------------------------
;;;
;;; Lyric cicada

(definstrument (lyric-cicada beg dur amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (p0 400)
	 (pulsef (make-env '(0.0 0.0  0.038 0.16  0.044 0.6  0.07 0.6   0.08 0.26  
			   0.1 0.09 0.12  0.43  0.14  0.19   0.18 0.07  0.22 0.048 
			   0.23 0.65 0.25 0.65  0.26  0.23 0.28 0.044 
			   0.3  0.55 0.31 0.31  0.35  0.05 0.38 0.01 0.39 1.0
			   0.41 0.36 0.42 0.16  0.44  0.02 0.46 0.7  0.48 0.23
			   0.5  0.1  0.55 0.01   0.56  0.97  
			   0.6  0.12 0.612 0.02 0.63  0.61 0.65 0.18 0.69 0.07 
			   0.7  0.61 0.72 0.16  0.76  0.05 0.78 0.37 0.790 0.13
			   0.82 0.02 0.83 0.4   0.85  0.12 
			   0.9  0.01 0.92 0.29  0.95  0.2 0.96 0.08
			   1.0 0)
			 :duration .02
			 :scaler 1.0))
	 (pulser (make-pulse-train 50))
	 (gen1 (make-oscil (* p0 16)))
	 (gen2 (make-oscil p0))
	 (rnd (make-rand-interp p0 (hz->radians 800)))
	 (ampf (make-env '(0 0 1 1 10 1 11 0) :duration dur :scaler amp)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let* ((pulse (pulse-train pulser)))
	   (if (> pulse .1)
	       (mus-reset pulsef))
	 (outa i (* (env ampf)
		    (env pulsef)
		    (oscil gen1 (+ (* .15 (oscil gen2))
				   (rand-interp rnd))))
	       *output*)))))))



;;; --------------------------------------------------------------------------------
;;;
;;; Confused ground-cricket

(definstrument (confused-ground-cricket beg dur amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (gen1 (make-oscil 5700))
	 (gen2 (make-oscil 5700))
	 (rnd (make-rand-interp 600 (hz->radians 166)))
	 (ampf (make-env '(0 0 1 1 10 1 11 0) :duration dur :scaler amp))
	 (songf (make-env '(0.0 0.0  0.02 0.5  0.18 0.5  0.24 0.28  0.28 0.27  0.45 0.8  0.65 1.0 0.93 0.94  1.0 0.0) :duration .4))
	 (pulsef (make-env '(0.0 0.0  0.16 0.0  0.23 0.57  0.36  0.57  0.42 0.83  0.56 1.0  0.64 0.81  0.75 0.2  0.86 0.02 1.0 0.0) :duration .01))
	 (pulse-ctr (seconds->samples (+ .01 (random .006))))
	 (song-ctr (seconds->samples (+ .5 (random .2)))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (if (<= song-ctr 0)
	     (begin
	       (set! song-ctr (seconds->samples (+ .5 (random .2))))
	       (mus-reset songf)
	       (set! pulse-ctr 0)))
	 (if (<= pulse-ctr 0)
	     (begin
	       (set! pulse-ctr (seconds->samples (+ .01 (random .006))))
	       (mus-reset pulsef)))
	 (outa i (* (env ampf)
		    (env songf)
		    (env pulsef)
		    (oscil gen1 (+ (* .1 (oscil gen2))
				   (rand-interp rnd))))
	       *output*)
	 (set! song-ctr (1- song-ctr))
	 (set! pulse-ctr (1- pulse-ctr)))))))

;(with-sound (:play #t) (confused-ground-cricket 0 3 .3))




;;; --------------------------------------------------------------------------------

(define (calling-all-animals)
  (with-sound (:play #t :scaled-to .5 :srate 44100) ;(srate needed by snd-test)
    (mosquito 0 5 560 .2)
    (mosquito 1 3 880 .05)
    (knudsens-frog 2 .5)
    (a-cricket 3 .12 4500 5400 .5 '(0 0 1 1 3 1 4 0)
	       (/ .11 3) '(0 0 1 .8 5 1 6 0 15 0))
    (let ((last-beg 3.0))
      (do ((k 0 (1+ k)))
	  ((= k 12))
	(let ((beg (+ last-beg .37 (random .08))))
	  (oak-toad beg (+ .25 (random .3)))
	  (set! last-beg beg))))
    (broad-winged-tree-cricket 5 4.0 0.1)
    (southern-cricket-frog 6 0.5)
    (long-spurred-meadow-katydid 7 .5)
    (northern-leopard-frog 8 .5)
    (southern-mole-cricket 9 6 .1)
    (green-tree-frog 10 .5)
    (spring-peeper 11 .5)
    (crawfish-frog 12 .5)
    (river-frog 13 .5)
    (indri 14 .25)
    (handsome-trig 15 2 .5)
    (fast-calling-tree-cricket 16 2 .25)
    (dog-day-cicada 17 2 .125)
    (linnaeus-cicada 18 2 .125)
    (lyric-cicada 19 2 .125)
    (confused-ground-cricket 20 3 .3)
    ))


