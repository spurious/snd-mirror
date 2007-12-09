;;; animals.scm


;;; -------- insects --------
;;; mosquito
;;; Long-spurred meadow katydid
;;; Handsome trig
;;; Dog-day cicada
;;; Linnaeus' cicada
;;; Lyric cicada
;;; Southern mole cricket
;;; Confused ground-cricket
;;; Tinkling ground-cricket
;;; Striped ground-cricket
;;; Sphagnum ground cricket
;;; Southeastern field cricket
;;; Snowy tree cricket
;;; Pine tree cricket
;;; Davis's tree cricket
;;; Broad-winged tree-cricket
;;; Fast-calling tree cricket
;;; Marsh meadow grasshopper
;;; Carolina grasshopper
;;; Slightly musical conehead

;;; -------- frogs and toads
;;; Oak toad
;;; Knudsen's frog
;;; Southern cricket frog
;;; Northern leopard frog
;;; Spring peeper
;;; Crawfish frog
;;; River frog
;;; Green tree-frog
;;; Pinewoods tree frog
;;; Squirrel tree frog
;;; Ornate chorus frog

;;; -------- mammals --------
;;; Indri

;;; -------- birds --------
;;; Fox sparrow
;;; White-throated sparrow
;;; Henslow's sparrow
;;; Field sparrow
;;; Savannah sparrow
;;; Chipping sparrow
;;; Bachman's sparrow
;;; Grasshopper sparrow
;;; Black-chinned sparrow
;;; Eastern wood-pewee (2)
;;; Tufted titmouse
;;; Least flycatcher
;;; Acadian flycatcher
;;; Swainson's thrush
;;; Carolina wren
;;; American robin
;;; Common loon (2)
;;; Hermit thrush
;;; Chuck-will's-widow
;;; California towhee
;;; Mourning dove
;;; Bobwhite
;;; Warbling vireo
;;; Great-horned owl
;;; Western tanager
;;; Pileated woodpecker
;;; Whip-poor-will
;;; Varied thrush
;;; Nashville warbler


(use-modules (ice-9 optargs) (ice-9 format))
(provide 'snd-animals.scm)

(if (not (provided? 'snd-generators.scm)) (load "generators.scm"))
(if (not (provided? 'snd-ws.scm)) (load "ws.scm"))


;;; many of these need srate=44100 since various frequencies are (well) over 10KHz
;;;   also, I have bare indices scattered around -- ideally these would be wrapped in hz->radians


;;; --------------------------------------------------------------------------------
;;;
;;; mosquito 
;;;
;;; from Richard Mankin, Reference Library of Digitized Insect Sounds, http://www.ars.usda.gov/sp2UserFiles/person/3559/soundlibrary.html
;;; need to make freq env flicks at call time (is there a comb-filter effect as it gets near?)

(definstrument (mosquito beg dur freq amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (carrier (make-oscil freq))
	 (modulator1 (make-oscil (* freq 2))) ; or 1 (but leave lower mult at 2??)
	 (modulator3 (make-oscil (* freq 3)))
	 (modulator2 (make-oscil (* freq 8))) ; or 9
	 (ampf (make-env '(0 0 .2 .5 1 .5 2 .5 3 1 4 .5 5 .5) :scaler amp :duration dur :base 32))
	 (frqf (make-env '(0 0  1 0  1.01 0.1  1.03 -0.1  1.05 0.0  3 0 3.02 .05 3.03 -0.1 3.1 0 5 0.0) :duration dur :scaler (hz->radians freq)))
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


;(with-sound (:play #t) (mosquito 0 5 560 .2) (mosquito 1 3 880 .05))


;;; --------------------------------------------------------------------------------
;;;
;;; Knudsen's frog
;;;
;;; from "The Diversity of Animal Sounds", Cornell Lab of Ornithology

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

;(with-sound (:play #t) (a-cricket 0 .12 4500 5400 .5 '(0 0 1 1 3 1 4 0) (/ .11 3) '(0 0 1 .8 5 1 6 0 15 0)))



;;; L Elliott "The Calls of Frogs and Toads"
;;; L Elliott and W Herschberger "The Songs of the Insects"
;;; L Elliott "Music of the Birds"


;;; --------------------------------------------------------------------------------
;;;
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
;;;
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
;;;
;;; Southern cricket frog

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
;;;
;;; Long-spurred meadow katydid
;;;
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
;;;
;;; Northern leopard frog (1)
;;;
;;; this is slightly low-passed, and I don't quite have the vowel right at the end

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
;;;
;;; Southern mole cricket

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
;;;
;;; Green tree-frog

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
;;;
;;; Pinewoods tree-frog

(definstrument (pinewoods-tree-frog beg dur amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0 0 1 1 20 1 21 0) :scaler amp :duration dur))

	 (pulse-dur .009)
	 (pulsef (make-env '(0.000 0.000 0.065 0.5 0.117 0.85 0.179 1.0 0.236 0.9 0.503 0.4 0.606 0.2 1.000 0.000) :duration pulse-dur))
	 (pulses (if (> (random 1.0) .6) 5 4))
	 (pulse-amps (vct .7 .9 1.0 .9 .6))

	 (pitch 205.0)
	 (gen1 (make-oscil (* 10 pitch) (* 0.5 pi)))
	 (gen3 (make-oscil (* pitch 18) (* 0.5 pi)))
	 (gen4 (make-oscil (* pitch 28) (* 0.5 pi)))

	 (pulse-ctr 0)
	 (next-pulse (seconds->samples pulse-dur))
	 (pulse-beg start)

	 (rnd (make-rand-interp (* 10 pitch) (hz->radians (* 3 pitch))))) ; not sure this actually helps
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))

	 (if (<= next-pulse 0)
	     (begin

	       (if (< pulse-ctr 3)
		   (set! (mus-frequency gen1) (* pitch 10))
		   (set! (mus-frequency gen1) (* pitch 11)))

	       (set! pulse-ctr (1+ pulse-ctr))
	       (if (>= pulse-ctr pulses)
		   (begin
		     (set! pulse-ctr 0)
		     (if (> (random 1.0) .6) 
			 (set! pulses 5)
			 (set! pulses 4))
		     (set! pulse-beg (+ pulse-beg (seconds->samples .078)))
		     (set! next-pulse (- pulse-beg i)))
		   (set! next-pulse (seconds->samples pulse-dur)))

	       (mus-reset pulsef)
	       (set! (mus-phase gen1) (* 0.5 pi))
	       (set! (mus-phase gen3) (* 0.5 pi))
	       (set! (mus-phase gen4) (* 0.5 pi))))

	 (let* ((noise (rand-interp rnd))
		(pulse-amp (vct-ref pulse-amps pulse-ctr)))
	   (outa i (* (env ampf)
		      (env pulsef)
		      pulse-amp
		      (+ (* .9 (oscil gen1 (* .1 noise)))
			 (* .08 (oscil gen3 (* .18 noise)))
			 (* .02 (oscil gen4 (* .28 noise)))))
		 *output*))
	 (set! next-pulse (1- next-pulse)))))))

;(with-sound (:play #t) (pinewoods-tree-frog 0 1 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Squirrel tree frog

(definstrument (squirrel-tree-frog-1 beg dur amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0 0 1 1 30 1 31 0) :scaler amp :duration dur))
	 (pitch 120)
	 (gen1 (make-blackman pitch 4))
	 (gen2 (make-oscil (* 10 pitch)))
	 (gen3 (make-oscil (* 24 pitch)))
	 (gen4 (make-oscil pitch))
	 (index (hz->radians .1))
	 (gen5 (make-oscil (* 14 pitch)))
	 (gen6 (make-oscil (* 6 pitch)))
	 (pulse-dur 0.24)
	 (rnd (make-rand-interp 100 (hz->radians 5)))
	 (frqf (make-env '(0 0  .2 0 .4 .75  .8 1  1.0 .5) :duration pulse-dur :scaler (hz->radians 15)))
	 (pulsef (make-env '(0 0 .5 .7 2 1 3.5 .7 4 0) :duration pulse-dur))
	 (pulser (make-pulse-train (/ 1.0 0.52)))
	 (indf (make-env '(0 .3 1 .5 2 .5 3 0) :duration pulse-dur)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (if (> (pulse-train pulser) .1)
	     (begin
	       (mus-reset frqf)
	       (mus-reset pulsef)
	       (mus-reset indf)))
	 (let* ((frq (env frqf))
		(ind (+ frq
			(* index (oscil gen4))
			(rand-interp rnd)))
		(intrp (env indf)))
	   (outa i (* (env ampf)
		      (env pulsef)
		      (blackman gen1 0.0)
		      (+ (* intrp (oscil gen2 (* 10 ind)))
			 (* (- 1.0 intrp) (oscil gen3 (* 24 ind)))
			 (* .1 (oscil gen5 (* 14 ind)))
			 (* .1 (oscil gen6 (* 6 ind)))
			 ))
		 *output*)))))))

;(with-sound (:play #t) (squirrel-tree-frog-1 0 1.0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Ornate chorus frog

(definstrument (ornate-chorus-frog beg dur amp)
  (let* ((pulse-dur 0.024)
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0 0  .1 1  20 1  20.1 0) :scaler amp :duration dur))
	 (pulsef (make-env '(0.000 0.000 0.057 0.445 0.124 0.797 0.220 0.977 0.337 1.000 0.477 0.987 0.634 0.907 0.760 0.791 0.828 0.475 0.913 0.206 1.000 0.000)
			   :duration pulse-dur))
	 (pitch 1210)
	 (gen1 (make-polyshape pitch :partials (list 1 .02  2 .95  3 .01  4 .02  5 .01 6 .04 7 .01 8 .02)))
	 (next-pulse (+ start (seconds->samples .4))))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (if (= i next-pulse)
	     (begin
	       (mus-reset pulsef)
	       (if (> (random 1.0) .8)
		   (set! next-pulse (+ next-pulse (seconds->samples (+ .25 (random .3)))))
		   (set! next-pulse (+ next-pulse (seconds->samples .4))))))
	 (outa i (* (env ampf)
		    (env pulsef)
		    (polyshape gen1 1.0))
	       *output*))))))

;(with-sound (:play #t) (ornate-chorus-frog 0 4 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Spring peeper

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
;;;
;;; Crawfish frog

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
;;;
;;; River frog
;;;
;;; original formants were much sharper, but using rxyk!cos to sharpen ours didn't seem to help
;;; animal seems to group these in 3's

(define (normalize-partials lst)
  (define (sum-partials lst sum)
    (if (null? lst)
	sum
	(sum-partials (cddr lst) (+ sum (cadr lst)))))
    
  (define (scale-partials lst scl newlst)
    (if (null? lst)
	newlst
	(scale-partials (cddr lst) scl (append newlst (list (car lst) (* scl (cadr lst)))))))
    
  (scale-partials lst (/ 1.0 (sum-partials lst 0.0)) '()))


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
;;;
;;; Indri
;;;
;;; close in spectrum, amp, freq, but the original has what sounds like a ton of reverb
;;;   if I add the reverb, it's close -- is this really what this animal sounds like?

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
;;;
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
;;;
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
;;;
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
		    (oscil gen1 (+ (* .01 (oscil gen2))
				   (rand-interp rnd))))
	       *output*)
	 (set! song-ctr (1- song-ctr))
	 (set! pulse-ctr (1- pulse-ctr)))))))

;(with-sound (:play #t) (confused-ground-cricket 0 3 .3))


;;; ------------------------------------------------------------------------------------------
;;;
;;; Tinkling ground-cricket
;;;
;;;  There's a secondary (slower) peep -- is this part of our cricket's song, or another cricket in the background?

(definstrument (tinkling-ground-cricket beg dur amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0 0 1 1 10 1 11 0) :duration dur :scaler amp))
	 (gen1 (make-oscil 7200))
	 (gen2 (make-oscil 80))
	 (pulser (make-pulse-train (/ 1.0 .15)))
	 (pulsef (make-env '(0.0 0.0  0.07 0.5  0.28 0.86  0.42 0.97  0.55 1.0  0.63 0.88  0.71 0.6  0.85 0.14  0.9 0.1 0.94 0.02 1.0 0.0) :duration .03)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((pulse (pulse-train pulser)))
	   (if (> pulse .1)
	       (mus-reset pulsef))
	   (outa i (* (env ampf)
		      (env pulsef)
		      (oscil gen1 (* .01 (oscil gen2))))
		 *output*)))))))

;(with-sound (:play #t) (tinkling-ground-cricket 0 3 .3))


;;; --------------------------------------------------------------------------------
;;;
;;; Marsh meadow grasshopper
;;;
;;; this is just a random number generator with an elaborate amplitude envelope

(definstrument (marsh-meadow-grasshopper beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 4.8)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0 0  .2 .6  .65 1.0  .87 .87 1.0 0) :duration dur :scaler amp))
	 (pulsef (make-env '(0.000  0.000   0.070  0.086   0.176  0.083   0.311  0.170   0.432  0.173  
			     0.470  0.321   0.487  0.021   0.503  0.021   0.504  0.304   0.540  0.298  
			     0.553  0.435   0.600  0.193   0.614  0.458   0.652  0.315   0.665  0.024  
			     0.689  0.018   0.699  0.638   0.725  0.582   0.727  0.027   0.790  0.009 
			     0.799  0.819   0.824  0.635   0.833  0.036   0.926  0.015   0.941  0.866
			     0.949  0.053   0.968  0.570   1.000  0.000)
			   :duration .19))
	 (pulser (make-pulse-train (/ 1.0 (+ .19 .02))))
	 (last-val 0.0))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((pulse (pulse-train pulser)))
	   (if (> pulse .1)
	       (mus-reset pulsef))
	   (let ((this-val (* (env ampf)
			      (env pulsef)
			      (- 1.0 (random 2.0)))))
	     (outa i (- last-val this-val) *output*)
	     (set! last-val this-val))))))))

;(with-sound (:play #t) (marsh-meadow-grasshopper 0 .3))


;;; --------------------------------------------------------------------------------
;;;
;;; Carolina grasshopper
;;;
;;; tricky!  spikes are at 48 Hz, but the sound they make requires 24 Hz pulse -- I presume
;;;   I'm seeing the 2 or 4 wings alternating?

(definstrument (carolina-grasshopper beg dur amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env ;'(0.000 0.000 0.032 0.115 0.151 0.044 0.181 0.485 0.254 0.841 0.620 0.431 0.731 0.051 0.757 0.292 0.898 0.620 0.984 0.380 1.000 0.000)
		          '(0.000 0.000 0.031 0.122 0.076 0.054 0.148 0.010 0.177 0.512 0.199 0.366 0.246 0.868 0.291 0.336 0.320 0.644 0.386 0.244 0.450 0.553 
			    0.535 0.142 0.583 0.424 0.611 0.427 0.727 0.081 0.778 0.315 0.835 0.319 0.899 0.600 0.972 0.356 1.000 0.000)
			 :duration dur :scaler (* 3 amp)))
	 (spikes1 (make-sum-of-sines 250 24))
	 (spikes1a (make-sum-of-sines 250 24 pi))
	 (spikes2 (make-sum-of-sines 140 48 .7))
	 (spikes3 (make-sum-of-sines 120 48 1.4))
	 (rnd (make-rand-interp 100 .0001)) ; perhaps this is too much -- it clobbers the comb filter 
	 (frqf (make-env '(0 0 1 -.4 2 0 3 -.2 4 .3 6 -1.0 ) :scaler (hz->radians .4) :duration dur))
	 (last-val 0.0))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let* ((noi (rand-interp rnd))
		(frq (+ noi (env frqf)))
		(this-val (* (env ampf)
			     (+ (* .6 (sum-of-sines spikes1 noi))
				(* .6 (sum-of-sines spikes1a noi))
				(* .4 (sum-of-sines spikes2 frq))
				(* .3 (sum-of-sines spikes3 frq))))))
	   (outa i (- this-val last-val) *output*)
	   (set! last-val this-val)))))))

;(with-sound (:play #t) (carolina-grasshopper 0 1.5 1.0))


;;; --------------------------------------------------------------------------------
;;;
;;; Striped ground cricket

(definstrument (striped-ground-cricket beg dur amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0 0 1 1 10 1 11 0) :duration dur :scaler amp))
	 (gen1 (make-oscil (* 2 6600)))
	 (gen2 (make-oscil (* 2 66) (* 0.5 (+ pi (hz->radians (* 2 66))))))
	 (gen3 (make-oscil 6600))
	 (gen4 (make-oscil 66 (* 0.5 (+ pi (hz->radians 66)))))
	 (pulser (make-pulse-train (/ 1.0 .015)))
	 (pulsef (make-env '(0.000 0.000  0.041 0.466  0.144 0.775  0.359 1.0  0.484 0.858  1.000 0.000) :duration .012))
	 (pulses 10)
	 (pulse-ctr 0)
	 (pulse-amp 0.5)
	 (long-pulser (make-pulse-train (/ 1.0 .54)))
	 (rnd (make-rand-interp (* 3 66) .04)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((pulse (pulse-train pulser))
	       (long-pulse (pulse-train long-pulser)))
	   (if (> long-pulse .1)
	       (begin
		 (set! pulse-amp 0.5)
		 (set! pulse-ctr 0)
		 (set! pulses (+ 8 (random 3)))))
	   (if (and (> pulse .1)
		    (< pulse-ctr pulses))
	       (begin
		 (mus-reset pulsef)
		 (if (> pulse-ctr 0)
		     (set! pulse-amp 1.0))
		 (set! pulse-ctr (1+ pulse-ctr))))
	   (outa i (* (env ampf)
		      pulse-amp
		      (env pulsef)
		      (+ (* .2 (oscil gen1 (* .075 (oscil gen2))))
			 (* .8 (oscil gen3 (+ (* .0125 (oscil gen4))
					      (rand-interp rnd))))))
		 *output*)))))))

;(with-sound (:play #t) (striped-ground-cricket 0 3 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Sphagnum ground cricket

(definstrument (sphagnum-ground-cricket beg dur amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (gen1 (make-oscil 8850))
	 (gen2 (make-oscil (* 8850 2)))
	 (rnd1 (make-rand-interp 885 .03))
	 (rnd2 (make-rand-interp 885 .1))
	 (ampf (make-env '(0 0 1 1 10 1 11 0) :duration dur :scaler amp))
	 (pulsef (make-env '(0.000 0.000  0.041  0.5  0.250 0.7  0.4 1.0  0.5  0.9  0.8  0.3  1.000 0.000) :duration .013))
	 (pulser (make-pulse-train (/ 1.0 .019))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (if (> (pulse-train pulser) .1)
	     (mus-reset pulsef))
	 (outa i (* (env ampf)
		    (env pulsef)
		    (+ (* .9 (oscil gen1 (rand-interp rnd1)))
		       (* .1 (oscil gen2 (rand-interp rnd2))))) ; I can't hear this unless transposed down 
	       *output*))))))

;(with-sound (:play #t) (sphagnum-ground-cricket 0 2 .3))


;;; --------------------------------------------------------------------------------
;;;
;;; Southeastern field cricket

(definstrument (southeastern-field-cricket beg dur amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (gen1 (make-oscil 4730))
	 (gen2 (make-oscil (* 2 4730)))
	 (gen3 (make-oscil (* 3 4730)))
	 (rnd (make-rand-interp (* 2 473) 1))
	 (ampf (make-env '(0 0 1 1 10 1 11 0) :duration dur :scaler amp))
	 (pulsef (make-env '(0.0 0.0  0.05  0.38  0.14 0.77  0.26  0.95  0.47  1.0  0.57  0.9  0.81 0.5  0.85  0.3  1.0 0.0) :duration .014))
	 (pulse-dur 0.027) ; occasionally a hicccup = .039, 30..100 pulses per song
	 (pulses (+ 30 (random 70)))
	 (pulse-amp 1.0)
	 (pulse-ctr (seconds->samples pulse-dur))
	 (song-ctr 0))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (if (= i song-ctr)
	     (begin
	       (set! pulses (+ 30 (random 70)))
	       (set! pulse-amp 1.0)
	       (set! pulse-ctr (seconds->samples pulse-dur))))
	 (if (and (<= pulse-ctr 0)
		  (> pulse-amp 0.0))
	     (begin
	       (if (> (random 1.0) .95)
		   (set! pulse-ctr (seconds->samples (+ pulse-dur .005 (random .01))))
		   (set! pulse-ctr (seconds->samples pulse-dur)))
	       (mus-reset pulsef)
	       (set! pulses (1- pulses))
	       (if (<= pulses 0)
		   (begin
		     (set! pulse-amp 0.0)
		     (set! song-ctr (+ i (seconds->samples (+ .2 (random .1))))))
		   (set! pulse-amp 1.0))))
	 (let ((rn (rand-interp rnd)))
	   (outa i (* (env ampf)
		      (env pulsef)
		      pulse-amp
		      (+ (* .8 (oscil gen1 0.0 rn))
			 (* .1 (oscil gen2 0.0 (* 2 rn)))
			 (* .1 (oscil gen3 0.0 (* 3 rn)))))
		 *output*))
	 (set! pulse-ctr (1- pulse-ctr)))))))


;(with-sound (:play #t) (southeastern-field-cricket 0 5 .3))


;;; --------------------------------------------------------------------------------
;;;
;;; Snowy tree cricket

(definstrument (snowy-tree-cricket beg dur amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (pitch 1690)
	 (gen1 (make-oscil pitch))
	 (gen2 (make-oscil (* pitch 2)))
	 (gen3 (make-oscil (* pitch 3)))
	 (rnd (make-rand-interp 1000 .014))
	 (ampf (make-env '(0 0 1 1 20 1 21 0) :duration dur :scaler amp))
	 (pulsef (make-env '(0.0 0.0    0.04 0.79  0.08 0.09  0.11 0.02  0.14 0.83  0.15 0.95  
			     0.21 0.05  0.26 0.02  0.29 0.79  0.31 0.89  0.35 0.07  0.38 0.04  
			     0.39 0.79  0.42 0.94  0.45 0.08  0.48 0.05  0.50 0.80  0.52 0.96  
			     0.59 0.02  0.64 0.01  0.66 0.78  0.68 0.95  0.72 0.06  0.75 0.04  
			     0.76 0.70  0.79 0.96  0.83 0.07  0.85 0.02  0.88 0.80  0.90 1.0
			     0.95 0.12  0.97 0.04  1.00 0.00)
			   :duration .352 :base .1))
	 (frqf (make-env '(0.0 0.0    0.04 1.0  0.08 0.0  0.11 0.0  0.14 1.0  0.15 0.0
			     0.21 0.0  0.26 0.0  0.29 1.0  0.31 0.0  0.35 0.0  0.38 0.0  
			     0.39 1.0  0.42 0.0  0.45 0.0  0.48 0.0  0.50 1.0  0.52 0.0
			     0.59 0.0  0.64 0.0  0.66 1.0  0.68 0.0  0.72 0.0  0.75 0.0  
			     0.76 1.0  0.79 0.0  0.83 0.0  0.85 0.0  0.88 1.0  0.90 0.0
			     0.95 0.0  0.97 0.0  1.00 0.0)
			   :duration .352 :scaler (hz->radians 60)))
	 (pulser (make-pulse-train (/ 1.0 .85))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((rn (+ (rand-interp rnd)
		      (env frqf))))
	   (if (> (pulse-train pulser) .1)
	       (begin
		 (mus-reset pulsef)
		 (mus-reset frqf)))
	   (outa i (* (env ampf)
		      (env pulsef)
		      (+ (* .9 (oscil gen1 rn))
			 (* .05 (oscil gen2 (* 2 rn)))
			 (* .05 (oscil gen3 (* 3 rn)))))
		 *output*)))))))

;(with-sound (:play #t) (snowy-tree-cricket 0 2 .3))


;;; --------------------------------------------------------------------------------
;;;
;;; Slightly musical conehead
;;;
;;; this could use some work

(definstrument (slightly-musical-conehead beg dur amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (gen1 (make-oscil 11000))
	 (gen2 (make-oscil 5500))
	 (gen3 (make-oscil 1100))
	 (rnd1 (make-rand-interp 1100 .05))
	 (rnd2 (make-rand-interp 1100 .4))
	 (ampf (make-env '(0 0 1 1 10 1 11 0) :duration dur :scaler amp))
	 (pulsef (make-env '(0.00 0.00  0.02 0.29  0.04 0.55  0.07 0.37  0.08 0.06  0.11 0.48  0.14 0.13  
			     0.15 0.69  0.18 0.31  0.20 0.07  0.22 0.77  0.23 0.52  0.25 0.77  0.26 0.21  
			     0.28 0.70  0.30 0.12  0.34 0.80  0.38 0.17  0.39 0.79  0.42 0.08  0.44 0.54 
			     0.48 0.53  0.50 0.85  0.53 0.11  0.55 0.51  0.58 0.79  0.60 0.22  0.62 0.84
			     0.65 0.09  0.67 0.56  0.70 0.91  0.74 0.81  0.77 0.10  0.79 0.70  0.83 0.51
			     0.85 0.90  0.88 0.03  0.90 0.55  0.93 0.60  0.95 0.11  0.97 0.97  1.0  0.00)
			   :duration .146))
	 (pulser (make-pulse-train (/ 1.0 .36))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (if (> (pulse-train pulser) .1)
	     (mus-reset pulsef))
	 (let ((rn (rand-interp rnd1)))
	   (outa i (* (env ampf)
		      (env pulsef)
		      (+ .6 (rand-interp rnd2))
		      (oscil gen1 
			     (+ (* .05 (oscil gen2 (* 5 rn)))
				(* .1 (oscil gen3 rn)))))
	       *output*)))))))

;(with-sound (:play #t) (slightly-musical-conehead 0 2 .4))


;;; --------------------------------------------------------------------------------
;;;
;;; Pine tree cricket


(definstrument (pine-tree-cricket beg dur amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (pulse-dur .014)
	 (pulsef (make-env '(0.000 0.000 0.027 0.196 0.104 0.448 0.236 0.773 0.341 0.910 0.416 0.975 0.532 1.0
			     0.671 0.868 0.751 0.711 0.833 0.504 0.926 0.160 1.000 0.000)
			   :duration pulse-dur))
	 (gen1 (make-oscil 3580))
	 (gen2 (make-oscil (* 3 3580)))
	 (frqf (make-env '(0 1 1 0) :scaler (hz->radians 100) :duration pulse-dur))
	 (pulser (make-pulse-train (/ 1.0 .022)))
	 (rnd (make-rand-interp 100 .004))
	 (ampf (make-env '(0 0 1 1 20 1 21 0) :duration dur :scaler amp)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((pulse (pulse-train pulser))
	       (noise (+ (env frqf)
			 (rand-interp rnd))))
	   (if (> pulse .1)
	       (begin
		 (mus-reset frqf)
		 (mus-reset pulsef)))
	   (outa i (* (env ampf)
		      (env pulsef)
		      (+ (* .97 (oscil gen1 noise))
			 (* .03 (oscil gen2 (* 3 noise)))))
		 *output*)))))))

;(with-sound (:play #t) (pine-tree-cricket 0 2 .25))


;;;--------------------------------------------------------------------------------
;;;
;;; Davis's tree cricket

(definstrument (davis-tree-cricket beg dur amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (pulse-dur .013)
	 (pulsef (make-env '(0.000 0.000 0.079 0.395 0.245 0.925 0.410 1.000 0.470 0.874 
			     0.554 0.549 0.614 0.312 0.728 0.170 1.000 0.000)
			   :duration pulse-dur))
	 (pitch 2420)
	 (gen1 (make-oscil pitch))
	 (gen2 (make-oscil (* 2 pitch)))
	 (pulser (make-pulse-train (/ 1.0 .014)))
	 (rnd (make-rand-interp 150 .004))
	 (ampf (make-env '(0 0 1 1 20 1 21 0) :duration dur :scaler amp)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((pulse (pulse-train pulser))
	       (noise (rand-interp rnd)))
	   (if (> pulse .1)
	       (mus-reset pulsef))
	   (outa i (* (env ampf)
		      (env pulsef)
		      (+ (* .93 (oscil gen1 noise))
			 (* .07 (oscil gen2 (* 2 noise)))))
		 *output*)))))))

;(with-sound (:play #t) (davis-tree-cricket 0 2 .25))


;;; --------------------------------------------------------------------------------
;;;
;;; Fox sparrow

(definstrument (fox-sparrow beg dur amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (amp-envs (make-vector 10 #f))
	 (frq-envs (make-vector 10 #f))
	 (gen1 (make-oscil 0.0))
	 (gen2 (make-oscil 0.0))
	 (peep 0)
	 (begs (vector 0.0   0.3  0.6   0.93  1.23  1.49 1.74  1.98  2.12  2.29))  
	 (ends (vector 0.12  0.46 0.85  1.17  1.44  1.7  1.95  2.08  2.26  2.45))
	 (durs (let ((v (make-vector 10 0.0)))
		 (do ((i 0 (1+ i)))
		     ((= i 10))
		   (vector-set! v i (- (vector-ref ends i) (vector-ref begs i))))
		 v))
	 (scls (vector .09 .19 .22 .19 .27 .23 .21 .04 .17 .17))
	 (amps (vector (list 0.000 0.000  0.17 0.13  0.38 0.67  0.64 1.0   0.78 0.79  0.9  0.04  1.0 0.0)
		       (list 0.000 0.000  0.15 0.15  0.27 0.67  0.37 0.89  0.69 1.0   0.79 0.6   0.8  0.05  1.0 0.0)
		       (list 0.000 0.000  0.11 0.28  0.18 0.66  0.35 0.98  0.90 0.92  1.0 0.0)
		       (list 0.000 0.000  0.11 0.28  0.14 0.70  0.32 0.98  0.85 0.84  1.0 0.0)
		       (list 0.000 0.000  0.11 0.28  0.14 0.70  0.32 0.98  0.85 0.84  1.0 0.0)
		       (list 0.000 0.000  0.15 0.86  0.24 1.00  0.63 0.64  0.89 0.61  1.0 0.0)
		       (list 0.000 0.000  0.27 0.80  0.37 1.00  0.63 0.64  0.88 0.51  1.0 0.0)
		       (list 0.000 0.000  0.08 0.48  0.37 1.00  0.88 0.76  1.00 0.0)
		       (list 0.000 0.000  0.12 0.43  0.24 1.00  0.59 0.72  0.88 0.35  1.0 0.0)
		       (list 0.000 0.000  0.12 0.43  0.24 1.00  0.59 0.72  0.88 0.35  1.0 0.0)))
	 (low-frqs (vector  4260 4010 3910 4970 3360 3160 2810 2310 2700 2700))
	 (high-frqs (vector 5120 4170 5420 5520 4220 3410 5470 2460 3710 3710))
	 (frqs (vector (list 0 1 1 0)
		       (list 0 0 1 .5 8 .6 9 1.0)
		       (list 0 1 1 .5 2 .2 3 0)
		       (list 0 1 1 0 5 0)
		       (list 0 1 1 .3 2 0)
		       (list 0 1 1 1)
		       (list 0 1 2 .2 3 0)
		       (list 0 0 1 .9 2 1)
		       (list 0 1 1 .4 2 0)
		       (list 0 1 1 .3 2 0)))
	 (song-start start)
	 (next-song-start (+ start (seconds->samples (+ 8.75 (random .1)))))
	 (next-peep (+ start (seconds->samples (vector-ref begs 1))))
	 (rnd (make-rand-interp 100 .01)))
    (do ((i 0 (1+ i)))
	((= i 10))
      (vector-set! amp-envs i (make-env (vector-ref amps i) 
					:scaler (/ (* amp (vector-ref scls i)) .27) 
					:duration (vector-ref durs i)))
      (vector-set! frq-envs i (make-env (vector-ref frqs i) 
					:scaler (hz->radians (- (vector-ref high-frqs i) (vector-ref low-frqs i))) 
					:offset (hz->radians (vector-ref low-frqs i))
					:duration (vector-ref durs i))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (if (= i next-song-start)
	     (begin
	       (set! peep -1)
	       (set! song-start next-song-start)
	       (set! next-song-start (+ next-song-start (seconds->samples (+ 8.75 (random .1)))))))
	 (if (= i next-peep)
	     (begin
	       (set! peep (1+ peep))
	       (mus-reset (vector-ref amp-envs peep))
	       (mus-reset (vector-ref frq-envs peep))
	       (if (< peep 9)
		   (set! next-peep (+ song-start (seconds->samples (vector-ref begs (1+ peep)))))
		   (set! next-peep next-song-start))))
	 (let ((frq (+ (env (vector-ref frq-envs peep))
		       (rand-interp rnd))))
	   (outa i (* (env (vector-ref amp-envs peep))
		      (oscil gen1 frq 
			     (* .03 (oscil gen2 (* 2 frq)))))
		 *output*)))))))

;(with-sound (:play #t) (fox-sparrow 0 3 .25))


	 
;;; --------------------------------------------------------------------------------
;;;
;;; White-throated sparrow

(definstrument (white-throated-sparrow beg amp)
  (let* ((start (seconds->samples beg))
	 (dur (+ 3.25 (random .2)))
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.00 0.00  0.04 0.44  0.09 0.59  0.12 0.59  0.16 0.75  0.17 0.00  
			   0.22 0.00
			   0.25 0.76  0.28 0.89  0.29 0.14  0.31 0.93  0.34 0.87  0.35 0.14  0.36 0.85  
			   0.37 0.84  0.38 0.00  
			   0.42 0.00  0.44 0.86  0.46 0.83  0.47 0.91  0.51 0.92  
			   0.52 0.87  0.53 0.00  
			   0.57 0.00  0.58 0.65  0.60 0.88  0.61 0.14  0.62 0.95  
			   0.63 0.94  0.64 0.20  0.66 0.99  0.67 0.88  0.68 0.00  
			   0.72 0.00  0.73 0.65  
			   0.75 0.82  0.76 0.13  0.77 0.78  0.78 0.88  0.79 0.83  0.797 0.12 0.803 0.12 0.81 0.79  
			   0.82 0.87  0.83 0.00  
			   0.87 0.00  0.89 0.58  0.91 0.75  0.917 0.13 0.923 0.13  0.93 0.73  
			   0.94 0.84  0.95 0.77  0.957 0.12 0.963 0.12  0.97 0.81  0.98 0.79  1.00 0.00)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0 0 .17 0.0 .21 1 
			   0.22 1.5 0.23 1 0.41 1
			   0.42 1.25 0.43 1 0.56 1
			   0.57 1.25 0.58 1 0.71 1
			   0.72 1.25 0.73 1 0.86 1
			   0.87 1.25 0.88 1 
			   1 1) 
			 :scaler (hz->radians 720) :duration dur))
	 (pitch 3800)
	 (gen1 (make-oscil pitch))
	 (gen2 (make-oscil (* 2 pitch)))
	 (gen3 (make-oscil (* 3 pitch)))
	 (rnd (make-rand-interp 30 .005)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((frq (+ (env frqf)
		       (mus-random 0.01)
		       (rand-interp rnd))))
	   (outa i (* (env ampf)
		      (+ (* .9 (oscil gen1 frq))
			 (* .025 (oscil gen2 (* 2 frq)))
			 (* .075 (oscil gen3 (* 3 frq)))))
		 *output*)))))))

;(with-sound (:play #t) (white-throated-sparrow 0 .25))


;;; --------------------------------------------------------------------------------
;;;
;;; Henslow's sparrow

(definstrument (henslows-sparrow beg amp)
  ;; "the poorest vocal effort of any bird" -- R T Peterson
  (let* ((start (seconds->samples beg))
	 (dur 0.24)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.00 0.00  0.05 0.42  0.08 0.03  0.10 0.30  0.12 0.02  
			   0.21 0.00  0.22 0.69  0.23 0.34  0.24 0.84  0.26 0.64  
			   0.28 0.04  0.29 0.11  0.30 0.47  0.32 0.00  0.46 0.00  
			   0.48 0.04  0.52 1.00  0.53 0.97  0.54 0.44  0.55 0.80  
			   0.57 0.02  0.58 0.07  0.59 0.72  0.60 0.45  0.61 0.79  
			   0.62 0.66  0.63 0.81  0.66 0.05  0.67 0.16  0.73 0.50  
			   0.75 0.52  0.78 0.04  0.81 0.03  0.82 0.08  0.85 0.47  
			   0.87 0.22  0.88 0.39  0.90 0.09  0.91 0.36  0.96 0.39
			   0.98 0.07  1.00 0.00)
			 :duration dur :scaler amp))
	 (gen1 (make-oscil 0.0))
	 (frqf (make-env '(0.00 9310 0.05 9560 0.08 9480 0.10 9900 0.11 8140 0.12 9900
			   0.21 9980 0.22 8630 0.23 8800 0.24 8400 0.26 8800  
			   0.28 8600 0.29 8800 0.30 8300 0.32 8100 0.46 8100  
			   0.48 5600 0.49 5200 0.52 6200 0.54 4800 0.55 6600  
			   0.57 5800 0.58 5800 0.59 6200 0.60 6200
			   0.62 5800 0.66 3600 0.67 4400 0.73 3900 0.78 3100 0.85 4900  
			   0.88 3600 0.90 3900 0.91 4400 1.00 3900)
			 :duration dur :scaler (hz->radians 1.0))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (oscil gen1 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (henslows-sparrow 0 .25))


;;; --------------------------------------------------------------------------------
;;;
;;; Eastern wood-pewee

(definstrument (eastern-wood-pewee-1 beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 1.07)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000  0.037 0.894  0.045 0.711  0.061 0.845  0.072 0.760  0.076 0.912 
			   0.084 0.838  0.099 0.982  0.111 0.729  0.124 0.879  0.142 0.011 0.15 0.01  0.165 0.778 
			   0.172 0.601  0.180 0.706  0.212 0.441  0.258 0.227  0.298 0.325  0.312 0.564 
			   0.334 0.312  0.365 0.399  0.416 0.260  0.475 0.196  0.555 0.356  0.631 0.363 
			   0.712 0.294  0.746 0.464  0.753 0.369  0.776 0.508  0.799 0.425  0.825 0.479 
			   0.869 0.485  0.877 0.567  0.907 0.541  0.918 0.459  0.942 0.513  0.977 0.366 
			   1.000 0.000)
			 :duration dur :scaler amp))
	 (gen1 (make-oscil 0.0))
	 (gen2 (make-oscil 0.0))
	 (gen3 (make-oscil 0.0))
	 (frqf (make-env '(0 3370 .03 4300 .1 4600  .14 3400 0.15 4400 .16 3700 .18 4400 .24 4700 .3 4600 .34 3600 .4 3700 .6 3800 .8 4000 1.0 3900)
			 :duration dur :base .1 :scaler (hz->radians 1.0)))
	 )
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((frq (env frqf)))
	   (outa i (* (env ampf)
		      (+ (* .905 (oscil gen1 frq))
			 (* .025 (oscil gen2 (* 2 frq)))
			 (* .025 (oscil gen3 (* 3 frq)))))
	       *output*)))))))

;(with-sound (:play #t) (eastern-wood-pewee-1 0 .25))

(definstrument (eastern-wood-pewee-2 beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 1.07)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000  0.055 0.665  0.081 0.657  0.101 0.456  0.140 0.572
			   0.165 0.477  0.219 0.564  0.288 0.526  0.306 0.668  0.328 0.613 
			   0.387 0.830  0.402 1.000  0.434 0.768  0.455 0.214  0.470 0.173 
			   0.484 0.387  0.499 0.631  0.512 0.229  0.559 0.142  0.582 0.165 
			   0.698 0.085  1.000 0.000)
			 :duration dur :scaler amp))
	 (gen1 (make-oscil 0.0))
	 (gen2 (make-oscil 0.0))
	 (gen3 (make-oscil 0.0))
	 (frqf (make-env '(0 3250 .1 4400 .2 4800 .3 4800 .47 4000 .49 6300 .51 3600 1.0 2800)
			 :duration dur :base .03 :scaler (hz->radians 1.0)))
	 (indf (make-env '(0 0 .35 0 .55 1 1 1) :duration dur)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((frq (env frqf))
	       (ind (env indf)))
	   (outa i (* (env ampf)
		      (+ (* .9 (oscil gen1 frq))
			 (* ind .1 (oscil gen2 (* 2 frq)))
			 (* (- 1.0 ind) .075 (oscil gen3 (* 3 frq)))))
	       *output*)))))))

;(with-sound (:play #t) (eastern-wood-pewee-2 0 .25))


;;; --------------------------------------------------------------------------------
;;;
;;; Field sparrow

(definstrument (field-sparrow beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 2.92)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000  0.025 0.201  0.095 0.307  0.113 0.235  0.122 0.005  0.146 0.000 
			   0.167 0.696  0.201 0.430  0.241 0.325  0.243 0.000  0.265 0.000 
			   0.287 0.840  0.300 0.791  0.312 0.567  0.354 0.369  0.365 0.000  0.388 0.000 
			   0.405 0.853  0.430 0.621  0.449 0.387  0.470 0.314  0.477 0.000  0.493 0.000 
			   0.511 0.887  0.529 0.796  0.552 0.402  0.566 0.327  0.578 0.000  0.594 0.000 
			   0.614 0.966  0.629 0.446  0.645 0.273  0.649 0.000  0.65 0.0 0.664 0.026 
			   0.673 1.0    0.690 0.459  0.706 0.000  0.714 0.031 
			   0.719 0.892  0.74 0.0 0.747 0.000 
			   0.755 0.851  0.773 0.0 0.781 0.000 
			   0.790 0.869  0.81 0.0 0.814 0.000 
			   0.827 0.827  0.845 0.0 0.849 0.000 
			   0.861 0.786  0.878 0.0 0.882 0.000 
			   0.894 0.716  0.912 0.0 0.918 0.000 
			   0.925 0.711  0.943 0.0 0.949 0.000 
			   0.959 0.657  0.97 0.0 0.975 0.000 
			   0.984 0.536  0.993 0.149 
			   1.000 0.000)
			 :duration dur :scaler amp))
	 (gen1 (make-oscil 0.0))
	 (gen2 (make-oscil 0.0))
	 (frqf (make-env '(0.000 4300  0.025 4300  0.1 3300 0.122 3300
			   0.146 4300  0.18 4300  0.23 3300 0.243 3300 
			   0.265 4300  0.3 4300  0.35 3300 0.365 3300
			   0.388 4300  0.42 4300  0.46 3300 0.477 3300
			   0.493 4300  0.52 4300  0.56 3300 0.578 3300
			   0.594 4300  0.61 4300  0.63 3300 0.649 3300 0.65 4300
			   0.664 4300  0.68 4300  0.714 3300
			   0.716 4300  0.74 3200 0.747 4300 
			   0.75 4300  0.773 3200 0.781 4300 
			   0.785 4300  0.81 3200 0.814 4300 
			   0.82 4300  0.845 3200 0.849 4300 
			   0.85 4300  0.878 3200 0.882 4300 
			   0.89 4300  0.912 3200 0.918 4300 
			   0.92 4300  0.943 3200 0.949 4300 
			   0.95 4300  0.97 3200 0.975 4300 
			   0.98 4300  0.993 3200
			   1.000 3300)
			 :duration dur :scaler (hz->radians 1.0))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((frq (env frqf)))
	   (outa i (* (env ampf)
		      (+ (* .99 (oscil gen1 frq))
			 (* .01 (oscil gen2 (* 2 frq)))))
	       *output*)))))))

;(with-sound (:play #t) (field-sparrow 0 .25))


;;; --------------------------------------------------------------------------------
;;;
;;; Tufted titmouse

(definstrument (tufted-titmouse beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 1.0)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.00  0.034 0.637  0.060 0.591  0.074 0.458  0.095 0.872  0.119 0.473  0.185 0.033  0.211 0.102 
			   0.233 0.00  0.384 0.00   0.425 0.926  0.447 0.898  0.461 0.665  0.471 1.0    0.497 0.578  0.529 0.422  0.565 0.054  0.594 0.107 
			   0.616 0.00  0.755 0.00   0.807 0.905  0.829 0.870  0.837 0.675  0.847 0.992  0.867 0.739  0.891 0.486  0.942 0.056  0.970 0.130 
			   1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.0 3730 .04 3900 .07 3770  .09 2800    .17 2470  .19 2050  .21 2320  .23 2100
			   .38 3730  .42 3900  .45 3770  .46 2760 .53 2470  .55 2050  .58 2320 .6 2100
			   .75 3730  .79  3900  .83 3770  .84 2720 .91 2470  .94 2050  .96 2320 1.0 2100)
			 :duration dur :base .1 :scaler (hz->radians 1.0)))
	 (gen1 (make-oscil 0.0))
	 (gen2 (make-oscil 0.0)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((frq (env frqf)))
	   (outa i (* (env ampf)
		      (+ (* .99 (oscil gen1 frq))
			 (* .01 (oscil gen2 (* frq 2)))))
	       *output*)))))))

;(with-sound (:play #t) (tufted-titmouse 0 .3))


;;; --------------------------------------------------------------------------------
;;;
;;; Savannah sparrow
;;;
;;; 8 separate calls make up a song

(definstrument (savannah-sparrow beg amp)

  (define (savannah-1 beg amp)
    ;; peeps
    (let* ((start (seconds->samples beg))
	   (dur .05)
	   (stop (+ start (seconds->samples dur)))
	   (hi-pitch 9000)
	   (lo-pitch 7460)
	   (ampf (make-env '(0 0 1 1 2 0) :duration dur :scaler amp))
	   (frqf (make-env '(0 1 1 0) :duration dur :scaler (hz->radians (- hi-pitch lo-pitch)) :offset (hz->radians lo-pitch)))
	   (gen1 (make-oscil 0.0))
	   (gen2 (make-oscil 0.0))
	   (gen3 (make-oscil 80)))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (let ((frq (env frqf)))
	     (outa i (* (env ampf)
			(+ .8 (* .2 (abs (oscil gen3))))
			(+ (* .98 (oscil gen1 frq))
			   (* .02 (oscil gen2 (* 2 frq)))))
	       *output*)))))))

  (define (savannah-2 beg amp pitch)
    ;; buzz
    (let* ((start (seconds->samples beg))
	   (dur .008)
	   (stop (+ start (seconds->samples dur)))
	   (hi-pitch (+ pitch 400))
	   (lo-pitch pitch)
	   (ampf (make-env '(0.000 0.000 0.094 0.228 0.148 0.832 0.248 1.000 0.364 0.695 0.522 0.586 0.634 0.284 0.801 0.558 0.891 0.102 1.000 0.000)
			   :duration dur :scaler amp))
	   (frqf (make-env '(0 1 1 0) :duration dur :scaler (hz->radians (- hi-pitch lo-pitch)) :offset (hz->radians lo-pitch)))
	   (gen1 (make-oscil 0.0))
	   (rnd (make-rand-interp 300 .03)))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf) 
		      (oscil gen1 (+ (env frqf)
				     (rand-interp rnd))))
		 *output*))))))

  (define (savannah-3 beg amp)
    ;; ticks
    (let* ((start (seconds->samples beg))
	   (dur .004)
	   (stop (+ start (seconds->samples dur)))
	   (rnd (make-rand 4000 .4))
	   (gen1 (make-oscil 8000))
	   (ampf (make-env '(0 0 1 1 2 .3 10 0) :duration dur :base 32 :scaler amp)))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf) 
		      (oscil gen1 (rand rnd)))
		 *output*))))))
      
    
  (define (savannah-4 beg amp)
    (let* ((start (seconds->samples beg))
	   (dur .034)
	   (stop (+ start (seconds->samples dur)))
	   (rnd (make-rand 12000 .1))
	   (gen1 (make-oscil 6400))
	   (frqf (make-env '(0 .5 1 0 2 1 4 1) :scaler (hz->radians 400) :duration dur))
	   (ampf (make-env '(0.000 0.000 0.045 0.018 0.067 0.196 0.096 0.004 0.166 0.032 0.196 0.207 0.209 0.116 
			     0.239 0.575 0.249 0.639 0.280 0.063 0.297 0.032 0.312 0.070 0.339 0.021 0.372 0.049 
			     0.378 0.544 0.391 0.088 0.437 0.211 0.441 0.611 0.455 0.218 0.480 0.140 0.498 0.253 
			     0.506 0.593 0.516 0.228 0.532 0.098 0.551 0.200 0.569 0.544 0.593 0.253 0.630 0.540 
			     0.660 0.133 0.684 0.540 0.722 0.158 0.748 0.604 0.760 0.779 0.787 0.260 0.824 1.000 
			     0.847 0.249 0.878 0.874 0.917 0.204 0.976 0.056 1.000 0.000)
			   :duration dur :scaler amp)))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf) 
		      (oscil gen1 (+ (env frqf) (rand rnd))))
		 *output*))))))
      
  (define (savannah-5 beg amp)
    (let* ((start (seconds->samples beg))
	   (dur .071)
	   (stop (+ start (seconds->samples dur)))
	   (rnd (make-rand 12000 .1))
	   (gen1 (make-oscil 8500))
	   (ampf (make-env '(0.000 0.000 0.067 0.060 0.084 0.432 0.098 0.414 0.111 1.000 0.123 0.267 0.148 0.028 
			     0.160 0.877 0.181 0.151 0.189 0.007 0.305 0.007 0.316 0.347 0.331 0.225 0.341 1.000 
			     0.353 0.382 0.360 0.137 0.374 0.039 0.388 0.053 0.398 0.919 0.404 0.688 0.415 0.196 
			     0.438 0.000 0.530 0.000 0.542 0.502 0.561 0.151 0.573 0.958 0.586 0.218 0.599 0.035 
			     0.616 0.067 0.623 0.811 0.642 0.144 0.661 0.000 0.767 0.000 0.785 0.225 0.799 0.923 
			     0.822 0.000 0.853 0.000 0.861 0.674 0.880 0.053 0.906 0.000 1.000 0.000)
			   :duration dur :scaler amp)))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf) 
		      (oscil gen1 (rand rnd)))
		 *output*))))))
      
  (define (savannah-6 beg amp)
    (let* ((start (seconds->samples beg))
	   (dur .023)
	   (stop (+ start (seconds->samples dur)))
	   (rnd (make-rand 3500 .15))
	   (gen1 (make-oscil 3600))
	   (ampf (make-env '(0.000 0.000 0.297 0.323 0.339 0.547 0.388 0.891 0.439 1.000 0.553 0.975 0.591 0.295 
			     0.615 0.168 0.678 0.011 0.731 0.105 0.758 0.709 0.800 0.312 0.884 0.077 1.000 0.000)
			   :duration dur :scaler amp)))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf) 
		      (oscil gen1 (rand rnd)))
		 *output*))))))
    
  (define (savannah-7 beg amp)
    (let* ((start (seconds->samples beg))
	   (dur .053)
	   (stop (+ start (seconds->samples dur)))
	   (hi-pitch 8250)
	   (lo-pitch 6900)
	   (ampf (make-env '(0 0 2 1 3 0) :duration dur :scaler amp))
	   (frqf (make-env '(0 0 1 1) :duration dur :scaler (hz->radians (- hi-pitch lo-pitch)) :offset (hz->radians lo-pitch)))
	   (gen1 (make-oscil 0.0))
	   (gen2 (make-oscil 220)))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (let ((frq (env frqf)))
	     (outa i (* (env ampf)
			(+ .25 (* .75 (abs (oscil gen2))))
			(oscil gen1 frq))
	       *output*)))))))

  (define (savannah-8 beg amp)
    (let* ((start (seconds->samples beg))
	   (dur .023)
	   (stop (+ start (seconds->samples dur)))
	   (gen1 (make-oscil 3800))
	   (gen2 (make-oscil 150))
	   (ampf (make-env '(0.000 0.000 0.138 0.098 0.199 0.218 0.258 0.018 0.367 0.404 0.422 0.361 0.462 0.011 
			     0.549 0.782 0.639 0.519 0.665 0.000 0.678 0.379 0.707 1.000 0.801 0.551 0.835 0.165 
			     0.850 0.337 1.000 0.000)
			   :duration dur :scaler amp)))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf) 
		      (oscil gen1 0.0 (* 2.0 (oscil gen2))))
		 *output*))))))
    
  ;; -------- 
  (savannah-1 beg (* amp .21))
  (savannah-1 (+ beg .35) (* amp .45))
  (savannah-1 (+ beg .35 .28) (* amp .51))
  (savannah-1 (+ beg .35 .28 .24) (* amp .64))
  (savannah-1 (+ beg .35 .28 .24 .26) amp)
  (savannah-1 (+ beg .35 .28 .24 .26 .17) amp)

  (savannah-4 (+ .97 beg) (* amp .21))

  (do ((i 0 (1+ i)))
      ((= i 6))
    (savannah-3 (+ beg 1.02 (* i .014)) (* amp .12)))

  (savannah-5 (+ beg 1.19) (* amp .2))
  (savannah-5 (+ beg 1.36) (* amp .2))

  (savannah-6 (+ beg 1.46) (* amp .15))
  (savannah-6 (+ beg 1.5) (* amp .15))

  (let* ((beg2 0.0)
	 (repeats 20)
	 (af-incr (/ .6 repeats)))
    (do ((i 0 (1+ i))
	 (beg2 0.0 (+ beg2 .004))
	 (af af-incr (+ af af-incr)))
	((= i repeats))
      (savannah-2 (+ beg 1.58 beg2) (* amp af) 5250)))
  
  (let ((af .24))
    (do ((i 0 (1+ i)))
	((= i 40))
      (savannah-2 (+ beg 1.29 .36 (* i .0145)) (* amp af) 5600)
      (if (< i 20)
	  (set! af (+ af .004))
	  (set! af (- af .004)))))

  (savannah-7 (+ beg 2.27) (* .4 amp))

  (let ((dist 0.01))
    (do ((i 0 (1+ i))
	 (beg2 (+ beg 2.36) (+ beg2 dist))
	 (af .1 (* af .85)))
	((= i 20))
      (savannah-8 beg2 (* amp af))
      (set! dist (+ dist .001)))))

;(with-sound (:play #t) (savannah-sparrow 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Chipping sparrow

(definstrument (chipping-sparrow beg amp)
  (let* ((start (seconds->samples beg))
	 (repeats (+ 20 (random 25)))
	 (total-dur (* repeats .068))
	 (stop (+ start (seconds->samples total-dur)))
	 (ampf (make-env '(0 0 1 .7 2 1 10 1 11 0) :duration total-dur :scaler amp))
	 (dur .055)
	 (pulsef (make-env '(0.000 0.000 0.049 0.091 0.179 0.636   0.253 0.186 0.293 0.518 0.361 0.170 0.413 0.079 
			   0.503 0.253 0.609 0.976 0.660 0.937 0.742 0.688 0.783 0.292 0.853 0.043 0.913 0.119 1.000 0.000)
			 :duration dur))
	 (frqf (make-env '(0 7600 .1 7900
			   .18 8700 .2 9000 .23 8300
			   .32 5300 .4 4300  .5 4800
			   .6 5600 0.8 6400 1.0 5400)
			 :duration dur :base 32 :scaler (hz->radians 1.0)))
	 (gen1 (make-oscil 0.0))
	 (pulser (make-pulse-train (/ 1.0 .068)))
	 (rnd (make-rand-interp 100 .02)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let* ((pulse (pulse-train pulser))
		(frq (+ (env frqf)
			(rand-interp rnd))))
	   (if (> pulse .1)
	       (begin
		 (mus-reset pulsef)
		 (mus-reset frqf)))
	   (outa i (* (env ampf)
		      (env pulsef)
		      (oscil gen1 frq))
	       *output*)))))))

;(with-sound (:play #t) (chipping-sparrow 0 .3))


;;; --------------------------------------------------------------------------------
;;;
;;; Least flycatcher

(definstrument (least-flycatcher beg amp)
  (let* ((start (seconds->samples beg))
	 (call1-dur .032)
	 (stop1 (+ start (seconds->samples call1-dur)))

	 (ampf1 (make-env '(0.000 0.000 0.223 0.158 0.386 0.379 0.617 1.0 0.679 0.929 0.810 0.458 1.000 0.000) :duration call1-dur :scaler amp))
	 (gen1 (make-oscil 0.0))
	 (gen11 (make-oscil 0.0))
	 (frqf1 (make-env '(0 3000 .4 6250 .5 6400 1.0 6000) :duration call1-dur :scaler (hz->radians 1.0)))
	 
	 (pause .065)
	 (start2 (+ start (seconds->samples pause)))
	 (call2-dur .04)
	 (stop2 (+ start2 (seconds->samples call2-dur)))

	 (ampf2 (make-env '(0.000 0.000 0.088 0.124 0.157 0.258 0.198 0.202 0.237 0.264 0.273 0.823 0.286 0.419 
			    0.328 0.814 0.343 1.000 0.360 0.329 0.409 0.413 0.435 0.935 0.448 0.295 0.481 0.183 
			    0.539 0.901 0.563 0.444 0.619 0.373 0.644 0.944 0.662 0.311 0.724 0.053 0.756 0.186 
			    0.782 0.432 0.823 0.512 0.865 0.174 0.886 0.230 1.000 0.000)
			  :duration call2-dur :scaler (* .5 amp)))
	 (gen2 (make-oscil 4600))
	 (gen21 (make-oscil 5600))
	 (gen22 (make-rand-interp 2000 (hz->radians 1000))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop1))
	 (let ((frq (env frqf1)))
	   (outa i (* (env ampf1)
		      (+ (* .9 (oscil gen1 frq))
			 (* .1 (oscil gen11 (* 2 frq)))))
	       *output*)))
       (do ((i start2 (+ i 1)))
	   ((= i stop2))
	 (let ((noise (rand-interp gen22)))
	 (outa i (* (env ampf2)
		    (+ (* .7 (oscil gen2 noise))
		       (* .3 (oscil gen21 noise))))
	       *output*)))))))

;(with-sound (:play #t) (least-flycatcher 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Acadian flycatcher

(definstrument (acadian-flycatcher beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.3)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.102 0.301 0.221 0.705 0.287 0.332 0.357 0.801 0.406 0.385 0.45 0 0.55 0
			   0.567 0.298 0.623 1.0 0.706 0.727 0.729 0.292 0.860 0.239 0.885 0.484 1.000 0.000)
			 :duration dur :scaler amp))
	 (gen1 (make-oscil 0.0))
	 (gen2 (make-oscil 0.0))
	 (frqf (make-env '(0 2800 .075 3600 .11 4200 .22 4900 .28 3200 .35 5400 .45 4200 .47 4000
			   .55 4900 .62 5000 .7 5500 .75 5200 .8 5500 .87 5400 1.0 2800)
			 :duration dur :scaler (hz->radians 1.0) :base 32)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((frq (env frqf)))
	   (outa i (* (env ampf)
		      (+ (* .95 (oscil gen1 frq))
			 (* .05 (oscil gen2 (* 2 frq)))))
	       *output*)))))))

;(with-sound (:play #t) (acadian-flycatcher 0 .25))


;;; --------------------------------------------------------------------------------
;;;
;;; Swainson's thrush
;;;
;;;   are there really multiphonics in this birdsong? 
;;;   also, is this a song that uses both parts of the syrinx? -- I think the doubled stuff is reverb

(definstrument (swainsons-thrush beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 2.0)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.058 0.146 0.075 0.127 
			   0.085 0.000 0.113 0.000 0.143 0.481 0.150 0.516 0.227 0.735 
			   0.238 0.000 0.260 0.000 0.289 0.474 0.311 0.423 
			   0.316 0.000 0.337 0.000 0.350 0.559 0.355 0.805 0.367 0.895 0.413 0.990 0.427 0.598 0.446 0.373 
			   0.452 0.066 0.460 0.103 0.470 0.651 0.492 0.828 0.510 0.502 0.531 0.457 
			   0.541 0.000 0.568 0.000 0.569 0.424 0.587 0.548 0.603 0.553 0.613 0.061 0.645 0.633 0.686 0.760 
			   0.703 0.000 0.726 0.000 0.727 0.236 0.735 0.037 0.793 0.301 0.815 0.125 0.835 0.130 
			   0.847 0.000 0.868 0.000 0.931 0.180 
			   1.000 0.000)
			 :duration dur :scaler amp))
	 (gen1 (make-polyshape 0.0 :partials (list 1 0.96  2 0.01  3 0.02 4 0.001 5 .005)))
	 (gen2 (make-oscil 0.0)) ; eschew aliasing
	 (intrpf (make-env '(0 1 .7 1 .75 0 1 0) :duration dur))
	 (frqf (make-env '(0.000 0.000 
				 0.01 0.175 0.015 0.264 
				 0.02 0.172 0.025 0.263 
				 0.03 0.171 0.035 0.260 
				 0.04 0.172 0.045 0.263 
				 0.05 0.172 0.055 0.265 
				 0.06 0.171 0.065 0.263 
				 0.07 0.170 0.077 0.267 
				 0.08 0.174 0.085 0.266 
				 0.09 0.170 0.095 0.265 
				 0.1 0.171 0.105 0.266 
				 0.107 0.186 0.128 0.172 0.141 0.240 0.155 0.238 0.164 0.280 0.170 0.180 
				 0.178 0.330 0.181 0.177 0.190 0.322 0.196 0.185 0.201 0.326 0.207 0.288 0.242 0.283 0.272 0.238 0.276 0.238 
				 0.288 0.238 0.318 0.238 0.342 0.240 0.344 0.270 0.355 0.325 0.365 0.376 0.370 0.325 0.378 0.376 0.383 0.325 
				 0.390 0.376 0.395 0.309 0.401 0.426 0.410 0.502 0.424 0.305 0.433 0.280 0.436 0.238 0.447 0.241 0.453 0.199 
				 0.466 0.378 0.471 0.431 0.482 0.391 0.494 0.384 0.504 0.350 0.516 0.334 0.532 0.334 0.558 0.330 0.564 0.412 
				 0.569 0.477 0.578 0.511 0.582 0.568 0.590 0.429 0.596 0.553 0.604 0.416 0.620 0.735 0.629 0.653 0.641 0.617 
				 0.647 0.572 0.656 0.542 0.662 0.510 0.681 0.436 0.689 0.379 0.694 0.293 0.719 0.395 0.723 0.510 0.734 0.555 
				 0.743 0.807 0.765 0.786 0.783 0.637 0.797 0.875 0.806 0.902 0.812 0.957 0.832 0.981 
				 0.85 .9 0.868 0.416 0.895 0.814 0.900 0.788 0.903 0.735 0.917 0.635 0.922 0.686 0.931 0.855 0.949 0.952 0.965 0.939 1.000 .9)
			 :duration dur 
			 :scaler (hz->radians 8200))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((frq (env frqf))
	       (intrp (env intrpf)))
	   (outa i (* (env ampf)
		      (+ (* intrp (polyshape gen1 1.0 frq))
			 (* (- 1.0 intrp) (oscil gen2 frq))))
	       *output*)))))))

;(with-sound (:play #t) (swainsons-thrush 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Carolina wren

(definstrument (carolina-wren beg amp)
  (let* ((start (seconds->samples beg))
	 (pulse-dur 0.25)
	 (pulses 7)
	 (dur 1.84)
	 (stop (+ start (seconds->samples dur)))
	 (pulsef (make-env '(0.000 0.000 0.027 0.031 0.049 0.185 0.065 0.749 0.121 0.508 0.137 0.339 0.146 0.270 
			     0.195 0.571 0.247 0.806 0.270 0.994 0.311 0.837 0.325 0.129 0.335 0.373 0.354 0.000 
			     0.512 0.000 0.525 0.677 0.548 0.831 0.560 0.737 0.594 0.082 0.602 0.000 0.618 0.223 
			     0.635 0.313 0.657 0.712 0.698 0.649 0.716 0.517 0.741 0.006 0.775 0.094 0.791 0.467 
			     0.808 0.373 0.838 0.480 0.885 0.414 0.917 0.160 0.930 0.031 1.000 0.000)
			   :duration pulse-dur))
	 (frqf (make-env '(0.000 0.3  0.06 0.209 0.079 0.204 0.084 0.158 0.171 0.160 0.260 0.175 0.310 0.185 
			   0.495 0.153 0.582 0.155 0.621 0.145 0.653 0.125 0.738 0.121 0.794 0.125 0.805 0.109 
			   0.835 0.104 1.000 0.102)
			 :duration pulse-dur :scaler (hz->radians 22050)))
	 (ampf (make-env '(0 0 1 .5 16 1 18 .8 20 0) :duration dur :scaler amp))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .95  2 .015  3 .025 3 .01)))
	 (pulser (make-pulse-train (/ 6 1.6))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((pulse (pulse-train pulser)))
	   (if (> pulse .1)
	       (begin
		 (mus-reset pulsef)
		 (mus-reset frqf)))
	   (outa i (* (env ampf)
		      (env pulsef)
		      (polyshape gen1 1.0 (env frqf)))
		 *output*)))))))

;(with-sound (:play #t) (carolina-wren 0 .25))


;;; --------------------------------------------------------------------------------
;;;
;;; Bachman's sparrow

(definstrument (bachmans-sparrow beg amp)
  ;; two pieces -- initial steady tone, then 10 repetitions of 2nd call
  (let* ((start (seconds->samples beg))
	 (call1-dur .576)
	 (stop1 (+ start (seconds->samples call1-dur)))
	 (ampf1 (make-env '(0.000 0.000 0.684 0.978 0.863 1.0 0.962 0.773 1.000 0.000) :duration call1-dur :scaler (* .5 amp)))
	 (gen1 (make-polyshape 0 :partials (list 1 .98 3 .02)))
	 (frqf1 (make-env '(0 4970 1 4850) :duration call1-dur :scaler (hz->radians 1.0)))
	 (pulser (make-pulse-train (/ 1.0 .184)))
	 (pulses 10)
	 (call2-dur .172)
	 (stop2 (+ start (seconds->samples 2.4)))
	 (ampf2 (make-env '(0.000 0.000 0.070 0.025 0.239 0.430 0.331 0.404 0.381 0.000 0.422 0.007 
			    0.495 0.560 0.541 0.596 0.552 0.466 0.578 0.469 0.592 1.000 0.616 0.798 
			    0.642 0.751 
			    0.75 0 0.786 0.000 0.834 0.267 0.859 0.227 0.902 0.043 1.000 0.000)
			  :duration call2-dur :scaler amp))
	 ;; these two envs may not be aligned correctly -- maybe backup the frq slightly?
	 (frqf2 (make-env '(0.000 0.252 0.129 0.266 0.210 0.291 0.282 0.293 0.336 0.293 0.442 0.404 
			    0.473 0.416 0.515 0.416 0.556 0.447  
			    0.576 0.6
			    0.598 0.443 0.607 0.386 
			    0.638 0.342 0.688 0.332 0.784 0.338 0.796 0.340 0.886 0.57 
			    0.948 0.697 1 .7) 
			  :duration call2-dur :scaler (hz->radians 10000)))
	 (ampf (make-env '(0 1 1.78 1 1.79 0 1.82 0) :duration 1.82)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop1))
	 (outa i (* (env ampf1)
		    (polyshape gen1 1.0 (env frqf1)))
	       *output*))
       (do ((i stop1 (1+ i)))
	   ((= i stop2))
	 (let ((pulse (pulse-train pulser)))
	   (if (> pulse .1)
	       (begin
		 (mus-reset ampf2)
		 (mus-reset frqf2)))
	   (outa i (* (env ampf2)
		      (env ampf)
		      (polyshape gen1 1.0 (env frqf2)))
		 *output*)))))))
	 
;(with-sound (:play #t) (bachmans-sparrow 0 .25))


;;; --------------------------------------------------------------------------------
;;;
;;; Grasshopper sparrow

(definstrument (grasshopper-sparrow beg amp)
  ;; 2 portions -- simple tones, then a buzz
  (let* ((start (seconds->samples beg))

	 (begs (vct 0.0 .24 .36 .44 .55))
	 (durs (vct 0.019 0.020 0.011 0.021 1.09))
	 (amps (vct .48 .54 .07 .22 1.0))
	 (frqs (vct 8500 7240 9730 4920 8000))
	 (starts (make-vector 4 0))
	 (stops (make-vector 4 0))
	 (ampfs (make-vector 4 #f))
	 (gen1 (make-oscil 0.0))

	 (buzz-start (+ start (seconds->samples (vct-ref begs 4))))
	 (buzz-end (+ buzz-start (seconds->samples (vct-ref durs 4))))
	 (buzz-ampf (make-env '(0.000 0.000  0.095 0.953  0.114 0.182  0.158 0.822 0.236 0.996 0.332 1.000 0.848 0.589 0.957 0.372 1.000 0.000)
			      :duration (vct-ref durs 4) :scaler amp))
	 (buzzer (make-sine-summation 40 :n 5 :a .5)) ; sawtooth not great here due to broad spectrum
	 (buzzer-index (hz->radians 2000))
	 (buzzer-amp (make-triangle-wave 40 0.8)))

    (do ((i 0 (1+ i)))
	((= i 4))
      (vector-set! ampfs i (make-env '(0 0 1 .8 1.5 1 2 .8 3 0) :duration (vct-ref durs i) :scaler (* amp (vct-ref amps i))))
      (vector-set! starts i (+ start (seconds->samples (vct-ref begs i))))
      (vector-set! stops i (+ (vector-ref starts i) (seconds->samples (vct-ref durs i)))))

   (run
     (lambda ()

       ;; first the 4 tones
       (do ((tone 0 (1+ tone)))
	   ((= tone 4))
	 (set! (mus-frequency gen1) (vct-ref frqs tone))
	 (let ((ampf (vector-ref ampfs tone))
	       (end (vector-ref stops tone)))
	   (do ((i (vector-ref starts tone) (1+ i)))
	       ((= i end))
	     (outa i (* (env ampf)
			(oscil gen1))
	       *output*))))

       ;; then the buzz
       (set! (mus-frequency gen1) 8000.0)
       (do ((i buzz-start (1+ i)))
	   ((= i buzz-end))
	 (outa i (* (env buzz-ampf)
		    (+ 0.2 (abs (triangle-wave buzzer-amp)))
		    (oscil gen1 (* buzzer-index
				   (sine-summation buzzer))))
	       *output*))))))

;(with-sound (:play #t) (grasshopper-sparrow 0 .25))


;;; --------------------------------------------------------------------------------
;;;
;;; American robin

(definstrument (american-robin beg amp)
  (let* ((start (seconds->samples beg))

	 (ampfs (make-vector 4 #f))
	 (frqfs (make-vector 4 #f))
	 (starts (make-vector 4 0))
	 (stops (make-vector 4 0))

	 (begs (vct 0.0 0.6 1.3 1.8))
	 (durs (vct 0.34 0.25 0.16 0.39))
	 (amps (vct 0.6 1.0 0.7 0.95))

	 (gen1 (make-polyshape 0 :partials (list 1 .95  2 .03 3 .01 4 .01)))

	 (amp-envs (vector 
		    (list 0.000 0.000 0.085 0.847 0.117 0.555 0.125 0.731 0.137 0.505 0.158 0.635 0.178 0.595 
			  0.200 0.449 0.224 0.578 0.241 0.395 0.274 0.515 0.372 0.415 0.409 0.243 0.434 0.266 
			  0.445 0.000 0.463 0.166 0.489 0.080 0.527 0.272 0.535 0.150 0.559 0.777 0.594 0.967 
			  0.632 0.811 0.650 0.897 0.692 0.718 0.710 0.525 0.726 0.588 0.743 0.000 0.746 0.661
			  0.764 0.748 0.782 0.711 0.799 0.528 0.824 0.678 0.834 0.615 0.864 0.721 0.883 0.548 
			  0.904 0.625 0.940 0.458 0.957 0.505 1.000 0.000)

		    (list 0.000 0.000 0.117 0.734 0.200 0.934 0.220 0.814 0.233 0.900 0.254 0.864 0.268 0.538 
			  0.280 0.718 0.302 0.063 0.328 0.439 0.340 0.445 0.356 0.070 0.401 0.123 0.437 0.000 
			  0.529 0.000 0.543 0.372 0.566 0.512 0.579 0.369 0.630 0.449 0.654 0.402 0.704 0.555 
			  0.794 0.515 0.814 0.442 0.822 0.213 0.838 0.654 0.947 0.502 1.000 0.000)

		    (list 0.000 0.000 0.013 0.282 0.085 0.568 0.140 0.522 0.174 0.678 0.217 0.960 0.232 1.000 
			  0.258 0.718 0.277 0.877 0.310 0.970 0.361 0.927 0.429 0.864 0.487 0.641 0.513 0.382 
			  0.565 0.425 0.586 0.166 0.611 0.040 0.647 0.319 0.692 0.784 0.806 0.947 0.846 0.671 
			  0.875 0.462 0.905 0.605 0.959 0.508 1.000 0.000)

		    (list 0.000 0.000 0.059 0.282 0.107 0.711 0.154 0.711 0.176 0.588 0.221 0.525 0.289 0.475 
			  0.386 0.445 0.415 0.199 0.438 0.286 0.451 0.060 0.455 0.269 0.479 0.090 0.493 0.196 
			  0.577 0.924 0.674 0.924 0.699 0.608 0.710 0.831 0.762 0.571 0.778 0.645 0.842 0.425 
			  0.899 0.372 0.933 0.415 1.000 0.000)))

	 (frq-envs (vector
		    (list 0.000 0.491 0.026 0.502 0.083 0.509 0.098 0.456 0.132 0.428 0.165 0.428 0.190 0.442 
			  0.350 0.436 0.367 0.419 0.440 0.415 0.450 0.606 0.485 0.608 0.502 0.634 0.583 0.636 
			  0.607 0.677 0.658 0.684 0.663 0.562 0.687 0.564 0.703 0.546 0.725 0.546 0.773 0.564 
			  0.793 0.560 0.808 0.555 1.000 0.555)

		    (list 0.000 0.634 0.074 0.647 0.094 0.666 0.113 0.671 0.130 0.663 0.147 0.625 0.172 0.620 
			  0.189 0.601 0.219 0.601 0.229 0.627 0.251 0.622 0.265 0.601 0.329 0.597 0.481 0.601 
			  0.508 0.602 0.514 0.504 0.556 0.502 0.625 0.473 0.722 0.477 0.729 0.583 0.825 0.572 
			  0.852 0.553 0.892 0.555 1.000 0.578)

		    (list 0.000 0.509 0.075 0.518 0.124 0.560 0.150 0.640 0.192 0.663 0.223 0.654 0.271 0.551 
			  0.313 0.539 0.354 0.551 0.389 0.583 0.433 0.601 0.542 0.606 0.595 0.564 0.669 0.558 
			  0.736 0.557 0.749 0.610 0.788 0.655 0.821 0.678 0.857 0.680 0.874 0.611 0.907 0.595 1.000 0.580)

		    (list 0.000 0.481 0.032 0.491 0.084 0.530 0.098 0.595 0.113 0.592 0.118 0.458 0.186 0.436 
			  0.229 0.445 0.313 0.442 0.330 0.452 0.346 0.440 0.386 0.442 0.464 0.428 0.504 0.560 
			  0.529 0.641 0.573 0.636 0.605 0.647 0.657 0.661 0.690 0.680 0.705 0.558 0.758 0.549 
			  0.787 0.555 0.821 0.544 0.849 0.532 0.913 0.535 0.939 0.548 1.000 0.537))))

    (do ((i 0 (1+ i)))
	((= i 4))
      (vector-set! ampfs i (make-env (vector-ref amp-envs i) :duration (vct-ref durs i) :scaler (* amp (vct-ref amps i))))
      (vector-set! frqfs i (make-env (vector-ref frq-envs i) :duration (vct-ref durs i) :scaler (hz->radians 5000)))
      (vector-set! starts i (+ start (seconds->samples (vct-ref begs i))))
      (vector-set! stops i (+ (vector-ref starts i) (seconds->samples (vct-ref durs i)))))

    (run
     (lambda ()
       (do ((tone 0 (1+ tone)))
	   ((= tone 4))
	 (let ((ampf (vector-ref ampfs tone))
	       (frqf (vector-ref frqfs tone))
	       (end (vector-ref stops tone)))
	   (do ((i (vector-ref starts tone) (1+ i)))
	       ((= i end))
	     (outa i (* (env ampf)
			(polyshape gen1 1.0 (env frqf)))
	       *output*))))))))

;(with-sound (:play #t) (american-robin 0 .25))


;;; --------------------------------------------------------------------------------
;;;
;;; Common loon
;;;
;;; Geoffrey Keller, "Bird Songs of California" (Cornell)

(definstrument (common-loon-1 beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 2.5)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.049 0.134 0.122 0.131 0.174 0.070 0.178 0.244 
			   0.522 0.954 0.649 0.922 0.736 1.0 0.860 0.962 0.957 0.839 .98 .5 1.000 0.000)
			 :duration dur :scaler amp))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .85  2 .1  3 .02  4 .05  5 .01 7 .003 9 .001)))
	 (frqf (make-env '(0.000 0.330 0.030 0.388 0.087 0.395  0.155 0.509 0.158 0.609 0.204 0.633 0.346 0.685 
			   0.35 0.852 0.469 0.882 0.585 0.886 0.780 0.888 0.896 0.878 0.961 0.869  .98 .8 1.000 0.76)
			 :duration dur :scaler (hz->radians 1000.0))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (common-loon-1 0 .25))


(definstrument (common-loon-2 beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.63)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.021 0.270 0.045 0.253 0.088 0.103 0.141 0.416 0.165 0.251 0.179 0.107 
			   0.192 0.403 0.211 0.399 0.222 0.208 0.242 0.206 0.286 0.895 0.303 0.882 0.327 0.672 
			   0.350 0.324 0.362 0.150 0.378 0.236 0.389 0.268 0.423 0.991 0.447 0.923 0.459 0.785 
			   0.488 0.242 0.506 0.200 0.531 0.245 0.540 0.371 0.556 0.785 0.566 0.792 0.573 0.981 
			   0.579 0.873 0.585 0.931 0.600 0.811 0.622 0.354 0.636 0.191 0.652 0.120 0.663 0.148 
			   0.674 0.099 0.687 0.163 0.707 0.489 0.720 0.631 0.731 0.624 0.765 0.339 0.778 0.170
			   0.810 0.056 0.842 0.116 0.863 0.227 0.899 0.240 0.907 0.189 0.965 0.122 1.000 0.000)
			 :duration dur :scaler amp))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .85  2 .02  3 .008 4 .01  5 .006)))
	 (frqf (make-env '(0.000 0.267 0.029 0.354 0.131 0.349 0.188 0.414 0.202 0.534 0.232 0.453 0.250 0.427 
			   0.267 0.455 0.279 0.505 0.296 0.540 0.312 0.549 0.332 0.532 0.365 0.442 0.380 0.427 
			   0.395 0.443 0.417 0.512 0.430 0.544 0.446 0.558 0.465 0.542 0.503 0.436 0.521 0.421 
			   0.535 0.440 0.558 0.510 0.570 0.534 0.588 0.544 0.608 0.525 0.625 0.479 0.646 0.425 
			   0.669 0.410 0.690 0.432 0.715 0.514 0.733 0.532 0.753 0.514 0.801 0.423 0.817 0.421 
			   0.830 0.304 0.866 0.343 0.891 0.354 0.913 0.338 0.950 0.312 1.000 0.304)
			 :duration dur :scaler (hz->radians 2000.0))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (common-loon-2 0 .25))


;;; --------------------------------------------------------------------------------
;;;
;;; Hermit thrush

(definstrument (hermit-thrush beg amp)
  (let* ((start (seconds->samples beg))

	 (ampfs (make-vector 3 #f))
	 (frqf1s (make-vector 3 #f))
	 (frqf2s (make-vector 3 #f))
	 (starts (make-vector 3 0))
	 (stops (make-vector 3 0))

	 (begs (vct 0.0 0.42 0.84))
	 (durs (vct 0.34 0.35 0.56))
	 (amps (vct 0.6 0.8 1.0))

	 (gen1 (make-polyshape 0 :partials (list 1 .95  2 .01 3 .03)))
	 (gen2 (make-polyshape 0 :partials (list 1 .95  2 .01 3 .03)))

	 (amp-envs (vector 
		    (list 0.000 0.000 0.117 0.054 0.301 0.269 0.518 0.605 0.608 0.620 0.703 0.783 0.779 0.900 
			  0.804 0.857 0.863 0.987 0.912 0.959 0.943 1.000 0.967 0.900 1.000 0.000)

		    (list 0.000 0.000 0.028 0.109 0.037 0.059 0.056 0.223 0.066 0.000 0.094 0.252 0.104 0.488 
			  0.136 0.605 0.151 0.770 0.154 0.000 0.162 0.857 0.175 0.727 0.187 0.000 0.198 0.505 
			  0.217 0.280 0.229 0.427 0.240 0.291 0.249 0.479 0.266 0.369 0.277 0.518 0.289 0.380 
			  0.297 0.503 0.310 0.380 0.321 0.495 0.334 0.000 0.343 0.584 0.376 0.545 0.398 0.649 
			  0.412 0.605 0.423 0.642 0.445 0.607 0.456 0.651 0.493 0.638 0.516 0.586 0.532 0.000 
			  0.541 0.386 0.570 0.579 0.577 0.210 0.603 0.885 0.627 0.704 0.645 0.000 0.647 0.482 
			  0.659 0.584 0.673 0.148 0.683 0.573 0.698 0.432 0.708 0.649 0.722 0.529 0.735 0.701 
			  0.754 0.586 0.764 0.709 0.777 0.575 0.791 0.716 0.802 0.568 0.819 0.681 0.837 0.601 
			  0.872 0.783 0.933 0.967 0.978 0.902 1.000 0.000)

		    (list 0.000 0.000 0.006 0.202 0.014 0.082 0.023 0.349 0.057 0.605 0.064 0.581 0.071 0.367 
			  0.105 0.184 0.107 0.000 0.152 0.421 0.171 0.538 0.183 0.443 0.210 0.000 0.230 0.150 
			  0.250 0.707 0.264 0.716 0.276 0.579 0.280 0.499 0.306 0.712 0.328 0.711 0.340 0.852 
			  0.356 0.850 0.367 0.957 0.391 0.904 0.412 0.616 0.420 0.000 0.446 0.516 0.452 0.482 
			  0.456 0.391 0.457 0.256 0.487 0.000 0.511 0.247 0.554 0.213 0.572 0.106 0.606 0.000 
			  0.645 0.340 0.668 0.167 0.706 0.000 0.725 0.273 0.736 0.267 0.753 0.000 0.779 0.364 
			  0.799 0.347 0.823 0.256 0.847 0.132 0.853 0.000 0.877 0.160 0.887 0.078 0.924 0.000 
			  0.948 0.095 1.000 0.000)))

	 (frq1-envs (vector
		     (list 0.000 0.352 1.000 0.345)

		     (list 0.000 0.485 0.037 0.482 0.058 0.511 0.068 0.482 0.085 0.523 0.102 0.489 0.112 0.562 
			   0.200 0.549 0.215 0.490 0.233 0.510 0.243 0.487 0.256 0.508 0.265 0.484 0.281 0.508 
			   0.293 0.482 0.302 0.499 0.318 0.489 0.528 0.492 0.544 0.516 0.557 0.487 0.572 0.492 
			   0.576 0.518 0.578 0.549 0.675 0.542 0.680 0.496 0.691 0.485 0.704 0.508 0.712 0.487 
			   0.728 0.506 0.743 0.480 0.755 0.503 0.770 0.482 0.782 0.497 0.794 0.478 0.805 0.492 
			   0.827 0.490 0.851 0.397 1.000 0.392)

		     (list 0.000 0.499 0.018 0.497 0.029 0.563 0.061 0.563 0.074 0.553 0.086 0.487 0.098 0.513 
			   0.105 0.492 0.116 0.510 0.127 0.612 0.137 0.641 0.190 0.641 0.202 0.510 0.213 0.487 
			   0.224 0.548 0.235 0.510 0.244 0.567 0.268 0.553 0.277 0.501 0.286 0.489 0.314 0.504 
			   0.323 0.449 0.423 0.442 0.432 0.553 0.446 0.567 0.457 0.503 0.467 0.487 0.481 0.497 
			   0.506 0.563 0.526 0.553 0.533 0.510 0.544 0.496 0.555 0.510 0.582 0.556 0.583 0.624 
			   0.591 0.645 0.649 0.643 0.662 0.515 0.674 0.489 0.708 0.551 0.722 0.553 0.740 0.490 
			   0.761 0.487 0.793 0.506 0.820 0.556 0.859 0.560 0.889 0.490 0.926 0.494 0.928 0.556 
			   0.946 0.555 0.972 0.489 1.000 0.487)))

	 (frq2-envs (vector
		     (list 0.000 0.352 1.000 0.345)

		     (list 0.000 0.000 0.001 0.352 0.025 0.440 0.034 0.388 0.051 0.438 0.064 0.380 0.088 0.425 
			   0.098 0.383 0.120 0.350 0.146 0.357 0.193 0.336 0.207 0.409 0.219 0.381 0.231 0.418 
			   0.243 0.388 0.259 0.421 0.268 0.388 0.279 0.426 0.293 0.388 0.307 0.426 0.316 0.397 
			   0.336 0.428 0.345 0.402 0.364 0.430 0.371 0.397 0.372 0.492 0.534 0.494 0.549 0.402 
			   0.565 0.386 0.585 0.347 0.614 0.355 0.663 0.336 0.672 0.354 0.678 0.395 0.697 0.388 
			   0.707 0.406 0.735 0.407 0.758 0.406 0.804 0.397 0.830 0.400 0.846 0.397 1.000 0.395)
		     
		     (list 0.000 0.409 0.018 0.392 0.034 0.364 0.069 0.343 0.080 0.400 0.092 0.357 0.100 0.399 
			   0.114 0.354 0.136 0.347 0.157 0.315 0.195 0.399 0.206 0.376 0.218 0.399 0.231 0.348 
			   0.257 0.345 0.270 0.393 0.286 0.400 0.293 0.440 0.312 0.447 0.410 0.445 0.424 0.350 
			   0.434 0.340 0.449 0.393 0.462 0.393 0.476 0.406 0.489 0.366 0.504 0.348 0.516 0.360 
			   0.530 0.404 0.538 0.380 0.551 0.397 0.569 0.348 0.589 0.350 0.615 0.324 0.643 0.357 
			   0.653 0.400 0.678 0.400 0.685 0.376 0.694 0.352 0.716 0.350 0.730 0.404 0.748 0.411 
			   0.757 0.447 0.830 0.447 0.851 0.350 0.873 0.400 0.901 0.406 0.917 0.360 0.946 0.357 
			   1.000 0.447))))

    (do ((i 0 (1+ i)))
	((= i 3))
      (vector-set! ampfs i (make-env (vector-ref amp-envs i) :duration (vct-ref durs i) :scaler (* amp (vct-ref amps i))))
      (vector-set! frqf1s i (make-env (vector-ref frq1-envs i) :duration (vct-ref durs i) :scaler (hz->radians 10000)))
      (vector-set! frqf2s i (make-env (vector-ref frq2-envs i) :duration (vct-ref durs i) :scaler (hz->radians 10000)))
      (vector-set! starts i (+ start (seconds->samples (vct-ref begs i))))
      (vector-set! stops i (+ (vector-ref starts i) (seconds->samples (vct-ref durs i)))))

    (run
     (lambda ()
       (do ((tone 0 (1+ tone)))
	   ((= tone 3))
	 (let ((ampf (vector-ref ampfs tone))
	       (frqf1 (vector-ref frqf1s tone))
	       (frqf2 (vector-ref frqf2s tone))
	       (end (vector-ref stops tone)))
	   (do ((i (vector-ref starts tone) (1+ i)))
	       ((= i end))
	     (outa i (* (env ampf)
			(+ (* .55 (polyshape gen1 1.0 (env frqf1)))
			   (* .45 (polyshape gen2 1.0 (env frqf2)))))
	       *output*))))))))

;(with-sound (:play #t) (hermit-thrush 0 .25))


;;; --------------------------------------------------------------------------------
;;;
;;; Chuck-will's-widow

(definstrument (chuck-wills-widow beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 1.05)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.017 0.656 0.026 0.136 0.048 0.000 
			   0.289 0.000 0.328 0.154 0.353 0.405 0.361 0.961 0.369 0.667 0.381 0.918 0.394 0.269 
			   0.402 0.204 0.425 0.333 0.440 0.570 0.466 0.444 0.515 0.470 0.592 0.294 
			   0.65 0.000 .68 0.0 .7 .1 .74 0  0.762 0.000 0.791 0.305 0.818 0.337 
			   0.832 1.000 0.844 0.699 0.857 0.903 0.867 0.405 0.883 0.398 0.895 0.853 0.907 0.853 
			   0.921 0.297 0.953 0.294 0.981 0.140 1.000 0.000)
			 :duration dur :scaler amp))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .97  3 .02  4 .01)))
	 (rnd (make-rand-interp 100 .25))
	 (frqf (make-env '(0.000 0.702 0.014 0.637 0.023 0.478 0.048 0.343 0.298 0.385 0.335 0.389 0.353 0.459 
			   0.362 0.546 0.371 0.687 0.376 0.715 0.383 0.687 0.388 0.635 0.391 0.565 0.398 0.474 
			   0.417 0.370 0.455 0.561 0.490 0.389 0.504 0.465 0.523 0.483 0.541 0.461 0.552 0.413 
			   0.605 0.409 0.610 0.370 0.781 0.380 0.804 0.417 0.823 0.457 0.837 0.517 0.851 0.693 
			   0.858 0.737 0.867 0.702 0.871 0.572 0.878 0.496 0.889 0.430 0.904 0.535 0.914 0.630 
			   0.924 0.554 0.933 0.457 0.951 0.380 1.000 0.354)
			 :duration dur :scaler (hz->radians 3000.0))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (+ .75 (abs (rand-interp rnd)))
		    (polyshape gen1 1.0 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (chuck-wills-widow 0 .25))


;;; --------------------------------------------------------------------------------
;;;
;;; California towhee

(definstrument (california-towhee beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 1.17)
	 (stop (+ start (seconds->samples dur)))
	 ;; peep pitch changes
	 (amps (vct .5 .8 .85 .9 .95 1.0))
	 (begs (vct 0.0 0.39 0.67 0.86 0.96 1.09))
	 (frqs (vct 4750 4950 4880 4920 5210 5140))
	 (starts (make-vector 7 0))
	 (peep-dur 0.055)
	 (ampf (make-env '(0.000 0.000 0.141 0.119 0.220 0.652 0.329 0.968 0.495 0.830 0.603 0.399 0.715 0.178 1.000 0.000)
			 :duration peep-dur :scaler amp))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .97  2 .02  3 .01)))
	 (frqf (make-env '(0 .5 .1 3  .2 1  .4 0  1 .2) :duration peep-dur :scaler (hz->radians 800)))
	 (next-start start)
	 (peep-amp 1.0)
	 (peep-ctr 0))

    (do ((i 0 (1+ i)))
	((= i 6))
      (vector-set! starts i (+ start (seconds->samples (vct-ref begs i)))))
    (vector-set! starts 6 (1+ stop))

   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (if (>= i next-start)
	     (begin
	       (set! peep-amp (vct-ref amps peep-ctr))
	       (set! (mus-frequency gen1) (vct-ref frqs peep-ctr))
	       (set! peep-ctr (1+ peep-ctr))
	       (set! next-start (vector-ref starts peep-ctr))
	       (mus-reset ampf)
	       (mus-reset frqf)))
	 (outa i (* (env ampf)
		    peep-amp
		    (polyshape gen1 1.0 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (california-towhee 0 .25))


;;; --------------------------------------------------------------------------------
;;;
;;; Black-chinned sparrow

(definstrument (black-chinned-sparrow beg amp gliss-up)
  (let* ((start (seconds->samples beg))
	 ;; initial stable pitch, then the gliss->buzz with frq going either up or down

	 (initial-dur .36)
	 (initial-pitch 6800)
	 (initial-amp .05)
	 (initial-stop (+ start (seconds->samples initial-dur)))
	 (initial-ampf (make-env '(0 0 1 1 10 1 11 0) :duration initial-dur :scaler (* amp initial-amp)))
	 (initial-gen (make-oscil initial-pitch))

	 (buzz-dur 2.62)
	 (buzz-stop (+ initial-stop (seconds->samples buzz-dur)))
	 (buzz-amp (make-env '(0.000 0.000 0.035 0.190 0.082 0.336 0.168 0.625 0.348 0.743 0.467 0.763 
			       0.530 0.723 0.628 0.818 0.668 1.000 0.728 0.913 0.777 0.506 0.818 0.174 1.000 0.000)
			     :duration buzz-dur :scaler amp))
	 (buzz-frq0 (/ 1.0 (* 2 .34)))
	 (buzz-frq1 (/ 1.0 .02))
	 (buzz-frqmid (/ 1.0 .15))
	 (buzz-frq (make-env (list 0 buzz-frq0 .5 buzz-frqmid 1 buzz-frq1) :duration buzz-dur :scaler (hz->radians 1.0)))
	 (buzz-size 128)
	 (buzz-low 3000)
	 (buzz-high 8500)
	 (buzz-mid 4000)
	 (buzz-frq-table (let ((v (make-vct buzz-size 0.0))
			       (bfrqf (make-env (if gliss-up 
						    (list 0 buzz-low .5 buzz-mid 1 buzz-high)
						    (list 0 buzz-high .5 buzz-mid 1 buzz-low))
						:end buzz-size 
						:scaler (hz->radians 1.0))))
			   (do ((i 0 (1+ i)))
			       ((= i buzz-size))
			     (vct-set! v i (env bfrqf)))
			   v))
	 (buzz-amp-table (let ((v (make-vct buzz-size 0.0))
			       (bampf (make-env (if gliss-up
						    '(0 0 1 1 2.5 .7 3 0 3.5 0)
						    '(0 0 .5 1 2 1 3 0 3.5 0))
						:end buzz-size)))
			   (do ((i 0 (1+ i)))
			       ((= i buzz-size))
			     (vct-set! v i (env bampf)))
			   v))
	 (buzz-frqf (make-table-lookup buzz-frq0 :wave buzz-frq-table))
	 (buzz-ampf (make-table-lookup buzz-frq0 :wave buzz-amp-table))
	 (buzz-gen (make-polyshape 0.0 :partials (list 1 .98 2 .005 3 .01)))
	 )

    (run
     (lambda ()

       (do ((i start (1+ i)))
	   ((= i initial-stop))
	 (outa i (* (env initial-ampf) (oscil initial-gen)) *output*))

       (do ((i initial-stop (1+ i)))
	   ((= i buzz-stop))
	 (let* ((frq (env buzz-frq)))
	   (outa i (* (env buzz-amp)
		      (table-lookup buzz-ampf frq)
		      (polyshape buzz-gen 1.0 (table-lookup buzz-frqf frq)))
		 *output*)))))))

;(with-sound (:play #t) (black-chinned-sparrow 0 .25 #t))


;;; --------------------------------------------------------------------------------
;;;
;;; Mourning dove

(definstrument (mourning-dove beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 4.1)
	 (stop (+ start (seconds->samples dur)))
	 (rnd (make-rand-interp 2000 (hz->radians 200.0)))
	 (rndf (make-env '(0 1 2 .3 5 .3) :duration dur :scaler .1))
	 (gen2 (make-oscil 620))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .95 2 .05 3 .005)))
	 (ampf (make-env '(0.000 0.000 0.012 0.256 0.032 0.247 0.048 0.188 0.197 0.156 0.224 0.988 0.238 0.844 
			   0.256 0.881 0.309 0.000 0.390 0.000 0.414 0.881 0.441 0.819 0.494 0.394 0.564 0.175 
			   0.579 0.000 0.647 0.000 0.678 0.725 0.703 0.659 0.786 0.000 0.856 0.000 0.879 0.631 
			   0.892 0.675 0.920 0.494 0.986 0.162 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.000 0.395 0.019 0.449 0.065 0.439 0.159 0.439 0.198 0.427 0.217 0.493 0.229 0.621 
			   0.236 0.658 0.270 0.642 0.298 0.555 0.309 0.495 0.414 0.487 0.432 0.499 0.477 0.497 
			   0.537 0.484 0.577 0.468 0.588 0.427 0.674 0.480 0.698 0.493 0.729 0.487 0.771 0.472 
			   0.877 0.468 0.903 0.493 0.960 0.478 1.000 0.462)
			 :duration dur :scaler (hz->radians 1000.0))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (+ (* .95 (polyshape gen1 1.0 (env frqf)))
		       (* (env rndf)
			  (oscil gen2 (rand-interp rnd)))))
	       *output*))))))

;(with-sound (:play #t) (mourning-dove 0 .25))


;;; --------------------------------------------------------------------------------
;;;
;;; Bobwhite
;;;
;;; Lang Elliott, Donald and Lillian Stokes, "Stokes Field Guide to Bird Songs, Eastern Region"

(definstrument (bobwhite beg amp)
  (let* ((call1-dur .32)
	 (call1-amp .07)
	 (call1-pitch 1500) ; 1530 down to 1450? 1 is pure, 2 and 3 have 1 2, no 3, 4
	 (call1-gen (make-oscil 1450))
	 (call1-ampf (make-env '(0 0 1 1 8 1 9 0) :duration call1-dur :scaler (* amp call1-amp)))
	 (call1-frqf (make-env '(0 1 1 0) :duration call1-dur :scaler (hz->radians 80)))
	 (call1-start (seconds->samples beg))
	 (call1-stop (+ call1-start (seconds->samples call1-dur)))

	 (call2-beg .80)
	 (call2-dur .20)
	 (call2-amp .35)
	 (call2-gen (make-polyshape 1320 :partials (list 1 .95 2 .04 3 .01 4 .02 5 .01)))
	 (call2-ampf (make-env '(0 0 1 1 4 1 5 0) :duration call2-dur :scaler (* amp call2-amp)))
	 (call2-frqf (make-env '(0 0 1 1 4 1 5 .5) :duration call2-dur :scaler (hz->radians 430)))
	 (call2-start (+ call1-start (seconds->samples call2-beg)))
	 (call2-stop (+ call2-start (seconds->samples call2-dur)))

	 (call3-beg 1.43)
	 (call3-dur .22)
	 (call3-amp 1.0)
	 (call3-gen (make-polyshape 0.0 :partials (list 1 .95 2 .04 4 .01)))
	 (call4-gen (make-polyshape 0.0 :partials (list 1 .05   2 .6  3 .2  4 .1  5 .01  6 .005)))
	 (call3-ampf (make-env '(0 0 .5 1  .75 .2  1 0) :duration call3-dur :scaler (* amp call3-amp)))
	 (call3-frqf (make-env '(0.000 0.245 0.135 0.304 0.399 0.335 0.439 0.345 0.491 0.384 0.551 0.434 0.591 0.485
				       0.65 0.65  .67 .5  1 .3)
			       :duration call3-dur :scaler (hz->radians 6000.0)))
	 (call3-f1 (make-env '(0 1 .6 1 .75 0 1 0) :duration call3-dur))
	 (call3-f2 (make-env '(0 0 .6 0 .64 1 1 1) :duration call3-dur))
	 (call3-start (+ call1-start (seconds->samples call3-beg)))
	 (call3-stop (+ call3-start (seconds->samples call3-dur))))
   (run
     (lambda ()

       (do ((i call1-start (1+ i)))
	   ((= i call1-stop))
	 (outa i (* (env call1-ampf)
		    (oscil call1-gen (env call1-frqf)))
	       *output*))

       (do ((i call2-start (1+ i)))
	   ((= i call2-stop))
	 (outa i (* (env call2-ampf)
		    (polyshape call2-gen 1.0 (env call2-frqf)))
	       *output*))

       (do ((i call3-start (1+ i)))
	   ((= i call3-stop))
	 (let ((f1 (env call3-f1))
	       (f2 (env call3-f2))
	       (frq (env call3-frqf)))
	   (outa i (* (env call3-ampf)
		      (+ (* f1 (polyshape call3-gen 1.0 frq))
			 (* f2 (polyshape call4-gen 1.0 (* 0.5 frq)))))
		 *output*)))))))

;(with-sound (:play #t) (bobwhite 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Warbling vireo

(definstrument (warbling-vireo beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 2.25)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.018 0.042 0.046 0.000 0.074 0.113 0.091 0.111 0.096 0.000 
			   0.113 0.000 0.129 0.124 0.144 0.089 0.148 0.026 0.164 0.000 0.187 0.108 
			   0.209 0.000 0.220 0.000 0.222 0.103 0.235 0.218 0.245 0.205 0.258 0.000 
			   0.268 0.000 0.279 0.087 0.305 0.089 0.316 0.000 0.338 0.000 0.345 0.216 
			   0.379 0.726 0.402 0.000 0.411 0.000 0.414 0.324 0.437 0.155 0.455 0.139 
			   0.461 0.000 0.473 0.000 0.482 0.126 0.492 0.126 0.497 0.321 0.509 0.139
			   0.520 0.003 0.536 0.308 0.552 0.187 0.565 0.250 0.572 0.000 0.587 0.000 
			   0.596 0.737 0.619 0.966 0.646 0.501 0.661 0.000 0.670 0.000 0.679 0.266 
			   0.697 0.097 0.703 0.711 0.719 0.000 0.736 0.000 0.746 0.997 0.756 0.282 
			   0.775 0.392 0.787 0.000 0.804 0.000 0.813 0.811 0.826 0.463 0.836 0.411 
			   0.847 0.000 0.862 0.000 0.873 0.284 0.893 0.192 0.899 0.066 0.912 0.329 
			   0.921 0.000 0.931 0.000 0.934 0.303 0.947 0.466 0.960 0.418 0.980 0.258 1.000 0.000 )
			 :duration dur :scaler amp))
	 (gen1 (make-oscil 0.0))
	 (frqf (make-env '(0.000 0.184 0.010 0.214 0.026 0.214 0.036 0.197 0.057 0.197 0.066 0.233 
			   0.085 0.266 0.099 0.260 0.113 0.255 0.124 0.274 0.125 0.222 0.134 0.249 
			   0.146 0.227 0.165 0.227 0.169 0.178 0.179 0.184 0.191 0.192 0.209 0.175 
			   0.221 0.186 0.226 0.312 0.227 0.258 0.233 0.285 0.234 0.236 0.242 0.274 
			   0.245 0.241 0.252 0.230 0.268 0.227 0.272 0.203 0.284 0.225 0.295 0.216 
			   0.306 0.208 0.316 0.219 0.346 0.233 0.357 0.282 0.359 0.252 0.366 0.296 
			   0.369 0.252 0.373 0.304 0.376 0.255 0.382 0.301 0.385 0.263 0.390 0.301 
			   0.412 0.279 0.418 0.321 0.421 0.247 0.424 0.279 0.427 0.233 0.441 0.211 
			   0.450 0.208 0.457 0.178 0.480 0.197 0.484 0.238 0.488 0.205 0.492 0.241 
			   0.495 0.200 0.499 0.247 0.506 0.241 0.512 0.186 0.529 0.192 0.530 0.255 
			   0.548 0.238 0.557 0.214 0.568 0.241 0.582 0.230 0.591 0.299 0.599 0.307 
			   0.609 0.301 0.615 0.274 0.627 0.342 0.645 0.359 0.648 0.329 0.670 0.332 
			   0.672 0.247 0.700 0.227 0.705 0.304 0.715 0.249 0.722 0.244 0.738 0.247 
			   0.749 0.307 0.753 0.425 0.762 0.422 0.770 0.468 0.774 0.392 0.786 0.342 
			   0.808 0.326 0.821 0.255 0.832 0.285 0.843 0.266 0.866 0.263 0.891 0.197 
			   0.915 0.247 0.935 0.285 0.942 0.345 0.945 0.290 0.947 0.441 0.950 0.353 
			   0.953 0.411 0.957 0.367 0.960 0.405 0.964 0.370 0.967 0.405 0.973 0.373 
			   0.979 0.373 0.990 0.296 1.000 0.255 )
			 :duration dur :scaler (hz->radians 11900))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (oscil gen1 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (warbling-vireo 0 .25))


;;; --------------------------------------------------------------------------------
;;;
;;; Great-horned owl

(definstrument (great-horned-owl beg amp)
  (let* ((begs (vct 0.0  0.26 1.42 2.16))
	 (durs (vct 0.14 0.40 0.43 0.37))
	 (amps (vct .75 .9 .95 1.0)))

    (do ((call 0 (1+ call)))
	((= call 4))
      (let* ((start (seconds->samples (+ beg (vct-ref begs call))))
	     (stop (+ start (seconds->samples (vct-ref durs call))))
	     (gen (make-polyshape 0.0 :partials (list 1 .9  2 .12  3 .007  7 .003)))
	     (rnd (make-rand-interp 30 (hz->radians 5)))
	     (ampf (make-env '(0 0 1 1 4 .9 5 0) :duration (vct-ref durs call) :scaler (* amp (vct-ref amps call))))
	     (frqf (make-env '(0 1.25  .5 2  4.4 1.95  5 1) :base .1 :duration (vct-ref durs call) :scaler (hz->radians (* 0.5 328)))))
	(run
	 (lambda ()
	   (do ((i start (1+ i)))
	       ((= i stop))
	     (outa i (* (env ampf)
			(polyshape gen 1.0 (+ (env frqf)
					      (rand-interp rnd))))
		   *output*))))))))

;(with-sound (:play #t) (great-horned-owl 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Western tanager

(definstrument (western-tanager beg amp)
  (let* ((gen (make-polyshape 0.0 :partials '(1 .98 2 .02)))
	 (begs (vct 0.0  0.7  1.4  2.0))
	 (durs (vct 0.27 0.32 0.25 0.24))
	 (amps (vct 0.32 0.85 1.0  0.65))

	 (ampfs (vector 
		 (make-env '(0.000 0.000 0.086 0.398 0.247 0.610 0.363 0.000 0.416 0.000 0.513 0.603 0.610 0.603 
			     0.708 0.507 0.733 0.232 0.798 0.895 0.848 1.000 0.898 0.927 1.000 0.000)
			   :duration (vct-ref durs 0) :scaler (* amp (vct-ref amps 0)))
		 (make-env '(0.000 0.000 0.060 0.735 0.303 1.000 0.394 0.408 0.503 0.318 0.617 0.879 0.912 0.258 
			     0.939 0.055 1.000 0.000)
			   :duration (vct-ref durs 1) :scaler (* amp (vct-ref amps 1)))
		 (make-env '(0.000 0.000 0.098 0.837 0.183 0.704 0.395 1.000 0.469 0.185 0.553 0.086 0.731 0.841 
			     0.785 0.834 1.000 0.000)
			   :duration (vct-ref durs 2) :scaler (* amp (vct-ref amps 2)))
		 (make-env '(0.000 0.000 0.047 0.837 0.117 0.172 0.167 0.157 0.234 0.993 0.296 0.826 0.319 0.609 
			     0.431 0.781 0.567 0.506 0.642 0.166 0.673 0.757 0.769 0.874 0.873 0.766 0.919 0.605 
			     0.956 0.230 1.000 0.000)
			   :duration (vct-ref durs 3) :scaler (* amp (vct-ref amps 3)))))

	 (frqfs (vector 
		 (make-env '(0.000 0.437 0.056 0.561 0.075 0.558 0.094 0.459 0.109 0.536 0.128 0.411 0.142 0.521 
			     0.156 0.435 0.172 0.532 0.190 0.450 0.214 0.556 0.226 0.459 0.239 0.556 0.350 0.318 
			     0.409 0.313 0.491 0.461 0.614 0.461 0.665 0.428 0.702 0.340 0.718 0.406 0.739 0.331 
			     0.756 0.470 0.772 0.336 0.803 0.510 0.818 0.353 0.845 0.536 0.862 0.415 0.886 0.545 
			     0.903 0.470 0.924 0.534 0.945 0.442 1.000 0.395)
			   :duration (vct-ref durs 0) :scaler (hz->radians 6000))
		 (make-env '(0.000 0.587 0.045 0.543 0.064 0.459 0.088 0.563 0.105 0.481 0.127 0.600 0.141 0.514 
			     0.172 0.620 0.185 0.532 0.212 0.640 0.233 0.567 0.251 0.629 0.266 0.589 0.374 0.448 
			     0.440 0.404 0.528 0.406 0.557 0.450 0.583 0.466 0.604 0.517 0.618 0.481 0.648 0.552 
			     0.667 0.499 0.691 0.556 0.710 0.517 0.739 0.561 0.758 0.519 0.791 0.561 0.814 0.510 
			     0.833 0.534 0.975 0.483 1.000 0.488)
			   :duration (vct-ref durs 1) :scaler (hz->radians 6000))
		 (make-env '(0.000 0.247 0.059 0.539 0.073 0.556 0.131 0.490 0.150 0.444 0.172 0.501 0.199 0.402 
			     0.222 0.512 0.249 0.430 0.279 0.552 0.304 0.464 0.340 0.600 0.360 0.479 0.383 0.567 
			     0.496 0.311 0.611 0.320 0.635 0.470 0.655 0.331 0.680 0.492 0.703 0.349 0.742 0.534 
			     0.770 0.373 0.797 0.536 0.823 0.419 0.856 0.536 0.881 0.433 0.910 0.506 0.950 0.397 
			     0.978 0.508 1.000 0.514)
			   :duration (vct-ref durs 2) :scaler (hz->radians 6000))
		 (make-env '(0.000 0.614 0.031 0.607 0.046 0.514 0.072 0.430 0.145 0.307 0.168 0.380 0.191 0.536 
			     0.205 0.453 0.223 0.570 0.239 0.457 0.261 0.547 0.282 0.426 0.297 0.503 0.318 0.426 
			     0.341 0.453 0.449 0.468 0.580 0.435 0.635 0.419 0.652 0.353 0.674 0.494 0.687 0.545 
			     0.706 0.455 0.732 0.556 0.754 0.457 0.783 0.547 0.807 0.455 0.840 0.558 0.858 0.453 
			     0.885 0.539 0.914 0.439 0.938 0.541 0.965 0.433 1.000 0.472)
			   :duration (vct-ref durs 3) :scaler (hz->radians 6000)))))

    (do ((call 0 (1+ call)))
	((= call 4))
      (let* ((ampf (vector-ref ampfs call))
	     (frqf (vector-ref frqfs call))
	     (start (seconds->samples (+ beg (vct-ref begs call))))
	     (stop (+ start (seconds->samples (vct-ref durs call)))))
	(run
	 (lambda ()
	   (do ((i start (1+ i)))
	       ((= i stop))
	     (outa i (* (env ampf)
			(polyshape gen 1.0 (env frqf)))
		   *output*))))))))

;(with-sound (:play #t) (western-tanager 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Pileated woodpecker

(definstrument (pileated-woodpecker beg amp)
  (let* ((gen (make-polyshape 0.0 :partials (normalize-partials (list 1 .06  2 1.0  3 .1  4 .13  5 .07  6 .01  7 .015  8 .01  9 .017  10 .006))))
	 (start (seconds->samples beg))
	 (dur 2.28)
	 (stop (+ start (seconds->samples 2.28)))
	 (ampf (make-env '(0 .5 4 1 15 1) :duration dur :scaler amp))
	 (pulse-space .137)
	 (pulse-dur .06)
	 (pulse-ampf (make-env '(0.000 0.000  0.20 0.625  0.511 0.985  0.663 1.000  0.802 0.940  0.906 0.731  0.961 0.157  1.000 0.000 )
			       :duration pulse-dur))
	 (pulse-frqf (make-env '(0 0  .3 .9  .6 1  .8 1  1 .75) :duration pulse-dur :scaler (hz->radians 300)))
	 (pulse-off (make-env '(0 1120 .25 1140 .4 1190 .9 1150) :end 17))
	 (pulser (make-pulse-train (/ 1.0 pulse-space))))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (if (> (pulse-train pulser) .1)
	     (begin
	       (mus-reset pulse-ampf)
	       (mus-reset pulse-frqf)
	       (set! (mus-frequency gen) (- (env pulse-off) 300.0))))
	 (outa i (* (env ampf)
		    (env pulse-ampf)
		    (polyshape gen 1.0 (env pulse-frqf)))
		   *output*))))))

;(with-sound (:play #t) (pileated-woodpecker 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Whip-poor-will

(definstrument (whip-poor-will beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.75)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000  0.043 0.172  0.063 0.341 0.093 0.256 0.188 0.159 0.209 0.000 
			   0.466 0.000 0.527 0.097 0.558 0.081   0.626 0.309 0.650 0.288 
			   0.657 0.140 0.674 0.142 0.708 0.322 0.750 0.343 0.765 0.121 
			   0.787 0.201 0.805 0.381 0.826 0.470 
			   0.861 0.144 0.867 0.451 0.877 0.106 
			   0.890 0.964 
			   0.914 0.117 0.919 0.377 0.926 0.233 0.931 0.324 0.949 0.244 0.977 0.377 1.000 0.000)
			 :duration dur :scaler amp))
	 (gen1 (make-polyshape 0.0 :partials '(1 .98  2 .01  3 .01)))
	 (frqf (make-env '(0 1170  .093 1680  .2 1640
			   .46 1170  .558 1170  .626 1450  .650 1530
			   .66 1290  .707 1480  .750 1480  .765 1290
			   .78 1320  .8   1600  
			   .81 1300 .82 1900  .83 1500 .84 2100 .85 1700 .86 2150 .87 1900 
			   .89 2460 .914 2160  .93 1680 1.0 1600)
			 :duration dur :scaler (hz->radians 1.0))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (whip-poor-will 0 .25))


;;; --------------------------------------------------------------------------------
;;;
;;; Varied thrush

(definstrument (varied-thrush beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 1.02)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.052 0.100 0.130 0.538 0.261 0.845 0.438 0.983 0.580 0.917 0.738 0.720 0.860 0.475 0.941 0.172 1.000 0.000)
			 :duration dur :scaler (/ amp 2.25)))
	 (gen1 (make-rxyk!cos 3360 -200 0.7)) 
	 (gen2 (make-rxyk!cos 3760 200 0.3))
	 (gen3 (make-polyshape 3660 :partials '(1 .98 2 .02)))
	 (frqf (make-env '(0 1 .1 0 .95 0 1.0 -.1) :duration dur :scaler (hz->radians 10.0)))
	 (rnd (make-rand-interp 100 (hz->radians 3))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((fm (+ (env frqf)
		      (rand-interp rnd))))
	   (outa i (* (env ampf)
		      (+ (rxyk!cos gen1 (- fm) )
			 (rxyk!cos gen2 fm)
			 (* .25 (polyshape gen3 1.0 fm))))
		 *output*)))))))

;(with-sound (:play #t) (varied-thrush 0 .25))


;;; --------------------------------------------------------------------------------
;;;
;;; Nashville warbler

(definstrument (nashville-warbler beg amp)
  
  (define (nashville-warbler-1 beg dur amp)
    (let* ((start (seconds->samples beg))
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0.000 0.000 0.111 0.837 0.169 0.995 0.328 1.000 0.430 0.905 0.561 0.333 
			     0.595 0.000 0.61 0.0 0.62 0.154 0.715 0.675 0.806 0.959 0.969 0.908 1.000 0.000)
			   :duration dur :scaler amp))
	   (frqf (make-env '(0 7200  .1 5500  .3 4800  .56 4400
			     .59 4500 .62 7200 .72 7200
			     .8 6800 .83 6700
			     .85 6700 .87 6600
			     .9 6600  .92 6500
			     .96 6500  1 6520)
			   :duration dur :scaler (hz->radians 1.0)))
	   (gen1 (make-polyshape 0.0 :partials (list 1 .95  2 .005  3 .03))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf)
		      (polyshape gen1 1.0 (env frqf)))
		 *output*))))))

  (define (nashville-warbler-2 beg dur amp)
    (let* ((start (seconds->samples beg))
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0.000 0.000 0.049 0.165 0.082 0.591  0.123 0.561 0.139 0.434 0.219 0.981 0.325 0.984 
			     0.374 0.100 0.438 0.100 0.484 0.415 0.552 0.818 0.618 0.995 0.753 0.748 0.888 0.439 1.000 0.000)
			   :duration dur :scaler amp))
	   (frqf (make-env '(0 3450 .1 5000
			     .14 4800  .37 5600
			     .44 8600  .55 6600 .61 5300  1 3200)
			   :duration dur :scaler (hz->radians 1.0)))
	   (gen1 (make-polyshape 0.0 :partials (list 1 .95  2 .005  3 .03))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf)
		      (polyshape gen1 1.0 (env frqf)))
		 *output*))))))

  (define (nashville-warbler-3 beg amp)
    (let* ((start (seconds->samples beg))
	   (dur .1)
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0 0 1 1 3 1 4 0) :duration dur :scaler amp))
	   (frqf (make-env '(0 7580 .6 5100  1 5000) :duration dur :scaler (hz->radians 1.0)))
	   (gen1 (make-polyshape 0.0 :partials (list 1 .95  2 .005  3 .03))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf)
		      (polyshape gen1 1.0 (env frqf)))
		 *output*))))))

  (define (nashville-warbler-4 beg amp)
    (let* ((start (seconds->samples beg))
	   (dur 0.07)
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0.000 0.000 0.129 0.201 0.182 0.382 0.197 1.000 
			     0.243 0.350 0.342 0.520 0.393 0.759 0.485 0.878 0.647 0.694 1.000 0.000)
			   :duration dur :scaler amp))
	   (frqf (make-env '(0 3300 .195 4800  
			     .24 5500 .26 5500 .4 4500 .7 3460 1 3000)
			   :duration dur :scaler (hz->radians 1.0)))
	   (gen1 (make-polyshape 0.0 :partials (list 1 .96  2 .03  3 .005  4 .004))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf)
		      (polyshape gen1 1.0 (env frqf)))
		 *output*))))))

  (let* ((amps1 (vct .2 .5 .7 .9 1.0 1.0)))

    (do ((call 0 (1+ call)))
	((= call 6))
      (nashville-warbler-1 (+ beg (* .21 call)) (+ .15 (random .02)) (* amp (vct-ref amps1 call))))

    (do ((call 0 (1+ call)))
	((= call 3))
      (nashville-warbler-2 (+ beg 1.26 (* .17 call)) (+ .13 (random .02)) amp))

    (nashville-warbler-3 (+ beg 1.8) amp)

    (nashville-warbler-4 (+ beg 1.94) (* 0.4 amp))))

;(with-sound (:play #t) (nashville-warbler 0 .25))
      


;;; --------------------------------------------------------------------------------

(define (calling-all-animals)
  (with-sound (:play #t :scaled-to .5 :srate 44100) ;(srate needed by snd-test)
    (mosquito 0 5 560 .2)
    (mosquito 1 3 880 .05)
    (knudsens-frog 2 .5)
    (a-cricket 3 .12 4500 5400 .5 '(0 0 1 1 3 1 4 0) (/ .11 3) '(0 0 1 .8 5 1 6 0 15 0))
    (oak-toad 4 .3)
    (eastern-wood-pewee-1 5 .25)
    (eastern-wood-pewee-2 6 .25)
    (broad-winged-tree-cricket 6 4.0 0.2)
    (southern-cricket-frog 8 0.5)
    (long-spurred-meadow-katydid 9 .5)
    (northern-leopard-frog 10 .5)
    (southern-mole-cricket 11 6 .15)
    (green-tree-frog 13 .5)
    (spring-peeper 14 .5)
    (crawfish-frog 15 .5)
    (river-frog 16 .5)
    (indri 17 .25)
    (field-sparrow 18.5 .25)
    (handsome-trig 20.5 2 .5)
    (fast-calling-tree-cricket 22 2 .25)
    (dog-day-cicada 23 2 .1)
    (linnaeus-cicada 25 2 .125)
    (lyric-cicada 26 2 .125)
    (confused-ground-cricket 27 3 .3)
    (tinkling-ground-cricket 29.5 3 .3)
    (marsh-meadow-grasshopper 31.5 .3)
    (striped-ground-cricket 32.5 3 .25)
    (sphagnum-ground-cricket 34.5 2 .3)
    (fox-sparrow 35.5 3 .25)
    (southeastern-field-cricket 37.5 2 .13)
    (snowy-tree-cricket 39 2.1 .3)
    (slightly-musical-conehead 40 2 .4)
    (white-throated-sparrow 41 .25)
    (tufted-titmouse 44 .3)
    (savannah-sparrow 45 .5)
    (chipping-sparrow 47 .3)
    (pine-tree-cricket 49 2 .125)
    (davis-tree-cricket 51 2 .125)
    (carolina-grasshopper 52.5 1.5 1.0)
    (pinewoods-tree-frog 54 1 .15)
    (henslows-sparrow 55.5 .5)
    (least-flycatcher 56 .5)
    (acadian-flycatcher 57 .25)
    (swainsons-thrush 57.5 .25)
    (squirrel-tree-frog-1 59 1 .1)
    (carolina-wren 60 .25)
    (ornate-chorus-frog 61 2 .1)
    (bachmans-sparrow 62 .25)
    (grasshopper-sparrow 63 .25)
    (american-robin 64.5 .25)
    (common-loon-1 67 .125)
    (common-loon-2 70 .125)
    (hermit-thrush 71.5 .25)
    (chuck-wills-widow 73.5 .25)
    (california-towhee 74.5 .25)
    (black-chinned-sparrow 75.5 .25 #t)
    (mourning-dove 77 .125)
    (bobwhite 79.5 .25) 
    (warbling-vireo 82 .25)
    (great-horned-owl 83.5 .25)
    (western-tanager 86 .2) 
    (pileated-woodpecker 89 .125)
    (whip-poor-will 91.5 .25)
    (varied-thrush 93 .125)
    (nashville-warbler 94.5 .25)
    ))


