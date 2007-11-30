;;; animals.scm


;;; -------- insects --------
;;; mosquito
;;; Broad-winged tree-cricket
;;; Long-spurred meadow katydid
;;; Southern mole cricket
;;; Handsome trig
;;; Fast-calling tree cricket
;;; Dog-day cicada
;;; Linnaeus' cicada
;;; Lyric cicada
;;; Confused ground-cricket
;;; Tinkling ground-cricket
;;; Marsh meadow grasshopper
;;; Striped ground-cricket
;;; Sphagnum ground cricket
;;; Southeastern field cricket
;;; Snowy tree cricket
;;; Slightly musical conehead

;;; -------- frogs and toads
;;; Knudsen's frog
;;; Oak toad
;;; Southern cricket frog
;;; Northern leopard frog
;;; Green tree-frog
;;; Spring peeper
;;; Crawfish frog
;;; River frog

;;; -------- mammals --------
;;; Indri

;;; -------- birds --------
;;; Fox sparrow
;;; White-throated sparrow
;;; Henslow's sparrow
;;; Eastern wood-pewee (2)
;;; Field sparrow
;;; Tufted titmouse
;;; Savannah sparrow


(use-modules (ice-9 optargs) (ice-9 format))
(provide 'snd-animals.scm)

(if (not (provided? 'snd-generators.scm)) (load "generators.scm"))
(if (not (provided? 'snd-ws.scm)) (load "ws.scm"))


;;; many of these need srate=44100 since various frequencies are (well) over 10KHz
;;;   also, I have bare indices scattered around -- ideally these would be wrapped in hz->radians


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
			 :duration dur :scaler (hz->radians 1.0)))
	 (rnd (make-rand-interp 10 1)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (oscil gen1 (+ (env frqf)
				   (rand-interp rnd))))
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
    ;; ticks
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
    ;; ticks
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
    (field-sparrow 19 .25)
    (handsome-trig 20 2 .5)
    (fast-calling-tree-cricket 21 2 .25)
    (dog-day-cicada 22 2 .1)
    (linnaeus-cicada 23 2 .125)
    (lyric-cicada 24 2 .125)
    (confused-ground-cricket 25 3 .3)
    (tinkling-ground-cricket 26.5 3 .3)
    (marsh-meadow-grasshopper 28 .3)
    (striped-ground-cricket 29 3 .5)
    (sphagnum-ground-cricket 30 2 .3)
    (fox-sparrow 31 3 .25)
    (southeastern-field-cricket 33 5 .3)
    (snowy-tree-cricket 34 2.1 .3)
    (slightly-musical-conehead 35 2 .4)
    (white-throated-sparrow 36 .25)
    (henslows-sparrow 37 .25)
    (tufted-titmouse 37.5 .3)
    (savannah-sparrow 38 .5)
    ))


