;;; animals.scm


;;; sources:
;;; "The Diversity of Animal Sounds", Cornell Lab of Ornithology
;;; Geoffrey Keller, "Bird Songs of California" (Cornell)
;;; Bret Whitney et al, "Voices of New World Parrots" (Cornell)
;;; Carlos Davidson, "Frog and Toad Calls of the Rocky Mountains" (Cornell)
;;; Geoffrey Keller, "Bird Songs of the Lower Rio Grande Valley" (Cornell)
;;; "Voices of North American Owls" (Cornell)
;;; Roche and Chevereau, "Guide to the Sounds of the Birds of Europe"
;;; from Richard Mankin, Reference Library of Digitized Insect Sounds, http://www.ars.usda.gov/sp2UserFiles/person/3559/soundlibrary.html
;;; Lang Elliott, Donald and Lillian Stokes, "Stokes Field Guide to Bird Songs, Eastern Region"
;;; Lang Elliott "The Calls of Frogs and Toads"
;;; Lang Elliott and W Herschberger "The Songs of the Insects"
;;; Lang Elliott "Music of the Birds"
;;; Cocroft, Morales, McDiarmid "Frogs of Tambopata, Peru" (Cornell)
;;; Ross, Whitney, "Voices of Costa Rican Birds" (Cornell)
;;; Rebolledo, Ramirez, Cuervo, "A Guide to the Bird Sounds of the Colombian Andes" (Humboldt and Cornell



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
;;; Bullfrog
;;; Texas toad
;;; American toad
;;; Plains spadefoot
;;; Barking tree-frog
;;; Western toad
;;; Southwestern toad
;;; Great Plains Narrow-mouthed toad

;;; -------- mammals --------
;;; Indri

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
;;; Golden-crowned sparrow
;;; Cassin's sparrow
;;; Song sparrow
;;; Purple finch
;;; House finch
;;; Eastern wood-pewee (2)
;;; Tufted titmouse
;;; Oak titmouse
;;; Bushtit
;;; California towhee
;;; Green-tailed towhee
;;; Carolina wren
;;; Warbling vireo
;;; Plumbeous vireo (2)
;;; Cassin's vireo
;;; Hutton's vireo
;;; Gray vireo (2)
;;; Yellow-green vireo
;;; Nashville warbler
;;; Orange-crowned warbler
;;; Yellow warbler
;;; Yellow-rumped warbler
;;; Lucy's warbler
;;; Macgillivray's warbler
;;; Wilson's warbler
;;; Magnolia warbler
;;; Western meadowlark
;;; Eastern meadowlark
;;; Ruby-crowned kinglet
;;; Least flycatcher
;;; Acadian flycatcher
;;; Vermillion flycatcher
;;; Ash-throated flycatcher
;;; Olive-sided flycatcher
;;; Willow flycatcher
;;; Black phoebe
;;; Say's phoebe
;;; Northern beardless tyrannulet
;;; Eastern bluebird
;;; Common yellowthroat
;;; Blue grosbeak
;;; Evening grosbeak
;;; Cardinal
;;; Swainson's thrush
;;; American robin
;;; Scott's oriole
;;; Varied thrush
;;; Hermit thrush
;;; Western tanager
;;; Chuck-will's-widow
;;; Whip-poor-will
;;; Lesser nighthawk
;;; Mourning dove
;;; Bobwhite
;;; California Quail
;;; Ruffed grouse
;;; Great-horned owl
;;; Barred owl
;;; Flammulated owl
;;; Burrowing owl
;;; Northern goshawk
;;; Red-shouldered hawk
;;; Bald eagle
;;; Pileated woodpecker
;;; White-headed woodpecker
;;; Acorn woodpecker
;;; Red-breasted nuthatch
;;; white-breasted nuthatch
;;; Pygmy nuthatch
;;; Common loon (2)
;;; American crow
;;; Brown jay
;;; Steller's jay
;;; Pinyon jay
;;; Loggerhead shrike (2)
;;; Common Gull
;;; Black-necked stilt
;;; White-faced ibis
;;; Least bittern
;;; Black rail
;;; Sora
;;; Plain chacalaca
;;; Black-billed cuckoo
;;; Eared grebe
;;; Killdeer


(use-modules (ice-9 optargs) (ice-9 format))
(provide 'snd-animals.scm)

(if (not (provided? 'snd-generators.scm)) (load "generators.scm"))
(if (not (provided? 'snd-ws.scm)) (load "ws.scm"))


;;; many of these need srate=44100 since various frequencies are (well) over 10KHz
;;;   also, I have bare indices scattered around -- ideally these would be wrapped in hz->radians



;;; ================ Frogs and Toads ================
;;;
;;;
;;; Knudsen's frog

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
			 (* .1 (oscil gen6 (* 6 ind)))))
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
;;; Bullfrog

(definstrument (bullfrog beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.81)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0 0 1 1 2 1 3 0) :duration dur :scaler amp :base 10))
	 (frqf (make-env '(0 0  1 6  2 0) :duration dur :scaler (hz->radians 1.0)))

	 (f1 (make-rxyk!cos 200 100 0.6))
	 (f2 (make-rxyk!cos 230 100 1.2))

	 (f3 (make-rxyk!cos 600 100 8.0))
	 (f4 (make-rxyk!cos 630 100 8.0))

	 (rnd (make-rand-interp 4000 .2))
	 (rnd1 (make-rand-interp 200 (hz->radians 2)))

	 (frm1 (make-formant .99 400 7))
	 (frm2 (make-formant .98 1200 14))
	 (frm3 (make-formant .97 5000 4))

	 (intrpf (make-env '(0 1 .6 0 1 1) :duration dur)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let* ((frq (+ (env frqf)
			(rand-interp rnd1)))
		(intrp (env intrpf))
		(val (* (env ampf)
			(+ .8 (rand-interp rnd))
			(+ (rxyk!cos f1 frq)
			   (* .5 (rxyk!cos f2 frq))
			   (* .1 (rxyk!cos f3 frq))
			   (* .1 (rxyk!cos f4 frq))))))
	   (set! (mus-frequency frm2) (+ 1000 (* intrp 200)))
	   (outa i (+ (formant frm1 val)
		      (formant frm2 val)
		      (formant frm3 val))
		 *output*)))))))

;(with-sound (:play #t) (bullfrog 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Texas toad

(define (texas-toad beg1 dur1 amp1)

  (definstrument (texas-toad-1 beg dur amp)
    (let* ((start (seconds->samples beg))
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0 0 .1 1 25 1 26 0) :duration dur :scaler amp))
	   (pulse-dur .0173)
	   (gen (make-polyshape 2460 :partials (list 1 .9  2 .01  3 .05  4 .005  5 .01)))
	   (pulsef (make-env '(0 0 1 1 3 1 4 0) :duration pulse-dur))
	   (pulse2 (make-blackman (/ 4.0 pulse-dur) 2))
	   (pulser (make-pulse-train (/ 1.0 .02666)))
	   (rnd (make-rand-interp 4000 (hz->radians 200))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (if (> (pulse-train pulser) .1)
	       (begin
		 (mus-reset pulsef)
		 (mus-reset pulse2)))
	   (outa i (* (env ampf)
		      (env pulsef)
		      (blackman pulse2 0.0)
		      (polyshape gen 1.0 (rand-interp rnd)))
		 *output*))))))

  (let ((last-dur 0.0)
	(last-call (+ beg1 dur1 (- 0.4))))
    (do ((call-beg beg1 (+ call-beg last-dur 0.3 (random 0.2))))
	((>= call-beg last-call))
      (set! last-dur (+ .6 (random .25)))
      (texas-toad-1 call-beg last-dur amp1))))

;(with-sound (:play #t) (texas-toad 0 2.0 0.5))


;;; --------------------------------------------------------------------------------
;;;
;;; American toad

(definstrument (american-toad beg dur amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0 0 4 1 20 1 21 0) :duration dur :scaler amp))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .94  2 .03  3 .01  4 .003 5 .005  7 .002)))
	 (pulse-dur .024)
	 (frqf (make-env '(0 150 .1 250 .5 300 .9 200 1 0) :duration pulse-dur :scaler (hz->radians 1.0)))
	 (pulsef (make-env '(0.000 0.000 0.147 0.700 0.261 0.968 0.405 0.996 0.601 0.830 0.878 0.198 1.000 0.000) :duration pulse-dur))
	 (pulse-sep .045)
	 (pulse-samps (seconds->samples pulse-sep))
	 (next-pulse (+ start pulse-samps))
	 (pulse-frqf (make-env (list 0 1100 .4 1300 dur (- 1300 (* dur 8))) :duration dur :scaler (hz->radians 1.0))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (if (>= i next-pulse)
	     (begin
	       (set! next-pulse (+ i pulse-samps))
	       (mus-reset pulsef)
	       (mus-reset frqf)))
	 (outa i (* (env ampf)
		    (env pulsef)
		    (polyshape gen1 1.0 (+ (env frqf)
					   (env pulse-frqf))))
	       *output*))))))

;(with-sound (:play #t) (american-toad 0 2 .25))


;;; --------------------------------------------------------------------------------
;;;
;;; Plains spadefoot

(definstrument (plains-spadefoot beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.73)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.098 0.423 0.310 0.747 0.630 0.929 0.785 0.830 0.902 0.553 1.000 0.000) :scaler amp :duration dur))
	 (gen1 (make-oscil 0.0))
	 (gen2 (make-polyshape 0.0 :partials (list 1 .01 2 .01  6 .01  8 .1 10 .01)))
	 (ampf2 (make-env '(0 0 .3 0 .8 1 1 1) :duration dur :scaler 0.4))
	 (pulse-dur .019)
	 (frqf (make-env '(0 1520  .4 1650 1 1630) :duration dur :scaler (hz->radians 1.0)))
	 (pulsef (make-env '(0.000 0.000  0.03 1.000 0.08 1.0  0.160 0.486 0.304 0.202 0.508 0.087 1.000 0.000) :duration pulse-dur))
	 (pulse-samps (seconds->samples pulse-dur))
	 (next-pulse (+ start pulse-samps))
	 (rnd (make-rand-interp 100 (hz->radians 100))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (if (>= i next-pulse)
	     (begin
	       (set! next-pulse (+ i pulse-samps))
	       (mus-reset pulsef)))
	 (let* ((frq (+ (env frqf)
			(rand-interp rnd))))
	   (outa i (* (env ampf)
		      (env pulsef)
		      (+ (oscil gen1 frq)
			 (* (env ampf2)
			    (polyshape gen2 1.0 (* 0.25 frq)))))
		 *output*)))))))

;(with-sound (:play #t) (plains-spadefoot 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Barking tree-frog

(definstrument (barking-tree-frog beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.165)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.015 0.131 0.038 0.110 0.066 0.621 0.078 0.488 0.090 0.977 0.104 0.423 
			   0.108 0.013 0.113 0.504 0.122 0.005 0.129 0.979 0.138 0.337 0.142 0.470 0.152 0.008 
			   0.156 0.561 0.160 0.008 0.165 1.000 0.177 0.535 0.183 0.744 0.189 0.290 0.193 0.731 
			   0.200 0.381 0.209 0.977 0.217 0.499 0.237 0.846 0.247 0.896 0.260 0.898 0.464 0.846 
			   0.623 0.689 0.801 0.305 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0 480  .3 430 1 425) :duration dur :scaler (hz->radians 1.0)))
	 (gen1 (make-polyshape 0.0 :partials (normalize-partials (list 1 .9  2 .06  3 .25  4 .79  5 .18  6 .03  7 .02  8 .03  9 .01  10 .02  11 .005 12 .005))))
	 (rnd (make-rand-interp 1000 (hz->radians 10)))

	 (gen2 (make-oscil 0.0))
	 (frqf2 (make-env '(0 4750 .2 4790  .5 4710  1 4300) :duration dur :scaler (hz->radians 1.0)))
	 (attack (make-rand-interp 4000 (hz->radians 400)))
	 (gen3 (make-oscil 1720))
	 (attackf (make-env '(0.000 0.000 0.068 0.000 0.093 0.614 0.098 0.000 0.114 0.000 0.120 0.969 0.131 0.000 
			      0.155 0.000 0.159 0.997 0.175 0.000 0.198 0.000 0.2 1.000 0.224 0.000 0.241 0.000 
			      0.243 0.984 0.260 0.000 1.000 0.000)
			    :duration dur :scaler amp)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (+ (* (env ampf)
		       (+ (polyshape gen1 1.0 (+ (env frqf)
						 (rand-interp rnd)))
			  (* .02 (oscil gen2 (env frqf2)))))
		    (* (env attackf)
		       (oscil gen3 (rand-interp attack))))
	       *output*))))))

;(with-sound (:play #t) (barking-tree-frog 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Western toad

(definstrument (western-toad beg dur amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (gen (make-polyshape 0 :partials (list 1 .95  2 .02  3 .03  4 .005)))
	 (ampf (make-env '(0 0 1 1 8 1 9 0) :duration dur :scaler amp))
	 (cur-start start)
	 (cur-is-long #t))
    (do ()
	((>= cur-start stop))
      (let* ((pulse-samps (seconds->samples (if cur-is-long 
						(+ 0.04 (random .04)) 
						(+ .01 (random .02)))))
	     (pulse-ampf (make-env (if cur-is-long 
				       (vct 0 0 .1 .5 2 1 3 0) 
				       (vct 0 0 1 1 1.5 .3 2 0)) 
				   :scaler (if cur-is-long (+ .6 (random .4)) (+ .1 (random .7)))
				   :length pulse-samps
				   :base (if cur-is-long 6.0 3.0)))
	     (pulse-frqf (make-env (if cur-is-long
				       '(0 -.5 .5 0 1 -.3)
				       '(0 -1 .1 0 1 0))
				   :length pulse-samps
				   :base .1
				   :offset (hz->radians (if cur-is-long (if (> (random 1.0) .6) 1340 1260) 1200))
				   :scaler (hz->radians (random 500.0)))))
	(run
	 (lambda ()
	   (do ((i 0 (1+ i)))
	       ((= i pulse-samps))
	     (outa (+ cur-start i)
		   (* (env ampf)
		      (env pulse-ampf)
		      (polyshape gen 1.0 (env pulse-frqf)))
		   *output*))))

	(if cur-is-long
	    (set! cur-start (+ cur-start pulse-samps (seconds->samples (+ .015 (if (> (random 1.0) .8) 
										   (random .15) 
										   (random .04))))))
	    (set! cur-start (+ cur-start pulse-samps (seconds->samples (+ .01 (random .01))))))
	(set! cur-is-long (or (not cur-is-long) (> (random 1.0) .3)))))))

;(with-sound (:play #t) (western-toad  0 2 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Southwestern toad

(definstrument (southwestern-toad beg dur amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env (list 0 0    1.3 1   dur 1   (* 1.01 dur) 0) :duration dur :scaler amp :base .3))
	 (frqf (make-env (list 0 940  1 1230  dur 1230) :base 3.0 :duration dur :scaler (hz->radians 1.0) :offset (hz->radians -300)))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .95  2 .02  3 .03)))
	 (rnd (make-rand-interp 4000 (hz->radians 80)))
	 (pulse-dur 0.0135)
	 (pulse-space 0.0236)
	 (pulse-samps (seconds->samples pulse-space))
	 (pulse-ampf (make-env '(0 0 1 1 1.5 1 2 .5 3 0) :base .3 :duration pulse-dur))
	 (pulse-frqf (make-env '(0 0  .3 .8  1.5 1  2.7 .8  3 .3) :duration pulse-dur :scaler (hz->radians 300)))
	 (next-pulse (+ start pulse-samps)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (if (>= i next-pulse)
	     (begin
	       (mus-reset pulse-ampf)
	       (mus-reset pulse-frqf)
	       (set! next-pulse (+ next-pulse pulse-samps))))
	 (outa i (* (env ampf)
		    (env pulse-ampf)
		    (polyshape gen1 1.0 (+ (env frqf)
					   (env pulse-frqf)
					   (rand-interp rnd))))
	       *output*))))))

;(with-sound (:play #t) (southwestern-toad 0 2 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Great Plains Narrow-mouthed toad

(definstrument (great-plains-narrow-mouthed-toad beg dur1 amp)
  ;; rocky 75 28
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples (max dur1 0.3))))

	 ;; attack portion
	 (attack-dur 0.155)
	 (attack-stop (seconds->samples (+ beg attack-dur)))
	 (attack-ampf (make-env '(0.000 0.000 0.015 0.078 0.020 0.289 0.042 0.000 0.057 0.000 0.069 0.409 0.093 0.425 
				  0.101 0.520 0.113 0.588 0.141 0.218 0.159 0.537 0.182 0.695 0.221 0.140 0.223 0.640 
				  0.233 0.872 0.253 0.602 0.269 0.000 0.280 0.000 0.292 0.915 0.319 0.520 0.322 0.000 
				  0.333 0.000 0.347 0.912 0.359 0.777 0.371 0.509 0.381 0.000 0.391 0.000 0.399 0.611 
				  0.407 0.948 0.428 0.475 0.433 0.000 0.448 0.000 0.468 0.905 0.491 0.206 0.504 0.000 
				  0.513 0.000 0.528 0.909 0.537 0.583 0.543 0.125 0.554 0.000 0.572 0.911 0.578 0.957 
				  0.597 0.132 0.611 0.000 0.619 0.000 0.639 0.803 0.648 0.643 0.655 0.108 0.660 0.000 
				  0.677 0.000 0.683 0.629 0.693 0.902 0.706 0.186 0.716 0.000 0.731 0.000 0.736 0.568 
				  0.747 0.985 0.767 0.000 0.790 0.000 0.791 0.358 0.804 0.666 0.818 0.145 0.825 0.000 
				  0.842 0.000 0.848 0.408 0.857 0.768 0.878 0.000 0.898 0.000 0.904 0.511 0.915 0.883 
				  0.929 0.000 0.952 0.000 0.970 0.688 0.977 0.280 0.988 0.052 1.000 0.000)
				:duration attack-dur :scaler amp))
	 (attack-frqf (make-env '(0.000 0.124 0.037 0.145 0.060 0.205 0.067 0.152 0.091 0.132 0.107 0.145 0.126 0.175 
				  0.147 0.150 0.166 0.173 0.190 0.173 0.206 0.137 0.219 0.179 0.232 0.192 0.245 0.177 
				  0.260 0.158 0.274 0.177 0.288 0.203 0.298 0.186 0.312 0.167 0.325 0.147 0.336 0.190 
				  0.347 0.207 0.362 0.184 0.377 0.158 0.393 0.190 0.403 0.220 0.421 0.190 0.434 0.158 
				  0.453 0.197 0.465 0.229 0.480 0.199 0.496 0.169 0.515 0.197 0.525 0.218 0.532 0.194 
				  0.544 0.171 0.558 0.197 0.573 0.222 0.584 0.197 0.606 0.177 0.623 0.201 0.633 0.222 
				  0.645 0.203 0.661 0.182 0.676 0.209 0.690 0.222 0.700 0.203 0.717 0.177 0.735 0.207 
				  0.749 0.207 0.771 0.179 0.791 0.205 0.805 0.203 0.814 0.179 0.846 0.209 0.862 0.212 
				  0.869 0.186 0.912 0.207 0.969 0.220 1.000 0.218)
				:duration attack-dur :scaler (hz->radians 8900.0)))
	 (attack-gen1 (make-polyshape 0.0 :partials (list 1 .92 2 .05  3 .02  4 .01))))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i attack-stop))
	 (outa i (* (env attack-ampf)
		    (polyshape attack-gen1 1.0 (env attack-frqf)))
	       *output*))))

    ;; main portion
    (let* ((dur (- dur1 attack-dur))
	 (ampf (make-env '(0 1 1 1 10 1 11 0) :duration dur :scaler amp))
	 (frqf (make-env (list 0 2000  .9 2000 1 1700) :duration dur :scaler (hz->radians 1.0)))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .92 2 .05  3 .02  4 .01)))
	 (frqf2 (make-env '(0 0  .1 25  4 20) :duration dur :scaler (hz->radians 1.0)))
	 (gen2 (make-ncos2 100 17))
	 (gen3 (make-oscil 900))
	 (gen4 (make-oscil 400))
	 (low-ampf (make-env (list 0 0 .2 1 dur 1) :duration dur :scaler .15))
	 (pulser (make-pulse-train 100))
	 (pulse-ampf (make-env '(0.000 0.000 0.227 0.057 0.319 0.164 0.407 0.946 0.554 0.706 0.707 0.036 0.839 0.031 0.930 0.097 1.000 0.000) :duration .008))
	 (rnd (make-rand-interp 20 (hz->radians 3))))
      (run
       (lambda ()
	 (do ((i attack-stop (1+ i)))
	     ((= i stop))
	   (let ((frq (env frqf))
		 (frq2 (+ (env frqf2)
			  (rand-interp rnd))))
	     (if (> (pulse-train pulser frq2) 0.1)
		 (begin
		   (mus-reset pulse-ampf)
		   (set! (mus-phase gen1) (* pi .75))))
	     (outa i (* (env ampf)
			(env pulse-ampf)
			(+ (* (env low-ampf)
			      (+ (oscil gen3 (* 9 frq2))
				 (oscil gen4 (* 4 frq2))))
			   (polyshape gen1 1.0 frq)))
		   *output*))))))))

;(with-sound (:play #t) (great-plains-narrow-mouthed-toad 0 2 .25))




;;; ================ Mammals? ================
;;;
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



;;; ================ Insects ================
;;;
;;;
;;; mosquito 
;;;
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
			 (* .02 (oscil gen5 (* 3 md)))))
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
	 (frqf (make-env '(0 0 1 -.4 2 0 3 -.2 4 .3 6 -1.0) :scaler (hz->radians .4) :duration dur))
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


;;; ================ Birds ================
;;;
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
			 :duration dur :base .1 :scaler (hz->radians 1.0))))
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
	 (buzz-gen (make-polyshape 0.0 :partials (list 1 .98 2 .005 3 .01))))

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
			   0.921 0.000 0.931 0.000 0.934 0.303 0.947 0.466 0.960 0.418 0.980 0.258 1.000 0.000)
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
			   0.979 0.373 0.990 0.296 1.000 0.255)
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
	 (pulse-ampf (make-env '(0.000 0.000  0.20 0.625  0.511 0.985  0.663 1.000  0.802 0.940  0.906 0.731  0.961 0.157  1.000 0.000)
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
		      (+ (rxyk!cos gen1 (- fm))
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
;;;
;;; Ruffed grouse

(definstrument (ruffed-grouse beg amp)
  (let* ((dur 10.33)
	 (start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.006 0.097 0.045 0.135 0.091 0.244 0.141 0.223 
			   0.219 0.552 0.301 0.682 0.360 0.689 0.503 0.766 0.696 0.619 
			   0.751 0.622 0.806 0.705 0.820 0.552 0.829 0.787 0.849  0.687 
			   0.867 1.000 0.910 0.494 0.929 0.559 0.944 0.527 0.969 0.339 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env (list 0 (/ 1.0 .45) .18 (/ 1.0 .45)
			       .19 (/ 1.0 .8)
			       .22  (/ 1.0 .8)
			       .25 (/ 1.0 .6)
			       .5 (/ 1.0 .4)
			       .74 (/ 1.0 .15)
			       .9 (/ 1.0 .08)
			       1   (/ 1.0 .4))
			 :duration dur :scaler (hz->radians 1.0)))
	 (pulser (make-pulse-train 0.0))

	 (bump-dur 0.27)
	 (bump-samps (seconds->samples bump-dur))
	 (bump (make-env '(0.000 0.499 0.040 0.499 0.063 0.506 0.089 0.492 0.108 0.499 0.122 0.523 
			   0.134 0.506 0.143 0.462 0.153 0.425 0.164 0.457 0.171 0.508 0.173 0.580 
			   0.176 0.647 0.181 0.691 0.186 0.650 0.195 0.404 0.197 0.355 0.202 0.311 
			   0.208 0.362 0.222 0.657 0.229 0.696 0.235 0.661 0.258 0.350 0.263 0.311 
			   0.271 0.297 0.283 0.343 0.311 0.661 0.316 0.703 0.322 0.733 0.333 0.698 
			   0.340 0.643 0.375 0.343 0.379 0.304 0.389 0.278 0.404 0.353 0.443 0.624 
			   0.458 0.661 0.473 0.631 0.494 0.508 0.517 0.434 0.537 0.394 0.557 0.436 
			   0.589 0.520 0.618 0.564 0.644 0.538 0.679 0.490 0.703 0.473 0.736 0.483 
			   0.794 0.510 0.831 0.510 0.909 0.494 1.000 0.499)
			 :duration bump-dur :offset -0.5)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((>= i stop))
	 (let ((vol (env ampf)))
	   (if (> (pulse-train pulser (env frqf)) .1)
	       (let ((bump-amp (/ (* amp vol) .15))
		     (bump-stop (+ i bump-samps)))
		 (mus-reset bump)
		 (do ((k i (1+ k)))
		     ((= k bump-stop))
		   (outa k (* bump-amp (env bump)) *output*))))))))))
	      
;(with-sound (:play #t :statistics #t) (ruffed-grouse 0 0.5))


;;; --------------------------------------------------------------------------------
;;;
;;; Plumbeous vireo

(definstrument (plumbeous-vireo-1 beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.34)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 .02 .1  0.124 0.146 0.142 0.370 0.251 1.000 0.277 0.373  .29 .1  0.393 0.326 0.419 0.731 
			   0.568 0.407 0.713 0.286 0.885 0.351 0.947 0.311 0.967 0.123 1.000 0.000)
			 :duration dur :scaler amp))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .98 3 .01  5 .004)))
	 (ampf1a (make-env '(0 .2  .15 .25 .2 .01 .24 .02 .25 .1 .32 .1 .34 .005 .37 .001 .4 .05 .6 .03  1 0) :duration dur))
	 (gen1a (make-oscil 0.0))
	 (frqf (make-env '(0.000 0.181 0.054 0.175 0.072 0.187 0.087 0.156 0.097 0.177 0.118 0.154 0.151 0.259 0.201 
			   0.320 0.243 0.293 0.256 0.261 0.275 0.202 0.295 0.162 0.316 0.204 0.328 0.314 0.339 0.446 
			   0.359 0.489 0.382 0.454 0.394 0.352 0.425 0.286 0.449 0.277 0.467 0.246 0.494 0.238 0.507 
			   0.211 0.525 0.234 0.551 0.166 0.570 0.215 0.586 0.207 0.595 0.161 0.617 0.211 0.633 0.203 
			   0.642 0.159 0.657 0.207 0.692 0.168 0.711 0.231 0.728 0.227 0.742 0.188 0.750 0.257 0.795 
			   0.218 0.802 0.283 0.845 0.234 0.856 0.296 0.897 0.229 0.909 0.292 0.958 0.227 0.969 0.261 1.000 0.252)
			 :duration dur :scaler (hz->radians 10000.0)))
	 (gen2 (make-polyshape 0.0 :partials (list 1 .05 2 .1  3 .2  4 .3  5 .2  6 .1  7 .05)))
	 (ampf2 (make-env '(0 1  .15 0  1 0) :duration dur :scaler (* .5 amp)))
	 (frqf2 (make-env '(0 850 1 700) :duration dur :scaler (hz->radians 1.0)))
	 (rnd (make-rand-interp 1000 (hz->radians 50)))
	 (buzz (make-rand-interp 1000 (hz->radians 40))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((frq (+ (env frqf)
		       (rand-interp rnd))))
	   (outa i (* (env ampf)
		      (+ (polyshape gen1 1.0 frq)
			 (* (env ampf1a)
			    (oscil gen1a (* 2 frq)))
			 (* (env ampf2) 
			    (polyshape gen2 1.0 (+ (env frqf2)
						   (rand-interp buzz))))))
		 *output*)))))))

;(with-sound (:play #t) (plumbeous-vireo-1 0 .25))


(definstrument (plumbeous-vireo-2 beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.455)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.106 0.184 0.148 0.082 0.169 0.156 0.183 0.441 0.199 0.484 0.216 0.379 
			   0.234 0.770 0.244 0.748 0.252 0.527 0.261 0.992 0.320 0.961 0.343 0.742 0.360 0.832 
			   0.395 0.916 0.411 0.145 0.416 0.559 0.421 0.126 0.424 0.577 0.435 0.791 0.448 0.529 
			   0.452 0.089 0.460 0.639 0.472 0.777 0.485 0.181 0.495 0.656 0.499 0.319 0.506 0.854 
			   0.515 0.668 0.516 0.186 0.523 0.559 0.535 0.933 0.540 0.599 0.545 0.923 0.554 0.282 
			   0.562 0.795 0.569 0.609 0.574 0.988 0.579 0.908 0.588 0.772 0.593 0.161 0.597 0.646 
			   0.604 0.879 0.612 0.931 0.625 0.800 0.630 0.267 0.634 0.782 0.644 0.926 0.649 0.812 
			   0.652 0.423 0.658 0.955 0.664 0.718 0.670 0.973 0.675 0.235 0.682 0.752 0.691 1.000 
			   0.698 0.938 0.706 0.153 0.712 0.889 0.720 0.693 0.735 0.906 0.745 0.661 0.750 0.228 
			   0.754 0.594 0.757 0.683 0.763 0.411 0.767 0.621 0.779 0.797 0.793 0.693 0.797 0.532 
			   0.802 0.564 0.810 0.386 0.814 0.653 0.839 0.535 0.840 0.092 0.843 0.495 0.851 0.572 
			   0.862 0.627 0.870 0.490 0.873 0.592 0.882 0.416 0.888 0.054 0.889 0.359 0.896 0.465 
			   0.901 0.379 0.908 0.543 0.919 0.413 0.926 0.443 0.931 0.218 0.949 0.516 0.959 0.423 
			   0.979 0.151 1.000 0.000 )
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.030 0.200 0.053 0.223 0.086 0.262 0.135 0.262 0.179 0.225 0.200 0.215 0.210 0.257 
			   0.222 0.374 0.235 0.441 0.266 0.396 0.293 0.376 0.332 0.401 0.345 0.418 0.353 0.450 
			   0.365 0.413 0.382 0.465 0.397 0.428 0.413 0.505 0.427 0.433 0.446 0.537 0.461 0.438 
			   0.480 0.550 0.494 0.446 0.508 0.525 0.533 0.386 0.546 0.473 0.565 0.371 0.581 0.455 
			   0.607 0.344 0.618 0.426 0.646 0.324 0.658 0.391 0.687 0.300 0.704 0.364 0.730 0.302 
			   0.747 0.359 0.773 0.297 0.789 0.347 0.815 0.290 0.832 0.342 0.861 0.285 0.883 0.347 
			   0.907 0.285 0.923 0.332 1.000 0.252)
			 :duration dur :scaler (hz->radians 8100.0)))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .99  3 .005)))
	 (gen2 (make-oscil 0.0))
	 (gen3 (make-oscil 0.0))
	 (ampf2 (make-env '(0 .2  .1 1   .13 0   1 0) :duration dur :scaler .5))
	 (ampf3 (make-env '(0 .3  .1 .3  .16 1  .19 0  1 0) :duration dur :scaler 1))
	 (rnd (make-rand-interp 1000 (hz->radians 50))))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((frq (+ (env frqf)
		       (rand-interp rnd))))
	   (outa i (* (env ampf)
		      (+ (polyshape gen1 1.0 frq)
			 (* (env ampf2)
			    (oscil gen2 (* 1.5 frq)))
			 (* (env ampf3)
			    (oscil gen3 (* 2 frq)))))
		 *output*)))))))

;(with-sound (:play #t) (plumbeous-vireo-2 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Least bittern

(definstrument (least-bittern beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 1.25)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.100 0.123 0.388 0.454 0.884 0.649 0.995 0.754 0.776 1.000 0.100) :duration dur :scaler amp))
	 (frqf (make-env '(0 .6  1 .75  2 1  3 .40) :duration dur :scaler (hz->radians 90)))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .25  2 .6  3 .2  4 .01 5 .01)))
	 (rnd (make-rand-interp 200 (hz->radians 40)))

	 (pulser (make-pulse-train (/ 1.0 .13)))
	 (pulse-dur .09)
	 (pulse-samps (seconds->samples pulse-dur))
	 (pulse-ampf (make-env '(0.000 0.000 0.119 0.698 0.258 1.000 0.355 0.310 0.564 0.1  0.7 0.070  1.3 0) :duration pulse-dur))
	 (pulse-frqf (make-env '(0 150 .3 300 1 250 2 200 3 200 3.5 150 4 230) :duration pulse-dur :scaler (hz->radians 1.0)))
	 (pulse-rnd (make-rand-interp 4000 .2)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((vol (env ampf))
	       (pit (env frqf)))
	   (if (> (pulse-train pulser) .1)
	       (begin
		 (mus-reset pulse-ampf)
		 (mus-reset pulse-frqf)))
	   (outa i (* (env pulse-ampf)
		      vol
		      (+ (polyshape gen1 1.0 (+ pit
						(env pulse-frqf)
						(rand-interp rnd)))
			 (rand-interp pulse-rnd)))
		 *output*)))))))

;(with-sound (:play #t) (least-bittern 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; American crow

(definstrument (american-crow beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.27)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 .02 .1  .04 .01 .06 0.056 0.110 0.700 0.258 1.000  0.344 0.970  0.369 0.677 .7 .3  1.000 0.000)
			 :duration dur :scaler (* 2 amp)))
	 (frqf (make-env '(0.000 0.360 0.038 0.362 0.052 0.396 0.076 0.403 0.095 0.445 0.129 0.445 0.153 0.493 
			   0.201 0.495 0.231 0.501 0.260 0.490 0.297 0.503 0.317 0.499 0.346 0.473 0.407 0.473 
			   0.435 0.424 0.495 0.439 0.528 0.392 0.589 0.405 0.621 0.362 0.677 0.373 0.704 0.332 
			   0.767 0.325 0.791 0.281 0.832 0.278 0.859 0.251 0.890 0.225 0.912 0.255 0.950 0.263 1.000 0.26)
			 :duration dur :scaler (hz->radians 1250.0)))
	 (frm1 (make-formant .995 1400 20))
	 (frm2 (make-formant .98 5500 1))
	 (frm3 (make-formant .98 3800 2))
	 (gen (make-nrcos 0.0 15 .75))
	 (rnd (make-rand-interp 5000 .007)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((inp (* (env ampf)
		       (nrcos gen (+ (env frqf)
				     (rand-interp rnd))))))
	   (outa i (+ (formant frm1 inp)
		      (formant frm2 inp)
		      (formant frm3 inp))
		 *output*)))))))

;(with-sound (:play #t) (american-crow 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Orange-crowned warbler

(definstrument (orange-crowned-warbler beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 1.5)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.05  0.347 1.000 0.838 0.831 1.000 0.000) :duration dur :scaler amp))
	 (gen1 (make-oscil 0.0))
	 (frqf (make-env '(0 0 7 1 10 0) :duration dur :scaler (hz->radians 600.0)))

	 (pulse-dur 0.045)
	 (pulse-samps (seconds->samples pulse-dur))
	 (pulse-ampf1 (make-env '(0.000 0.000 0.038 0.152 0.062 0.115 0.148 0.468 0.211 0.530 0.228 0.290 
				 0.266 0.642 0.313 0.873 0.362 0.623 0.401 0.918 0.443 0.054 0.475 0.983 
				 0.490 0.901 0.501 0.290 0.525 0.668 0.576 0.189 0.605 0.155 0.621 0.313 
				 0.656 0.082 0.679 0.259 0.703 0.118 0.730 0.177 0.752 0.062 0.775 0.155 
				 0.798 0.048 0.812 0.099 0.831 0.059 0.885 0.096 0.922 0.048 0.947 0.087 1.000 0.000)
			       :duration pulse-dur))
	 (pulse-frqf1 (make-env '(0 3700 .2 4500  .3 4500 .45  4000 .8 4000 1 4300) :duration pulse-dur :scaler (hz->radians 1.0)))
	 (pulser (make-pulse-train (/ 1.0 pulse-dur)))

	 (call1-stop (+ start (seconds->samples 1.35)))
	 (call2-start (+ start (seconds->samples 1.36)))
	 (call2-dur 0.13)
	 (pulse-ampf2 (make-env '(0.000 0.000 0.074 0.994 0.135 0.375 0.184 0.637 0.238 0.721 0.290 0.411 0.381 0.003 
				  0.537 0.000 0.591 0.186 0.704 0.121 0.737 0.437 0.843 0.358 0.880 0.045 0.908 0.225 1.000 0.000)
				:duration call2-dur :scaler (* 0.5 amp)))
	 (pulse-frqf2 (make-env '(0 4800 .1 3950  .15 3820  .22 3950 .4 4800 .6 4600 .7 3480 .75 3940 .86 5200 1 5150)
				:duration call2-dur :scaler (hz->radians 1.0))))
   (run
     (lambda ()
       ;; 30 repetitions, then 2 special end tones
       (do ((i start (1+ i)))
	   ((= i call1-stop))
	 (let ((vol (env ampf))
	       (frq (env frqf)))
	   (if (> (pulse-train pulser) .1)
	       (begin
		 (mus-reset pulse-ampf1)
		 (mus-reset pulse-frqf1)))
	   (outa i (* vol
		      (env pulse-ampf1)
		      (oscil gen1 (+ frq (env pulse-frqf1))))
		 *output*)))

       (do ((i call2-start (1+ i)))
	   ((= i stop))
	 (outa i (* (env pulse-ampf2)
		    (oscil gen1 (env pulse-frqf2)))
	       *output*))))))

;(with-sound (:play #t) (orange-crowned-warbler 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Loggerhead shrike

(definstrument (loggerhead-shrike-1 beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.65)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0 0 .01 1 .99 1 1 0) :duration dur :scaler amp))
	 (gen1 (make-oscil 0.0))
	 (frqf (make-env '(0 0 1 1) :duration dur :scaler (hz->radians 100.0)))

	 (open-dur .036)
	 (open-samps (seconds->samples open-dur))
	 (open-stop (+ start open-samps))
	 (open-ampf (make-env '(0.000 0.000 0.158 0.063 0.280 0.162 0.386 0.775 0.418 0.644 
				0.465 0.000 0.573 0.000 0.592 0.921 .62 .1 0.633 0.866 
				0.726 0.000 0.788 0.000 0.829 0.399 0.889 0.154 1.000 0.000)
			      :duration open-dur :scaler .75))
	 (open-frqf (make-env '(0 4200 .35 3900 .46 3800
				.57 3600 .6 3900 .61 5100 .72 5100
				.8 3400 .83 4200 1 4200)
			      :duration open-dur :scaler (hz->radians 1.0)))

	 (pulse-dur .008)
	 (pulser (make-pulse-train (/ 1.0 pulse-dur)))
	 (pulse-ampf (make-env '(0 0 .05 1  .3 1  .8 .1  1 0) :duration pulse-dur))
	 (pulse-frqf (make-env '(0 3600 .2 4800 .5 5000 .85 4000 1 3700) :duration pulse-dur :scaler (hz->radians 1.0)))
	 (trem (make-oscil 160))
	 (rnd (make-rand-interp 500 .04))
	 (rnd1 (make-rand-interp 100 .01)))
   (run
     (lambda ()
       ;; special starting call, then the buzz
       (do ((i start (1+ i)))
	   ((= i open-stop))
	 (outa i (* (env open-ampf)
		    (env ampf)
		    (oscil gen1 (+ (env frqf) 
				   (env open-frqf))))
	       *output*))

       (do ((i open-stop (1+ i)))
	   ((= i stop))
	 (if (> (pulse-train pulser) .1)
	     (begin
	       (mus-reset pulse-ampf)
	       (mus-reset pulse-frqf)))
	 (outa i (* (env ampf)
		    (env pulse-ampf)
		    (+ .5 (* .5 (abs (oscil trem (rand-interp rnd1)))))
		    (oscil gen1 (+ (env frqf)
				   (env pulse-frqf)
				   (rand-interp rnd))))
	       *output*))))))

;(with-sound (:play #t) (loggerhead-shrike-1 0 .5))


(definstrument (loggerhead-shrike-2 beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.4)
	 (stop (+ start (seconds->samples dur)))
	 (frqs (vct .9 .95 .95 1.0 1.0 1.0))

	 (pulse-dur .08)
	 (pulse-samps (seconds->samples pulse-dur))
	 (pulse-ampf (make-env '(0.000 0.000 0.114 0.077 0.153 0.943 0.191 0.960 0.291 0.510 0.320 0.114 
				 0.342 0.647 0.373 0.191 0.400 0.490 0.419 0.066 0.456 0.291 0.728 0.288 
				 0.769 0.479 0.792 0.407 0.812 0.003 1.000 0.000)
			       :duration pulse-dur :scaler amp))
	 (frqf1 (make-env '(0.000 0.229  0.310 0.231  0.325 0.271  0.345 0.273  0.377 0.237  0.397 0.240  
			    0.615 0.235  0.7 0.235 1.000 0.235)
			  :duration pulse-dur :scaler (hz->radians 10500.0)))
	 (frqf2 (make-env '(0.000 0.13 .6 .13   0.7 0.15 .9 .15 1.000 0.1)
			  :duration pulse-dur :scaler (hz->radians 10500.0)))
	 (next-pulse (+ start pulse-samps))

	 (ampf1 (make-env '(0 1 .8 1 1 0) :duration pulse-dur))
	 (ampf2 (make-env '(0 0 .6 0 .7 1 1 1) :duration pulse-dur))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .95 2 .04 3 .005)))
	 (gen2 (make-polyshape 0.0 :partials (list 1 .5 2 .5 4 .01)))
	 (pulse-frq (vct-ref frqs 0))
	 (pulse-ctr 1)
	 (rnd (make-rand-interp 500 .02)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (if (>= i next-pulse)
	     (begin
	       (set! next-pulse (+ next-pulse pulse-samps))
	       (set! pulse-frq (vct-ref frqs pulse-ctr))
	       (set! pulse-ctr (+ pulse-ctr 1))
	       (mus-reset pulse-ampf)
	       (mus-reset ampf1)
	       (mus-reset ampf2)
	       (mus-reset frqf1)
	       (mus-reset frqf2)))

	 (let ((noise (rand-interp rnd)))
	   (outa i (* (env pulse-ampf)
		      (+ (* (env ampf1)
			    (polyshape gen1 1.0 (* pulse-frq (+ noise (env frqf1)))))
			 (* (env ampf2)
			    (polyshape gen2 1.0 (* pulse-frq (+ noise (env frqf2)))))))
		 *output*)))))))

;(with-sound (:play #t) (loggerhead-shrike-2 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; California Quail

(definstrument (california-quail beg amp)
  (let* ((durs (vct .075 .23 .16))
	 (begs (vct 0.0 .21 .58))
	 (ampfs (make-vector 3 #f))
	 (frqfs (make-vector 3 #f))
	 (gens (make-vector 3 #f))
	 (starts (make-vector 3 0))
	 (stops (make-vector 3 0))
	 (frm1 (make-formant .995 1000 5))
	 (frm2 (make-formant .99 1700 15))
	 (frm3 (make-formant .98 5600 5)))

    (do ((i 0 (1+ i)))
	((= i 3))
      (vector-set! starts i (seconds->samples (+ beg (vct-ref begs i))))
      (vector-set! stops i (+ (vector-ref starts i) (seconds->samples (vct-ref durs i)))))
    
    (vector-set! gens 0 (make-sine-summation 530 0.0 8 .5))
    (vector-set! gens 1 (make-sine-summation 450 0.0 15 .6))
    (vector-set! gens 2 (make-sine-summation 400 0.0 8 .5))

    (vector-set! ampfs 0 (make-env '(0 0 1.25 1 1.75 1 3 0) :base 10 :duration (vct-ref durs 0) :scaler (* amp 0.25)))
    (vector-set! ampfs 1 (make-env '(0.000 0.000 0.208 0.719 0.292 0.965 0.809 0.869 0.928 0.682 1.000 0.000) :base 10 :duration (vct-ref durs 1) :scaler (* 0.5 amp)))
    (vector-set! ampfs 2 (make-env '(0 0 1 1 3 1 6 0) :base 10 :duration (vct-ref durs 2) :scaler (* amp .375)))

    (vector-set! frqfs 0 (make-env '(0 0 1.3 1 2 0) :duration (vct-ref durs 0) :scaler (hz->radians 300)))
    (vector-set! frqfs 1 (make-env '(0 0  1.5 .8 2.5 1 4 .95 5 .25) :base .03 :duration (vct-ref durs 1) :scaler (hz->radians 520)))
    (vector-set! frqfs 2 (make-env '(0 0 .2 .7  .3 1  1 .5) :duration (vct-ref durs 2) :scaler (hz->radians 450.0)))

   (run
     (lambda ()
       (do ((k 0 (1+ k)))
	   ((= k 3))
	 (let ((start (vector-ref starts k))
	       (stop (vector-ref stops k))
	       (ampf (vector-ref ampfs k))
	       (frqf (vector-ref frqfs k))
	       (gen (vector-ref gens k)))
	   (do ((i start (1+ i)))
	       ((= i stop))
	     (let ((val (* (env ampf)
			   (sine-summation gen (env frqf)))))
	       (outa i (+ (formant frm1 val)
			  (formant frm2 val)
			  (formant frm3 val))
		     *output*)))))))))

;(with-sound (:play #t) (california-quail 0 .25))


;;; --------------------------------------------------------------------------------
;;;
;;; Vermillion flycatcher

(definstrument (vermillion-flycatcher beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.72)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.023 0.527 
			   0.045 0.000 0.264 0.000 0.282 0.868 0.295 0.231 
			   0.312 0.000 0.462 0.000 0.481 0.918 0.503 0.206 
			   0.520 0.000 0.598 0.000 0.614 0.848 0.633 0.729 0.648 0.164 
			   0.660 0.000 0.685 0.000 0.696 0.654 0.709 0.746 0.719 0.532 
			   0.738 0.000 0.747 0.100 0.750 0.570 0.770 0.435 
			   0.779 0.000 0.814 0.000 0.823 0.923 0.836 0.525 0.840 0.998 0.855 0.968 0.866 0.321 
			   0.883 0.095 0.909 0.517 0.924 0.600 0.961 0.440 1.000 0.000)
			 :duration dur :scaler amp))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .98  2 .015  3 .006)))
	 (frqf (make-env '(0.000 0.378 0.022 0.474 0.032 0.400 
			   0.266 0.378 0.278 0.496 0.297 0.402 0.462 0.351 0.469 0.466 0.482 
			   0.526 0.504 0.398 0.602 0.378 0.607 0.486 0.617 0.546 0.637 0.490 0.643 0.398 
			   0.686 0.402 0.689 0.510 0.709 0.582 0.721 0.560 0.736 0.558 0.744 0.406 0.752 0.562 0.767 0.679 
			   0.793 0.394 0.810 0.394 0.827 0.851 0.836 0.633 0.849 0.564 0.856 0.478 0.871 0.448 
			   0.887 0.480 0.902 0.506 0.924 0.510 0.950 0.500 0.973 0.466 1.000 0.470)
			 :duration dur :scaler (hz->radians 8000.0))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (vermillion-flycatcher 0 .5))


;;;--------------------------------------------------------------------------------
;;;
;;; Cardinal

(definstrument (cardinal beg amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples 3.26)))
	 (call 0)
	 (next-call (+ start (seconds->samples 0.35)))
	 (gen1 (make-polyshape 0.0 :partials (normalize-partials (list 1 1  2 .08  3 .01  4 .05  5 .005  6 .01))))

	 (call1-dur 0.185)
	 (call1-ampf (make-env '(0.000 0.000 0.174 0.333 0.243 0.273 0.332 0.446 0.391 0.373 0.446 0.488 
				 0.496 0.363 0.545 0.064 0.606 0.048 0.632 0.614 0.676 0.783 0.732 0.655 
				 0.764 0.667 0.802 0.992 0.841 0.659 0.888 0.633 0.951 0.347 0.974 0.122 1.000 0.000)
			       :duration call1-dur :scaler (* 0.5 amp)))
	 (call1-frqf (make-env '(0.000 0.299 0.230 0.320 0.387 0.339 0.513 0.349 0.586 0.349 0.610 0.534 
				 0.622 0.570 0.654 0.585 0.703 0.594 0.753 0.584 0.778 0.566 0.803 0.560 
				 0.911 0.434 1.000 0.435)
			       :duration call1-dur :scaler (hz->radians 6000.0)))

	 (call2-dur 0.19)
	 (call2-ampf (make-env '(0.000 0.000 0.041 0.159 0.101 0.263 0.167 0.247 0.246 0.126 0.266 0.150 
				 0.443 0.000 0.573 0.000 0.599 0.202 0.635 0.299 0.667 0.273 0.683 0.371 0.724 0.411 
				 0.796 0.000 0.83 0.0 0.848 0.155 0.870 1.000 0.925 0.639 0.951 0.126 1.000 0.000)
			       :duration call2-dur :scaler amp))
	 (call2-frqf (make-env '(0.000 0.138 0.032 0.173 0.063 0.187 0.215 0.176 0.403 0.140 0.542 0.117 0.590 0.214 
				 0.659 0.218 0.750 0.250 0.794 0.244 0.832 0.618  0.843 0.518   
				 0.876 0.352 0.909 0.335 0.954 0.323 1.000 0.311)
			       :duration call2-dur :scaler (hz->radians 10000.0))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (if (>= i next-call)
	     (begin
	       (set! call (1+ call))
	       (if (= call 1)
		   (begin
		     (set! next-call (+ start (seconds->samples 0.63)))
		     (mus-reset call1-ampf)
		     (mus-reset call1-frqf))
		   (begin
		     (set! next-call (+ next-call (seconds->samples 0.24)))
		     (mus-reset call2-ampf)
		     (mus-reset call2-frqf)))))
	 (outa i (if (< call 2)
		     (* (env call1-ampf)
			(polyshape gen1 1.0 (env call1-frqf)))
		     (* (env call2-ampf)
			(polyshape gen1 1.0 (env call2-frqf))))
	       *output*))))))

;(with-sound (:play #t) (cardinal 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Black phoebe

(definstrument (black-phoebe beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.36)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.082 0.899 0.098 0.957 0.118 0.892 0.142 0.396 
			   0.181 0.000 0.287 0.000 0.367 0.661 0.396 0.000 0.440 0.778 
			   0.458 0.739 0.479 0.300 0.507 0.636 0.532 0.558 0.555 0.380 
			   0.588 0.535 0.807 0.325 0.926 0.181 1.000 0.000)
			 :duration dur :scaler amp))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .9  2 .1  3 .006)))
	 (frqf (make-env '(0.000 0.167 0.066 0.212 0.077 0.234 0.104 0.231 0.132 0.187 
			   0.148 0.181 0.166 0.153 0.231 0.146 0.289 0.101 0.298 0.196 
			   0.319 0.229 0.339 0.222 0.349 0.240 0.357 0.219 0.377 0.159 
			   0.388 0.146 0.401 0.167 0.417 0.199 0.438 0.209 0.456 0.202 
			   0.467 0.177 0.479 0.174 0.485 0.196 0.503 0.206 0.531 0.201 
			   0.550 0.176 0.563 0.194 0.602 0.196 0.622 0.186 0.658 0.192 
			   0.931 0.163 1.000 0.141)
			 :duration dur :scaler (hz->radians 22050.0))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (black-phoebe 0 .25))


;;; --------------------------------------------------------------------------------
;;;
;;; Yellow warbler

(definstrument (yellow-warbler beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 1.38)
	 (stop (+ start (seconds->samples dur)))
	 (gen1 (make-oscil 0.0))

	 (ampf (make-env '(0.000 0.000 0.028 0.077 0.056 0.000 0.135 0.000 0.156 0.125 0.182 0.112 0.197 0.000 
			   0.268 0.000 0.287 0.235 0.314 0.243 0.326 0.000 0.406 0.000 0.415 0.339 0.440 0.301 0.463 0.000 
			   0.486 0.000 0.499 0.403 0.513 0.611 0.531 0.592 0.553 0.000 
			   0.582 0.000 0.596 0.517 0.606 0.648 0.627 0.621 0.640 0.000 0.667 0.000 0.673 0.533 0.696 0.896 0.720 0.000 
			   0.750 0.000 0.774 1.000 0.800 0.000 0.831 0.000 0.858 0.971 0.884 0.000 
			   0.905 0.000 0.926 0.349 0.942 0.424 0.978 0.421 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.000 0.827 0.026 0.661 0.042 0.706 0.104 0.695 0.134 0.909 0.167 0.672 0.184 0.708 
			   0.229 0.677 0.271 0.909 0.292 0.715 0.303 0.670 0.310 0.713 0.342 0.674 0.396 0.911 
			   0.418 0.715 0.425 0.672 0.441 0.727 0.480 0.720 0.487 0.476 0.491 0.533 0.510 0.558 
			   0.526 0.704 0.539 0.765 0.563 0.754 0.578 0.472 0.582 0.526 0.594 0.540 0.618 0.720 
			   0.630 0.781 0.656 0.765 0.674 0.683 0.693 0.567 0.713 0.408 0.735 0.410 0.751 0.711 
			   0.765 0.654 0.795 0.358 0.817 0.355 0.826 0.708 0.839 0.681 0.854 0.565 0.881 0.330 
			   0.904 0.308 0.924 0.351 0.957 0.460 0.967 0.408 0.976 0.362 1.000 0.314)
			 :duration dur :scaler (hz->radians 10000.0))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (oscil gen1 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (yellow-warbler 0 .25))


;;; --------------------------------------------------------------------------------
;;;
;;; Barred owl (the "hoo-aw" call, not the more complex one -- I haven't succeeded with the latter yet)

(definstrument (barred-owl-1 beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 1.27)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.030 0.308 0.052 0.345 0.057 0.932 0.104 0.567 0.161 0.605 0.259 0.510 
			   0.299 0.399 0.371 0.535 0.396 0.463 0.472 0.678 0.495 1.000 0.517 0.995 
			   0.534 0.000 0.538 0.365 0.560 0.435 0.630 0.254 0.828 0.338 0.850 0.190 
			   0.897 0.154 0.929 0.079 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.000 0.09 0.025 0.09  0.092 0.156  0.435 0.149 0.491 0.132 0.517 0.124 
			   0.53 .124 0.536 0.088 0.830 0.061 1.000 0.013)
			 :base .03 :duration dur :scaler (hz->radians 4060.0)))

	 (gen1 (make-nrcos 0.0 9 .5))
	 (vib (make-oscil 12))
	 (rnd (make-rand-interp 300 (hz->radians 5)))
	 (intrpf (make-env '(0 0 .53 0 .54 1 1 1) :duration dur))
	 (rnd1 (make-rand-interp 1000 .1))

	 (frm1 (make-formant .995 730 15))
	 (frm2 (make-formant .999 1090 1))
	 (frm3 (make-formant .993 2240 1)))

   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let* ((intrp (env intrpf))
		(frq (+ (env frqf)
			(* intrp (+ (* (hz->radians 7) (oscil vib))
				    (rand-interp rnd)))))
		(inp (* (env ampf) 
			(+ .9 (rand-interp rnd1))
			(nrcos gen1 frq))))
	   (set! (mus-frequency frm1) (+ 550 (* intrp 80)))
	   (set! (mus-frequency frm2) (- 1500 (* intrp 400)))
	   (set! (mus-frequency frm3) (+ 2300 (* intrp 150)))
	   (outa i (+ (formant frm1 inp)
		      (formant frm2 inp)
		      (formant frm3 inp))
		 *output*)))))))

;(with-sound (:play #t) (barred-owl-1 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Say's phoebe

(definstrument (says-phoebe beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.64)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.017 0.223 0.039 0.372 0.056 0.000 0.101 0.000 0.122 0.132 
			   0.145 0.000 0.192 0.000 0.214 0.639 0.232 0.507 0.324 0.981 0.415 0.402 
			   0.496 0.413 0.610 0.317 0.771 0.287 0.970 0.179 1.000 0.000)
			 :duration dur :scaler amp))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .98  2 .07  3 .08  4 .01 5 .003)))
	 (frqf (make-env '(0.000 0.221 0.022 0.305 0.032 0.318 0.040 0.297 0.054 0.225 0.106 0.229 
			   0.111 0.250 0.130 0.240 0.139 0.215 0.196 0.238 0.205 0.274 0.223 0.322 
			   0.249 0.337 0.295 0.333 0.324 0.310 0.356 0.293 0.584 0.265 0.781 0.261 
			   0.958 0.250 1.000 0.204)
			 :duration dur :scaler (hz->radians 10040.0))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (says-phoebe 0 .25))


;;; --------------------------------------------------------------------------------
;;;
;;; Yellow-rumped warbler

(definstrument (yellow-rumped-warbler beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 1.6)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.009 0.038 0.022 0.220 0.028 0.000 0.037 0.152 0.048 0.000 0.079 0.000 
			   0.088 0.218 0.094 0.000 0.109 0.132 0.122 0.488 0.127 0.363 0.128 0.000 0.133 0.303 
			   0.141 0.381 0.147 0.000 0.178 0.000 0.196 0.296 0.201 0.000 0.216 0.432 0.227 0.515 
			   0.231 0.000 0.242 0.601 0.250 0.000 0.269 0.000 0.290 0.187 0.298 0.040 0.305 0.227 
			   0.311 0.000 0.316 0.486 0.327 0.579 0.331 0.000 0.336 0.407 0.343 0.459 0.345 0.000 
			   0.371 0.000 0.376 0.085 0.386 0.122 0.393 0.399 0.397 0.000 0.405 0.194 0.412 0.045 
			   0.419 0.470 0.428 0.505 0.431 0.000 0.444 0.630 0.446 0.100 0.458 0.000 0.474 0.000 
			   0.477 0.114 0.488 0.169 0.496 0.412 0.500 0.000 0.505 0.207 0.509 0.100 0.516 0.265 
			   0.524 0.748 0.530 0.599 0.537 0.000 0.541 0.613 0.545 0.819 0.551 0.000 0.577 0.000 
			   0.584 0.151 0.591 0.200 0.601 0.577 0.607 0.004 0.610 0.319 0.614 0.111 0.618 0.283 
			   0.621 0.132 0.626 0.993 0.637 0.924 0.642 0.000 0.644 0.621 0.652 0.855 0.655 0.000 
			   0.681 0.000 0.687 0.156 0.696 0.214 0.706 0.613 0.709 0.000 0.711 0.187 0.719 0.120 
			   0.726 0.902 0.733 0.494 0.737 0.955 0.749 0.000 0.753 0.673 0.768 0.000 0.784 0.000 
			   0.810 0.768 0.821 0.227 0.829 0.000 0.849 0.592 0.855 0.281 0.863 0.000 0.874 0.354 
			   0.875 0.000 0.901 0.000 0.913 0.430 0.931 0.517 0.941 0.000 0.948 0.000 0.965 0.454 
			   0.971 0.169 0.975 0.120 0.987 0.225 0.993 0.045 1.000 0.000)
			 :duration dur :scaler amp))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .99  2 .01 3 .005)))
	 (frqf (make-env '(0.000 0.371 0.017 0.571 0.030 0.491 0.035 0.397 0.037 0.321 0.043 0.466 0.047 0.438 
			   0.052 0.387 0.087 0.344 0.095 0.417 0.102 0.290 0.106 0.458 0.108 0.507 0.117 0.542 
			   0.122 0.513 0.131 0.446 0.135 0.384 0.136 0.323 0.145 0.446 0.150 0.407 0.151 0.356 
			   0.185 0.403 0.189 0.442 0.190 0.337 0.201 0.440 0.205 0.294 0.212 0.483 0.218 0.536 
			   0.224 0.515 0.234 0.434 0.235 0.317 0.243 0.446 0.248 0.436 0.252 0.309 0.279 0.352 
			   0.286 0.438 0.290 0.405 0.291 0.356 0.298 0.425 0.302 0.307 0.307 0.468 0.311 0.530 
			   0.318 0.530 0.330 0.436 0.335 0.350 0.340 0.429 0.343 0.458 0.345 0.436 0.350 0.382 
			   0.377 0.389 0.381 0.434 0.386 0.350 0.397 0.446 0.403 0.331 0.408 0.495 0.411 0.517 
			   0.417 0.526 0.428 0.458 0.431 0.417 0.434 0.348 0.441 0.476 0.449 0.401 0.479 0.362 
			   0.485 0.431 0.490 0.354 0.499 0.438 0.502 0.342 0.509 0.515 0.513 0.540 0.521 0.515 
			   0.529 0.456 0.536 0.366 0.541 0.479 0.551 0.374 0.582 0.370 0.588 0.440 0.591 0.348 
			   0.601 0.421 0.605 0.356 0.611 0.515 0.616 0.532 0.623 0.517 0.636 0.425 0.638 0.339 
			   0.644 0.464 0.650 0.438 0.654 0.395 0.655 0.344 0.680 0.368 0.688 0.425 0.692 0.356 
			   0.702 0.421 0.706 0.344 0.710 0.497 0.718 0.509 0.727 0.476 0.740 0.397 0.743 0.470 
			   0.747 0.534 0.754 0.497 0.760 0.446 0.788 0.499 0.793 0.528 0.796 0.448 0.814 0.507 
			   0.816 0.374 0.826 0.352 0.835 0.360 0.839 0.389 0.846 0.438 0.851 0.337 0.862 0.333 
			   0.868 0.395 0.900 0.485 0.907 0.489 0.909 0.431 0.922 0.452 0.926 0.485 0.934 0.317 
			   0.943 0.350 0.954 0.352 0.961 0.423 0.965 0.325 0.974 0.317 0.983 0.376 0.985 0.436 1.000 0.454)
			 :duration dur :scaler (hz->radians 10125.0))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (yellow-rumped-warbler 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Purple finch

(definstrument (purple-finch beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 2.5)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.0000 0.0000 0.0150 0.0420 0.0210 0.0018 0.0291 0.0804 0.0355 0.1682 0.0402 0.0658 
			   0.0428 0.2468 0.0449 0.0676 0.0505 0.1243 0.0522 0.0000 0.0651 0.0000 0.0702 0.1298 
			   0.0741 0.0841 0.0766 0.1590 0.0848 0.0000 0.0890 0.0530 0.0950 0.2724 0.1006 0.2249 
			   0.1032 0.1499 0.1057 0.3784 0.1104 0.2578 0.1143 0.0000 0.1259 0.0000 0.1284 0.1901 
			   0.1323 0.1079 0.1361 0.2358 0.1417 0.0000 0.1468 0.0000 0.1507 0.2486 0.1558 0.2888 
			   0.1614 0.1207 0.1648 0.5503 0.1678 0.2267 0.1691 0.3693 0.1717 0.0000 0.1858 0.0000 
			   0.1896 0.1938 0.1931 0.0859 0.1952 0.2267 0.2016 0.0000 0.2063 0.0420 0.2149 0.4771 
			   0.2188 0.4004 0.2209 0.1865 0.2256 0.6399 0.2265 0.2596 0.2295 0.4644 0.2354 0.0000 
			   0.2436 0.0000 0.2491 0.2431 0.2513 0.1718 0.2568 0.3528 0.2616 0.0000 0.2748 0.0640 
			   0.2868 0.4388 0.2967 0.4954 0.3018 0.0000 0.3215 0.0000 0.3318 0.1865 0.3339 0.1097 
			   0.3412 0.7697 0.3438 0.3857 0.3450 0.7770 0.3472 0.4260 0.3485 0.7843 0.3502 0.3565 
			   0.3528 0.7751 0.3566 0.0000 0.3669 0.0000 0.3716 0.5686 0.3737 0.2303 0.3776 0.9854 
			   0.3801 0.3108 0.3840 0.3309 0.3857 0.5960 0.3908 0.1207 0.3934 0.5759 0.4003 0.0000 
			   0.4127 0.0000 0.4199 0.2176 0.4234 0.1152 0.4272 0.5850 0.4281 0.2980 0.4324 0.5009 
			   0.4345 0.3693 0.4379 0.4241 0.4405 0.2943 0.4452 0.5046 0.4533 0.1408 0.4568 0.5795 
			   0.4606 0.0000 0.4735 0.0000 0.4837 0.8501 0.4842 0.6088 0.4880 0.7093 0.4893 0.6581 
			   0.4923 0.9049 0.4953 0.1828 0.4974 0.7770 0.5009 0.0859 0.5060 0.8537 0.5132 0.0000 
			   0.5256 0.0000 0.5321 0.7477 0.5368 0.3364 0.5420 0.6527 0.5454 0.2834 0.5488 0.2212 
			   0.5518 0.0512 0.5552 0.6691 0.5634 0.0000 0.5766 0.0000 0.5775 0.3364 0.5809 0.0000 
			   0.5826 0.4826 0.5860 0.0037 0.5920 0.6362 0.5963 0.5448 0.5967 0.6600 0.5993 0.5759 
			   0.6002 0.6289 0.6040 0.5503 0.6100 0.9360 0.6160 0.0000 0.6271 0.0000 0.6280 0.3583 
			   0.6322 0.0000 0.6327 0.4388 0.6366 0.0000 0.6430 0.7733 0.6468 0.6088 0.6481 0.7532 
			   0.6528 0.7166 0.6541 0.4899 0.6563 0.8355 0.6648 0.0000 0.6777 0.0000 0.6781 0.2523 
			   0.6817 0.0000 0.6832 0.2943 0.6870 0.0000 0.6952 0.5850 0.7033 0.4095 0.7098 0.6728 
			   0.7145 0.0000 0.7286 0.0000 0.7295 0.1609 0.7334 0.0000 0.7359 0.3382 0.7380 0.0000 
			   0.7483 0.4845 0.7539 0.5814 0.7624 0.0000 0.7748 0.0000 0.7783 0.2633 0.7811 0.0000 
			   0.7825 0.2541 0.7869 0.0000 0.7958 0.4717 0.8031 0.5393 0.8116 0.0000 0.8271 0.0000 
			   0.8288 0.2431 0.8315 0.0000 0.8339 0.0000 0.8399 0.2687 0.8433 0.1700 0.8446 0.3967 
			   0.8506 0.4826 0.8600 0.0000 0.8754 0.0000 0.8767 0.1207 0.8796 0.0000 0.8818 0.2377 
			   0.8844 0.0000 0.8951 0.3528 0.9114 0.0000 0.9255 0.0000 0.9285 0.1024 0.9322 0.0000 
			   0.9336 0.1737 0.9388 0.0000 0.9439 0.0859 0.9482 0.2431 0.9521 0.1024 0.9546 0.2761 
			   0.9636 0.0000 0.9850 0.0000 0.9927 0.0402 1.0000 0.0000)
			 :duration dur :scaler amp))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .98  2 .01  3 .005  4 .003)))
	 (rnd (make-rand-interp 800 (hz->radians 200)))
	 (frqf (make-env '(0.0000 0.2374 0.0125 0.1869 0.0192 0.1616 0.0250 0.1835 0.0370 0.3316 0.0388 0.2407 
			   0.0419 0.3013 0.0450 0.2003 0.0473 0.2694 0.0513 0.2155 0.0646 0.2003 0.0727 0.2761 
			   0.0728 0.2020 0.0798 0.2559 0.0852 0.1751 0.0936 0.2424 0.0994 0.3535 0.1025 0.2424 
			   0.1048 0.3030 0.1074 0.2138 0.1110 0.2576 0.1271 0.2290 0.1315 0.2037 0.1355 0.2374 
			   0.1404 0.3451 0.1453 0.1684 0.1587 0.3350 0.1605 0.2542 0.1632 0.3131 0.1658 0.2525 
			   0.1690 0.2744 0.1868 0.2239 0.1935 0.3249 0.1979 0.2357 0.2069 0.1835 0.2167 0.3316 
			   0.2207 0.2559 0.2225 0.3199 0.2269 0.2273 0.2300 0.2795 0.2341 0.2290 0.2461 0.2222 
			   0.2479 0.2559 0.2514 0.2138 0.2555 0.2576 0.2595 0.2020 0.2662 0.1667 0.2853 0.2458 
			   0.2951 0.2778 0.3027 0.2256 0.3255 0.2306 0.3290 0.1902 0.3388 0.2811 0.3433 0.3232 
			   0.3455 0.2424 0.3486 0.3754 0.3513 0.2155 0.3522 0.3333 0.3549 0.2879 0.3691 0.3519 
			   0.3749 0.2710 0.3785 0.3384 0.3807 0.2811 0.3848 0.2576 0.3870 0.3064 0.3923 0.2492 
			   0.3941 0.2997 0.3968 0.2744 0.4155 0.1852 0.4222 0.2357 0.4289 0.2845 0.4391 0.2845 
			   0.4463 0.2795 0.4525 0.2340 0.4579 0.2980 0.4610 0.2138 0.4753 0.2980 0.4819 0.3249 
			   0.4864 0.3636 0.4900 0.4040 0.4931 0.3468 0.4962 0.2795 0.4993 0.3535 0.5029 0.2542 
			   0.5051 0.3788 0.5087 0.3114 0.5127 0.2811 0.5145 0.2189 0.5310 0.3519 0.5372 0.2811 
			   0.5408 0.3603 0.5439 0.2862 0.5515 0.2441 0.5582 0.2997 0.5631 0.2239 0.5778 0.3131 
			   0.5809 0.5017 0.5863 0.3266 0.5934 0.3990 0.5961 0.3064 0.6054 0.3603 0.6135 0.2761 
			   0.6291 0.3215 0.6349 0.5051 0.6371 0.2997 0.6433 0.3822 0.6469 0.3232 0.6540 0.3451 
			   0.6603 0.2929 0.6643 0.2458 0.6799 0.3114 0.6830 0.4983 0.6906 0.2424 0.6973 0.3098 
			   0.7044 0.3485 0.7107 0.3165 0.7156 0.2593 0.7321 0.3249 0.7356 0.5286 0.7445 0.2138 
			   0.7477 0.3030 0.7543 0.3064 0.7633 0.2256 0.7793 0.2761 0.7864 0.5034 0.7900 0.2189 
			   0.7954 0.2205 0.7967 0.2778 0.8038 0.3182 0.8123 0.1734 0.8292 0.2290 0.8324 0.4293 
			   0.8377 0.2155 0.8444 0.2121 0.8471 0.2929 0.8533 0.2963 0.8613 0.2037 0.8823 0.2391 
			   0.8850 0.4478 0.8903 0.1886 0.8939 0.2256 0.8979 0.2946 0.9015 0.3131 0.9104 0.2088 
			   0.9309 0.2172 0.9358 0.5556 0.9403 0.2172 0.9456 0.2290 0.9563 0.3215 0.9666 0.2104 
			   0.9924 0.1785 1.0000 0.1801)
			 :duration dur :scaler (hz->radians 10035.0))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (+ (env frqf)
					   (rand-interp rnd))))
	       *output*))))))

;(with-sound (:play #t) (purple-finch 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Northern goshawk

(definstrument (northern-goshawk beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.31)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.124 0.399 0.174 0.520 0.223 0.881 0.355 0.998 0.499 0.644 1.000 0.000)
			 :duration dur :scaler amp))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .1  2 .8  3 .3  4 .05 5 .01 6 .005 7 .003)))
	 (frqf (make-env '(0.000 0.137  0.126 0.138  0.237 0.144  0.314 0.142  0.400 0.140  1.000 0.130)
			 :duration dur :scaler (hz->radians 9040.0))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (northern-goshawk 0 .25))


;;;--------------------------------------------------------------------------------
;;;
;;; Common Gull

(definstrument (common-gull beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.42)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0 0 1 1 1.4 1 1.7 .8  1.8 .2 2 0) :duration dur :scaler amp))
	 (gen1 (make-rcos 1200 0.5))
	 (gen2 (make-rxycos 1800 1200 0.75))
	 (frqf (make-env '(0.000 0.326 0.057 0.599 0.075 0.602 0.102 0.637 0.140 0.618 0.255 0.626 0.378 0.607 
			   0.401 0.589 0.441 0.584 0.491 0.562 0.591 0.544 0.628 0.557 0.675 0.541 0.733 0.538 
			   0.756 0.523 0.809 0.501 0.853 0.469 0.887 0.390 1.000 0.325)
			 :duration dur :offset (hz->radians -1200) :scaler (hz->radians 2000.0)))
	 (intrpf (make-env '(0 1 .2 1 .5 0 1 0) :duration dur :scaler .3 :base 10))
	 (rnd (make-rand-interp 800 .2))
	 (attf (make-env '(0 1 .15 0 1 0) :duration dur :base 10))

	 (frm1 (make-formant .99 2300 5))
	 (frm2 (make-formant .98 6100 3))
	 (frm3 (make-formant .98 3800 5))
	 (frm4 (make-formant .99 1800 7))

	 (rnd2 (make-rand-interp 300 (hz->radians 15))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let* ((frq (+ (env frqf)
			(rand-interp rnd2)
			(* (env attf)
			   (rand-interp rnd))))
		(val (* (env ampf)
		      (+ (rcos gen1 frq)
			 (* (env intrpf)
			    (rxycos gen2 frq))))))
	   (outa i (+ (formant frm1 val)
		      (formant frm2 val)
		      (formant frm3 val)
		      (formant frm4 val))
		 *output*)))))))

;(with-sound (:play #t :scaled-to .5) (common-gull 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Ash-throated flycatcher

(definstrument (ash-throated-flycatcher beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.47)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.018 0.176 0.030 0.756 0.041 1.000 0.052 0.916 0.066 0.198 0.097 0.046 
			   0.128 0.000 0.165 0.000 0.211 0.141 0.224 0.108 0.244 0.149 0.261 0.105 0.267 0.031 
			   0.272 0.218 0.278 0.125 0.302 0.237 0.315 0.510 0.336 0.255 0.346 0.000 0.369 0.000 
			   0.386 0.345 0.403 0.246 0.412 0.000 0.481 0.000 0.490 0.084 0.504 0.330 0.514 0.174 
			   0.527 0.070 0.531 0.000 0.546 0.000 0.550 0.055 0.556 0.000 0.560 0.000 0.565 0.053 
			   0.571 0.000 0.575 0.000 0.580 0.048 0.587 0.000 0.592 0.000 0.597 0.064 0.601 0.000 
			   0.605 0.000 0.609 0.084 0.616 0.000 0.620 0.000 0.623 0.086 0.631 0.000 0.636 0.000 
			   0.638 0.103 0.644 0.000 0.650 0.000 0.653 0.095 0.657 0.000 0.663 0.000 0.669 0.105 
			   0.675 0.000 0.679 0.000 0.683 0.046 0.689 0.000 0.722 0.000 0.727 0.084 0.736 0.312 
			   0.741 0.365 0.747 0.314 0.756 0.000 0.819 0.000 0.830 0.086 0.838 0.369 0.845 0.411 
			   0.854 0.365 0.861 0.000 0.864 0.092 0.869 0.081 0.873 0.200 0.881 0.336 0.889 0.374 
			   0.901 0.033 0.908 0.000 0.913 0.066 0.929 0.310 0.936 0.266 0.940 0.079 0.948 0.000 
			   0.967 0.044 0.973 0.101 1.000 0.000)
			 :duration dur :scaler amp))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .98  2 .02  4 .01)))
	 (frqf (make-env '(0.000 0.317 0.013 0.368 0.029 0.464 0.048 0.485 0.062 0.469 0.100 0.310 0.176 0.320 
			   0.224 0.389 0.242 0.395 0.270 0.370 0.280 0.492 0.284 0.304 0.292 0.363 0.305 0.411 
			   0.322 0.464 0.347 0.306 0.376 0.269 0.386 0.368 0.396 0.398 0.407 0.366 0.415 0.290 
			   0.485 0.288 0.520 0.405 0.536 0.315 0.690 0.313 0.728 0.267 0.733 0.350 0.743 0.375 
			   0.756 0.345 0.759 0.281 0.827 0.285 0.834 0.349 0.850 0.375 0.868 0.312 0.878 0.358 
			   0.886 0.375 0.897 0.352 0.910 0.297 0.923 0.342 0.932 0.361 0.942 0.342 0.960 0.283 
			   0.968 0.327 0.978 0.350 0.989 0.335 1.000 0.290)
			 :duration dur :scaler (hz->radians 6070.0))))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (ash-throated-flycatcher 0 .25))


;;; --------------------------------------------------------------------------------
;;;
;;; White-header woodpecker

(definstrument (white-headed-woodpecker beg amp)
  ;; spectrum travels right off the top -- I wonder how high it actually goes
  (let* ((start (seconds->samples beg))
	 (dur 0.16)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.053 0.952 0.066 0.865 0.079 0.386 0.091 0.937 0.145 0.963 0.182 0.923 
			   0.197 0.384 0.221 0.892 0.256 0.751 0.298 0.000 0.410 0.000 0.430 0.915 0.450 0.836 
			   0.464 0.307 0.479 0.873 0.496 0.952 0.531 0.886 0.554 0.291 0.575 0.892 0.606 0.717 
			   0.654 0.000 0.775 0.000 0.801 0.712 0.814 0.233 0.822 0.712 0.893 0.712 0.923 0.225 
			   0.938 0.758 0.959 0.640 1.000 0.000 )
			 :duration dur :scaler amp))
	 (frqf1 (make-env '(0.000 0.106 0.051 0.119 0.063 0.177 0.087 0.230 0.198 0.228 0.211 0.175 0.236 0.153 
			    0.282 0.122 0.427 0.103 0.465 0.156 0.483 0.214 0.542 0.220 0.563 0.159 0.588 0.146 
			    0.615 0.095 0.767 0.122  0.8 0.2 0.851 0.2 0.871 0.16 0.903 0.148 
			    0.939 0.143  .95 .14)
			 :duration dur :scaler (hz->radians (/ 22050.0 3.0))))
	 (frqf2 (make-env '(0.000 0.230 0.061 0.262 0.088 0.341 0.152 0.323 0.206 0.341 0.219 0.265 0.237 0.235 
			    0.450 0.220 0.459 0.317 0.514 0.302 0.558 0.354 0.605 0.246 0.772 0.246 0.838 0.323 
			    0.857 0.278 0.864 0.325 0.914 0.222 .95 .22)
			 :duration dur :scaler (hz->radians (/ 22050.0 5.0))))

	 (gen1 (make-polyshape 0.0 :partials (list 1 .01 2 .1 3 .6  4 .02  6 .4 7 .05 9 .1 10 .1 12 .01)))
	 (gen2 (make-polyshape 0.0 :partials (list 5 .9   8 .3   11 .1)))

	 (rnd (make-sawtooth-wave 700 .01))
	 (rndf (make-env '(0 0  .01 1 .08 .02 .25 .02 .3 1   .45 1 .47 .03 .64 .01 .65 1  .75 1 .77 .05 .92 .01 1 1)
			 :duration dur)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((noise (* (env rndf) 
			 (sawtooth-wave rnd))))
	 (outa i (* (env ampf)
		    (+ (* .8 (polyshape gen1 1.0 (+ (env frqf1)
						    noise)))
		       (* .2 (polyshape gen2 1.0 (+ (env frqf2)
						    (* 10 noise))))))
	       *output*)))))))

;(with-sound (:play #t) (white-headed-woodpecker 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Phainopepla

(definstrument (phainopepla beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.26)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.063 0.073 0.119 0.181 0.142 0.290 0.178 0.617 0.192 0.525 0.236 0.288 
			   0.255 0.000 0.272 0.557 0.285 0.178 0.296 0.095 0.393 0.088 0.501 0.000 0.522 0.108
			   0.538 0.634 0.647 0.000 0.663 0.000 0.681 0.484 0.704 0.211 0.777 0.643 0.850 0.961 
			   0.880 0.998 0.899 0.974 0.958 0.277 1.000 0.000)
			 :duration dur :scaler amp))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .9 2 .1 3 .01)))
	 (frqf (make-env '(0.000 0.203 0.061 0.309 0.259 0.317 0.276 0.533 0.300 0.720 0.329 0.739 0.373 0.697 
			   0.450 0.792 0.496 0.836 0.516 0.794 0.525 0.689 0.532 0.417 0.550 0.351 0.573 0.314 
			   0.607 0.296 0.624 0.351 0.629 0.435 0.652 0.425 0.660 0.219 0.698 0.398 0.726 0.441 
			   0.839 0.433 1.000 0.427)
			 :duration dur :scaler (hz->radians 8040.0))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (phainopepla 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Golden-crowned sparrow

(definstrument (golden-crowned-sparrow beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 2.13)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.079 0.343 0.188 0.391 0.301 0.504 0.343 0.963 0.379 0.887 0.388 0.000 
			   0.459 0.000 0.474 0.480 0.494 0.549 0.595 0.984 0.637 1.000 0.688 0.720 0.701 0.000 
			   0.770 0.000 0.795 0.311 0.863 0.420 0.916 0.383 0.985 0.272 1.000 0.000)
			 :duration dur :scaler amp))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .9 2 .01 3 .1)))
	 (frqf (make-env '(0.000 0.814 0.028 0.730 0.064 0.690 0.122 0.549 0.156 0.551 0.178 0.573 0.207 0.544 
			   0.272 0.556 0.296 0.580 0.320 0.549 0.343 0.501 0.377 0.468 0.452 0.446 0.467 0.475 
			   0.488 0.470 0.523 0.477 0.529 0.561 0.535 0.480 0.572 0.473 0.589 0.494 0.612 0.477 
			   0.655 0.456 0.672 0.482 0.691 0.463 0.767 0.413 0.819 0.394 0.861 0.396 0.865 0.449 
			   0.873 0.408 0.894 0.401 0.930 0.401 0.975 0.396 1.000 0.329 )
			 :duration dur :scaler (hz->radians 8040.0)))
	 (vib (make-rand-interp 50 (hz->radians 80))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (+ (env frqf)
					   (rand-interp vib))))
	       *output*))))))

;(with-sound (:play #t) (golden-crowned-sparrow 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; House finch

(definstrument (house-finch beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 3.16)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.014 0.128 0.021 0.000 0.052 0.000 0.058 0.146 0.062 0.314 0.068 0.301 
			   0.073 0.000 0.097 0.000 0.101 0.296 0.104 0.151 0.106 0.447 0.114 0.459 0.119 0.143 
			   0.125 0.000 0.151 0.000 0.162 0.415 0.180 0.375 0.188 0.000 0.209 0.000 0.212 0.353 
			   0.214 0.175 0.217 0.442 0.222 0.153 0.225 0.528 0.232 0.002 0.246 0.000 0.247 0.109 
			   0.258 0.030 0.259 0.294 0.263 0.180 0.267 0.333 0.271 0.000 0.290 0.000 0.295 0.128 
			   0.298 0.388 0.303 0.000 0.313 0.857 0.322 0.000 0.346 0.000 0.358 0.990 0.359 0.528 
			   0.361 0.778 0.370 0.000 0.384 0.000 0.390 0.504 0.395 0.227 0.399 0.328 0.409 0.000 
			   0.420 0.993 0.425 0.402 0.433 0.000 0.444 0.000 0.449 0.380 0.455 0.978 0.461 0.000 
			   0.472 0.000 0.480 0.477 0.487 0.000 0.492 0.558 0.500 0.430 0.506 0.000 0.516 0.000 
			   0.525 0.844 0.533 0.104 0.537 0.072 0.541 0.538 0.550 0.067 0.557 0.067 0.559 0.232 
			   0.564 0.528 0.567 0.064 0.573 0.901 0.579 0.084 0.582 0.551 0.587 0.430 0.593 0.000 
			   0.606 0.000 0.611 0.257 0.618 0.000 0.625 0.079 0.629 0.516 0.636 0.642 0.639 0.489 
			   0.643 0.104 0.653 0.000 0.672 0.000 0.680 0.736 0.694 0.696 0.701 0.121 0.707 0.057 
			   0.713 0.558 0.722 0.000 0.752 0.000 0.757 0.723 0.774 0.546 0.782 0.116 0.794 0.207 
			   0.801 0.499 0.818 0.689 0.833 0.388 0.847 0.000 0.859 0.000 0.863 0.123 0.867 0.726 
			   0.872 0.000 0.878 0.116 0.879 0.385 0.885 0.669 0.944 0.835 0.989 0.328 0.995 0.035 
			   1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.0000 0.3997 0.0085 0.4810 0.0103 0.3943 0.0166 0.4228 0.0573 0.4268 0.0605 0.4932 
			   0.0663 0.4160 0.0708 0.4363 0.0985 0.4228 0.1008 0.2846 0.1066 0.4228 0.1146 0.3089 
			   0.1187 0.3238 0.1339 0.3225 0.1487 0.4864 0.1617 0.4607 0.1769 0.4512 0.1818 0.3740 
			   0.1948 0.3794 0.2100 0.4214 0.2114 0.2859 0.2172 0.4350 0.2248 0.3157 0.2293 0.3320 
			   0.2427 0.5054 0.2512 0.4214 0.2557 0.3374 0.2584 0.4648 0.2620 0.5434 0.2665 0.3835 
			   0.2888 0.4119 0.2942 0.3794 0.3014 0.5813 0.3032 0.3130 0.3081 0.4892 0.3144 0.3266 
			   0.3193 0.3388 0.3395 0.4065 0.3462 0.4824 0.3542 0.4458 0.3605 0.3333 0.3645 0.3144 
			   0.3820 0.2927 0.3842 0.2751 0.3887 0.3360 0.3932 0.4173 0.4017 0.5705 0.4035 0.4607 
			   0.4107 0.3862 0.4187 0.3360 0.4263 0.2737 0.4438 0.3062 0.4456 0.3686 0.4478 0.4228 
			   0.4541 0.3266 0.4635 0.3225 0.4720 0.2317 0.4765 0.3225 0.4814 0.3726 0.4828 0.4702 
			   0.4877 0.4363 0.4913 0.3672 0.4922 0.3130 0.4993 0.2846 0.5204 0.3496 0.5280 0.2913 
			   0.5311 0.2195 0.5361 0.4241 0.5428 0.3089 0.5567 0.2913 0.5584 0.4295 0.5625 0.3198 
			   0.5665 0.5000 0.5723 0.4065 0.5808 0.3225 0.5902 0.2060 0.6064 0.2642 0.6117 0.3374 
			   0.6122 0.2195 0.6189 0.1789 0.6296 0.3591 0.6444 0.1599 0.6458 0.4160 0.6771 0.4201 
			   0.6950 0.4214 0.7026 0.2778 0.7112 0.2764 0.7228 0.1897 0.7394 0.4092 0.7546 0.4160 
			   0.7721 0.4187 0.7895 0.3198 0.8343 0.3184 0.8616 0.2344 0.8634 0.3808 0.8697 0.2276 
			   0.8804 0.3279 0.9149 0.3835 0.9472 0.4688 1.0000 0.8306)
			 :duration dur :scaler (hz->radians 9130.0)))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .98 2 .01 3 .01)))
	 (gen2 (make-polyshape 0.0 :partials (list 1 .98 2 .005)))
	 (buzz (make-oscil 0))
	 (buzzsweep (make-sine-summation 0 :n 5 :a .5))
	 (bsweep (hz->radians 500))
	 (buzzf (make-env '(0 0 .14 0 .15 1 .19 1 .20 0  .66 0 .67 1 1 1) :duration dur))
	 (buzzfrqf (make-env '(0 110  .5 110  .6 70 .85 70 .86 130 1 130) :duration dur :scaler (hz->radians 1.0)))
	 (rnd (make-rand-interp 400 (hz->radians 100))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((buzzing (env buzzf))
	       (frq (env frqf))
	       (bfrq (env buzzfrqf)))
	 (outa i (* (env ampf)
		    (+ (* (- 1.0 buzzing)
			  (polyshape gen1 1.0 frq))
		       (* buzzing
			  (+ .1 (* .9 (abs (oscil buzz bfrq))))
			  (polyshape gen2 1.0 (+ frq
						 (* bsweep (sine-summation buzzsweep bfrq))
						 (rand-interp rnd))))))
	       *output*)))))))

;(with-sound (:play #t) (house-finch 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Ruby-crowned kinglet

(definstrument (ruby-crowned-kinglet beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 2.17)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.017 0.053 0.025 0.000 0.034 0.000 0.044 0.088 0.053 0.076 0.057 0.000 
			   0.112 0.000 0.133 0.298 0.151 0.060 0.167 0.158 0.179 0.000 0.214 0.000 0.232 0.496 
			   0.247 0.100 0.265 0.745 0.275 0.000 0.310 0.000 0.318 0.601 0.326 0.714 0.344 0.169 
			   0.361 0.334 0.372 0.000 0.401 0.000 0.417 1.000 0.433 0.115 0.450 0.979 0.459 0.000 
			   0.496 0.000 0.506 0.745 0.516 0.778 0.526 0.322 0.537 0.236 0.546 0.258 0.553 0.212 
			   0.559 0.000 0.599 0.000 0.613 0.967 0.631 0.189 0.647 0.933 0.656 0.000 0.697 0.000 
			   0.706 0.726 0.714 0.589 0.719 0.714 0.729 0.236 0.737 0.160 0.743 0.241 0.754 0.198 
			   0.761 0.000 0.808 0.000 0.826 0.752 0.839 0.098 0.856 0.599 0.862 0.558 0.868 0.000 
			   0.921 0.000 0.932 0.470 0.941 0.511 0.951 0.179 0.976 0.229 0.980 0.057 1.000 0.000)
			 :duration dur :scaler amp))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .98 2 .01 3 .004)))
	 (frqf (make-env '(0.000 0.671 0.017 0.698 0.026 0.786 0.031 0.785 0.032 0.556 0.039 0.556 0.053 0.646 
			   0.059 0.591 0.119 0.476 0.146 0.284 0.193 0.269 0.216 0.549 0.225 0.464 0.238 0.564 
			   0.244 0.477 0.250 0.411 0.262 0.411 0.268 0.442 0.274 0.414 0.316 0.446 0.337 0.289 
			   0.363 0.285 0.368 0.267 0.391 0.287 0.404 0.523 0.410 0.481 0.423 0.553 0.432 0.426 
			   0.441 0.401 0.452 0.441 0.458 0.396 0.502 0.444 0.528 0.285 0.550 0.285 0.557 0.259 
			   0.572 0.260 0.594 0.513 0.607 0.482 0.618 0.568 0.626 0.529 0.629 0.451 0.642 0.429 
			   0.650 0.462 0.653 0.427 0.703 0.442 0.729 0.282 0.750 0.285 0.756 0.259 0.778 0.259 
			   0.793 0.616 0.806 0.588 0.813 0.481 0.825 0.571 0.841 0.444 0.851 0.416 0.860 0.442 
			   0.923 0.444 0.952 0.279 1.000 0.275)
			 :duration dur :scaler (hz->radians 9060.0))))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (ruby-crowned-kinglet 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Green-tailed towhee
;;;
;;; not very elegant, but a real test of the envelope editor

(definstrument (green-tailed-towhee beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 1.86)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.010 0.168 0.013 0.000 0.018 0.000 0.021 0.200 0.027 0.255 0.033 0.119 
			   0.039 0.000 0.141 0.000 0.149 0.104 0.159 0.586 0.163 0.438 0.166 0.586 0.168 0.374 
			   0.171 0.519 0.173 0.429 0.176 0.487 0.179 0.284 0.180 0.554 0.184 0.261 0.186 0.539 
			   0.189 0.270 0.190 0.530 0.194 0.243 0.197 0.449 0.200 0.272 0.202 0.443 0.205 0.243 
			   0.207 0.377 0.218 0.200 0.222 0.000 0.277 0.000 0.286 0.122 0.289 0.417 0.292 0.055 
			   0.296 0.417 0.299 0.490 0.303 0.058 0.307 0.336 0.309 0.000 0.323 0.093 0.327 0.658 
			   0.330 0.087 0.335 0.530 0.341 0.000 0.343 0.438 0.346 0.000 0.356 0.061 0.362 0.232 
			   0.365 0.872 0.368 0.145 0.372 0.655 0.378 0.061 0.381 0.542 0.383 0.000 0.392 0.000 
			   0.397 0.159 0.403 0.739 0.406 0.081 0.411 0.600 0.416 0.000 0.420 0.452 0.422 0.000 
			   0.432 0.078 0.437 0.235 0.441 0.788 0.445 0.078 0.450 0.614 0.454 0.078 0.459 0.357 
			   0.460 0.000 0.470 0.081 0.476 0.281 0.479 0.733 0.485 0.070 0.489 0.588 0.493 0.000 
			   0.498 0.194 0.500 0.000 0.533 0.000 0.535 0.374 0.538 0.090 0.550 1.000 0.557 0.991 
			   0.572 0.078 0.577 0.145 0.583 0.354 0.612 0.000 0.646 0.000 0.663 0.588 0.671 0.583 
			   0.686 0.081 0.701 0.591 0.706 0.577 0.721 0.000 0.759 0.000 0.763 0.420 0.765 0.084 
			   0.768 0.191 0.772 0.136 0.775 0.000 0.778 0.464 0.780 0.119 0.783 0.212 0.788 0.154 
			   0.790 0.003 0.794 0.571 0.796 0.136 0.799 0.241 0.803 0.165 0.807 0.009 0.810 0.710 
			   0.812 0.125 0.814 0.275 0.819 0.174 0.822 0.017 0.824 0.536 0.828 0.136 0.831 0.272 
			   0.835 0.186 0.837 0.006 0.841 0.684 0.844 0.142 0.846 0.345 0.855 0.006 0.856 0.617 
			   0.860 0.130 0.862 0.287 0.870 0.006 0.873 0.606 0.876 0.142 0.879 0.290 0.883 0.154 
			   0.885 0.009 0.889 0.623 0.892 0.139 0.893 0.281 0.903 0.023 0.904 0.600 0.908 0.130 
			   0.911 0.241 0.918 0.006 0.921 0.580 0.924 0.110 0.926 0.209 0.934 0.009 0.938 0.455 
			   0.940 0.090 0.943 0.159 0.951 0.003 0.955 0.391 0.956 0.093 0.959 0.136 0.967 0.012 
			   0.970 0.386 0.974 0.116 0.983 0.012 0.987 0.333 0.990 0.049 1.000 0.000)
			 :duration dur :scaler amp))
	 (gen1 (make-oscil 0.0))
	 (frqf (make-env '(0.0000 0.3575 0.0067 0.4860 0.0110 0.5475 0.0119 0.3203 0.0181 0.3911 0.0219 0.5438 
			   0.0229 0.3389 0.0267 0.4860 0.0300 0.3948 0.0334 0.3259 0.1411 0.3315 0.1478 0.4953 
			   0.1554 0.4413 0.1635 0.3296 0.1640 0.4376 0.1697 0.3315 0.1698 0.4302 0.1755 0.3389 
			   0.1759 0.4153 0.1797 0.3296 0.1821 0.4358 0.1859 0.3240 0.1864 0.4078 0.1892 0.3575 
			   0.1907 0.4358 0.1959 0.3240 0.1964 0.4134 0.2031 0.3128 0.2032 0.4376 0.2078 0.3035 
			   0.2126 0.3166 0.2127 0.4246 0.2183 0.3464 0.2194 0.4320 0.2216 0.3669 0.2698 0.3892 
			   0.2827 0.7318 0.2898 0.5214 0.2941 0.3054 0.2979 0.5978 0.3046 0.3091 0.3207 0.7745 
			   0.3241 0.6685 0.3322 0.2868 0.3346 0.5512 0.3394 0.2626 0.3547 0.8209 0.3608 0.6462 
			   0.3689 0.2756 0.3723 0.5196 0.3785 0.2868 0.3918 0.8425 0.3999 0.6331 0.4066 0.3110 
			   0.4109 0.5568 0.4175 0.3147 0.4280 0.8541 0.4337 0.7263 0.4409 0.5531 0.4447 0.2961 
			   0.4485 0.6555 0.4561 0.2607 0.4682 0.8375 0.4743 0.6853 0.4790 0.5661 0.4833 0.3315 
			   0.4871 0.6089 0.4933 0.3315 0.4976 0.6462 0.5353 0.4860 0.5362 0.6872 0.5377 0.2849 
			   0.5405 0.4488 0.5458 0.4860 0.5567 0.4600 0.5648 0.3464 0.5758 0.2458 0.5877 0.3240 
			   0.6025 0.2495 0.6482 0.3222 0.6568 0.4860 0.6754 0.4618 0.6888 0.4823 0.6892 0.2775 
			   0.7021 0.3315 0.7188 0.2682 0.7541 0.2793 0.7610 0.6542 0.7649 0.2737 0.7650 0.6574 
			   0.7717 0.4097 0.7769 0.6499 0.7798 0.3222 0.7822 0.5791 0.7879 0.3743 0.7927 0.6480 
			   0.7939 0.3003 0.7974 0.6034 0.8022 0.3799 0.8065 0.6518 0.8117 0.2961 0.8136 0.6425 
			   0.8184 0.4041 0.8241 0.6499 0.8255 0.2896 0.8282 0.6167 0.8341 0.4004 0.8379 0.6610 
			   0.8442 0.2857 0.8451 0.6145 0.8506 0.3969 0.8537 0.6491 0.8585 0.3066 0.8600 0.6269 
			   0.8652 0.3816 0.8700 0.6422 0.8755 0.2760 0.8756 0.6238 0.8831 0.3697 0.8870 0.6499 
			   0.8919 0.2794 0.8923 0.6145 0.8985 0.3526 0.9021 0.6405 0.9066 0.3408 0.9075 0.3066 
			   0.9080 0.6071 0.9145 0.3748 0.9175 0.6388 0.9238 0.3135 0.9249 0.6065 0.9306 0.3714 
			   0.9351 0.6303 0.9390 0.2998 0.9404 0.5791 0.9442 0.3855 0.9514 0.6294 0.9542 0.3575 
			   0.9571 0.5791 0.9628 0.3724 0.9670 0.6235 0.9720 0.3152 0.9747 0.5680 0.9795 0.3594 
			   0.9858 0.5963 0.9900 0.3520 1.0000 0.2777)
			 :duration dur :scaler (hz->radians 10030.0))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (oscil gen1 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (green-tailed-towhee 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; White-faced ibis

(definstrument (white-faced-ibis beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.19)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.016 0.156 0.052 0.337 0.527 0.317 0.751 0.834 0.916 0.683 1.000 0.000)
			 :duration dur :scaler (* 0.5 amp)))
	 (frqf (make-env '(0.000 0.452 0.342 0.457 0.431 0.502 1.000 0.49 )
			 :duration dur :offset (hz->radians -250) :scaler (hz->radians (* 1 512.0))))
	 (frm1 (make-formant .995 4300 3))
	 (frm2 (make-formant .99 2200 6))
	 (frm3 (make-formant .9 3000 5))
	 (frm1f (make-env '(0 4400 1 3800) :duration dur))
	 (frm2f (make-env '(0 2100 1 1700) :duration dur))
	 (frm3f (make-env '(0 3000 1 2200) :duration dur))
	 (gen (make-nxycos 1000 250 13))
	 (rnd (make-rand-interp 1000 (hz->radians 20))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((inp (* (env ampf)
		       (nxycos gen (+ (* 1 (env frqf))
				      (rand-interp rnd))))))
	   (set! (mus-frequency frm1) (env frm1f))
	   (set! (mus-frequency frm2) (env frm2f))
	   (set! (mus-frequency frm3) (env frm3f))
	   (outa i (+ (formant frm1 inp)
		      (formant frm2 inp)
		      (formant frm3 inp))
		 *output*)))))))

;(with-sound (:play #t) (white-faced-ibis 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Lucy's warbler

(definstrument (lucys-warbler beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 1.72)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.019 0.093 0.038 0.000 0.057 0.000 0.066 0.170 0.073 0.132 0.081 0.168 
			   0.091 0.000 0.116 0.000 0.125 0.269 0.132 0.211 0.137 0.280 0.149 0.000 0.174 0.000 
			   0.185 0.384 0.191 0.261 0.196 0.461 0.209 0.000 0.233 0.000 0.239 0.416 0.245 0.474 
			   0.248 0.284 0.254 0.543 0.259 0.405 0.267 0.000 0.287 0.000 0.295 0.659 0.301 0.507 
			   0.308 0.655 0.320 0.000 0.343 0.000 0.355 0.881 0.358 0.629 0.365 0.761 0.377 0.000 
			   0.396 0.000 0.408 0.965 0.424 0.573 0.431 0.000 0.450 0.000 0.459 0.983 0.471 0.466 
			   0.477 0.000 0.494 0.000 0.503 0.853 0.516 0.442 0.520 0.000 0.536 0.000 0.549 0.853 
			   0.567 0.000 0.581 0.000 0.596 0.981 0.614 0.000 0.628 0.000 0.634 0.813 0.640 0.425 
			   0.642 0.860 0.653 0.422 0.657 0.000 0.678 0.000 0.682 0.757 0.686 0.364 0.688 0.875 
			   0.704 0.000 0.720 0.000 0.727 0.761 0.731 0.440 0.733 0.875 0.743 0.461 0.750 0.000 
			   0.768 0.000 0.773 0.993 0.781 0.601 0.785 0.918 0.798 0.000 0.816 0.000 0.823 0.877 
			   0.827 0.535 0.830 0.877 0.855 0.000 0.879 0.000 0.885 0.638 0.889 0.741 0.919 0.000 
			   0.945 0.000 0.950 0.556 0.955 0.379 0.959 0.563 0.986 0.037 1.000 0.000)
			 :duration dur :scaler amp))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .98  2 .015  3 .005)))
	 (frqf (make-env '(0.000 0.651 0.018 0.637 0.040 0.401 0.049 0.361 0.055 0.658 0.072 0.644 0.087 0.526 
			   0.098 0.358 0.106 0.340 0.110 0.677 0.130 0.665 0.136 0.595 0.146 0.509 0.155 0.361 
			   0.161 0.342 0.175 0.628 0.191 0.602 0.202 0.536 0.212 0.394 0.219 0.361 0.229 0.648 
			   0.244 0.623 0.254 0.590 0.263 0.500 0.271 0.382 0.278 0.354 0.285 0.627 0.302 0.611 
			   0.315 0.536 0.322 0.391 0.328 0.356 0.333 0.661 0.357 0.644 0.369 0.547 0.379 0.387 
			   0.390 0.363 0.396 0.630 0.415 0.613 0.425 0.543 0.432 0.394 0.442 0.351 0.449 0.724 
			   0.462 0.708 0.474 0.538 0.475 0.418 0.488 0.365 0.491 0.774 0.502 0.743 0.513 0.613 
			   0.519 0.438 0.528 0.389 0.533 0.821 0.544 0.800 0.551 0.688 0.562 0.526 0.565 0.387 
			   0.577 0.354 0.579 0.807 0.588 0.795 0.597 0.701 0.606 0.595 0.612 0.384 0.621 0.339 
			   0.624 0.797 0.634 0.773 0.648 0.627 0.657 0.385 0.663 0.333 0.664 0.809 0.678 0.799 
			   0.686 0.696 0.695 0.575 0.701 0.389 0.709 0.345 0.714 0.825 0.724 0.797 0.736 0.630 
			   0.746 0.432 0.748 0.373 0.759 0.325 0.760 0.813 0.770 0.780 0.785 0.623 0.789 0.507 
			   0.793 0.405 0.802 0.342 0.803 0.837 0.818 0.795 0.829 0.630 0.836 0.535 0.854 0.372 
			   0.864 0.309 0.868 0.826 0.878 0.802 0.884 0.727 0.888 0.618 0.915 0.373 0.930 0.314 
			   0.931 0.819 0.944 0.792 0.951 0.670 0.963 0.563 0.977 0.448 0.981 0.387 1.000 0.318)
			 :duration dur :scaler (hz->radians 8070.0))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (lucys-warbler 0 .25))


;;; --------------------------------------------------------------------------------
;;;
;;; Cassin's vireo

(definstrument (cassins-vireo beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.5)
	 (stop (+ start (seconds->samples dur)))

	 (ampf (make-env '(0.000 0.000 0.028 0.110 0.037 0.062 0.050 0.098 0.060 0.075 0.078 0.179 0.085 0.065 
			   0.093 0.116 0.100 0.379 0.119 0.163 0.136 0.457 0.147 0.473 0.159 0.180 0.173 0.608 
			   0.182 0.655 0.193 0.001 0.200 0.292 0.210 0.180 0.219 0.262 0.224 0.103 0.241 0.204 
			   0.253 0.055 0.268 0.225 0.285 0.061 0.299 0.251 0.312 0.171 0.330 0.298 0.366 0.182 
			   0.375 0.138 0.384 0.159 0.391 0.000 0.512 0.000 0.527 0.085 0.539 0.267 0.553 0.111 
			   0.565 0.200 0.574 0.150 0.583 0.360 0.587 0.117 0.595 0.257 0.602 0.096 0.610 0.297 
			   0.623 0.072 0.635 0.241 0.640 0.201 0.653 0.336 0.669 0.996 0.679 0.730 0.689 0.235 
			   0.702 0.336 0.714 0.688 0.725 0.752 0.733 0.466 0.744 0.680 0.752 0.070 0.763 0.628 
			   0.766 0.567 0.774 0.671 0.786 0.094 0.792 0.514 0.797 0.099 0.800 0.187 0.807 0.224 
			   0.810 0.523 0.820 0.444 0.824 0.155 0.829 0.481 0.832 0.598 0.838 0.521 0.843 0.070 
			   0.847 0.209 0.857 0.476 0.862 0.294 0.873 0.775 0.880 0.175 0.884 0.495 0.888 0.083 
			   0.896 0.644 0.916 0.074 0.919 0.379 0.926 0.072 0.940 0.657 0.944 0.613 0.955 0.070 
			   0.960 0.181 0.966 0.087 0.970 0.111 0.975 0.069 0.981 0.173 0.989 0.021 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.000 0.069 0.028 0.063 0.060 0.075 0.078 0.110 0.100 0.144 0.121 0.159 0.141 0.162 
			   0.160 0.154 0.188 0.130 0.206 0.119 0.216 0.120 0.225 0.100 0.239 0.119 0.254 0.093 
			   0.265 0.117 0.281 0.094 0.298 0.120 0.325 0.129 0.356 0.133 0.382 0.128 0.395 0.100 
			   0.516 0.090 0.568 0.108 0.600 0.075 0.621 0.150 0.639 0.183 0.662 0.178 0.682 0.159 
			   0.703 0.155 0.721 0.139 0.735 0.157 0.753 0.123 0.769 0.148 0.784 0.124 0.795 0.159 
			   0.816 0.139 0.831 0.172 0.845 0.148 0.857 0.180 0.872 0.178 0.881 0.159 0.892 0.198 
			   0.908 0.178 0.922 0.206 0.942 0.194 0.957 0.212 0.982 0.199 1.000 0.179)
			 :duration dur :scaler (hz->radians 22050.0)))
	 (gen1 (make-oscil 0.0))
	 (gen2 (make-oscil 0.0))
	 (gen3 (make-oscil 0.0))
	 (gen4 (make-oscil 0.0))
	 (gen5 (make-oscil 0.0))
	 (f1 (make-env '(0 .01  .06 .01 .1 1  .3 1  .5 .01 .65 .05 .67 1  1 1) :duration dur))
	 (f2 (make-env '(0 .25  .06 .5 .1 .01  .3 .01  .5 .75  .6 .5 .64 .01  1 .01) :duration dur))
	 (f3 (make-env '(0 1  .06 .5 .1 .01  .3 .01  .5 .01 .6 .3 .65 .01 .67 .01  1 .01) :duration dur)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((frq (env frqf)))
	   (outa i (* (env ampf)
		      (+ (* (env f1) (oscil gen1 frq))
			 (* (env f2) (oscil gen2 (* 2 frq)))
			 (* (env f3) (oscil gen3 (* 3 frq)))
			 (* .005 (oscil gen4 (* 4 frq)))
			 (* .005 (oscil gen5 (* 5 frq)))))
		 *output*)))))))

;;; formants sounded bad here, polyshape worse

;(with-sound (:play #t) (cassins-vireo 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Plain chacalaca
;;;
;;;  the recording doesn't do this bird justice -- it actually does say "chacalaca"
;;;    (and the mosquitos darken the skies)

(define (plain-chacalaca beg1 amp)

  (definstrument (plain-chacalaca-1 beg dur amp frmfrq frqlst)
    (let* ((start (seconds->samples beg))
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0.000 0.000 2 1 4 1 6 0) :duration dur :scaler (* 0.5 amp)))
	   (frqf (make-env frqlst :duration dur :scaler (hz->radians 1.0)))
	   (frm1 (make-formant .99 frmfrq 20))
	   (frm2 (make-formant .98 4200 1))
	   (frm3 (make-formant .98 2800 8))
	   (gen (make-nrcos 0.0 15 .75))
	   (rnd (make-rand-interp 5000 .03))
	   (rnd1 (make-rand-interp 1000 .15))
	   (vib (make-blackman 50 4))
	   (vib-index (hz->radians -100)))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (let ((inp (* (env ampf)
			 (+ .85 (abs (rand-interp rnd1)))
			 (nrcos gen (+ (env frqf)
				       (rand-interp rnd)
				       (* vib-index (blackman vib 0.0)))))))
	     (outa i (+ (formant frm1 inp)
			(formant frm2 inp)
			(formant frm3 inp))
		   *output*)))))))

  (plain-chacalaca-1 beg1 0.17    (* .7 amp) 1700 (list 0 450  1 680))
  (plain-chacalaca-1 (+ beg1 0.20) 0.12 amp  1400 (list 0 500  1 680  2 660))
  (plain-chacalaca-1 (+ beg1 0.35) 0.20 amp  1400 (list 0 500  1 680  4 660)))

;(with-sound (:play #t) (plain-chacalaca 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Black-billed cuckoo

(definstrument (black-billed-cuckoo beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.68)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.013 0.388 0.023 0.303 0.026 0.419 0.042 0.530 0.066 0.104 0.128 0.000 
			   0.215 0.000 0.243 0.642 0.257 0.536 0.266 0.725 0.296 0.737 0.311 0.044 0.357 0.070 
			   0.367 0.000 0.421 0.000 0.448 0.856 0.460 0.807 0.468 0.913 0.483 0.960 0.515 0.928 
			   0.535 0.048 0.575 0.085 0.584 0.000 0.638 0.000 0.658 0.903 0.674 0.989 0.708 0.822 
			   0.732 0.079 0.781 0.038 0.798 0.070 0.806 0.028 0.823 0.059 0.845 0.000 0.883 0.000 
			   0.892 0.593 0.913 0.739 0.940 0.031 0.976 0.066 1.000 0.000)
			 :duration dur :scaler amp))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .97  2 .005 3 .008  5 .005)))
	 (rnd (make-rand-interp 400 (hz->radians 25)))
	 (frqf (make-env '(0.000 0.568 0.057 0.568 0.078 0.399 0.203 0.315 0.218 0.632 0.252 0.557 0.314 0.553 
			   0.340 0.350 0.410 0.337 0.426 0.667 0.469 0.557 0.528 0.559 0.548 0.381 0.616 0.352 
			   0.624 0.617 0.659 0.544 0.721 0.544 0.748 0.350 0.840 0.297 0.851 0.615 0.889 0.540 
			   0.926 0.542 0.948 0.416 1.000 0.419)
			 :duration dur :scaler (hz->radians 1190.0))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (+ (env frqf)
					   (rand-interp rnd))))
	       *output*))))))

;(with-sound (:play #t) (black-billed-cuckoo 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Eared grebe

(definstrument (eared-grebe beg amp)
  ;; note #1
  (let* ((start (seconds->samples beg))
	 (dur 0.3)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0 0 9 1 10 0) :duration dur :scaler amp))
	 (frqf (make-env '(0 1050 1 1400) :duration dur :scaler (hz->radians 1.0)))
	 (gen1 (make-oscil 0.0))
	 (gen2 (make-oscil 0.0))
	 (gen3 (make-oscil 0.0))
	 (gen4 (make-oscil 0.0))
	 (gen5 (make-oscil 0.0))
	 (f1 (make-env '(0 .03 9 .144 10 .1) :duration dur))
	 (f2 (make-env '(0 .5 9 .844 10 .2) :duration dur))
	 (f3 (make-env '(0 .01 9 .03 10 .02) :duration dur))
	 (f4 (make-env '(0 0 1 .002 7 .003 10 0) :duration dur)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((frq (env frqf)))
	   (outa i (* (env ampf)
		      (+ (* (env f1) (oscil gen1 frq))
			 (* (env f2) (oscil gen2 (* 2 frq)))
			 (* (env f3) (oscil gen3 (* 3 frq)))
			 (* (env f4) (oscil gen4 (* 4 frq)))
			 (* .005 (oscil gen5 (* 5 frq)))))
		 *output*))))))

  ;; note #2
  (let* ((start (seconds->samples (+ beg 0.29)))
	 (dur 0.085)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0 0  1 1  2 .5  2.9 1  3 0) :duration dur :scaler amp))
	 (frqf (make-env '(0 2280  .25 2320  .6 2440 .65 3240  .8 3470  1 3260) :duration dur :scaler (hz->radians 1.0)))
	 (gen1 (make-oscil 0.0))
	 (gen2 (make-oscil 0.0))
	 (f1 (make-env '(0 .5  .6 1  .62 .05  .65 .5  1 .5) :duration dur)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((frq (env frqf)))
	   (outa i (* (env ampf)
		      (+ (* (env f1) (oscil gen1 frq))
			 (* .01 (oscil gen2 (* 2 frq)))))
		 *output*))))))

  ;; note #3
  (let* ((start (seconds->samples (+ beg 0.446)))
	 (dur 0.02)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0 0  1 1  2.5 0) :duration dur :scaler (* .5 amp)))
	 (frqf1 (make-env '(0 1120  .5 1540  1 1100) :duration dur :scaler (hz->radians 1.0)))
	 (frqf2 (make-env '(0 2400  .5 2520  1 2300) :duration dur :scaler (hz->radians 1.0)))
	 (gen1 (make-oscil 0.0))
	 (gen2 (make-oscil 0.0))
	 (gen3 (make-oscil 0.0))
	 (gen4 (make-oscil 0.0))
	 (f1 (make-env '(0 .9  .2 1   .6 1  .8 0  1 0) :duration dur))
	 (f2 (make-env '(0 .5  .2 1   .6 .01  1 0) :duration dur))
	 (f3 (make-env '(0 .1  .2 0  1 0) :duration dur))
	 (f4 (make-env '(0 0  .2 0   .7 .25  1 .1) :duration dur))
	 (rnd (make-rand-interp 3000 (hz->radians 100))))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((frq1 (+ (env frqf1)
			(rand-interp rnd))))
	   (outa i (* (env ampf)
		      (+ (* (env f1) (oscil gen1 (* 2 frq1)))
			 (* (env f2) (oscil gen2 frq1))
			 (* (env f3) (oscil gen3 (* 3 frq1)))
			 (* (env f4) (oscil gen4 (env frqf2)))))
		 *output*)))))))


;(with-sound (:play #t :statistics #t) (eared-grebe 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Brown jay

(definstrument (brown-jay beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.26)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.012 0.106 0.044 0.151 0.072 0.267 0.129 0.766 0.221 0.889 0.372 1.000 
			   0.455 0.837 0.534 0.553 0.632 0.660 0.762 0.540 0.912 0.105 1.000 0.000)
			 :duration dur :scaler amp))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .1  2 .9  3 .3  4 .07  5 .05  6 .2  7 .01  8 .005  9 .004 10 .003)))
	 (gen2 (make-polyshape 0.0 :partials (list  2 .5  3 .1  4 .05)))
	 (intrpf (make-env '(0 1 .5 1 .8 .6 1 0) :duration dur))
	 (frqf (make-env '(0.000 0.201 0.052 0.201 0.091 0.271 0.143 0.285 0.631 0.288 0.659 0.268 
			   0.858 0.257  .93 .2  1.000 0.22)
			 :duration dur :scaler (hz->radians 4060.0)))
	 (rnd (make-rand-interp 500 (hz->radians 15))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((intrp (env intrpf))
	       (frq (+ (env frqf)
		       (rand-interp rnd))))
	   (outa i (* (env ampf)
		      (+ (* intrp (polyshape gen1 1.0 frq))
			 (* (- 1.0 intrp) (polyshape gen2 1.0 frq))))
	       *output*)))))))

;(with-sound (:play #t) (brown-jay 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Blue grosbeak

(definstrument (blue-grosbeak beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 2.26)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.010 0.029 0.019 0.110 0.032 0.024 0.055 0.260 0.058 0.003 0.062 0.034 
			   0.071 0.282 0.078 0.000 0.119 0.000 0.124 0.057 0.129 0.335 0.134 0.410 0.138 0.035 
			   0.143 0.464 0.146 0.430 0.150 0.034 0.156 0.593 0.162 0.513 0.167 0.355 0.172 0.319 
			   0.175 0.000 0.213 0.000 0.222 0.279 0.223 0.099 0.226 0.224 0.228 0.055 0.232 0.087 
			   0.238 0.503 0.249 0.663 0.253 0.025 0.255 0.060 0.256 0.435 0.261 0.468 0.274 0.000 
			   0.315 0.000 0.326 0.426 0.333 0.046 0.342 0.499 0.349 0.658 0.351 0.090 0.355 0.868 
			   0.360 0.677 0.367 0.000 0.390 0.000 0.399 0.673 0.400 0.270 0.403 0.949 0.406 0.934 
			   0.418 0.002 0.429 0.072 0.442 0.240 0.449 0.528 0.455 0.585 0.458 0.533 0.464 0.000 
			   0.484 0.000 0.487 0.323 0.491 0.187 0.495 0.439 0.505 0.683 0.513 0.000 0.526 0.000 
			   0.534 0.784 0.541 0.648 0.545 0.781 0.548 0.160 0.553 0.734 0.569 0.077 0.572 0.097 
			   0.580 0.532 0.583 0.505 0.589 0.000 0.620 0.000 0.627 0.520 0.631 0.543 0.637 0.071 
			   0.641 0.402 0.653 0.846 0.656 0.075 0.662 1.000 0.674 0.000 0.696 0.000 0.702 0.155 
			   0.703 0.361 0.705 0.046 0.710 0.491 0.719 0.133 0.723 0.091 0.730 0.074 0.735 0.110 
			   0.744 0.406 0.753 0.467 0.758 0.053 0.760 0.007 0.765 0.029 0.771 0.481 0.774 0.539 
			   0.781 0.000 0.820 0.000 0.823 0.168 0.825 0.045 0.827 0.005 0.830 0.050 0.831 0.360 
			   0.834 0.047 0.837 0.017 0.843 0.441 0.844 0.086 0.848 0.036 0.854 0.437 0.857 0.085 
			   0.860 0.231 0.864 0.626 0.869 0.136 0.873 0.324 0.880 0.081 0.883 0.344 0.888 0.076 
			   0.890 0.045 0.893 0.235 0.899 0.000 0.932 0.000 0.942 0.091 0.946 0.007 0.951 0.035 
			   0.956 0.104 0.963 0.319 0.970 0.424 0.973 0.065 0.976 0.011 0.977 0.058 0.980 0.244 
			   0.992 0.059 1.000 0.000)
			 :duration dur :scaler amp))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .97  2 .03 3 .01 4 .005)))
	 (frqf (make-env '(0.0000 0.5530 0.0090 0.5109 0.0278 0.3367 0.0521 0.5167 0.0589 0.7257 0.0718 0.5675 
			   0.0756 0.4949 0.1252 0.4717 0.1282 0.5501 0.1354 0.3120 0.1418 0.4775 0.1495 0.3222 
			   0.1568 0.4761 0.1649 0.3962 0.1717 0.4557 0.1743 0.3919 0.2174 0.3266 0.2213 0.3904 
			   0.2234 0.5559 0.2268 0.3033 0.2358 0.3861 0.2456 0.4833 0.2499 0.5457 0.2525 0.7083 
			   0.2563 0.5225 0.2597 0.4441 0.2704 0.3367 0.2994 0.3367 0.3093 0.5849 0.3165 0.5835 
			   0.3217 0.4833 0.3268 0.4180 0.3319 0.2917 0.3353 0.4935 0.3396 0.4441 0.3456 0.4688 
			   0.3481 0.5109 0.3520 0.7547 0.3537 0.5602 0.3584 0.5094 0.3635 0.4630 0.3904 0.5167 
			   0.3934 0.7010 0.3947 0.5065 0.3964 0.6546 0.3981 0.4659 0.3998 0.7242 0.4020 0.5007 
			   0.4032 0.6168 0.4041 0.5399 0.4075 0.4833 0.4122 0.3904 0.4186 0.2467 0.4421 0.3759 
			   0.4541 0.4296 0.4724 0.4282 0.4776 0.3077 0.484 0.3091  0.488 0.4146  0.4920 0.3120  
			   0.4964 0.4209 0.5015 0.4601 0.5058 0.4238 0.5088 0.3062 0.5203 0.3048 0.5229 0.6067 
			   0.5284 0.6081 0.5344 0.5138 0.5412 0.4630 0.5451 0.5181 0.5481 0.6923 0.5515 0.5733 
			   0.5540 0.4702 0.5592 0.4078 0.5716 0.3106 0.5805 0.4282 0.6010 0.4282 0.6168 0.3672 
			   0.6215 0.3687 0.6232 0.5704 0.6258 0.4398 0.6314 0.4267 0.6369 0.2903 0.6416 0.4369 
			   0.6489 0.4644 0.6536 0.5327 0.6561 0.7983 0.6587 0.6038 0.6617 0.5472 0.6702 0.4514 
			   0.6822 0.4485 0.6890 0.3425 0.6980 0.3440 0.7031 0.3890 0.7053 0.7286 0.7074 0.5152 
			   0.7172 0.3498 0.7258 0.3048 0.7347 0.3396 0.7428 0.4035 0.7531 0.5007 0.7612 0.7257 
			   0.7800 0.5007 0.8232 0.5936 0.8249 0.7837 0.8300 0.6734 0.8334 0.5646 0.8351 0.7576 
			   0.8394 0.6415 0.8428 0.5094 0.8462 0.7460 0.8484 0.6299 0.8526 0.4586 0.8578 0.6386 
			   0.8620 0.4151 0.8672 0.5631 0.8710 0.3701 0.8770 0.4848 0.8804 0.3527 0.8855 0.4761 
			   0.8902 0.3295 0.8958 0.4964 0.8975 0.2874 0.9381 0.2903 0.9436 0.5036 0.9470 0.2642 
			   0.9680 0.4731 0.9718 0.5559 0.9765 0.7881 0.9795 0.5414 0.9889 0.3657 1.0000 0.3033)
			 :base 10 :duration dur :scaler (hz->radians 7190.0))))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (blue-grosbeak 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Acorn woodpecker

(definstrument (acorn-woodpecker beg1 amp1)

  (define (acorn-woodpecker-1 beg dur amp ampf frqf ampf2 ampf4 rndf)
    (let* ((start (seconds->samples beg))
	   (stop (+ start (seconds->samples dur)))
	   (gen1 (make-polyshape 0.0 :partials (list 1 .2  2 1  3 .01  4 .005)))
	   (gen2 (make-polyshape 0.0 :partials (list 3 .5  5 1  6 .05  7 .1)))
	   (gen3 (make-polyshape 0.0 :partials (list   5 .005 6 .01  7 .003  8 .005  9 .002 10 .005 11 .001 13 .003 15 .001)))
	   (gen4 (make-oscil 0.0))
	   (rnd (make-rand-interp 1000 (hz->radians 200))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (let ((frq (+ (env frqf)
			 (* (env rndf) 
			    (rand-interp rnd))))
		 (amp2 (env ampf2)))
	     (outa i (* (env ampf)
			(+ (polyshape gen1 1.0 (* 2 frq))
			   (* amp2 (polyshape gen2 1.0 frq))
			   (* (- 1.0 amp2) 2 (polyshape gen3 1.0 (* 2 frq)))
			   (* (env ampf4) (oscil gen4 (* 6 frq)))))
		   *output*)))))))
  ;; note #1
  (let ((dur1 0.36))
    (acorn-woodpecker-1 beg1 dur1 amp1
			(make-env '(0.000 0.000 0.046 0.257 0.248 0.331 0.371 0.745 0.564 1.000 0.738 0.970 0.909 0.442 1.000 0.000)
				  :duration dur1 :scaler amp1)
			(make-env '(0.000 0.430 0.054 0.526 0.171 0.534 0.244 0.591 0.284 0.603 0.317 0.630 0.378 0.639 0.414 0.658 
				    0.518 0.662 0.581 0.693 0.627 0.697 0.671 0.685 0.726 0.685 0.762 0.710 0.807 0.714 0.829 0.691 
				    0.854 0.710 1.000 0.618)
				  :duration dur1 :scaler (hz->radians (* 0.25 3030)))
			(make-env '(0 .2  .2 .2  .3 0  1 0) :duration dur1)
			(make-env '(0 1  .2 1 .25 0 .4 1  .5 0  .6 1  .7 0  .9 1  1 1) :duration dur1 :scaler .1)
			(make-env '(0 .03  .85 .03  1 1) :duration dur1)))
  ;; note #2
  (let ((dur1 0.35))
    (acorn-woodpecker-1 (+ beg1 0.55) dur1 amp1
			(make-env '(0 0 .2 .1 .9 1 1 0) :duration dur1 :scaler amp1)
			(make-env '(0.000 0.466 0.088 0.497 0.202 0.513 0.319 0.585 0.386 0.596 0.747 0.632 
				    0.835 0.671 0.882 0.661 1.000 0.350)
				  :duration dur1 :scaler (hz->radians (* 0.25 3080)))
			(make-env '(0 0  .2 .1  .3 .1 .4 0  1 0) :duration dur1)
			(make-env '(0 1  .2 1  .25 0  .4 0 .6 1  .7 0  .9 1  1 1) :duration dur1 :scaler .2)
			(make-env '(0 .3 .05 .03 .8 .03  1 .3) :duration dur1 :base 10)))
  ;; note #3
  (let ((dur1 0.34))
    (acorn-woodpecker-1 (+ beg1 1.17) dur1 amp1
			(make-env '(0 0 .2 .1 .8 1 .9 1 1 0) :duration dur1 :scaler amp1)
			(make-env '(0.000 0.310 0.076 0.331 0.118 0.388 0.184 0.422 0.239 0.484 0.336 0.544 
				    0.720 0.549 0.831 0.581 0.896 0.570 0.920 0.630 0.938 0.604 1.000 0.448)
				  :duration dur1 :scaler (hz->radians (* 0.25 3310)))
			(make-env '(0 0  .2 .1  .3 .1 .4 0  .5 0 .6 .1 .7 .1  .8 0  1 0) :duration dur1)
			(make-env '(0 1  .2 1  .25 0  .4 0 .6 1  .7 0 .8 1  .9 1  1 1) :duration dur1 :scaler .3)
			(make-env '(0 .1 .05 .03 .8 .03  1 .5) :duration dur1 :base 10))))

;(with-sound (:play #t) (acorn-woodpecker 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Lesser nighhawk

(definstrument (lesser-nighthawk beg dur amp)
  (let* ((start (seconds->samples beg))
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0 0 2 1 10 1 11 0) :duration dur :scaler amp :base 10))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .94  3 .04  4 .01)))
	 (pulse-dur .021)
	 (pulsef (make-env '(0.000 0.000 .6 1 1 0) :base .1  :duration pulse-dur))
	 (pulse-sep .047)
	 (pulse-samps (seconds->samples pulse-sep))
	 (next-pulse (+ start pulse-samps))
	 (frqf (make-env (list 0 600  .5 620 (max .55 (- dur .5)) 620  (max .6 dur) 570) :duration dur :scaler (hz->radians 1.0))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (if (>= i next-pulse)
	     (begin
	       (set! next-pulse (+ i pulse-samps))
	       (mus-reset pulsef)))
	 (outa i (* (env ampf)
		    (env pulsef)
		    (polyshape gen1 1.0 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (lesser-nighthawk 0 1 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Olive-sided flycatcher

(definstrument (olive-sided-flycatcher beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 1.08)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.021 0.129 0.035 0.369 0.053 0.250 0.064 0.000 0.352 0.000 0.381 0.674 
			   0.394 0.483 0.407 0.834 0.418 0.852 0.425 0.795 0.440 0.898 0.457 0.931 0.489 0.922 
			   0.506 0.878 0.534 0.991 0.548 0.988 0.577 0.717 0.593 0.834 0.615 0.000 0.690 0.000 
			   0.704 0.698 0.710 0.436 0.712 0.610 0.726 0.395 0.736 0.483 0.756 0.545 0.773 0.795 
			   0.786 0.583 0.808 0.919 0.816 0.843 0.826 0.898 0.837 0.797 0.844 0.659 0.860 0.640 
			   0.879 0.334 0.975 0.176 0.989 0.034 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.000 0.175 0.020 0.235 0.035 0.269 0.041 0.311 0.070 0.224 0.356 0.209 0.377 0.309 
			   0.402 0.352 0.449 0.360 0.549 0.348 0.574 0.339 0.588 0.354 0.596 0.330 0.603 0.354 
			   0.612 0.235 0.691 0.213 0.702 0.313 0.725 0.380 0.743 0.397 0.765 0.354 0.794 0.328 
			   0.844 0.299 0.876 0.277 0.975 0.254 1.000 0.198)
			 :duration dur :scaler (hz->radians 10100.0)))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .95  2 .03  3 .01  4 .005))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (olive-sided-flycatcher 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Red-shouldered hawk
;;;
;;;   ending is not right

(definstrument (red-shouldered-hawk beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.475)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.071 0.704 0.128 0.964 0.168 0.593 0.383 0.356 0.399 0.798 0.446 0.901 
			   0.492 0.628 0.595 0.771 0.677 0.700 0.693 0.439 0.715 0.593 0.750 0.715 0.872 0.648 
			   0.894 0.360 0.938 0.360 0.984 0.213 1.000 0.000)
			 :duration dur :scaler (* 0.2 amp)))
	 (frqf (make-env '(0.000 0.083 0.027 0.094 0.035 0.129 0.103 0.143 0.116 0.180  0.370 0.167  0.381 0.13 
                           0.66 .114 .72 .116 .8 .112
			   0.871 0.105 0.888 0.080 0.976 0.078 1.000 0.08)
			 :base .1 :duration dur :scaler (hz->radians (* 0.5 9130.0))))
	 (histuff (make-polyshape 0.0 :partials (list 1 .1  2 .75   3 .1  4 .1  5 .01  6 .01  7 .01  8 .02 9 .01 11 .005 )))
	 (lostuff (make-polyshape 0.0 :partials (list 2 .1 3 .3  5 .03  7 .1   9 .01   13 .1  15 .1  17 .05  19 .03)))
	 (midstuff (make-polyshape 0.0 :partials (list 1 .3 3 .7)))
	 (midf (make-env '(0 1 .3 1 .4 .1 1 0) :duration dur :scaler 1.0))
	 (oddf (make-env '(0 1  .1 1  .12 .01 .45 .01 .55 .75 1 0) :duration dur :scaler 0.7 :base 10))
	 (frm1 (make-formant .98 2300 10))
	 (frm2 (make-formant .99 3200 3))
	 (frm3 (make-formant .97 5300 5))
	 (frm4 (make-formant .99 1600 5))
	 (rnd (make-rand-interp 400 (hz->radians 10))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let* ((frq (+ (env frqf)
			(rand-interp rnd)))
		(val (* (env ampf)
			(+ (* (polyshape histuff 1.0 (* 2 frq)))
			   (* (env oddf) 
			      (polyshape lostuff 1.0 frq))
			   (* (env midf)
			      (polyshape midstuff 1.0 (* 2 frq)))))))
	   (outa i (+ val
		      (formant frm1 val) 
		      (formant frm2 val) 
		      (formant frm3 val)
		      (formant frm4 val))
	       *output*)))))))

;(with-sound (:play #t) (red-shouldered-hawk 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Common yellowthroat

(define (common-yellowthroat beg1 amp1)

  (definstrument (common-yellowthroat-1 beg amp) 
    (let* ((start (seconds->samples beg))
	   (dur .42)
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0.000 0.000 0.067 0.312 0.105 0.773 0.132 0.685 0.160 0.168 0.178 0.075 0.192 0.178 
			     0.211 0.636 0.227 0.782 0.236 0.623 0.258 0.807 0.283 0.639 0.299 0.000 0.434 0.000 
			     0.482 0.751 0.503 0.804 0.518 0.651 0.540 0.000 0.638 -0.000 0.661 0.576 0.715 0.664
			     0.736 0.984 0.763 0.685 0.784 0.620 0.817 0.894 0.830 0.745 0.912 0.134 1.000 0.000)
			   :duration dur :scaler amp))
	   (frqf (make-env '(0.000 0.296 0.029 0.290 0.052 0.305 0.072 0.334 0.088 0.367 0.108 0.381 0.132 0.348 
			     0.159 0.305 0.183 0.352 0.210 0.410 0.241 0.436 0.267 0.441 0.292 0.434 0.398 0.417 
			     0.410 0.682 0.428 0.686 0.457 0.581 0.475 0.534 0.491 0.503 0.521 0.485 0.531 0.468 
			     0.645 0.488 0.672 0.506 0.690 0.530 0.704 0.543 0.718 0.521 0.733 0.486 0.763 0.457 
			     0.791 0.423 0.838 0.356 0.918 0.261 0.951 0.252 1.000 0.194)
			   :duration dur :scaler (hz->radians 10150.0)))
	   (gen1 (make-polyshape 0.0 :partials (list 1 .99  2 .01))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf)
		      (polyshape gen1 1.0 (env frqf)))
		 *output*))))))

  (common-yellowthroat-1 beg1 (* 0.4 amp1))
  (common-yellowthroat-1 (+ beg1 0.44) amp1)
  (common-yellowthroat-1 (+ beg1 0.90) amp1)
  (common-yellowthroat-1 (+ beg1 1.36) (* 0.8 amp1)))
  

;(with-sound (:play #t) (common-yellowthroat 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Cassin's sparrow

(define (cassins-sparrow beg amp)

  ;; buzz1
  (let* ((start (seconds->samples beg))
	 (dur 0.2)
	 (stop (+ start (seconds->samples dur)))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .95 2 .05)))
	 (ampf (make-env '(0.000 0.000 0.139 0.174 0.302 0.577 0.492 0.601 0.720 0.415 0.853 0.628 0.962 0.945 1.000 0.000) 
			 :duration dur :scaler (* .3 amp)))
	 (pulse-dur .0064)
	 (pulse-samps (seconds->samples pulse-dur))
	 (next-pulse (+ start pulse-samps))
	 (pulse-frqf (make-env '(0 0 1 200 3 0) :duration pulse-dur :scaler (hz->radians 1.0)))
	 (frqf (make-env '(0 5850  .2 6200 .3 6000 1 6100) :duration dur :scaler (hz->radians 1.0)))
	 (pulse-ampf (make-env '(0.000 0.2 0.435 0.356 0.701 0.925 0.785 0.984 0.880 0.779 0.973 0.395 1.000 0.2) :duration pulse-dur)))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (if (>= i next-pulse)
	       (begin
		 (set! next-pulse (+ next-pulse pulse-samps))
		 (mus-reset pulse-ampf)
		 (mus-reset pulse-frqf)))
	   (outa i (* (env ampf)
		      (env pulse-ampf)
		      (polyshape gen1 1.0 (+ (env pulse-frqf)
					     (env frqf))))
		 *output*)))))

  ;; buzz2
  (let* ((start (seconds->samples (+ beg .2)))
	 (dur 0.22)
	 (stop (+ start (seconds->samples dur)))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .95 2 .05)))
	 (pulse-dur .022)
	 (pulse-samps (seconds->samples pulse-dur))
	 (next-pulse (+ start pulse-samps))
	 (pulse-frqf (make-env '(0 5400 1 6700) :duration pulse-dur :scaler (hz->radians 1.0)))
	 (pulse-ampf (make-env '(0 0 .1 0 .7 1 1 0) :duration pulse-dur :base .1 :scaler (* .6 amp)))
	 (rnd (make-rand-interp 600 (hz->radians 100))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (if (>= i next-pulse)
	       (begin
		 (set! next-pulse (+ next-pulse pulse-samps))
		 (mus-reset pulse-ampf)
		 (mus-reset pulse-frqf)))
	   (outa i (* (env pulse-ampf)
		      (polyshape gen1 1.0 (+ (env pulse-frqf)
					     (rand-interp rnd))))
		 *output*)))))

  ;; buzz3
  (let* ((start (seconds->samples (+ beg .425)))
	 (dur 0.51)
	 (stop (+ start (seconds->samples dur)))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .98 2 .01  3 .005)))
	 (pulse-dur .051)
	 (pulse-samps (seconds->samples 0.064))
	 (next-pulse (+ start pulse-samps))
	 (pulse-frqf (make-env '(0 5300 .1 5200 .2 5300 .3 6000 1 6000) :duration pulse-dur :scaler (hz->radians 1.0)))
	 (pulse-ampf (make-env '(0 0 .1 .5 .2 0 .3 1 .4 .9 .8 1 1 0) :duration pulse-dur :scaler amp))
	 (frqf (make-env '(0 100 .6 0 1 50) :duration dur :scaler (hz->radians 1.0)))
	 (ampf (make-env '(0 .6 1 1) :duration dur))
	 (rnd (make-rand-interp 600 (hz->radians 10))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (if (>= i next-pulse)
	       (begin
		 (set! next-pulse (+ next-pulse pulse-samps))
		 (mus-reset pulse-ampf)
		 (mus-reset pulse-frqf)))
	   (outa i (* (env ampf)
		      (env pulse-ampf)
		      (polyshape gen1 1.0 (+ (env pulse-frqf)
					     (env frqf)
					     (rand-interp rnd))))
		 *output*)))))

  ;; 4 long pitches
  (let* ((start (seconds->samples (+ beg .95)))
	 (dur 0.74)
	 (stop (+ start (seconds->samples dur)))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .99 2 .01)))
	 (frqf (make-env '(0.000 0.446 0.014 0.403 0.054 0.385 0.121 0.376 0.248 0.374 0.274 0.367 0.290 0.198 
			   0.308 0.166 0.339 0.159 0.418 0.162 0.545 0.162 0.674 0.162 0.714 0.164 0.718 0.458 
			   0.735 0.451 0.743 0.415 0.761 0.403 0.847 0.387 1.000 0.387)
		:duration dur :scaler (hz->radians 22050.0)))
	 (ampf (make-env '(0.000 0.000 0.025 0.833 0.062 0.951 0.087 0.882 0.120 1.000 0.172 0.961 0.226 0.849 
			   0.238 0.666 0.253 0.000 0.299 0.000 0.319 0.689 0.329 0.679 0.346 0.000 0.409 0.000 
			   0.450 0.593 0.478 0.689 0.537 0.767 0.649 0.626 0.666 0.469 0.679 0.000 0.737 0.000 
			   0.771 0.816 0.784 0.698 0.795 0.911 0.828 0.895 0.853 0.774 0.882 0.734 0.942 0.603 
			   0.979 0.475 0.992 0.325 1.000 0.000)
		:duration dur :scaler (* .9 amp))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf)
		      (polyshape gen1 1.0 (env frqf)))
		 *output*)))))

  ;; last buzz
  (let* ((start (seconds->samples (+ beg 1.73)))
	 (dur 0.32)
	 (stop (+ start (seconds->samples dur)))
	 (gen1 (make-oscil 3100.0))
	 (ampf (make-env '(0 0 1 1 2 1 3 0) :base .3 :duration dur :scaler (* .4 amp)))
	 (buzz (make-oscil 120))
	 (rnd (make-rand-interp 400 (hz->radians 100))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf)
		      (+ .25 (* .75 (abs (oscil buzz))))
		      (oscil gen1 (rand-interp rnd)))
		 *output*))))))

;(with-sound (:play #t) (cassins-sparrow 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Steller's jay

(define (stellers-jay beg1 amp1)

  (definstrument (stellers-jay-1 beg amp)
    (let* ((start (seconds->samples beg))
	   (dur .09)
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0.000 0.000 0.171 0.175 0.268 0.475 0.314 0.294 0.433 1.000 0.502 0.306 0.671 0.586 
			     0.794 0.236 0.847 0.615 1.000 0.000)
			   :duration dur :scaler amp))
	   (frqf1 (make-env '(0.000 0.393 0.343 0.411 0.423 0.366 0.478 0.405 0.542 0.508 0.582 0.526 0.683 0.414 
			      0.849 0.417 0.957 0.218 1.000 0.196)
			 :duration dur :scaler (hz->radians 6050.0)))
	   (frqf2 (make-env '(0.000 0.405 0.213 0.405 0.277 0.311 0.391 0.290 0.497 0.275 0.558 0.242 0.635 0.242 
			      0.729 0.190 0.868 0.205 1.000 0.160)
			    :duration dur :scaler (hz->radians 6050.0)))
	   (gen1 (make-polyshape 0.0 :partials (list 1 .95  2 .1  3 .1)))
	   (gen2 (make-polyshape 0.0 :partials (list 1 .95  2 .03  3 .01)))
	   (ampf2 (make-env '(0 0 1 1 2 0) :duration dur :scaler .3))
	   (rnd (make-rand-interp 8000 ))

	   (frm1 (make-formant .99 2460 5))
	   (frm2 (make-formant .98 5200 5))
	   (frm3 (make-formant .97 8200 2))
	   (frmf (make-env '(0 5200 .7 4900 .9 2200 1 2000) :duration dur))
	   (frmf3 (make-env '(0 8200 .7 8400 .9 4000 1 4000) :duration dur))
	   (frmaf (make-env '(0 0 .6 .3 .9 .8  1 .5) :duration dur))
	   (frmf1 (make-env '(0 2460 .7 2400 .9 1400 1 1500) :duration dur)))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (set! (mus-frequency frm2) (env frmf))
	   (set! (mus-frequency frm3) (env frmf3))
	   (set! (mus-frequency frm1) (env frmf1))
	 
	   (let* ((fintrp (env frmaf))
		  (val1 (* (env ampf)
			   (+ (polyshape gen1 1.0 (env frqf1))
			      (* (env ampf2)
				 (polyshape gen2 1.0 (env frqf2))))))
		  (val (* val1
			  (rand-interp rnd))))
	     (outa i (+ (* .75 val1)
			(* (- 1.0 fintrp)
			   (formant frm1 val))
			(* fintrp (formant frm2 val))
			(formant frm3 val))
		   *output*)))))))

  (do ((beg beg1 (+ beg .15))
       (i 0 (1+ i)))
      ((= i 6))
    (stellers-jay-1 beg amp1)))

;(with-sound (:play #t) (stellers-jay 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Black rail

(definstrument (black-rail beg amp)
  
  ;; notes 1 and 2
  (let* ((start (seconds->samples beg))
	 (dur 0.2)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.015 0.069 0.029 0.738 0.083 0.868 0.133 0.868 0.230 0.725 0.267 0.501 
			   0.324 0.000 0.694 0.000 0.716 1.000 0.778 0.975 0.850 0.906 0.898 0.814 0.939 0.585 
			   0.981 0.081 1.000 0.000)
			 :duration dur :scaler (* 0.9 amp)))
	 (frqf (make-env '(0.000 0.484 0.028 0.458 0.210 0.456 0.296 0.444 0.324 0.404 0.700 0.461 0.728 0.453 
			   0.916 0.453 1.000 0.390)
			 :duration dur :scaler (hz->radians (* 0.5 8150.0))))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .09  2 1  3 .07  4 .003 5 .01  6 .03  7 .005  9 .003)))
	 (buzz (make-oscil 240))
	 (buzz1 (make-oscil 240)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (+ .8 (* .2 (oscil buzz)))
		    (polyshape gen1 1.0 (+ (env frqf)
					   (* (hz->radians 20)
					      (oscil buzz1)))))
	       *output*)))))

  ;; note 3
  (let* ((start (seconds->samples (+ beg .254)))
	 (dur 0.21)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.031 0.980 0.106 0.725 0.141 0.811 0.182 0.567 0.225 0.779 0.272 0.473 
			   0.318 0.648 0.375 0.292 0.410 0.585 0.467 0.212 0.507 0.335 0.550 0.192 0.585 0.301 
			   0.643 0.155 0.678 0.261 0.721 0.152 0.756 0.201 0.811 0.172 0.845 0.052 0.879 0.140 
			   0.924 0.066 1.000 0.000)
			 :duration dur :scaler (* 0.4 amp)))
	 (frqf (make-env '(0.000 0.000 0.003 0.366 0.027 0.469 0.047 0.516 0.086 0.478 0.107 0.516 0.130 0.537 
			   0.155 0.507 0.178 0.472 0.201 0.510 0.227 0.528 0.249 0.501 0.273 0.463 0.291 0.496 
			   0.317 0.516 0.342 0.484 0.364 0.437 0.386 0.484 0.413 0.504 0.439 0.460 0.461 0.422 
			   0.482 0.466 0.507 0.493 0.528 0.451 0.547 0.398 0.569 0.434 0.599 0.469 0.619 0.431 
			   0.642 0.383 0.661 0.425 0.678 0.454 0.707 0.410 0.731 0.372 0.774 0.422 0.815 0.336 
			   0.867 0.372 0.905 0.319 1.000 0.304)
			 :duration dur :scaler (hz->radians (* 0.25 8040.0))))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .1  2 1  3 .4  4 .1   5 .03  6 .005 7 .005  10 .005))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (black-rail 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Pinyon jay

(definstrument (pinyon-jay beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.3)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.008 0.035 0.013 0.118 0.024 0.032 0.087 0.596 0.111 0.617 0.159 0.460 
			   0.188 0.546 0.240 0.876 0.293 1.000 0.345 0.991 0.377 0.938 0.436 0.959 0.501 0.764 
			   0.537 0.534 0.628 0.442 0.718 0.466 0.812 0.386 0.858 0.316 0.903 0.342 0.949 0.094 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.000 0.100 0.075 0.163 0.130 0.185 0.192 0.207 0.327 0.245 0.385 0.256 0.558 0.200 
			   0.724 0.207 0.806 0.209 0.875 0.180 0.944 0.138 1.000 0.134)
			 :base .1 :duration dur :scaler (hz->radians (* 0.5 8160))))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .03  2 .8  3 .05  4 .01  5 .03  6 .075  7 .03  8 .008  9 .005  10 .005  11 .003)))
	 (gen2 (make-oscil 0.0 (* 0.5 pi)))
	 (ampf2 (make-env '(0 .2  .1 .2 .2 0 .4 .05 .8 .02 .9 .2 1 0) :duration dur :scaler 3))
	 (rnd (make-rand-interp 1000 (hz->radians 10))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((frq (+ (env frqf)
		       (rand-interp rnd))))
	   (outa i (* (env ampf)
		      (+ (polyshape gen1 1.0 frq)
			 (* (env ampf2)
			    (oscil gen2 (* 3 frq)))))
		 *output*)))))))

;(with-sound (:play #t) (pinyon-jay 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Sora

(definstrument (sora beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.41)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.011 0.109 0.066 0.180 0.360 0.192 0.442 0.229 0.473 0.203 0.575 0.276 
			   0.621 0.345 0.702 0.361 0.751 0.661 0.792 0.713 0.839 0.606 0.885 0.873 0.904 0.788 
			   0.937 0.980 0.953 0.998 0.962 0.911 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.000 0.358 0.103 0.332 0.209 0.327 0.328 0.341 0.572 0.363 0.771 0.429 0.893 0.458 
			   0.965 0.462 1.000 0.448)
			 :duration dur :scaler (hz->radians (* 0.5 6020))))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .05  2 .9  3 .05  4 .005))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (sora 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Killdeer

(definstrument (killdeer beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.88)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.015 0.477 0.028 0.499 0.054 0.172 0.078 0.107 0.184 0.199 0.212 0.177 
			   0.245 0.249 0.275 0.291 0.364 0.688 0.388 0.714 0.416 0.944 0.436 0.906 0.451 0.998 
			   0.478 0.000 0.535 0.000 0.551 0.976 0.564 1.000 0.582 0.981 0.610 0.838 0.634 0.908 
			   0.661 0.782 0.682 0.000 0.732 0.000 0.735 0.562 0.746 0.709 0.798 0.554 0.814 0.579 
			   0.825 0.472 0.837 0.000 0.910 0.000 0.915 0.392 0.920 0.554 0.947 0.458 0.962 0.341 
			   0.978 0.370 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.000 0.373 0.013 0.471 0.029 0.487 0.052 0.449 0.074 0.422 0.244 0.442 0.319 0.453 
			   0.353 0.456 0.412 0.458 0.454 0.464 0.471 0.442 0.477 0.389 0.537 0.378 0.539 0.436 
			   0.551 0.464 0.586 0.460 0.649 0.462 0.673 0.444 0.684 0.404 0.736 0.373 0.742 0.422 
			   0.758 0.440 0.820 0.440 0.835 0.416 0.839 0.382 0.911 0.353 0.912 0.393 0.924 0.420 
			   0.941 0.409 0.959 0.404 0.982 0.413 1.000 0.396)
			 :duration dur :scaler (hz->radians (* 0.25 10170.0))))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .04  2 1  3 .07  4 .04  5 .005)))
	 (gen2 (make-polyshape 0.0 :partials (list 1 .01  2 .05  3 .03  4 1  5 .12  6 .1  7 .01  8 .05  9 .005)))
	 (ampf2 (make-env '(0 0 .53 0 .64 .7  .67 0  .73 .1 .9 0  .91 .07 .94 0 1 0) :duration dur)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((frq (env frqf)))
	   (outa i (* (env ampf)
		      (+ (polyshape gen1 1.0 (* 2 frq))
			 (* (env ampf2)
			    (polyshape gen2 1.0 frq))))
		 *output*)))))))

;(with-sound (:play #t) (killdeer 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Oak titmouse

(define (oak-titmouse beg1 amp1)

  (definstrument (oak-titmouse-1 beg amp)
    (let* ((start (seconds->samples beg))
	   (dur 0.117)
	   (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.019 0.149 0.034 0.099 0.058 0.121 0.067 0.041 0.099 0.032 0.117 0.061 
			   0.147 0.510 0.170 0.537 0.182 0.138 0.195 0.066 0.228 0.058 0.247 0.086 0.258 0.151 
			   0.269 0.701 0.290 0.382 0.305 0.537 0.319 0.128 0.341 0.041 0.363 0.077 0.383 0.173 
			   0.400 0.989 0.412 0.613 0.433 0.545 0.443 0.475 0.452 0.631 0.461 0.139 0.477 0.061 
			   0.501 0.086 0.516 0.149 0.530 0.979 0.548 0.601 0.558 0.504 0.576 0.524 0.589 0.183 
			   0.606 0.080 0.633 0.094 0.648 0.180 0.671 0.979 0.686 0.549 0.705 0.815 0.718 0.214 
			   0.731 0.131 0.757 0.100 0.773 0.146 0.788 0.315 0.805 0.876 0.822 0.606 0.834 0.503 
			   0.849 0.544 0.865 0.120 0.888 0.079 0.910 0.121 0.924 0.182 0.940 0.542 0.959 0.430 
			   0.972 0.183 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0 7200 1 5200) :duration dur :scaler (hz->radians 1.0)))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .97  2 .03)))
	 (gen2 (make-oscil 63.5 (+ pi 1)))
	 (vib-amp (hz->radians 1200)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (+ (env frqf)
					   (* vib-amp (oscil gen2)))))
	       *output*))))))

  (definstrument (oak-titmouse-2 beg amp)
    (let* ((start (seconds->samples beg))
	   (dur 0.14)
	   (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.027 0.067 0.088 0.113 0.129 0.322 0.175 0.718 0.244 0.987 0.283 0.941 
			   0.315 0.710 0.344 0.322 0.374 0.450 0.388 0.351 0.450 0.220 0.565 0.118 0.673 0.062 
			   0.725 0.070 0.789 0.126 0.836 0.209 0.870 0.177 0.892 0.233 0.908 0.475 .92 .1 1 0)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.000 0.125  .04 .125  0.070 0.214  0.1 0.296 0.146 0.333 0.2 0.329 0.263 0.300 0.322 0.264 
			   0.407 0.218 0.514 0.196 0.660 0.183 0.746 0.190 0.822 0.212 0.873 0.252 0.902 0.254 0.926 0.212 1.000 0.207)
			 :duration dur :scaler (hz->radians 10230.0)))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .9  2 .05  3 .01  4 .01  5 .005)))
	 (gen2 (make-polyshape 0.0 :partials (list 2 1  3 .5  4 .1)))
	 (ampf2 (make-env '(0 1 .1 0 .8 0 1 1) :duration dur :scaler .5)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((frq (env frqf)))
	 (outa i (* (env ampf)
		    (+ (polyshape gen1 1.0 frq)
		       (* (env ampf2)
			  (polyshape gen2 1.0 frq))))
	       *output*)))))))

  (let ((amps (vct .5 1.0 1.0 .9)))
    (do ((i 0 (1+ i))
	 (bg beg1 (+ bg .35)))
	((= i 4))
      (oak-titmouse-1 bg (* amp1 (vct-ref amps i)))
      (oak-titmouse-2 (+ bg .156) (* amp1 (vct-ref amps i))))))

;(with-sound (:play #t) (oak-titmouse 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Macgillivray's warbler

(define (macgillivrays-warbler beg1 amp1)
  ;; much more complex than it sounds -- like the Hermit Thrush, this has 2 independent sources

  (definstrument (macgillivrays-warbler-1 beg amp)
    ;; 5 of these to start
    (let* ((start (seconds->samples beg))
	   (dur 0.137)
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0.000 0.000 0.066 0.200 0.208 0.310 0.228 0.400 0.259 0.403 0.331 0.175 0.370 0.377 
			     0.414 0.166 0.462 0.639 0.475 0.149 0.495 0.648 0.612 0.992 0.647 0.924 0.700 0.473 
			     0.710 0.138 0.720 0.654 0.770 0.352 0.815 0.848 0.829 0.724 0.840 0.837 0.864 0.625 
			     0.891 0.715 0.937 0.687 0.989 0.456 1.000 0.000)
			   :duration dur :scaler (* .75 amp)))

	   (frqf1 (make-env '(0.000 0.515 0.096 0.506 0.193 0.475 0.287 0.421 0.358 0.362 0.470 0.346 0.495 0.588 
			      0.532 0.593 0.567 0.607 0.625 0.635 0.680 0.711 0.711 0.739 0.735 0.800 0.762 0.800 
			      0.785 0.645 0.808 0.565 0.851 0.551 1.000 0.544)
			    :duration dur :scaler (hz->radians 8830.0)))
	   (ampf1 (make-env '(0 1 .35 1 .356 0 .49 0 .494 1 .73 1 .735 .25 .8 .25 .81 1 1 1) :duration dur))
	   (gen1 (make-polyshape 0.0 :partials (list 1 .97  2 .02  3 .005)))

	   (frqf2 (make-env '(0.000 0.515 0.326 0.522 0.370 0.454 0.413 0.416 0.419 0.492 0.427 0.586 0.451 0.534 
			      0.472 0.461 0.493 0.593 0.540 0.598 0.587 0.614 0.647 0.656 0.698 0.725 0.728 0.562 
			      0.758 0.492 0.788 0.468 0.803 0.501 0.814 0.555 1.000 0.544)
			    :duration dur :scaler (hz->radians 8830.0)))
	   (ampf2 (make-env '(0 0 .324 0 .327 .5  .73 .5 .735 1 .8 1 .81 0 1 0) :duration dur))
	   (gen2 (make-polyshape 0.0 :partials (list 1 .97  2 .02  3 .005)))
	   
	   (rnd (make-rand-interp 6000 .1)))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf)
		      (+ .9 (rand-interp rnd))
		      (+ (* (env ampf1)
			    (polyshape gen1 1.0 (env frqf1)))
			 (* (env ampf2)
			    (polyshape gen2 1.0 (env frqf2)))))
		 *output*))))))

  (definstrument (macgillivrays-warbler-2 beg amp)
    ;; 3 of these to end
    (let* ((start (seconds->samples beg))
	   (dur 0.135)
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0.000 0.000 0.031 0.052 0.184 0.078 0.275 0.254 0.311 0.285 0.327 0.461 0.377 0.635 
			     0.455 0.216 0.521 0.426 0.533 0.605 0.601 0.991 0.619 0.951 0.628 0.584 0.674 0.318 
			     0.711 0.440 0.732 0.680 0.771 0.581 0.806 0.315 0.857 0.256 0.892 0.132 0.949 0.108 1.000 0.000)
			   :duration dur :scaler amp))

	   (frqf1 (make-env '(0.000 0.202 0.048 0.277 0.077 0.289 0.106 0.268 0.130 0.286 0.152 0.275 0.169 0.232 
			      0.202 0.216 0.229 0.232 0.264 0.277 0.292 0.291 0.324 0.310 0.351 0.340 0.382 0.303 
			      0.430 0.329 0.470 0.366 0.533 0.408 0.612 0.479 0.638 0.575 0.712 0.343 0.730 0.286 
			      0.750 0.258 0.778 0.272 0.804 0.312 0.826 0.345 0.852 0.352 0.891 0.317 0.934 0.286 1.000 0.2)
			    :duration dur :scaler (hz->radians 9140.0)))
	   (ampf1 (make-env '(0 1  .1 .4  .3 .1  .5 .2  .63 .2 .64 0 .71 0 .72 .1 .8 1 1 1) :duration dur))
	   (gen1 (make-polyshape 0.0 :partials (list 1 .6  2 .4 3 .005)))

	   (frqf2 (make-env '(0.000 0.357 0.196 0.357 0.275 0.373 0.310 0.413 0.350 0.446 0.385 0.484 0.425 0.538 
			      0.462 0.540 0.470 0.373 0.535 0.418 0.591 0.462 0.621 0.500 0.637 0.573 0.667 0.392 
			      0.712 0.406 0.744 0.455 0.778 0.462 0.833 0.556 1.000 0.32)
			    :duration dur :scaler (hz->radians 9140.0)))
	   (ampf2 (make-env '(0 0 .19 0 .195 1  .45 .1 .46 0 .47 1  .63 1 .64 0 .67 0 .68 1 .82 1 .83 0 1 0) :duration dur))
	   (gen2 (make-polyshape 0.0 :partials (list 1 .99  2 .01)))
	   
	   (rnd1 (make-rand-interp 600 (hz->radians 40)))
	   (rnd (make-rand-interp 6000 .1)))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf)
		      (+ .9 (rand-interp rnd))
		      (+ (* (env ampf1)
			    (polyshape gen1 1.0 (env frqf1)))
			 (* (env ampf2)
			    (polyshape gen2 1.0 (+ (env frqf2)
						   (rand-interp rnd1))))))
		 *output*))))))

  (let ((amps (vct .4 .6 .8 .9 1.0)))
    (do ((note 0 (1+ note))
	 (bg beg1 (+ bg 0.18)))
	((= note 5))
      (macgillivrays-warbler-1 bg (* amp1 (vct-ref amps note)))))

  (let ((amps (vct 1.0 .9 .7)))
    (do ((note 0 (1+ note))
	 (bg (+ beg1 0.93) (+ bg 0.17)))
	((= note 3))
      (macgillivrays-warbler-2 bg (* amp1 (vct-ref amps note))))))

;(with-sound (:play #t) (macgillivrays-warbler 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Hutton's vireo

(definstrument (huttons-vireo beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.4)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.054 0.080 0.063 0.040 0.078 0.086 0.113 0.107 0.136 0.061 0.148 0.086 
			   0.155 0.066 0.161 0.123 0.176 0.087 0.184 0.112 0.204 0.067 0.214 0.070 0.220 0.055 
			   0.226 0.077 0.233 0.034 0.239 0.083 0.246 0.032 0.254 0.081 0.260 0.038 0.265 0.067 
			   0.274 0.040 0.281 0.074 0.287 0.043 0.293 0.060 0.300 0.014 0.307 0.066 0.315 0.018 
			   0.320 0.074 0.330 0.021 0.335 0.070 0.342 0.025 0.351 0.069 0.355 0.017 0.360 0.051 
			   0.375 0.040 0.380 0.063 0.388 0.023 0.396 0.077 0.404 0.025 0.410 0.081 0.417 0.032 
			   0.425 0.089 0.428 0.058 0.435 0.047 0.441 0.093 0.447 0.041 0.456 0.116 0.463 0.044 
			   0.471 0.138 0.479 0.061 0.489 0.150 0.497 0.070 0.506 0.191 0.510 0.124 0.514 0.089 
			   0.521 0.211 0.526 0.138 0.530 0.121 0.537 0.234 0.547 0.103 0.556 0.263 0.561 0.204 
			   0.567 0.213 0.571 0.319 0.578 0.286 0.580 0.237 0.586 0.292 0.591 0.234 0.594 0.288 
			   0.600 0.132 0.602 0.217 0.606 0.262 0.611 0.420 0.614 0.357 0.617 0.162 0.630 0.472 
			   0.636 0.132 0.649 0.587 0.657 0.133 0.666 0.587 0.671 0.380 0.677 0.271 0.683 0.640 
			   0.692 0.343 0.696 0.248 0.704 0.680 0.711 0.358 0.715 0.288 0.721 0.779 0.731 0.204 
			   0.736 0.357 0.739 0.776 0.745 0.882 0.753 0.320 0.760 0.695 0.769 0.994 0.775 0.322 
			   0.781 0.662 0.789 0.856 0.794 0.582 0.797 0.240 0.801 0.608 0.809 0.825 0.816 0.877 
			   0.823 0.311 0.830 0.900 0.834 0.877 0.838 0.758 0.844 0.809 0.858 0.605 0.867 0.879 
			   0.879 0.819 0.883 0.968 0.890 0.969 0.898 0.885 0.912 0.862 0.919 0.776 0.934 0.769 
			   0.940 0.740 0.952 0.565 0.969 0.485 0.974 0.403 0.982 0.389 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.0000 0.4927 0.0133 0.5498 0.0177 0.6092 0.0275 0.8519 0.0276 0.5073 0.0413 0.8422 
			   0.0428 0.4454 0.0624 0.5510 0.0954 0.5534 0.1264 0.5570 0.1391 0.5194 0.1632 0.5522 
			   0.1750 0.4976 0.1912 0.5437 0.1996 0.4672 0.2055 0.5413 0.2134 0.4660 0.2207 0.5413 
			   0.2266 0.4454 0.2335 0.5388 0.2380 0.4357 0.2458 0.5352 0.2522 0.4357 0.2596 0.5243 
			   0.2635 0.4320 0.2699 0.5182 0.2788 0.4296 0.2842 0.5061 0.2935 0.4248 0.2999 0.5061 
			   0.3068 0.4260 0.3147 0.5000 0.3210 0.4248 0.3289 0.4939 0.3358 0.4187 0.3422 0.4782 
			   0.3500 0.4223 0.3569 0.4721 0.3658 0.4223 0.3736 0.4794 0.3800 0.4211 0.3884 0.4745 
			   0.3948 0.4199 0.4036 0.4745 0.4105 0.4126 0.4174 0.4721 0.4258 0.4090 0.4341 0.4733 
			   0.4410 0.4066 0.4489 0.4648 0.4577 0.4053 0.4656 0.4648 0.4730 0.3993 0.4823 0.4697 
			   0.4887 0.3993 0.4990 0.4709 0.5059 0.4005 0.5143 0.4769 0.5236 0.3968 0.5339 0.4879 
			   0.5393 0.3993 0.5521 0.4988 0.5590 0.4029 0.5674 0.5024 0.5777 0.4053 0.5870 0.5097 
			   0.5954 0.4090 0.6057 0.5146 0.6121 0.4150 0.6229 0.5303 0.6332 0.4187 0.6416 0.5437 
			   0.6529 0.4284 0.6618 0.5437 0.6711 0.4308 0.6814 0.5413 0.6868 0.4345 0.6986 0.5473 
			   0.7065 0.4393 0.7183 0.5510 0.7281 0.4430 0.7404 0.5558 0.7557 0.4490 0.7630 0.5595 
			   0.7763 0.4563 0.7861 0.5558 0.7984 0.4612 0.8117 0.5680 0.8265 0.5231 0.9641 0.5095 
			   0.9754 0.3944 0.9823 0.4); 1 .4)
			 :duration dur :scaler (hz->radians 7500.0)))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .95  2 .03  3 .005))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (huttons-vireo 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Western meadowlark

(define (western-meadowlark beg1 amp1)

  ;; 1st sequence of notes
  (definstrument (western-meadowlark-1 beg amp)
    (let* ((start (seconds->samples beg))
	   (dur 1.075)
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0.000 0.000 0.007 0.072 0.016 0.253 0.028 0.300 0.048 0.000 0.115 0.000 0.122 0.062 
			     0.132 0.437 0.148 0.462 0.162 0.444 0.188 0.000 0.365 0.000 0.392 0.050 0.397 0.017 
			     0.406 0.052 0.412 0.000 0.432 0.000 0.438 0.082 0.455 0.541 0.486 0.722 0.503 0.759 
			     0.518 0.744 0.529 0.774 0.547 0.700 0.570 0.000 0.635 0.000 0.656 0.715 0.680 0.963 
			     0.701 1.000 0.730 0.938 0.742 0.883 0.754 0.715 0.770 0.000 0.850 0.000 0.855 0.211 
			     0.857 0.072 0.858 0.360 0.861 0.112 0.862 0.568 0.864 0.181 0.866 0.742 0.867 0.290 
			     0.872 0.933 0.874 0.625 0.883 0.963 0.899 0.940 0.906 0.844 0.920 0.856 0.922 0.821 
			     0.929 0.891 0.935 0.844 0.949 0.911 0.970 0.861 0.981 0.667 0.993 0.104 1.000 0.000)
			 :duration dur :scaler amp))
	   (frqf (make-env '(0.000 0.280 0.069 0.288 0.125 0.280 0.185 0.275 0.258 0.280 0.366 0.184 0.394 0.179 
			     0.405 0.218 0.409 0.134 0.426 0.134 0.428 0.352 0.568 0.345 0.618 0.350 0.630 0.258 
			     0.765 0.261 0.828 0.258 0.848 0.454 1.000 0.452)
			 :duration dur :scaler (hz->radians 7470.0)))
	   (gen1 (make-oscil 0.0)))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf)
		      (oscil gen1 (env frqf)))
		 *output*))))))

  ;; 2nd sequence of notes
  (definstrument (western-meadowlark-2 beg amp)
    (let* ((start (seconds->samples beg))
	   (dur 0.45)
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0.000 0.000 0.014 0.080 0.030 0.297 0.036 0.100 0.043 0.109 0.050 0.436 0.055 0.191 
			     0.057 0.435 0.064 0.338 0.067 0.000 0.070 0.830 0.079 0.255 0.084 0.883 0.090 0.982 
			     0.099 0.000 0.100 0.553 0.102 0.626 0.113 0.035 0.118 0.261 0.129 0.047 0.151 0.000 
			     0.218 0.000 0.224 0.041 0.227 0.015 0.233 0.083 0.248 0.000 0.263 0.141 0.282 0.209 
			     0.310 0.389 0.331 0.377 0.354 0.148 0.383 0.829 0.393 0.733 0.406 0.294 0.412 0.559 
			     0.436 0.512 0.440 0.250 0.443 0.465 0.446 0.117 0.453 0.324 0.460 0.094 0.464 0.070 
			     0.470 0.192 0.477 0.058 0.483 0.023 0.490 0.686 0.501 0.788 0.509 0.756 0.532 0.129 
			     0.550 0.000 0.613 0.000 0.619 0.062 0.625 0.015 0.631 0.042 0.634 0.117 0.641 0.070 
			     0.659 0.102 0.675 0.067 0.687 0.000 0.730 0.000 0.734 0.118 0.745 0.139 0.746 0.252 
			     0.762 0.329 0.770 0.898 0.772 0.223 0.777 0.856 0.785 0.900 0.792 0.126 0.797 0.071 
			     0.803 0.742 0.809 0.688 0.824 0.179 0.832 0.197 0.836 0.294 0.880 0.017 0.895 0.041 
			     0.915 0.029 0.924 0.008 0.935 0.029 0.956 0.041 0.978 0.033 1.000 0.000)
			 :duration dur :scaler amp))
	   (frqf (make-env '(0.000 0.506 0.042 0.501 0.068 0.481 0.125 0.300 0.153 0.293 0.204 0.963 0.217 0.918 
			     0.223 0.720 0.235 0.588 0.353 0.553 0.358 0.328 0.400 0.323 0.412 0.293 0.418 0.318 
			     0.421 0.273 0.429 0.315 0.435 0.263 0.443 0.263 0.449 0.221 0.472 0.218 0.483 0.223 
			     0.487 0.320 0.499 0.266 0.530 0.266 0.545 0.253 0.550 0.201 0.622 0.211 0.635 0.241 
			     0.640 0.149 0.684 0.161 0.725 0.169 0.727 0.305 0.736 0.268 0.742 0.238 0.752 0.236 
			     0.757 0.310 0.759 0.166 0.773 0.400 0.777 0.350 0.791 0.347 0.803 0.303 0.829 0.303 
			     0.840 0.206 0.872 0.208 0.894 0.146 0.918 0.161 0.930 0.102 0.964 0.134 1.000 0.146)
			 :duration dur :scaler (hz->radians 7470.0)))
	   (gen1 (make-polyshape 0.0 :partials (list 1 .99  2 .01))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf)
		      (polyshape gen1 1.0 (env frqf)))
		 *output*))))))


  (western-meadowlark-1 beg1 (* 0.6 amp1))
  (western-meadowlark-2 (+ beg1 1.125) amp1))

;(with-sound (:play #t) (western-meadowlark 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Northern beardless tyrannulet
;;;
;;; this should have used 4 calls on one note

(definstrument (northern-beardless-tyrannulet beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 1.91)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.013 0.124 0.018 0.713 0.038 0.303 0.052 0.469 0.065 0.236 0.079 0.685 
			   0.084 0.483 0.089 0.666 0.094 0.545 0.101 0.638 0.107 0.542 0.113 0.671 0.123 0.531 
			   0.132 0.590 0.157 0.879 0.167 0.784 0.173 0.410 0.177 0.646 0.182 0.228 0.192 0.587 
			   0.203 0.000 0.238 0.000 0.240 0.010 0.248 0.843 0.255 0.404 0.266 0.952 0.281 1.000 
			   0.331 0.963 0.350 0.702 0.358 0.784 0.375 0.534 0.391 0.778 0.406 0.000 0.444 0.000
			   0.446 0.010 0.457 0.899 0.464 0.334 0.473 1.000 0.492 0.854 0.499 0.610 0.506 0.730 
			   0.525 0.652 0.552 0.801 0.560 0.553 0.570 0.865 0.582 0.534 0.583 0.626 0.590 0.483 
			   0.595 0.570 0.611 0.000 0.649 0.000 0.651 0.010 0.662 0.899 0.671 0.654 0.679 0.969 
			   0.698 0.787 0.704 0.843 0.711 0.772 0.726 0.910 0.768 0.643 0.780 0.680 0.803 0.000 
			   0.845 0.000 0.847 0.010 0.856 0.770 0.863 0.492 0.866 0.860 0.873 0.890 0.883 0.680 
			   0.889 0.817 0.901 0.722 0.915 0.823 0.934 0.534 0.953 0.576 0.972 0.435 0.977 0.562 
			   0.990 0.101 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.000 0.396 0.021 0.439 0.188 0.431 0.197 0.340 0.247 0.375 
			   0.256 0.447 0.268 0.437 0.396 0.426 0.400 0.394 0.402 0.358 0.457 0.369 
			   0.461 0.426 0.469 0.447 0.484 0.437 0.599 0.418 0.607 0.394 0.611 0.345 0.658 0.361 
			   0.663 0.426 0.669 0.456 0.682 0.429 0.786 0.415 0.795 0.407 0.801 0.345 0.856 0.372 
			   0.861 0.420 0.865 0.450 0.875 0.431 0.982 0.407 0.990 0.361 1.000 0.278)
			 :duration dur :scaler (hz->radians 9230.0)))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .95  2 .005 3 .02  4 .005  5 .003)))
	 (noise (make-rand-interp 8000))
	 (noisef (make-env '(0 0 .011 1 .015 0 
			     .236 0 .24 1 .245 0
			     .44 0 .445 1 .452 0
			     .647 0 .65 1 .657 0
			     .84 0 .845 1 .853 0
			     1 0)
			   :duration dur :scaler .25)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((rnd (env noisef)))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (+ (env frqf)
					   (* rnd (rand-interp noise)))))
	       *output*)))))))

;(with-sound (:play #t) (northern-beardless-tyrannulet 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Scott's oriole

(definstrument (scotts-oriole beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 1.83)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.020 0.103 0.030 0.113 0.038 0.099 0.062 0.000 0.142 0.000 0.150 0.050 
			   0.160 0.326 0.170 0.333 0.178 0.309 0.200 0.050 0.207 0.076 0.218 0.282 0.238 0.018 
			   0.255 0.353 0.265 0.415 0.276 0.392 0.292 0.000 0.318 0.000 0.328 0.442 0.331 0.090 
			   0.335 0.408 0.339 0.279 0.342 0.545 0.357 0.000 0.405 0.000 0.413 0.250 0.419 0.281 
			   0.424 0.250 0.436 0.058 0.442 0.072 0.462 0.511 0.471 0.500 0.477 0.000 0.484 0.000 
			   0.487 0.980 0.505 0.514 0.521 0.304 0.530 0.000 0.601 0.000 0.616 0.559 0.622 0.599 
			   0.637 0.581 0.646 0.507 0.661 0.000 0.676 0.000 0.678 0.239 0.680 0.094 0.682 0.424 
			   0.687 0.096 0.689 0.374 0.693 0.000 0.702 0.000 0.716 0.698 0.725 0.763 0.735 0.000 
			   0.746 0.000 0.753 0.396 0.758 0.423 0.760 0.399 0.771 0.054 0.777 0.095 0.783 0.207 
			   0.786 0.227 0.800 0.142 0.823 0.072 0.828 0.000 0.851 0.000 0.853 0.243 0.856 0.087 
			   0.857 0.351 0.862 0.147 0.863 0.300 0.870 0.000 0.877 0.000 0.892 0.888 0.911 0.000 
			   0.920 0.000 0.926 0.353 0.933 0.406 0.943 0.059 0.948 0.025 0.962 0.167 0.970 0.174 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.000 0.560 0.023 0.489 0.052 0.487 0.079 0.487 0.138 0.556 0.150 0.556 0.162 0.487 
			   0.181 0.473 0.202 0.473 0.212 0.415 0.234 0.415 0.241 0.491 0.252 0.442 0.267 0.431 
			   0.285 0.438 0.319 0.455 0.322 0.533 0.328 0.634 0.332 0.467 0.338 0.775 0.348 0.839 
			   0.354 0.810 0.366 0.737 0.392 0.415 0.409 0.411 0.421 0.386 0.440 0.397 0.472 0.438 
			   0.477 0.531 0.485 0.607 0.495 0.507 0.508 0.408 0.527 0.406 0.587 0.406 0.607 0.491 
			   0.624 0.462 0.637 0.458 0.654 0.467 0.658 0.422 0.679 0.393 0.682 0.585 0.684 0.482 
			   0.686 0.643 0.690 0.420 0.700 0.576 0.706 0.621 0.710 0.583 0.730 0.612 0.751 0.413 
			   0.766 0.406 0.789 0.362 0.819 0.359 0.852 0.388 0.856 0.645 0.858 0.491 0.861 0.621 
			   0.864 0.435 0.867 0.576 0.873 0.565 0.879 0.600 0.887 0.576 0.896 0.596 0.903 0.654 
			   0.924 0.420 0.940 0.395 0.973 0.359 1.000 0.353)
			 :duration dur :scaler (hz->radians 5080.0)))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .98  2 .01  3 .003  4 .007))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (scotts-oriole 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Wilson's warbler

(define (wilsons-warbler beg1 amp1)
  ;; 3 different calls, 2(?) with 2 different tones

  (definstrument (wilsons-warbler-1 beg dur amp)
    (let* ((start (seconds->samples beg))
	   (stop (+ start (seconds->samples dur)))

	   (ampf (make-env '(0.000 0.000 0.101 0.169 0.138 0.169 0.179 0.297 0.226 0.377 0.269 0.092 0.318 0.513 
			     0.340 0.651 0.359 0.156 0.372 0.654 0.387 0.156 0.428 0.700 0.491 0.808 0.607 0.721 
			     0.659 0.923 0.747 0.995 0.909 0.813 0.965 0.528 1.000 0.000)
			   :duration dur :scaler amp))
	   (frqf1 (make-env '(0.000 0.692 0.038 0.698 0.077 0.647 0.136 0.614 0.258 0.606 0.275 0.575 0.314 0.571 
			      0.365 0.550 0.381 0.500 0.681 0.502 0.844 0.496 0.873 0.401 1.000 0.390)
			    :duration dur :scaler (hz->radians 10210.0)))
	   (gen1 (make-oscil 0.0))
	   (frqf2 (make-env '(0.000 0.692 0.038 0.698 0.077 0.647 0.136 0.614 0.258 0.606 0.275 0.575 0.314 0.571 
			      0.365 0.550 0.376 0.394 0.490 0.353 0.594 0.347 0.681 0.392 0.740 0.418 0.818 0.416 
			      0.886 0.399 1.000 0.390)
			    :duration dur :scaler (hz->radians 10210.0)))
	   (ampf1 (make-env '(0 1 .6 1 .8 0 1 0) :duration dur))
	   (gen2 (make-oscil 0.0))
	   (ampf2 (make-env '(0 .25 .35 .25 .36 0 .6 0 .7 1 1 1) :duration dur))

	   (rnd (make-rand-interp 4000 (hz->radians 300))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (let ((noise (rand-interp rnd)))
	     (outa i (* (env ampf)
			(+ (* (env ampf1)
			      (oscil gen1 (+ (env frqf1)
					     noise)))
			   (* (env ampf2)
			      (oscil gen2 (env frqf2)))))
		   *output*)))))))

  (definstrument (wilsons-warbler-2 beg dur frq amp)
    (let* ((start (seconds->samples beg))
	   (stop (+ start (seconds->samples dur)))

	   (ampf (make-env '(0.000 0.000 0.049 0.091 0.109 0.276 0.129 0.280 0.149 0.164 0.157 0.388 0.179 0.677 
			     0.221 0.642 0.241 0.448 0.319 1.000 0.360 0.953 0.489 0.765 0.513 0.539 0.591 0.282 
			     0.619 0.345 0.727 0.244 0.762 0.446 0.828 0.442 0.925 0.313 0.982 0.136 1.000 0.000)
			   :duration dur :scaler amp))
	   (frqf (make-env '(0.000 0.909 0.050 0.838 0.080 0.799 0.119 0.786 0.162 0.752 0.225 0.692 0.262 0.634 
			     0.317 0.538 0.366 0.478 0.482 0.376 0.554 0.342 0.628 0.366 0.701 0.392 0.762 0.394 
			     0.832 0.373 1.000 0.3)
			   :duration dur :scaler (hz->radians (* frq 9780.0))))
	   (gen1 (make-oscil 0.0))

	   (rnd (make-rand-interp 4000 (hz->radians 200))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf)
		      (oscil gen1 (+ (env frqf)
				     (rand-interp rnd))))
		 *output*))))))


  (definstrument (wilsons-warbler-3 beg amp)
    (let* ((start (seconds->samples beg))
	   (dur 0.07)
	   (stop (+ start (seconds->samples dur)))

	   (ampf (make-env '(0.000 0.000 0.087 0.099 0.126 0.201 0.148 0.086 0.201 0.483 0.223 0.337 0.243 0.561 
			     0.257 0.384 0.269 0.658 0.282 0.332 0.292 0.428 0.311 0.211 0.357 0.290 0.390 0.601 
			     0.407 0.674 0.433 0.658 0.437 0.222 0.456 0.572 0.485 0.629 0.498 0.885 0.529 0.627 
			     0.552 0.807 0.642 0.689 0.658 0.836 0.683 0.877 0.704 0.543 0.733 0.634 0.837 0.428 
			     0.873 0.634 0.924 0.185 0.974 0.238 1.000 0.000)
			   :duration dur :scaler amp))
	   (frqf (make-env '(0.000 0.770 0.120 0.768 0.190 0.752 0.227 0.715 0.261 0.692 0.292 0.695 0.310 0.648 
			     0.334 0.629 0.423 0.616 0.477 0.561 0.510 0.554 0.534 0.501 0.645 0.413 0.690 0.402 
			     0.737 0.371 0.893 0.347 1.000 0.295)
			   :duration dur :scaler (hz->radians 9780.0)))
	   (gen1 (make-oscil 0.0))

	   (rnd (make-rand-interp 4000 (hz->radians 200))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf)
		      (oscil gen1 (+ (env frqf)
				     (rand-interp rnd))))
		 *output*))))))

  (wilsons-warbler-1 beg1 .046 (* .05 amp1))
  (wilsons-warbler-1 (+ beg1 .147) .05 (* .1 amp1))

  (let ((durs2 (vct 0.067 0.07 0.075 0.08 0.086 0.084 0.08 0.08 0.078))
	(frqs2 (vct 1.0   1.0  0.95  0.93 1.0   1.0   1.0  1.0  0.95))
	(amps2 (vct .2    .4    .7   1    1      .8   1    1    1)))
    (do ((i 0 (1+ i)))
	((= i 9))
      (wilsons-warbler-2 (+ beg1 0.285 (* i .13)) (vct-ref durs2 i) (vct-ref frqs2 i) (* amp1 (vct-ref amps2 i)))))

  (do ((i 0 (1+ i)))
      ((= i 3))
    (wilsons-warbler-3 (+ beg1 1.45 (* i 0.12)) (* amp1 .9))))

;(with-sound (:play #t) (wilsons-warbler 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Willow flycatcher

(definstrument (willow-flycatcher beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.65)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.016 0.054 0.026 0.142 0.033 0.292 0.043 0.228 0.056 0.240 0.088 0.162 
			   0.107 0.218 0.125 0.105 0.130 0.019 0.196 0.012 0.208 0.053 0.215 0.031 0.221 0.086 
			   0.223 0.711 0.228 0.828 0.231 0.760 0.237 0.978 0.242 0.679 0.245 0.966 0.250 0.272 
			   0.253 0.177 0.262 0.131 0.264 0.000 0.405 0.000 0.417 0.069 0.426 0.174 0.431 0.402 
			   0.435 0.301 0.445 0.338 0.452 0.240 0.460 -0.000 0.501 0.000 0.504 0.159 0.509 0.069
			   0.519 0.096 0.521 0.228 0.524 0.000 0.536 0.000 0.539 0.336 0.541 0.103 0.547 0.449 
			   0.550 0.157 0.554 0.000 0.557 0.086 0.560 0.419 0.562 0.169 0.565 0.000 0.567 0.066 
			   0.568 0.174 0.569 0.419 0.572 0.123 0.574 0.027 0.576 0.167 0.577 0.537 0.582 0.056 
			   0.583 0.237 0.584 0.564 0.588 0.211 0.590 0.051 0.592 0.232 0.594 0.515 0.595 0.179 
			   0.597 0.041 0.599 0.177 0.601 0.426 0.603 0.153 0.606 0.029 0.609 0.172 0.612 0.395 
			   0.614 0.029 0.617 0.169 0.620 0.412 0.623 0.024 0.625 0.148 0.628 0.375 0.631 0.022 
			   0.634 0.148 0.636 0.336 0.640 0.031 0.644 0.150 0.646 0.348 0.650 0.041 0.654 0.165 
			   0.655 0.338 0.659 0.039 0.664 0.165 0.665 0.336 0.668 0.058 0.671 0.260 0.674 0.162 
			   0.677 0.321 0.680 0.218 0.682 0.343 0.684 0.167 0.696 0.265 0.702 0.348 0.708 0.326 
			   0.716 0.385 0.720 0.520 0.727 0.451 0.731 0.561 0.738 0.267 0.745 0.321 0.750 0.108 
			   0.772 0.118 0.782 0.167 0.792 0.282 0.802 0.409 0.811 0.257 0.816 0.083 0.833 0.069 
			   0.852 0.103 0.863 0.152 0.867 0.250 0.877 0.047 0.929 0.091 0.938 0.054 0.953 0.027 0.986 0.039 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.000 0.174 0.042 0.324 0.074 0.339 0.111 0.390 0.130 0.453 0.190 0.831 0.210 0.780 
			   0.225 0.605 0.227 0.504 0.240 0.489 0.248 0.453 0.251 0.346 0.262 0.165 0.412 0.155 
			   0.422 0.230 0.430 0.288 0.435 0.334 0.448 0.337 0.456 0.395 0.458 0.467 0.503 0.402 
			   0.506 0.235 0.513 0.213 0.520 0.240 0.524 0.443 0.537 0.441 0.540 0.298 0.544 0.262 
			   0.547 0.298 0.553 0.443 0.558 0.453 0.561 0.324 0.563 0.508 0.570 0.324 0.573 0.511 
			   0.577 0.329 0.581 0.482 0.585 0.332 0.590 0.479 0.594 0.332 0.598 0.467 0.602 0.320 
			   0.606 0.453 0.611 0.300 0.615 0.443 0.620 0.291 0.623 0.450 0.628 0.262 0.631 0.438 
			   0.637 0.259 0.640 0.436 0.646 0.264 0.649 0.436 0.655 0.257 0.659 0.438 0.664 0.247 
			   0.668 0.407 0.673 0.264 0.679 0.400 0.683 0.291 0.702 0.298 0.716 0.334 0.721 0.397 
			   0.728 0.370 0.733 0.402 0.738 0.438 0.742 0.375 0.748 0.249 0.755 0.232 0.766 0.264 
			   0.785 0.295 0.797 0.310 0.801 0.354 0.803 0.426 0.808 0.361 0.815 0.203 0.824 0.184 
			   0.834 0.225 0.852 0.254 0.861 0.293 0.867 0.349 0.881 0.157 0.889 0.155 0.895 0.184 
			   0.913 0.206 0.926 0.257 0.930 0.310 0.946 0.136 0.956 0.123 0.969 0.169 0.987 0.208 1.000 0.262)
			 :duration dur :scaler (hz->radians 10800.0)))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .98  2 .005  3 .01 4 .005)))
	 (gen2 (make-oscil 0.0))
	 (ampf2 (make-env '(0 0 .68 0 .9 1 1 1) :duration dur :scaler .2)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((frq (env frqf)))
	   (outa i (* (env ampf)
		      (+ (polyshape gen1 1.0 frq)
			 (* (env ampf2)
			    (oscil gen2 (* 2 frq)))))
	       *output*)))))))

;(with-sound (:play #t) (willow-flycatcher 0 .5))


;;; --------------------------------------------------------------------------------
;;; 
;;; Black-necked stilt

(definstrument (black-necked-stilt beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.085)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.034 0.133 0.121 0.287 0.210 0.586 0.358 0.950 0.419 1.000 0.514 1.000 
			   0.611 0.912 0.819 0.251 0.893 0.204 0.962 0.105 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.000 0.246 0.073 0.252 0.107 0.294 0.131 0.276 0.160 0.300 0.269 0.334 0.345 0.350 
			   0.451 0.361 0.548 0.359 0.692 0.352 0.765 0.328 0.803 0.300 0.832 0.318 0.865 0.285 1.000 0.266)
			 :duration dur :scaler (hz->radians (* 1/3 10100.0))))
	 (gen1 (make-polyshape 0.0 :partials (normalize-partials (list 1 .05  2 .25  3 1  4 .5  5 .2  6 .01  8 .03  9 .03  10 .01  11 .005  12 .005))))
	 (gen2 (make-polyshape 0.0 :partials (list 3 .03  5 .05  7 .1  9 .03  11 .04  13 .015  15 .01  17 .005  19 .005  21 .005  23 .005)))
	 (ampf2 (make-env '(0 1  .2 .1  .3 1  .5 0 .7 0 1 .5) :duration dur))

	 (rnd (make-rand-interp 4000 (hz->radians 500)))
	 (rndf (make-env '(0 1 .2 0 .9 0 1 1) :duration dur)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((frq (+ (env frqf)
		       (* (env rndf)
			  (rand-interp rnd)))))
	   (outa i (* (env ampf)
		      (+ (polyshape gen1 1.0 frq)
			 (* (env ampf2)
			    (polyshape gen2 1.0 (* 0.5 frq)))))
		 *output*)))))))

;(with-sound (:play #t) (black-necked-stilt 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Bushtit

(definstrument (bushtit beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.368)
	 (stop (+ start (seconds->samples dur)))
	 (frqf (make-env '(0 0 1 -1) :duration dur :scaler (hz->radians 500)))
	 (pulse-dur .044)
	 (pulse-spacing (seconds->samples .064))
	 (next-pulse (+ start pulse-spacing))
	 (pulse-ampf (make-env '(0.000 0.000 0.114 0.486 0.182 0.988 0.394 0.763 0.620 1.000 0.769 0.937 0.889 1.000 1.000 0.000)
			       :duration pulse-dur :scaler amp))
	 (pulse-frqf (make-env '(0.000 0.230 0.109 0.291 0.212 0.322 0.298 0.343 0.410 0.348 0.654 0.357 0.821 0.354 0.889 0.337 0.952 0.304 1.000 0.231)
			       :duration pulse-dur :scaler (hz->radians 22050.0)))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .99  2 .01)))
	 (gen2 (make-polyshape 0.0 :partials (list 5 .9  7 .07  8 .02  9 .01  11 .02)))
	 (pulse-ampf2 (make-env '(0 0 .65 0 .8 1 .9 1 1 0) 
				:duration pulse-dur :scaler .4))
	 (pulse-frqf2 (make-env '(0 5400  .6 5400  .75 6300  1 5400) 
				:duration pulse-dur :scaler (hz->radians 0.2) :base .1))
	 (pulse2 #f))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (if (>= i next-pulse)
	     (begin
	       (mus-reset pulse-ampf)
	       (mus-reset pulse-frqf)
	       (if pulse2
		   (begin
		     (mus-reset pulse-ampf2)
		     (mus-reset pulse-frqf2))
		   (set! pulse2 #t))
	       (set! next-pulse (+ next-pulse pulse-spacing))))
	 (outa i (+ (* (env pulse-ampf)
		       (polyshape gen1 1.0 (+ (env pulse-frqf)
					      (env frqf))))
		    (if pulse2
			(* (env pulse-ampf2)
			   (polyshape gen2 1.0 (env pulse-frqf2)))
			0.0))
	       *output*))))))

;(with-sound (:play #t) (bushtit 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Red-breasted nuthatch

(definstrument (red-breasted-nuthatch beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.287)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.084 0.402 0.289 0.914 0.400 0.957 0.501 0.957 0.530 0.895 0.600 0.988 
			   0.680 1.000 0.786 0.926 0.860 0.984 0.912 0.969 0.962 0.855 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.000 0.257 0.029 0.306 0.061 0.322 0.101 0.318 0.965 0.361 1.000 0.316)
			 :duration dur :scaler (hz->radians (* 0.2 7510.0))))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .95  2 .03  3 .03)))
	 (mod1 (make-oscil 0.0))
	 (index (hz->radians (* 510 1.25)))
	 (rnd (make-rand-interp 100 (hz->radians 6))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((frq (+ (env frqf)
		       (rand-interp rnd))))
	   (outa i (* (env ampf)
		      (polyshape gen1 1.0 (+ (* 5 frq)
					     (* index (oscil mod1 frq)))))
		 *output*)))))))

;(with-sound (:play #t) (red-breasted-nuthatch 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; White-breasted nuthatch

(definstrument (white-breasted-nuthatch beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.31)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.157 0.871 0.386 1.000 0.728 0.759 0.951 0.306 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.000 0.318 0.045 0.392 0.160 0.418 0.254 0.418 0.517 0.404 0.816 0.367 1.000 0.310)
			 :duration dur :scaler (hz->radians (* 0.25 8900))))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .96  2 .02 3 .03)))
	 (gen2 (make-polyshape 0.0 :partials (list 4 .005 5 .01)))
	 (ampf2 (make-env '(0 1 .65 1 .75 0 1 0) :duration dur))
	 (mod1 (make-oscil 0.0))
	 (index (hz->radians (* 800 0.4)))
	 (rnd (make-rand-interp 100 (hz->radians 16)))
	 (vib (make-oscil 40.0))
	 (vib-index (hz->radians 50)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let* ((vb (oscil vib))
		(frq (+ (env frqf)
			(rand-interp rnd)
			(* vib-index vb)))
		(mfrq (+ (* 3 frq)
			 (* index (oscil mod1 frq)))))
	   (outa i (* (env ampf)
		      (+ .7 (* .3 (abs vb)))
		      (+ (polyshape gen1 1.0 mfrq)
			 (* (env ampf2)
			    (polyshape gen2 1.0 mfrq))))
		 *output*)))))))

;(with-sound (:play #t) (white-breasted-nuthatch 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Pygmy nuthatch

(definstrument (pygmy-nuthatch beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.12)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.096 0.247 0.148 0.591 0.206 0.767 0.244 0.995 0.260 0.470 0.385 0.406 
			   0.406 0.240 0.425 0.370 0.475 0.244 0.488 0.324 0.511 0.326 0.528 0.258 0.544 0.336 
			   0.594 0.331 0.638 0.256 0.689 0.288 0.720 0.279 0.769 0.299 0.795 0.249 0.818 0.272 
			   0.841 0.242 0.856 0.292 0.865 0.205 0.876 0.304 0.897 0.304 0.932 0.126 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.000 0.096 0.063 0.174 0.090 0.199 0.127 0.205 0.25 0.210 0.26 0.237 0.733 0.222 
			   0.865 0.210 0.890 0.195 0.912 0.159 0.930 0.145 1.000 0.138)
			 :duration dur :scaler (hz->radians 21500.0)))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .98  2 .005  3 .01))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (pygmy-nuthatch 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Flammulated owl

(definstrument (flammulated-owl beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.25)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.146 0.603 0.286 0.858 0.455 0.992 0.629 0.983 0.789 0.819 0.907 0.473 0.946 0.445 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0 340  .1 400  .15 410  .9 420  1 380) :duration dur :scaler (hz->radians 1.0)))
	 (gen1 (make-polyshape 0 :partials (list 1 .98  2 .01  3 .005))))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (flammulated-owl 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Song sparrow

(define (song-sparrow beg1 amp1)

  (definstrument (song-sparrow-big-buzz beg amp)
    (let* ((start (seconds->samples beg))
	   (dur 0.25)
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0.000 0.000 0.106 0.826 0.190 0.996 0.418 0.818 0.688 0.458 0.897 0.447 0.962 0.348 1.000 0.000)
			   :duration dur :scaler amp))
	   (frqf (make-env '(0 4400 .4 4200 1 4300) :duration dur :scaler (hz->radians 1.0)))
	   (gen-up (make-polyshape 0.0 :partials (vct 1 .95  2 .03  3 .02)))
	   (gen-down (make-polyshape 0.0 :partials (vct 1 .95  2 .03  3 .02)))
	   (mod-up (make-oscil 142.0))
	   (mod-down (make-oscil 142.0))
	   (pulser (make-pulse-train 142.0))
	   (ampf-up (make-env '(0 .1  .23 .01  .27 .01  .6 .5  .75 1  .8 .4  1 .1) :duration (/ 1.0 142.0)))
	   (ampf-down (make-env '(0 .1  .2 .6  .25 1  .6 .1  .75 .001 1 .1) :duration (/ 1.0 142.0)))
	   (ampf2 (make-env '(0 1 1 0) :duration dur :scaler 0.7))
	   (rnd (make-rand-interp 6000 (hz->radians 300))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (let ((modup (oscil mod-up))
		 (moddown (oscil mod-down))
		 (frq (+ (env frqf)
			 (rand-interp rnd))))
	     (if (> (pulse-train pulser) .1)
		 (begin
		   (mus-reset ampf-up)
		   (mus-reset ampf-down)))
	     (outa i (* (env ampf)
			(+ (* (env ampf-up)
			      (polyshape gen-up 1.0 (+ frq (hz->radians 700)
						       (* (hz->radians 800) 
							  modup))))
			   (* (env ampf-down)
			      (env ampf2)
			      (polyshape gen-down 1.0 (+ frq (hz->radians -600)
							 (* (hz->radians 800)
							    moddown))))))
		   *output*)))))))

  (definstrument (song-sparrow-clear-tone beg frq amp)
    (let* ((start (seconds->samples beg))
	   (dur 0.062)
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0.000 0.000 0.054 0.208 0.138 0.327 0.162 0.292 0.306 0.659 0.352 0.698 0.400 0.835 
			     0.468 0.894 0.507 0.808 0.548 0.906 0.593 0.833 0.623 0.939 0.668 0.902 0.710 0.971 
			     0.735 0.678 0.759 0.937 0.777 1.000 0.792 0.820 0.800 0.922 0.847 0.806 1.000 0.000)
			   :duration dur :scaler amp))
	   (frqf (make-env '(0.000 0.203 0.043 0.253 0.118 0.276 0.446 0.263 0.575 0.251 0.707 0.226 0.883 0.187 1.000 0.153)
			   :duration dur :scaler (hz->radians (* frq 22050.0))))
	   (gen1 (make-polyshape 0.0 :partials (list 1 .98  2 .005  3 .01))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf)
		      (polyshape gen1 1.0 (env frqf)))
		 *output*))))))

  (definstrument (song-sparrow-sweep-tone beg amp)
    (let* ((start (seconds->samples beg))
	   (dur 0.036)
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0.000 0.000 0.048 0.158 0.098 0.474 0.201 0.574 0.235 0.206 0.357 0.206 0.419 0.366 
			     0.468 0.331 0.489 0.173 0.519 0.579 0.567 0.639 0.604 0.972 0.647 0.556 0.697 0.391 0.75 0.0)
			   :duration dur :scaler amp))
	   (frqf (make-env '(0.000 0.358 0.033 0.355 0.060 0.321 0.078 0.240 0.098 0.214 0.120 0.257 0.153 0.364 
			     0.187 0.420 0.242 0.428 0.289 0.418 0.348 0.390 0.465 0.381 0.531 0.360 0.571 0.338 
			     0.609 0.306 0.668 0.246 0.75 0.223)
			   :duration dur :scaler (hz->radians 22050.0)))
	   (gen1 (make-polyshape 0.0 :partials (list 1 .95  2 .05))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf)
		      (polyshape gen1 1.0 (env frqf)))
		 *output*))))))

  (definstrument (song-sparrow-sweep-caw beg amp)
    (let* ((start (seconds->samples beg))
	   (dur 0.025)
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0.000 0.000 0.155 0.364 0.185 0.154 0.242 0.542 0.283 0.526 0.321 0.660 0.386 0.846 
			     0.451 0.292 0.481 1.000 0.546 0.881 0.584 0.676 0.636 0.798 0.685 0.269 0.755 0.640 
			     0.791 0.316 0.957 0.115 1.000 0.000)
			   :duration dur :scaler amp))
	   (frqf (make-env '(0.000 0.210 0.170 0.232 0.187 0.164 0.453 0.212 0.462 0.159 0.668 0.201 0.699 0.147
			     0.905 0.181 1.000 0.164)
			   :duration dur :scaler (hz->radians 22050.0)))
	   (gen (make-polyshape 0.0 :partials (list 2 .92 3 .02 4 .01 5 .005)))
	   (rnd (make-rand-interp 4000 (hz->radians 500))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf)
		      (polyshape gen 1.0 (+ (* .5 (env frqf))
					    (rand-interp rnd))))
		 *output*))))))

  (definstrument (song-sparrow-little-buzz beg amp)
    (let* ((start (seconds->samples beg))
	   (dur 0.17)
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0 0 1 1 2 1 4 0) :duration dur :scaler amp))
	   (frqf (make-env '(0 0 1 1) :duration dur :scaler (hz->radians 20)))
	   (gen (make-polyshape 610.0 :partials (normalize-partials (list 3 .4 4 1 5 .01 6 .2 7 .03 8 .02 9 .01 10 .01 11 .01 12 .005))))
	   (rnd (make-rand-interp 4000 (hz->radians 50)))
	   (tri (make-triangle-wave 60 0.6)))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf)
		      (+ .4 (abs (triangle-wave tri)))
		      (polyshape gen 1.0 (+ (env frqf)
					    (rand-interp rnd))))
		 *output*))))))

  (definstrument (song-sparrow-sweep-down beg amp)
    (let* ((start (seconds->samples beg))
	   (dur 0.186)
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0.000 0.000 0.125 0.212 0.164 1.000 0.206 0.218 0.231 0.278 0.243 0.470 0.273 0.136 
			     0.301 0.419 0.311 0.691 0.329 0.530 0.341 0.501 0.354 0.295 0.373 0.382 0.383 0.193 
			     0.397 0.232 0.403 0.127 0.415 0.238 0.426 0.173 0.433 0.238 0.452 0.088 0.528 0.286 
			     0.552 0.150 0.564 0.309 0.590 0.133 0.608 0.303 0.635 0.204 0.659 0.215 0.673 0.275 
			     0.709 0.380 0.718 0.292 0.727 0.354 0.749 0.235 0.800 0.139 0.831 0.153 0.853 0.232 
			     0.886 0.130 0.911 0.337 0.932 0.147 0.949 0.224 0.965 0.082 1.000 0.000)
			   :duration dur :scaler amp))
	   (frqf (make-env '(0 9000 .08 7900  .164 7385 .2 7000 1 6960) :duration dur :scaler (hz->radians 1.0)))
	   (gen1 (make-polyshape 0.0 :partials (list 1 .95  2 .05))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf)
		      (polyshape gen1 1.0 (env frqf)))
		 *output*))))))

  (definstrument (song-sparrow-sweep-up beg amp)
    (let* ((start (seconds->samples beg))
	   (dur 0.076)
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0 0 1 1 2 1 3 0) :base .1
			   :duration dur :scaler amp))
	   (frqf (make-env '(0 2260 .3 3000 .9 3100 1 3300)
			   :duration dur :scaler (hz->radians 1.0)))
	   (gen1 (make-polyshape 0.0 :partials (list 1 .98  2 .005  3 .01 4 .005))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf)
		      (polyshape gen1 1.0 (env frqf)))
		 *output*))))))

  (definstrument (song-sparrow-2nd-buzz beg amp)
    (let* ((start (seconds->samples beg))
	   (dur 0.053)
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0.000 0.000 0.101 0.110 0.174 0.229 0.206 0.229 0.220 0.153 0.231 0.285 0.263 0.449 
			     0.293 0.424 0.315 0.698 0.374 0.661 0.416 0.884 0.554 0.785 0.587 0.661 0.600 0.037 
			     0.608 0.859 0.685 0.554 0.692 0.034 0.705 0.734 0.729 0.602 0.741 0.079 0.753 0.695 
			     0.787 0.737 0.803 0.110 0.813 0.520 0.841 0.757 0.868 0.085 0.882 0.398 0.899 0.514 
			     0.922 0.172 0.952 0.280 0.962 0.150 0.993 0.198 1.000 0.000)
			   :duration dur :scaler amp))
	   (frqf (make-env '(0 3100  .4 3430  1 3140)
			   :duration dur :offset (hz->radians 40) :scaler (hz->radians 0.5)))
	   (gen1 (make-polyshape 0.0 :partials (list 1 .1 2 .5  3 .2  4 .1  6 .05  8 .03)))
	   (vib (make-blackman 320 4)))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (let ((vb (blackman vib 0.0)))
	     (outa i (* (env ampf)
			(+ .4 (* .6 vb))
			(polyshape gen1 1.0 (+ (env frqf)
					       (* (hz->radians 200) vb))))
		   *output*)))))))


  (definstrument (song-sparrow-chiff beg amp)
    (let* ((start (seconds->samples beg))
	   (dur 0.019)
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0 0 1 1 2 0) :duration dur :scaler amp))
	   (frqf (make-env '(0 4800  .4 5500  .6 6060  .9 4200 1 4100)
			   :duration dur :scaler (hz->radians 0.32)))
	   (gen1 (make-polyshape 0.0 :partials (list 3 .7 4 .3  7 .01))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf)
		      (polyshape gen1 1.0 (env frqf)))
		 *output*))))))

  (song-sparrow-little-buzz beg1 (* .25 amp1))
  (song-sparrow-clear-tone (+ beg1 0.2) 1.0 (* 0.9 amp1))

  (song-sparrow-little-buzz (+ beg1 .37) (* .4 amp1))
  (song-sparrow-clear-tone (+ beg1 0.57) 1.0 amp1)

  (let ((amps (vct .14 .33 .37 .37 .30 .30 .30)))
    (do ((i 0 (1+ i))
	 (x 0.68 (+ x .1)))
	((= i 7)) 
      (song-sparrow-sweep-tone (+ beg1 x) (* (vct-ref amps i) amp1))
      (song-sparrow-sweep-caw (+ beg1 x .05) (* 0.5 amp1))))

  (song-sparrow-sweep-tone (+ beg1 1.37) (* .27 amp1))
  (song-sparrow-big-buzz (+ beg1 1.44) (* 0.75 amp1))

  (song-sparrow-2nd-buzz (+ 1.73 beg1) (* .25 amp1))
  (song-sparrow-sweep-up (+ 1.8 beg1) (* .5 amp1))
  (song-sparrow-chiff (+ 1.92 beg1) (* .3 amp1))
  (song-sparrow-sweep-down (+ beg1 1.96) (* .3 amp1))
  (song-sparrow-clear-tone (+ beg1 2.22) 0.86 (* .8 amp1)))
  
;(with-sound (:play #t) (song-sparrow 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Burrowing owl

(definstrument (burrowing-owl beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.6)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.038 0.510 0.060 0.743 0.082 0.822 0.114 0.775 0.125 0.692 0.204 0.000 
			   0.416 0.000 0.465 0.858 0.500 1.000 0.571 0.921 0.611 0.937 0.927 0.526 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.000 0.029 0.035 0.108 0.045 0.133 0.091 0.135 0.129 0.132 0.176 0.107 0.190 0.043 
			   0.424 0.046 0.446 0.110 0.468 0.133 0.75 .133 0.8 .129 0.936 0.127 0.973 0.104 1.000 0.035) 
			 :duration dur :scaler (hz->radians 6800.0)))
	 (rnd (make-rand-interp 200 (hz->radians 5)))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .92  2 .08)))
	 (gen2 (make-polyshape 0.0 :partials (list 3 .01  4 .01  6 .005 7 .007 8 .002  9 .002  10 .002 12 .001)))
	 (ampf2 (make-env '(0 0 .05 0 .09 1  .13 1 .2 0 .45 0 .5 1 .9 1 1 0) :duration dur))
	 (gen3 (make-oscil 0.0))
	 (ampf3 (make-env '(0 0  .08 0  .12 .4  .2 0  .46 0  .5 .5  .6 0  .65 0  .8 1  .9 1  1 0) :duration dur :scaler .015)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((frq (+ (env frqf)
		       (rand-interp rnd))))
	 (outa i (* (env ampf)
		    (+ (polyshape gen1 1.0 frq)
		       (* (env ampf2)
			  (polyshape gen2 1.0 frq))
		       (* (env ampf3)
			  (oscil gen3 (* 5 frq)))))
	       *output*)))))))

;(with-sound (:play #t) (burrowing-owl 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Gray vireo

(definstrument (gray-vireo-1 beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.23)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.059 0.052 0.078 0.132 0.085 0.076 0.099 0.100 0.108 0.260 0.125 0.102 
			   0.134 0.280 0.143 0.254 0.146 0.306 0.158 0.197 0.184 0.403 0.193 0.403 0.201 0.180 
			   0.208 0.239 0.217 0.375 0.222 0.000 0.229 0.232 0.237 0.158 0.254 0.800 0.263 0.846 
			   0.307 0.785 0.322 0.850 0.334 1.000 0.350 0.731 0.357 0.822 0.369 0.599 0.378 0.711 
			   0.384 0.375 0.389 0.688 0.393 0.315 0.396 0.657 0.410 0.291 0.419 0.414 0.491 0.412 
			   0.566 0.258 0.593 0.06 0.659 0.01 0.754 0.06 0.772 0.245 0.854 0.523 0.892 0.330 
			   0.932 0.436 0.947 0.182 0.957 0.228 0.981 0.063 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.000 0.203 0.033 0.198 0.051 0.218 0.064 0.189 0.085 0.226 0.101 0.187 0.116 0.214 
			   0.133 0.183 0.199 0.172 0.216 0.186 0.242 0.288 0.256 0.330 0.297 0.330 0.324 0.316 
			   0.441 0.243 0.506 0.215 0.556 0.206 0.690 0.107 0.721 0.104 0.739 0.136 0.769 0.215 
			   0.796 0.266 0.833 0.282 1.000 0.29)
			 :duration dur :scaler (hz->radians 10250.0)))
	 (gen1 (make-oscil 0.0))
	 (gen2 (make-oscil 0.0))
	 (interpf (make-env '(0 .5  .2 .1  .25 .99  .6 .95  .7 .3   .75 .99  1 .99) :duration dur)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((frq (env frqf))
	       (interp (env interpf)))
	   (outa i (* (env ampf)
		      (+ (* interp
			    (oscil gen1 frq))
			 (* (- 1.0 interp)
			    (oscil gen2 (* 2 frq)))))
		 *output*)))))))

;(with-sound (:play #t) (gray-vireo-1 0 .5))


(definstrument (gray-vireo-2 beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.23)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.026 0.099 0.067 0.176 0.097 0.111 0.110 0.199 0.147 0.096 0.186 0.110 
			   0.214 0.068 0.228 0.113 0.245 0.069 0.254 0.097 0.264 0.209 0.274 0.143 0.291 0.096 
			   0.301 0.250 0.315 0.347 0.332 0.166 0.343 0.144 0.353 0.369 0.364 0.446 0.371 0.429 
			   0.390 0.196 0.403 0.383 0.411 0.495 0.434 0.372 0.442 0.192 0.456 0.449 0.471 0.364 
			   0.479 0.505 0.486 0.487 0.492 0.413 0.500 0.455 0.509 0.323 0.515 0.361 0.519 0.212 
			   0.524 0.259 0.529 0.469 0.548 0.549 0.562 0.235 0.565 0.316 0.576 0.385 0.582 0.488 
			   0.594 0.493 0.600 0.402 0.612 0.355 0.616 0.414 0.624 0.306 0.636 0.289 0.641 0.223 
			   0.648 0.349 0.655 0.287 0.674 0.325 0.682 0.270 0.691 0.133 0.712 0.275 0.726 0.319 
			   0.743 0.578 0.765 0.722 0.776 0.651 0.796 1.000 0.808 0.686 0.816 0.556 0.825 0.826 
			   0.833 0.527 0.835 0.725 0.848 0.281 0.855 0.639 0.861 0.405 0.863 0.485 0.869 0.250 
			   0.871 0.414 0.876 0.193 0.882 0.468 0.886 0.443 0.894 0.141 0.897 0.312 0.901 0.181 
			   0.903 0.435 0.907 0.339 0.910 0.432 0.937 0.399 0.949 0.234 0.974 0.243 0.993 0.159 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.000 0.260 0.040 0.245 0.091 0.203 0.122 0.174 0.152 0.221 0.174 0.240 0.190 0.297 
			   0.211 0.232 0.227 0.284 0.248 0.250 0.261 0.331 0.289 0.260 0.304 0.333 0.321 0.336 
			   0.337 0.286 0.356 0.349 0.371 0.346 0.386 0.299 0.406 0.359 0.422 0.359 0.439 0.307 
			   0.460 0.383 0.474 0.380 0.489 0.352 0.513 0.414 0.533 0.378 0.546 0.385 0.564 0.432 
			   0.582 0.383 0.598 0.393 0.609 0.435 0.678 0.430 0.719 0.466 0.780 0.518 0.821 0.560 
			   0.856 0.620 0.922 0.677 1.000 0.701)
			 :duration dur :scaler (hz->radians 6030.0)))
	 (gen1 (make-oscil 0.0))
	 (gen2 (make-oscil 0.0))
	 (gen3 (make-oscil 0.0))
	 (interpf (make-env '(0 .1  .2 .1  .3 .99  1 .99) :duration dur)))
   (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((frq (env frqf))
	       (interp (env interpf)))
	   (outa i (* (env ampf)
		      (+ (* interp
			    (oscil gen1 frq))
			 (* 0.5 (- 1.0 interp)
			    (+ (oscil gen2 (* 2 frq))
			       (oscil gen3 (* 3 frq))))))
		 *output*)))))))

;(with-sound (:play #t) (gray-vireo-2 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Bald eagle

(define (bald-eagle beg amp)

  (definstrument (bald-eagle-1 beg amp)
    (let* ((start (seconds->samples beg))
	   (dur 0.153)
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0.000 0.000 0.016 0.145 0.042 0.773 0.084 0.471 0.097 0.265 0.112 0.124 0.123 0.297 
			     0.134 0.937 0.151 0.665 0.166 0.810 0.194 0.733 0.228 0.218 0.244 0.059 0.269 0.373 
			     0.280 0.621 0.292 0.597 0.314 0.681 0.368 0.241 0.389 0.332 0.411 0.688 0.431 0.674 
			     0.447 0.539 0.477 0.452 0.517 0.572 0.534 0.550 0.555 0.455 0.576 0.457 0.597 0.494 
			     0.613 0.391 0.630 0.073 0.648 0.164 0.671 0.119 0.680 0.014 0.700 0.077 0.737 0.045 
			     0.757 0.075 0.803 0.024 0.817 0.065 0.855 0.028 0.866 0.063 0.906 0.033 0.973 0.042 1.000 0.000)
			   :duration dur :scaler amp))
	   (frqf1 (make-env '(0.000 0.268 0.042 0.273 0.132 0.280 0.281 0.280 0.409 0.273 0.609 0.282 0.686 0.289 1.000 0.273)
			    :duration dur :scaler (hz->radians 10000.0)))
	   (frqf2 (make-env '(0.000 0.541 0.050 0.543 0.130 0.555 0.202 0.559 0.271 0.568 0.413 0.534 0.522 0.543 
			      0.586 0.559 0.638 0.582 0.706 0.566 0.791 0.539 0.852 0.516 0.922 0.466 0.962 0.400 1.000 0.309)
			    :duration dur :scaler (hz->radians 10000.0)))
	   (gen1 (make-polyshape 0.0 :partials (list 1 .98  3 .02)))
	   (gen2 (make-polyshape 0.0 :partials (list 1 .95  2 .05)))
	   (intrpf (make-env '(0 .9 .6 .9 .7 .5 1 .5) :duration dur))
	   (rnd (make-rand-interp 2000 ))
	   (rndf (make-env '(0 0 .02 0 .04 1 .06 0 .13 0 .135 1 .14 0 .27 0 .276 1 .28 0 .4 0 .405 1 .41 0 .678 0 .682 1 1 1) 
			   :duration dur :offset (hz->radians 40) :scaler (hz->radians 100))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (let ((intrp (env intrpf))
		 (noise (* (env rndf)
			   (rand-interp rnd))))
	     (outa i (* (env ampf)
			(+ (* intrp (polyshape gen1 1.0 (+ (env frqf1)
							   noise)))
			   (* (- 1.0 intrp) (polyshape gen2 1.0 (+ (env frqf2)
								   (* 2 noise))))))
		   *output*)))))))
  
  (definstrument (bald-eagle-2 beg amp)
    (let* ((start (seconds->samples beg))
	   (dur 0.074)
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0.000 0.000 0.045 0.084 0.073 0.223 0.176 0.395 0.207 1.000 0.245 0.616 0.276 0.093 
			     0.301 0.325 0.349 0.316 0.396 0.211 0.445 0.075 0.643 0.145 0.777 0.170 0.804 0.291 
			     0.848 0.164 1.000 0.000)
			   :duration dur :scaler amp))
	   (frqf (make-env '(0.000 0.752 0.065 0.617 0.116 0.597 0.140 0.513 0.158 0.420 0.177 0.273 0.255 0.285 
			     0.351 0.285 0.393 0.285 0.467 0.287 0.518 0.293 0.582 0.301 0.638 0.163 0.690 0.225 
			     0.752 0.282 0.800 0.262 0.875 0.268 1.000 0.290)
			   :duration dur :scaler (hz->radians 10100.0)))
	   (gen1 (make-polyshape 0.0 :partials (list 1 .99  2 .01)))
	   (rnd (make-rand-interp 2000))
	   (rndf (make-env '(0 0 .16 0 .25 1 1 .5) :duration dur :offset (hz->radians 100) :scaler (hz->radians 500))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf)
		      (polyshape gen1 1.0 (+ (env frqf)
					     (* (env rndf)
						(rand-interp rnd)))))
		 *output*))))))
  
  (definstrument (bald-eagle-3 beg amp)
    (let* ((start (seconds->samples beg))
	   (dur 0.074)
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0.000 0.000 0.048 0.225 0.130 0.273 0.170 0.507 0.190 1.000 0.204 0.775 0.259 0.761 
			     0.318 0.349 0.361 0.501 0.447 0.045 0.476 0.375 0.539 0.476 0.560 0.679 0.593 0.670 
			     0.613 0.583 0.668 0.028 0.684 0.177 0.727 0.068 0.741 0.400 0.766 0.504 0.784 0.372 
			     0.826 0.400 0.857 0.318 0.879 0.085 0.937 0.045 0.979 0.073 1.000 0.000)
			   :duration dur :scaler amp))
	   (frqf (make-env '(0.000 0.449 0.076 0.410 0.122 0.325 0.150 0.195 0.190 0.195 0.255 0.198 0.371 0.198 
			     0.436 0.198 0.465 0.215 0.521 0.203 0.745 0.198 1.000 0.195)
			   :duration dur :scaler (hz->radians 14000.0)))
	   (gen1 (make-polyshape 0.0 :partials (list 1 .99   3 .01)))
	   (gen2 (make-polyshape 0.0 :partials (list 1 .9   2 .1)))
	   (ampf2 (make-env '(0 0 .2 0 .25 1 .3 1 .35 0 .5 0 .55 1 .6 1 .65 0 .75 0 .8 .5 .9 0 1 0) :scaler .1 :duration dur))
	   (rnd (make-rand-interp 2000 (hz->radians 200))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (let ((frq (+ (env frqf)
			 (rand-interp rnd))))
	     (outa i (* (env ampf)
			(+ (polyshape gen1 1.0 frq)
			   (* (env ampf2)
			      (polyshape gen2 1.0 (* 2 frq)))))
		   *output*)))))))
  
  (definstrument (bald-eagle-4 beg frqscl amp)
    (let* ((start (seconds->samples beg))
	   (dur 0.056)
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0.000 0.000 0.079 0.116 0.125 0.198 0.154 0.867 0.190 0.994 0.214 0.743 0.260 0.867 
			     0.282 0.802 0.315 0.825 0.330 0.636 0.371 0.678 0.423 0.825 0.468 0.734 0.504 0.542 
			     0.549 0.619 0.595 0.960 0.637 0.686 0.669 0.130 0.772 0.418 0.823 0.147 0.890 0.056 
			     0.929 0.090 1.000 0.000)
			   :duration dur :scaler amp))
	   (frqf (make-env '(0.000 0.215 0.035 0.243 0.060 0.285 0.086 0.268 0.121 0.198 0.135 0.137 0.154 0.167 
			     0.218 0.186 0.880 0.181 1.000 0.192)
			   :duration dur :scaler (hz->radians (* frqscl 13920))))
	   (gen1 (make-polyshape 0.0 :partials (list 1 .96  2 .04   3 .007  4 .002)))
	   (rnd (make-rand-interp 4000 (hz->radians 100))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (let ((frq (+ (env frqf)
			 (rand-interp rnd))))
	     (outa i (* (env ampf)
			(polyshape gen1 1.0 frq))
		   *output*)))))))
  
  
  (bald-eagle-1 beg (* amp .8))
  (bald-eagle-2 (+ beg .195) (* amp .7))
  (bald-eagle-3 (+ beg .32) (* amp .7))
  (bald-eagle-4 (+ beg .47) 1.1 amp)  
  (bald-eagle-4 (+ beg .63) 1.05 amp)  
  (bald-eagle-4 (+ beg .806) 1.0 amp))

;(with-sound (:play #t) (bald-eagle 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Eastern meadowlark

(definstrument (eastern-meadowlark beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 1.65)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.011 0.304 0.022 0.310 0.032 0.162 0.039 0.000 0.121 0.000 0.136 0.148 
			   0.145 0.138 0.152 0.184 0.170 0.430 0.188 0.406 0.213 0.556 0.244 0.700 0.273 0.999 
			   0.280 0.992 0.293 0.000 0.335 0.000 0.341 0.169 0.356 0.786 0.369 0.941 0.389 0.689 
			   0.399 0.622 0.430 0.515 0.480 0.426 0.494 0.000 0.518 0.000 0.524 0.383 0.528 0.017 
			   0.540 0.446 0.549 0.000 0.585 0.000 0.591 0.113 0.610 0.134 0.631 0.175 0.644 0.261 
			   0.656 0.212 0.665 0.282 0.680 0.196 0.691 0.233 0.702 0.182 0.715 0.226 0.726 0.281 
			   0.733 0.268 0.747 0.370 0.753 0.328 0.760 0.450 0.767 0.402 0.770 0.469 0.777 0.412 
			   0.795 0.714 0.812 0.515 0.825 0.444 0.839 0.250 0.843 0.015 0.852 0.295 0.856 0.254 
			   0.858 0.045 0.863 0.008 0.867 0.075 0.870 0.279 0.878 0.304 0.891 0.189 0.910 0.115 
			   0.932 0.068 0.960 0.034 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.000 0.197 0.051 0.192 0.097 0.437 0.129 0.421 0.201 0.371 0.285 0.332 0.325 0.343 
			   0.341 0.358 0.386 0.269 0.410 0.245 0.481 0.231 0.524 0.229 0.533 0.214 0.557 0.214 
			   0.576 0.231 0.579 0.725 0.588 0.710 0.594 0.635 0.606 0.596 0.644 0.509 0.724 0.439 
			   0.756 0.397 0.800 0.297 0.825 0.264 0.851 0.271 0.870 0.275 0.892 0.214 0.925 0.194 1.000 0.197)
			 :duration dur :scaler (hz->radians 10280.0)))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .98  2 .005  3 .01  4 .005  5 .002)))
	 (buzz (make-oscil 350))
	 (buzzf (make-env '(0 0 .59 0 .6 1 .61 1 .64 0 1 0) :duration dur :scaler (hz->radians 300))))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (+ (env frqf)
					   (* (env buzzf)
					      (oscil buzz)))))
	       *output*))))))

;(with-sound (:play #t) (eastern-meadowlark 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Yellow-green vireo

(definstrument (yellow-green-vireo beg amp)
  (let* ((start (seconds->samples beg))
	 (dur 0.22)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.054 0.070 0.083 0.403 0.109 0.142 0.137 0.558 0.155 0.682 0.173 0.408 
			   0.183 0.915 0.213 0.879 0.236 1.000 0.290 0.938 0.309 0.257 0.311 0.800 0.321 0.384 
			   0.329 0.607 0.337 0.117 0.345 0.930 0.357 0.155 0.390 0.329 0.395 0.486 0.418 0.359 
			   0.436 0.274 0.493 0.384 0.518 0.363 0.529 0.276 0.542 0.471 0.567 0.490 0.573 0.361 
			   0.587 0.618 0.595 0.380 0.600 0.722 0.609 0.365 0.622 0.845 0.652 0.193 0.665 0.913 
			   0.715 0.713 0.731 0.837 0.745 0.486 0.753 0.826 0.764 0.713 0.780 0.204 0.787 0.437 
			   0.799 0.053 0.816 0.255 0.859 0.501 0.885 0.735 0.910 0.378 0.924 0.724 0.958 0.098 
			   0.966 0.197 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.000 0.310 0.043 0.321 0.074 0.470 0.095 0.523 0.113 0.544 0.138 0.513 0.170 0.451 
			   0.224 0.413 0.291 0.418 0.314 0.451 0.330 0.392 0.348 0.340 0.369 0.298 0.392 0.278 
			   0.442 0.262 0.515 0.266 0.574 0.295 0.617 0.338 0.647 0.380 0.672 0.418 0.692 0.439 
			   0.738 0.454 0.793 0.464 0.843 0.464 0.890 0.477 0.923 0.490 0.955 0.494 0.978 0.475 
			   1.000 0.433)
			 :duration dur :scaler (hz->radians 10000.0)))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .98  2 .003  3 .02))))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (yellow-green-vireo 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Magnolia warbler

(definstrument (magnolia-warbler beg amp)
  ;; east 13 3
  (let* ((start (seconds->samples beg))
	 (dur 0.96)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.018 0.030 0.034 0.085 0.060 0.000 0.071 0.040 0.087 0.058 0.105 0.137 
			   0.110 0.027 0.118 0.000 0.176 0.000 0.211 0.076 0.226 0.218 0.229 0.366 0.240 0.419 
			   0.252 0.088 0.261 0.000 0.300 0.000 0.321 0.391 0.325 0.340 0.330 0.490 0.336 0.516 
			   0.338 0.482 0.342 0.620 0.344 0.364 0.346 0.600 0.357 0.562 0.365 0.000 0.370 0.059 
			   0.384 0.542 0.387 0.484 0.393 0.626 0.406 0.788 0.422 0.000 0.458 0.000 0.474 0.186 
			   0.490 0.246 0.498 0.275 0.511 0.541 0.513 0.475 0.526 0.917 0.536 0.633 0.539 0.698 
			   0.554 0.000 0.593 0.000 0.610 0.414 0.613 0.794 0.617 0.689 0.619 0.847 0.625 0.942 
			   0.628 0.480 0.631 0.904 0.636 0.814 0.653 0.000 0.657 0.083 0.671 0.768 0.675 0.655 
			   0.679 0.801 0.686 0.999 0.695 0.818 0.704 0.000 0.734 0.000 0.750 0.657 0.756 0.797 
			   0.762 0.750 0.768 0.581 0.772 0.970 0.778 0.869 0.783 0.710 0.789 0.573 0.796 0.549 
			   0.805 0.251 0.807 0.524 0.816 0.199 0.823 0.257 0.831 0.541 0.841 0.565 0.847 0.847 
			   0.852 0.653 0.856 0.511 0.861 0.429 0.866 0.472 0.871 0.285 0.876 0.463 0.882 0.216 
			   0.887 0.387 0.891 0.311 0.895 0.369 0.902 0.301 0.905 0.359 0.912 0.326 0.915 0.410 
			   0.919 0.369 0.924 0.394 0.925 0.357 0.932 0.398 0.952 0.277 0.973 0.064 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.000 0.530 0.020 0.591 0.048 0.447 0.055 0.754 0.073 0.660 0.101 0.579 0.108 0.519 
			   0.180 0.349 0.196 0.348 0.212 0.359 0.223 0.440 0.233 0.499 0.243 0.414 0.258 0.268 
			   0.287 0.268 0.297 0.523 0.306 0.532 0.317 0.558 0.328 0.575 0.341 0.512 0.353 0.458 
			   0.365 0.773 0.379 0.660 0.392 0.606 0.411 0.543 0.455 0.353 0.495 0.353 0.524 0.486 
			   0.540 0.386 0.553 0.272 0.571 0.274 0.588 0.471 0.605 0.518 0.620 0.551 0.642 0.425 
			   0.650 0.739 0.673 0.628 0.687 0.562 0.700 0.536 0.740 0.433 0.768 0.527 0.800 0.702 
			   0.804 0.636 0.811 0.516 0.821 0.421 0.833 0.490 0.845 0.532 0.855 0.621 0.870 0.702 
			   0.875 0.638 0.888 0.455 1.000 0.301)
			 :duration dur :scaler (hz->radians 10000.0)))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .98  2 .005  3 .01))))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (outa i (* (env ampf)
		    (polyshape gen1 1.0 (env frqf)))
	       *output*))))))

;(with-sound (:play #t) (magnolia-warbler 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Eastern bluebird

(define (eastern-bluebird beg1 amp1)
  ;; east 75 10

  (definstrument (eastern-bluebird-1 beg amp)
    (let* ((start (seconds->samples beg))
	   (dur 0.285)
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0.000 0.000 0.067 0.103  0.083 0.145  0.134 0.636 0.152 0.615 0.197 0.872 0.238 1.000 
			     0.272 1.000 0.298 0.842 0.340 0.889 0.415 0.820 0.455 0.194 0.464 0.400 0.500 0.243 
			     0.559 0.267 0.589 0.229 0.635 0.274 0.687 0.835 0.712 0.896 0.805 0.811 0.926 0.759 
			     0.972 0.225 1.000 0.000)
			   :duration dur :scaler amp))
	   (frqf (make-env '(0.000 0.103 0.049 0.138 0.119 0.200 0.198 0.229 0.251 0.234 0.317 0.247 0.422 0.250 
			     0.540 0.250 0.643 0.235 0.722 0.229 0.902 0.217 0.936 0.207 0.970 0.150 1.000 0.136)
			   :duration dur :scaler (hz->radians 10200.0)))
	   (gen1 (make-polyshape 0.0 :partials (list 1 .8  2 .01 3 .05  4 .01  5 .005)))
	   (gen2 (make-polyshape 0.0 :partials (list 1 .92  2 .01 3 .05  4 .01  5 .005)))
	   (ampf2 (make-env '(0 1 .2 0 1 0) :duration dur))
	   (rnd (make-sine-summation 200 :n 2 :a .5))
	   (rndf (make-env '(0 0 .1 1 .25 0 .45 0 .5 1 .6 0 1 0) :duration dur :scaler (hz->radians 200)))
	   (rndfrqf (make-env '(0 1 .45 1 .5 .2 .6 .2 1 0) :duration dur :scaler (hz->radians 200))))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (let ((frq (+ (env frqf)
			 (* (env rndf)
			    (sine-summation rnd (env rndfrqf))))))
	     (outa i (* (env ampf)
			(+ (polyshape gen1 1.0 frq)
			   (* (env ampf2)
			      (polyshape gen2 1.0 (* 1.5 frq)))))
		   *output*)))))))

  (definstrument (eastern-bluebird-2 beg amp)
    (let* ((start (seconds->samples beg))
	   (dur 0.33)
	   (stop (+ start (seconds->samples dur)))
	   (ampf (make-env '(0.000 0.000 0.023 0.324 0.046 0.438 0.089 0.551 0.136 0.994 0.248 0.978 0.470 0.452 
			     0.506 0.432 0.556 0.479 0.629 0.197 0.681 0.211 0.746 0.330 0.791 0.161 0.814 0.180 
			     0.828 0.127 0.916 0.291 0.959 0.102 1.000 0.000)
			   :duration dur :scaler amp))
	   (frqf (make-env '(0.000 0.195 0.019 0.246 0.042 0.294 0.066 0.308 0.093 0.294 0.122 0.254 0.202 0.246 
			     0.554 0.237 0.604 0.206 0.643 0.175 0.673 0.184 0.707 0.206 0.743 0.220 0.791 0.201 
			     0.832 0.167 0.856 0.198 0.879 0.212 0.918 0.234 0.942 0.209 0.960 0.186 1.000 0.158)
			   :duration dur :scaler (hz->radians 10000.0)))
	   (gen1 (make-polyshape 0.0 :partials (list 1 .8  2 .01 3 .05  4 .01  5 .005)))
	   (gen2 (make-polyshape 0.0 :partials (list 2 .01 4 .7  5 .01 6 .01 7 .01 8 .05 9 .01  10 .01 11 .01  12 .005 13 .01 14 .10 15 .10 16 .01)))
	   (frqf2 (make-env '(0.000 0.345 0.052 0.393 0.098 0.345 0.271 0.311 0.443 0.305 0.506 0.319 0.559 0.339 
			      0.613 0.322 0.661 0.294 0.705 0.314 0.738 0.353 0.769 0.364 0.797 0.356 0.818 0.316 
			      0.845 0.285 0.882 0.339 0.905 0.362 0.928 0.364 0.960 0.319 1.000 0.209)
			    :duration dur :scaler (hz->radians 2500.0)))
	   (ampf2 (make-env '(0 0 .1 0 .2 1 1 1) :scaler .3 :duration dur)))
      (run
       (lambda ()
	 (do ((i start (1+ i)))
	     ((= i stop))
	   (outa i (* (env ampf)
		      (+ (polyshape gen1 1.0 (env frqf))
			 (* (env ampf2)
			    (polyshape gen2 1.0 (env frqf2)))))
		 *output*))))))

  (eastern-bluebird-1 beg1 amp1)
  (eastern-bluebird-2 (+ beg1 0.33) amp1))

;(with-sound (:play #t) (eastern-bluebird 0 .5))


;;; --------------------------------------------------------------------------------
;;;
;;; Evening grosbeak

(definstrument (evening-grosbeak beg amp)
  ;; east 98 7.5
  (let* ((start (seconds->samples beg))
	 (dur 0.17)
	 (stop (+ start (seconds->samples dur)))
	 (ampf (make-env '(0.000 0.000 0.050 0.121 0.068 0.000 0.081 0.000 0.097 0.329 0.109 0.000 0.125 0.451 
			   0.135 0.344 0.144 0.462 0.162 0.000 0.171 0.058 0.177 0.688 0.188 0.558 0.208 0.477 
			   0.220 0.283 0.246 0.373 0.265 0.373 0.277 0.566 0.303 0.775 0.328 0.824 0.360 0.711 
			   0.383 0.838 0.405 0.835 0.413 0.711 0.443 0.939 0.514 0.948 0.576 1.000 0.626 0.942 
			   0.658 0.951 0.707 0.864 0.742 0.728 0.777 0.682 0.820 0.610 0.926 0.260 0.949 0.072 
			   0.978 0.107 1.000 0.000)
			 :duration dur :scaler amp))
	 (frqf (make-env '(0.000 0.196 0.021 0.259 0.038 0.355 0.055 0.322 0.066 0.276 0.096 0.255 0.116 0.287 
			   0.124 0.371 0.134 0.390 0.146 0.325 0.158 0.371 0.181 0.386 0.203 0.397 0.227 0.449 
			   0.262 0.451 0.283 0.437 0.465 0.388 0.599 0.374 0.701 0.350 0.834 0.327 1.000 0.243)
			 :duration dur :scaler (hz->radians 10200.0)))
	 (gen1 (make-polyshape 0.0 :partials (list 1 .97  2 .01  3 .015  4 .004)))
	 (vib (make-oscil 300))
	 (vibf (make-env '(0 0 .25 0 .3 1 1 1) :duration dur :scaler (hz->radians 200)))
	 (emph (make-oscil 0.0))
	 (emphf (make-env '(0 0  .3 1 .4 0 1 0) :duration dur :scaler .2)))
    (run
     (lambda ()
       (do ((i start (1+ i)))
	   ((= i stop))
	 (let ((frq (+ (env frqf)
		       (* (env vibf)
			  (oscil vib)))))
	   (outa i (* (env ampf)
		      (+ (polyshape gen1 1.0 frq)
			 (* (env emphf)
			    (oscil emph (* 2 frq)))))
		 *output*)))))))

;(with-sound (:play #t) (evening-grosbeak 0 .5))



;;; ================ calling-all-animals ================


(define (calling-all-animals)
  (with-sound (:scaled-to .5 :srate 44100) ;(srate needed by snd-test)
    (ruffed-grouse 0 0.5)
    (mosquito 0 5 560 0.2)
    (mosquito 1.5 3 880 0.05)
    (knudsens-frog 5.506 0.5)
    (oak-toad 6.535 0.3)
    (eastern-wood-pewee-1 7.5 0.25)
    (eastern-wood-pewee-2 9.5 0.25)
    (broad-winged-tree-cricket 10.87 2.0 0.2)
    (southern-cricket-frog 13.225 0.5)
    (long-spurred-meadow-katydid 13.5 0.5)
    (northern-leopard-frog 15.0 0.5)
    (southern-mole-cricket 16.5 3 0.15)
    (green-tree-frog 19.5 0.5)
    (spring-peeper 21.0 0.5)
    (crawfish-frog 22.5 0.5)
    (river-frog 24.0 0.5)
    (indri 26.224 0.25)
    (field-sparrow 27.677 0.25)
    (handsome-trig 30.747 2 0.5)
    (fast-calling-tree-cricket 32.951 2 0.25)
    (dog-day-cicada 34.861 2 0.1)
    (linnaeus-cicada 36.890 2 0.125)
    (lyric-cicada 39.0 2 0.125)
    (confused-ground-cricket 41.292 2 0.3)
    (tinkling-ground-cricket 43.521 2 0.3)
    (marsh-meadow-grasshopper 45.650 0.3)
    (striped-ground-cricket 50.584 2 0.25)
    (sphagnum-ground-cricket 52.688 2 0.3)
    (fox-sparrow 54.908 3 0.25)
    (southeastern-field-cricket 57.914 2 0.13)
    (snowy-tree-cricket 60.243 2.1 0.3)
    (slightly-musical-conehead 62.748 2 0.4)
    (white-throated-sparrow 65.227 0.25)
    (tufted-titmouse 69.369 0.3)
    (savannah-sparrow 71.072 0.5)
    (chipping-sparrow 74.478 0.3)
    (pine-tree-cricket 76.482 2 0.125)
    (davis-tree-cricket 79.012 2 0.125)
    (carolina-grasshopper 81.517 1.5 1.0)
    (pinewoods-tree-frog 83.345 1.5 0.15)
    (henslows-sparrow 85.399 0.5)
    (least-flycatcher 86.376 0.5)
    (acadian-flycatcher 86.826 0.25)
    (swainsons-thrush 87.677 0.25)
    (squirrel-tree-frog-1 90.007 1 0.1)
    (carolina-wren 91.334 0.25)
    (ornate-chorus-frog 93.839 2 0.1)
    (bachmans-sparrow 96.143 0.25)
    (grasshopper-sparrow 98.848 0.25)
    (american-robin 100.977 0.25)
    (common-loon-1 103.457 0.125)
    (common-loon-2 106.663 0.125)
    (hermit-thrush 107.990 0.25)
    (chuck-wills-widow 109.969 0.25)
    (california-towhee 111.822 0.25)
    (black-chinned-sparrow 113.594 0.25 #t)
    (mourning-dove 117.0 0.125)
    (bobwhite 121.36 0.25)
    (warbling-vireo 123.478 0.25)
    (great-horned-owl 126.745 0.25)
    (western-tanager 129.795 0.2)
    (pileated-woodpecker 132.494 0.125)
    (whip-poor-will 135.274 0.25)
    (varied-thrush 136.602 0.125)
    (nashville-warbler 137.929 0.25)
    (plumbeous-vireo-1 140.534 0.25)
    (american-crow 141.511 0.5)
    (least-bittern 142.413 0.5)
    (orange-crowned-warbler 144.191 0.25)
    (loggerhead-shrike-1 146.295 0.1)
    (loggerhead-shrike-2 147.431 0.1)
    (california-quail 148.351 0.25)
    (vermillion-flycatcher 149.628 0.25)
    (cardinal 150.705 0.2)
    (black-phoebe 154.538 0.25)
    (yellow-warbler 155.299 0.25)
    (barred-owl-1 157.443 0.25)
    (says-phoebe 159.347 0.25)
    (yellow-rumped-warbler 160.599 0.25)
    (purple-finch 162.853 0.25)
    (bullfrog 165.909 0.125)
    (northern-goshawk 167.437 0.125)
    (common-gull 168.489 0.25)
    (ash-throated-flycatcher 169.440 0.25)
    (white-headed-woodpecker 170.728 0.2)
    (phainopepla 171.454 0.5)
    (golden-crowned-sparrow 172.272 0.25)
    (house-finch 174.902 0.25)
    (ruby-crowned-kinglet 178.918 0.25)
    (green-tailed-towhee 181.539 0.25)
    (white-faced-ibis 183.869 0.25)
    (lucys-warbler 184.670 0.25)
    (cassins-vireo 186.849 0.25)
    (texas-toad 187.951 2.0 0.125)
    (plain-chacalaca 190.381 0.5)
    (black-billed-cuckoo 191.408 0.25)
    (eared-grebe 192.560 0.25)
    (brown-jay 193.912 0.5)
    (blue-grosbeak 194.664 0.25)
    (acorn-woodpecker 197.594 0.5)
    (american-toad 199.848 3 0.25)
    (red-shouldered-hawk 203.706 0.5)
    (lesser-nighthawk 204.808 2 0.25)
    (olive-sided-flycatcher 207.262 0.125)
    (common-yellowthroat 208.983 0.25)
    (cassins-sparrow 211.087 0.25)
    (stellers-jay 213.641 0.25)
    (black-rail 215.119 0.25)
    (pinyon-jay 216.397 0.25)
    (sora 217.449 0.25)
    (killdeer 218.225 0.25)
    (oak-titmouse 219.703 0.25)
    (macgillivrays-warbler 221 0.25)
    (huttons-vireo 222 0.25)
    (plains-spadefoot 222.740 0.25)
    (western-meadowlark 223.967 0.25)
    (northern-beardless-tyrannulet 225.996 0.25)
    (scotts-oriole 228.275 0.25)
    (wilsons-warbler 230.279 0.25)
    (barking-tree-frog 232.508 0.25)
    (western-toad 233 2 0.25)
    (southwestern-toad 235.188 2 0.25)
    (willow-flycatcher 237.643 0.25)
    (black-necked-stilt 238.594 0.25)
    (bushtit 239.146 0.25)
    (red-breasted-nuthatch 240 0.25)
    (white-breasted-nuthatch 240.5 0.25)
    (pygmy-nuthatch 241 0.25)
    (flammulated-owl 241.600 0.25)
    (song-sparrow 242.301 0.25)
    (burrowing-owl 245 .25)
    (gray-vireo-1 246 .25)
    (gray-vireo-2 246.5 .25)
    (bald-eagle 247 .25)
    (eastern-meadowlark 248 .25)
    (plumbeous-vireo-2 250 .25)
    (yellow-green-vireo 251 .25)
    (great-plains-narrow-mouthed-toad 252 2 .25)
    (magnolia-warbler 254.5 .25)
    (eastern-bluebird 256 .25)
    (evening-grosbeak 257 .25)
    ))

