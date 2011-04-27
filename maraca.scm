;;; Perry Cook's maraca from CMJ vol 21 no 3 (Fall 97) p 44
;;;   translated from CLM's maraca.ins

(provide 'snd-maraca.scm)
(if (not (provided? 'snd-ws.scm)) (load "ws.scm"))

(define two-pi (* 2 pi))

(definstrument (maraca beg dur (amp .1) 
		 (sound-decay 0.95) 
		 (system-decay 0.999) 
		 (probability .0625)
		 (shell-freq 3200.0)
		 (shell-reso 0.96))
  (let* ((st (seconds->samples beg))
	 (nd (+ st (seconds->samples dur)))
	 (temp 0.0)
	 (shake-energy 0.0)
	 (snd-level 0.0)
	 (input 0.0)
	 (output (make-vct 2))
	 (coeffs (make-vct 2))
	 (num-beans 64)
	 (j 0)
	 (sndamp (/ amp 16384.0))
	 (srate4 (floor (/ (mus-srate) 4)))
	 (gain (/ (* (/ (log num-beans) (log 4.0)) 40) num-beans)))
    ;; gourd resonance filter
    (set! (coeffs 0) (* -2.0 shell-reso (cos (hz->radians shell-freq))))
    (set! (coeffs 1) (* shell-reso shell-reso))

    (run
     (do ((i st (+ 1 i)))
	 ((= i nd))
       (if (< temp two-pi)
	   (begin
	     ;; shake over 50msec and add shake energy
	     (set! temp (+ temp (hz->radians 20)))
	     (set! shake-energy (+ shake-energy (- 1.0 (cos temp))))))
       (if (= j srate4)		;shake 4 times/sec
	   (begin
	     (set! temp 0.0)
	     (set! j 0)))
       (set! j (+ 1 j))
       (set! shake-energy (* shake-energy system-decay))
       ;; if collision, add energy
       (if (< (random 1.0) probability)
	   (set! snd-level (+ snd-level (* gain shake-energy))))
       ;; actual sound is random
       (set! input (* snd-level (- (random 2.0) 1.0)))
       ;; compute exponential sound decay
       (set! snd-level (* snd-level sound-decay))
       ;; gourd resonance filter calc
       (set! input (- input 
		      (* (output 0) (coeffs 0)) 
		      (* (output 1) (coeffs 1))))
       (set! (output 1) (output 0))
       (set! (output 0) input)
       ;; extra zero for spectral shape, also fixup amp since Perry is assuming maxamp 16384
       (outa i (* sndamp (- (output 0) (output 1))))))))

;;; maraca: (maraca 0 5 .5)
;;; cabasa: (maraca 0 5 .5 0.95 0.997 0.5 3000.0 0.7)

(definstrument (big-maraca beg dur (amp .1) 
			   (sound-decay 0.95) 
			   (system-decay 0.999) 
			   (probability .0625)
			   (shell-freqs '(3200.0))
			   (shell-resos '(0.96))
			   (randiff .01)
			   (with-filters #t))
  ;; like maraca, but takes a list of resonances and includes low-pass filter (or no filter)			   
  (let* ((st (seconds->samples beg))
	 (nd (+ st (seconds->samples dur)))
	 (temp 0.0)
	 (temp1 0.0)
	 (resn (length shell-freqs))
	 (shake-energy 0.0)
	 (snd-level 0.0)
	 (input 0.0)
	 (sum 0.0)
	 (last-sum 0.0)
	 (last-diff 0.0)
	 (diff 0.0)
	 (output (make-vct (* resn 2)))
	 (coeffs (make-vct (* resn 2)))
	 (basesf (make-vct resn))
	 (num-beans 64)
	 (j 0)
	 (sndamp (/ amp (* 16384.0 resn)))
	 (srate4 (floor (/ (mus-srate) 4)))
	 (gain (/ (* (/ (log num-beans) (log 4)) 40) num-beans)))
    ;; gourd resonance filters
    (do ((i 0 (+ 1 i)))
	((= i resn))
      (set! (coeffs    (* i 2)   ) (* -2.0 (shell-resos i) (cos (hz->radians (shell-freqs i)))))
      (set! (basesf i) (coeffs (+ (* i 2) 0)))
      (set! (coeffs (+ (* i 2) 1)) (* (shell-resos i) (shell-resos i))))

    (run
     (do ((i st (+ 1 i)))
	 ((= i nd))
       (if (< temp two-pi)
	   (begin
	     ;; shake over 50msec and add shake energy
	     (set! temp (+ temp (hz->radians 20.0)))
	     (set! shake-energy (+ shake-energy (- 1.0 (cos temp))))))
       (if (= j srate4)		;shake 4 times/sec
	   (begin
	     (set! temp 0.0)
	     (set! j 0)))
       (set! j (+ 1 j))
       (set! shake-energy (* shake-energy system-decay))
       ;; if collision, add energy
       (if (< (random 1.0) probability)
	   (begin
	     (set! snd-level (+ snd-level (* gain shake-energy)))
	     ;; randomize res freqs a bit
	     (do ((i 0 (+ 1 i)))
		 ((= i resn))
	       (set! (coeffs (* i 2)) (+ (basesf i) (- (random (* 2.0 randiff)) randiff))))))
       ;; actual sound is random
       (set! input (* snd-level (- (random 2.0) 1.0)))
       ;; compute exponential sound decay
       (set! snd-level (* snd-level sound-decay))
       ;; gourd resonance filter calcs
       (set! temp1 input)
       (set! last-sum sum)
       (set! sum 0.0)
       (do ((i 0 (+ 1 i)))
	   ((= i resn))
	 (set! input temp1)
	 (set! input (- input 
			(* (output (+ (* i 2) 0)) (coeffs (+ (* i 2) 0)))
			(* (output (+ (* i 2) 1)) (coeffs (+ (* i 2) 1)))))
	 (set! (output (+ (* i 2) 1)) (output (+ (* i 2) 0)))
	 (set! (output    (* i 2)   ) input)
	 (set! sum (+ sum input)))
       (if with-filters
	   (begin
	     (set! last-diff diff)
	     (set! diff (- sum last-sum))
	     (set! temp1 (+ last-diff diff)))
	   (set! temp1 sum))
       ;; extra zero for spectral shape, also fixup amp since Perry is assuming maxamp 16384
       (outa i (* sndamp temp1))))))

;;; tambourine: (big-maraca 0 1 .25 0.95 0.9985 .03125 '(2300 5600 8100) '(0.96 0.995 0.995) .01)
;;; sleighbells: (big-maraca 0 2 .15 0.97 0.9994 0.03125 '(2500 5300 6500 8300 9800) '(0.999 0.999 0.999 0.999 0.999))
;;; sekere: (big-maraca 0 2 .5 0.96 0.999 .0625 '(5500) '(0.6))
;;; windchimes: (big-maraca 0 2 .5 0.99995 0.95 .001 '(2200 2800 3400) '(0.995 0.995 0.995) .01 #f)

