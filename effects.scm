;;; add an effects menu, as per request of Dave Phillips
;;;
;;; Has: reverse
;;;      normalize (normalization)
;;;      gain (gain-amount)
;;;      invert
;;;      chordalize
;;;      flange (increase speed and amount to get phasing)
;;;      compand
;;;      reverberate (reverb-amount)
;;;      intensify (contrast-amount)
;;;      echo (echo-length, echo-amount)
;;;      trim front and trim back (to/from marks)
;;;      crop (first and last marks)
;;;      squelch (squelch-amount)
;;;
;;; These follow sync lists starting from current chan
;;;
;;; TODO:
;;;      delays
;;;      filters & EQs
;;;      distortion fx -- is this compand?
;;;      chorus
;;;      noise reduction -- how?
;;;      cut/copy to new: save-selection cut open-sound
;;;      increase/decrease file length -- meaning src or expand?
;;;      append (selection?)
;;;      mix/crossfade
;;;      add silence at start/end or at mark
;;;
;;; get the args from the minibuffer (with defaults), also report somewhere


(define effects-menu (add-to-main-menu "Effects"))


;;; -------- reverse
(add-to-menu effects-menu "reverse" (lambda () (reverse-sound)))


;;; -------- normalize (peak set by normalize-amount)
(define normalization 1.0)
(add-to-menu effects-menu "normalize" (lambda () (scale-to normalization)))


;;; -------- invert
(add-to-menu effects-menu "invert" (lambda () (scale-by -1)))


;;; -------- gain (gain set by gain-amount)
(define gain-amount 1.0)
(add-to-menu effects-menu "gain" (lambda () (scale-by gain-amount)))


;;; -------- chordalize (comb filters to make a chord using chordalize-amount and chordalize-base)
(define chordalize-amount .95)
(define chordalize-base 100)
(define chordalize-chord '(1 3/4 5/4))

(define (chordalize)
  ;; chord is a list of members of chord such as '(1 5/4 3/2)
  (let ((combs (map (lambda (interval)
		      (make-comb chordalize-amount (* chordalize-base interval)))
		    chordalize-chord))
	(scaler (/ 0.5 (length chordalize-chord)))) ; just a guess -- maybe this should rescale to old maxamp
    (lambda (x)
      (* scaler (apply + (map (lambda (c) (comb c x)) combs))))))

(add-to-menu effects-menu "chordalize" (lambda () (map-chan-with-sync (chordalize) "chordalize")))


;;; -------- compand
(define vct (lambda args (list->vct args)))

(define (compand)
  "compand distorts a sound"
  (let* ((tbl (vct -1.000 -0.960 -0.900 -0.820 -0.720 -0.600 -0.450 -0.250 
		   0.000 0.250 0.450 0.600 0.720 0.820 0.900 0.960 1.000)))
    ;; (we're eye-balling the curve on p55 of Steiglitz's "a DSP Primer")
    (lambda (inval)
      (let ((index (+ 8.0 (* 8.0 inval))))
	(array-interp tbl index 17)))))

(define all-chans
  (lambda ()
    (let ((sndlist '())
	  (chnlist '()))
      (map (lambda (snd)
	     (do ((i (1- (channels snd)) (1- i)))
		 ((< i 0))
	       (set! sndlist (cons snd sndlist))
	       (set! chnlist (cons i chnlist))))
	   (sounds))
      (list sndlist chnlist))))

(define map-chan-with-sync
  (lambda (func origin)
    (let ((snc (syncing)))
      (if (> snc 0)
	  (apply map
		 (lambda (snd chn)
		   (if (= (syncing snd) snc)
		       (map-chan func #f #f origin snd chn)))
		 (all-chans))
	  (map-chan func #f #f origin)))))

(add-to-menu effects-menu "compand" (lambda () (map-chan-with-sync (compand) "compand")))


;;; -------- reverberate (reverberation set by reverb-amount)
(define reverb-amount .05)

(define (reverberate)
  "reverberate adds reverberation scaled by reverb-amount"
  (save-control-panel)
  (reset-control-panel)
  (set! (reverbing) #t)
  (set! (reverb-scale) reverb-amount)
  (call-apply)
  (restore-control-panel))

(add-to-menu effects-menu "reverberate" reverberate)


;;; -------- intensify (contrast-enhancement set by contrast-amount)
(define contrast-amount 1.0)

(define (intensify)
  (let ((peak (maxamp)))
    (save-control-panel)
    (reset-control-panel)
    (set! (contrasting) #t)
    (set! (contrast) contrast-amount)
    (set! (contrast-amp) (/ 1.0 peak))
    (set! (amp) peak)
    (call-apply)
    (restore-control-panel)))

(add-to-menu effects-menu "intensify" intensify)


;;; -------- echo (controlled by echo-length and echo-amount)
(define echo-length .5) ; i.e. delay between echoes
(define echo-amount .2)

(define (echo)
  "echo adds echos spaced by echo-length seconds and scaled by echo-amount"
  (let ((del (make-delay (round (* echo-length (srate))))))
    (lambda (inval)
      (+ inval (delay del (* echo-amount (+ (tap del) inval)))))))

(add-to-menu effects-menu "echo" (lambda () (map-chan-with-sync (echo) "echo")))


;;; -------- flange (and phasing)
(define flange-speed 2.0)
(define flange-amount 5.0)
(define flange-time 0.001)

(define (flange) ; increase speed and amount to get phaser
  (let* ((ri (make-rand-interp :frequency flange-speed :amplitude flange-amount))
	 (len (round (* flange-time (srate))))
	 (del (make-delay len :max-size (+ len flange-amount 1))))
    (lambda (inval)
      (* .75 (+ inval 
	       (delay del 
		      inval
		      (rand-interp ri)))))))

(add-to-menu effects-menu "flange" (lambda () (map-chan-with-sync (flange) "flange")))


;;; -------- trim from and back (goes by first or last mark)
(define (trim-front)
  "trim-front finds the first mark in each of the syncd channels and removes all samples before it"
  (let ((snc (syncing)))
    (define (trim-front-one-channel snd chn)
      (if (< (length (marks snd chn)) 1)
	  (report-in-minibuffer "trim-front needs a mark" snd)
	  (delete-samples 0 (mark-sample (car (marks snd chn))) snd chn)))
    (if (> snc 0)
	(apply map
	       (lambda (snd chn)
		 (if (= (syncing snd) snc)
		     (trim-front-one-channel snd chn)))
	       (all-chans))
	(trim-front-one-channel (selected-sound) (selected-channel)))))

(add-to-menu effects-menu "trim front" trim-front)

(define (trim-back)
  "trim-back finds the last mark in each of the syncd channels and removes all samples after it"
  (let ((snc (syncing)))
    (define (trim-back-one-channel snd chn)
      (if (< (length (marks snd chn)) 1)
	  (report-in-minibuffer "trim-back needs a mark" snd)
	  (let ((endpt (mark-sample (car (reverse (marks snd chn))))))
	    (delete-samples (+ endpt 1) (- (frames snd chn) endpt)))))
    (if (> snc 0)
	(apply map
	       (lambda (snd chn)
		 (if (= (syncing snd) snc)
		     (trim-back-one-channel snd chn)))
	       (all-chans))
	(trim-back-one-channel (selected-sound) (selected-channel)))))

(add-to-menu effects-menu "trim back" trim-back)


;;; -------- crop (trims front and back)
(define (crop)
  "crop finds the first and last marks in each of the syncd channels and removes all samples outside them"
  (let ((snc (syncing)))
    (define (crop-one-channel snd chn)
      (if (< (length (marks snd chn)) 2)
	  (report-in-minibuffer "crop needs start and end marks" snd)
	  (as-one-edit
	   (lambda ()
	     (delete-samples 0 (mark-sample (car (marks snd chn))) snd chn)
	     (let ((endpt (mark-sample (car (reverse (marks snd chn))))))
	       (delete-samples (+ endpt 1) (- (frames snd chn) endpt)))))))
    (if (> snc 0)
	(apply map
	       (lambda (snd chn)
		 (if (= (syncing snd) snc)
		     (crop-one-channel snd chn)))
	       (all-chans))
	(crop-one-channel (selected-sound) (selected-channel)))))

(add-to-menu effects-menu "crop" crop)


;;; squelch (silencer set by squelch-amount -- this is a kind of "gate" in music-dsp-jargon)
(define (squelch-one-channel silence snd chn)
  (let* ((buffer-size 128)
	 (buffer0 #f)
	 (tmp #f)
	 (sum0 0.0)
	 (buffer1 (make-vct buffer-size))
	 (chan-samples (frames snd chn))
	 (pad-samples (+ chan-samples buffer-size))
	 (tempfilename (snd-tempnam))
	 (new-file (open-sound-file tempfilename 1 (srate snd)))
	 (reader (make-sample-reader 0 snd chn)))
    (do ((i 0 (+ i buffer-size)))
	((>= i pad-samples))
      (let ((sum 0.0))
	(do ((j 0 (+ j 1)))
	    ((= j buffer-size))
	  (let ((val (next-sample reader)))
	    (vct-set! buffer1 j val)
	    (set! sum (+ sum (* val val)))))
	(if buffer0
	    (begin
	      (if (> sum silence)
		  (if (<= sum0 silence)
		      (do ((j 0 (+ j 1))
			   (incr 0.0 (+ incr (/ 1.0 buffer-size))))
			  ((= j buffer-size))
			(vct-set! buffer0 j (* (vct-ref buffer0 j) incr))))
		  (if (<= sum0 silence)
		      (vct-fill! buffer0 0.0)
		      (do ((j 0 (+ j 1))
			   (incr 1.0 (- incr (/ 1.0 buffer-size))))
			  ((= j buffer-size))
			(vct-set! buffer0 j (* (vct-ref buffer0 j) incr)))))
	      (vct->sound-file new-file buffer0 buffer-size))
	    (set! buffer0 (make-vct buffer-size)))
	(set! tmp buffer0)
	(set! buffer0 buffer1)
	(set! buffer1 tmp)
	(set! sum0 sum)))
    (free-sample-reader reader)
    (close-sound-file new-file (* chan-samples 4))
    (set! (samples 0 chan-samples snd chn) tempfilename)))

(define squelch-amount .07)

(define (squelch)
  (let ((snc (syncing)))
    (if (> snc 0)
	(apply map
	       (lambda (snd chn)
		 (if (= (syncing snd) snc)
		     (squelch-one-channel squelch-amount snd chn)))
	       (all-chans))
	(squelch-one-channel squelch-amount (selected-sound) (selected-channel)))))

(add-to-menu effects-menu "squelch" squelch)


