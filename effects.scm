;;; add an effects menu, as per request of Dave Phillips
;;;
;;; Has: reverse
;;;      normalize (normalization)
;;;      gain (gain-amount)
;;;      invert
;;;      chordalize (chordalize-amount, chordalize-base)
;;;      flange (increase speed and amount to get phasing, flange-speed, flange-amount, flange-time)
;;;      compand
;;;      reverberate (reverb-amount)
;;;      intensify (contrast-amount)
;;;      echo (echo-length, echo-amount)
;;;      trim front and trim back (to/from marks)
;;;      crop (first and last marks)
;;;      squelch (squelch-amount, omit-silence)
;;;      selection->new
;;;      cut selection->new
;;;      add silence (at cursor) (silence-amount)
;;;      append selection (and append sound)
;;;      remove DC
;;;      expsrc (independent pitch/time scaling) (time-scale and pitch-scale)
;;;      notch filter (notch-freq and notch-bw)
;;;
;;; These follow sync lists starting from current chan
;;;
;;; TODO filters & EQs
;;;      ISO center freqs for a ten band EQ are (reported to be) (16), 31.5, 63, 125, 250, 500, 1000, 2000, 4000, 8000, 16000
;;;      this is kinda pointless -- the filter display in the control panel can have any number of "bands" which can be changed in "real-time".
;;; TODO chorus (see below -- it works on some files)
;;; TODO noise reduction -- how?
;;; TODO mix/crossfade
;;; TODO phase-vocoder time/pitch
;;; TODO unvoice?
;;; TODO  for some of these, we should write C modules, loaded when this file is loaded -- see grfsnd.html (to speed up flanging etc)
;;;
;;; to modify, for example, the gain (set! gain-amount .75)


(use-modules (ice-9 format))

(define pi 3.141592653589793)
(define effects-list '()) ; menu labels are updated to show current default settings

(define effects-menu (add-to-main-menu "Effects" (lambda ()
						   (define (update-label effects)
						     (if (not (null? effects))
							 (begin
							   ((car effects))
							   (update-label (cdr effects)))))
						   (update-label effects-list))))
(define (all-chans)
  (let ((sndlist '())
	(chnlist '()))
    (for-each (lambda (snd)
		(do ((i (1- (channels snd)) (1- i)))
		    ((< i 0))
		  (set! sndlist (cons snd sndlist))
		  (set! chnlist (cons i chnlist))))
	      (sounds))
    (list sndlist chnlist)))

(define map-chan-with-sync
  (lambda (func origin)
    (let ((snc (sync)))
      (if (> snc 0)
	  (apply map
		 (lambda (snd chn)
		   (if (= (sync snd) snc)
		       (map-chan (func) #f #f origin snd chn)))
		 (all-chans))
	  (map-chan (func) #f #f origin)))))


;;; -------- trim from and back (goes by first or last mark)
(define (trim-front)
  "trim-front finds the first mark in each of the syncd channels and removes all samples before it"
  (let ((snc (sync)))
    (define (trim-front-one-channel snd chn)
      (if (< (length (marks snd chn)) 1)
	  (report-in-minibuffer "trim-front needs a mark" snd)
	  (delete-samples 0 (mark-sample (car (marks snd chn))) snd chn)))
    (if (> snc 0)
	(apply map
	       (lambda (snd chn)
		 (if (= (sync snd) snc)
		     (trim-front-one-channel snd chn)))
	       (all-chans))
	(trim-front-one-channel (selected-sound) (selected-channel)))))

(add-to-menu effects-menu "Trim front" trim-front)

(define (trim-back)
  "trim-back finds the last mark in each of the syncd channels and removes all samples after it"
  (let ((snc (sync)))
    (define (trim-back-one-channel snd chn)
      (if (< (length (marks snd chn)) 1)
	  (report-in-minibuffer "trim-back needs a mark" snd)
	  (let ((endpt (mark-sample (car (reverse (marks snd chn))))))
	    (delete-samples (+ endpt 1) (- (frames snd chn) endpt)))))
    (if (> snc 0)
	(apply map
	       (lambda (snd chn)
		 (if (= (sync snd) snc)
		     (trim-back-one-channel snd chn)))
	       (all-chans))
	(trim-back-one-channel (selected-sound) (selected-channel)))))

(add-to-menu effects-menu "Trim back" trim-back)


;;; -------- crop (trims front and back)
(define (crop)
  "crop finds the first and last marks in each of the syncd channels and removes all samples outside them"
  (let ((snc (sync)))
    (define (crop-one-channel snd chn)
      (if (< (length (marks snd chn)) 2)
	  (report-in-minibuffer "crop needs start and end marks" snd)
	  (as-one-edit
	   (lambda ()
	     (delete-samples 0 (mark-sample (car (marks snd chn))) snd chn)
	     (let ((endpt (mark-sample (car (reverse (marks snd chn))))))
	       (delete-samples (+ endpt 1) (- (frames snd chn) endpt))))
	   "crop")))
    (if (> snc 0)
	(apply map
	       (lambda (snd chn)
		 (if (= (sync snd) snc)
		     (crop-one-channel snd chn)))
	       (all-chans))
	(crop-one-channel (selected-sound) (selected-channel)))))

(add-to-menu effects-menu "Crop" crop)


;;; -------- selection -> new file

(define selctr 0)

(define (selection->new)
  (if (selection?)
      (let ((new-file-name (format #f "sel-~D.snd" selctr)))
	(set! selctr (+ selctr 1))
	(save-selection new-file-name)
	(open-sound new-file-name))))

(add-to-menu effects-menu "Selection->new" selection->new)


;;; -------- cut selection -> new file

(define (cut-selection->new)
  (if (selection?)
      (let ((new-file-name (format #f "sel-~D.snd" selctr)))
	(set! selctr (+ selctr 1))
	(save-selection new-file-name)
	(delete-selection)
	(open-sound new-file-name))))

(add-to-menu effects-menu "Cut selection->new" cut-selection->new)


;;; -------- insert silence (at cursor, silence-amount in secs)
(define silence-amount .1)
(define silence-label "Add silence")

(add-to-menu effects-menu silence-label (lambda () 
					  (insert-silence (cursor)
							  (inexact->exact (* (srate) silence-amount)))))

(set! effects-list (cons (lambda ()
			   (let ((new-label (format #f "Add-silence (~1,2F)" silence-amount)))
			     (change-menu-label effects-menu silence-label new-label)
			     (set! silence-label new-label)))
			 effects-list))


;;; -------- append sound (and append selection for lafs)

(define (append-sound name)
  ;; appends sound file
  (insert-sound name (frames)))

(define (append-selection)
  (if (selection?)
      (insert-selection (frames))))

(add-to-menu effects-menu "Append selection" append-selection)


(add-to-menu effects-menu #f #f) ; separator


;;; -------- normalize (peak set by normalize-amount)
(define normalization 1.0)
(define normalize-label "Normalize")

(define normalize-menu (add-to-menu  effects-menu normalize-label (lambda () (scale-to normalization))))

(set! effects-list (cons (lambda ()
			   ;; update menu label to show current normalization
			   (let ((new-label (format #f "Normalize (~1,2F)" normalization)))
			   (change-menu-label effects-menu normalize-label new-label)
			   (set! normalize-label new-label)))
			 effects-list))



;;; -------- invert
(add-to-menu effects-menu "Invert" (lambda () (scale-by -1)))


;;; -------- gain (gain set by gain-amount)
(define gain-amount 0.5)
(define gain-label "Gain")

(add-to-menu effects-menu gain-label (lambda () (scale-by gain-amount)))

(set! effects-list (cons (lambda ()
			   (let ((new-label (format #f "Gain (~1,2F)" gain-amount)))
			     (change-menu-label effects-menu gain-label new-label)
			     (set! gain-label new-label)))
			 effects-list))


;;; -------- squelch (silencer set by squelch-amount -- this is a kind of "gate" in music-dsp-jargon)

(define omit-silence #f) ; if #t, the silences will be omitted from the result

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
	    (let ((all-zeros #f))
	      (if (> sum silence)
		  (if (<= sum0 silence)
		      (do ((j 0 (+ j 1))
			   (incr 0.0 (+ incr (/ 1.0 buffer-size))))
			  ((= j buffer-size))
			(vct-set! buffer0 j (* (vct-ref buffer0 j) incr))))
		  (if (<= sum0 silence)
		      (begin
			(vct-fill! buffer0 0.0)
			(set! all-zeros #t))
		      (do ((j 0 (+ j 1))
			   (incr 1.0 (- incr (/ 1.0 buffer-size))))
			  ((= j buffer-size))
			(vct-set! buffer0 j (* (vct-ref buffer0 j) incr)))))
	      (if (not (and omit-silence all-zeros))
		  (vct->sound-file new-file buffer0 buffer-size)))
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
  (let ((snc (sync)))
    (if (> snc 0)
	(apply map
	       (lambda (snd chn)
		 (if (= (sync snd) snc)
		     (squelch-one-channel squelch-amount snd chn)))
	       (all-chans))
	(squelch-one-channel squelch-amount (selected-sound) (selected-channel)))))

(define squelch-label "Squelch")

(add-to-menu effects-menu squelch-label squelch)

(set! effects-list (cons (lambda ()
			   (let ((new-label (format #f "Squelch (~1,2F)" squelch-amount)))
			     (change-menu-label effects-menu squelch-label new-label)
			     (set! squelch-label new-label)))
			 effects-list))


;;; -------- intensify (contrast-enhancement set by contrast-amount)
(define contrast-amount 1.0)
(define contrast-label "Intensify")

(define (intensify)
  (let ((peak (maxamp)))
    (save-controls)
    (reset-controls)
    (set! (contrast-control?) #t)
    (set! (contrast-control) contrast-amount)
    (set! (contrast-control-amp) (/ 1.0 peak))
    (set! (amp-control) peak)
    (apply-controls)
    (restore-controls)))

(add-to-menu effects-menu contrast-label intensify)

(set! effects-list (cons (lambda ()
			   (let ((new-label (format #f "Intensify (~1,2F)" contrast-amount)))
			     (change-menu-label effects-menu contrast-label new-label)
			     (set! contrast-label new-label)))
			 effects-list))




(add-to-menu effects-menu #f #f) ; separator

;;; -------- echo (controlled by echo-length and echo-amount)
(define echo-length .5) ; i.e. delay between echoes
(define echo-amount .2)
(define echo-label "Echo")

(define (cp-echo)
  "echo adds echos spaced by echo-length seconds and scaled by echo-amount"
  (let ((del (make-delay (round (* echo-length (srate))))))
    (lambda (inval)
      (+ inval 
	 (delay del 
		(* echo-amount (+ (tap del) inval)))))))

(add-to-menu effects-menu echo-label (lambda () (map-chan-with-sync (lambda () (cp-echo)) "echo")))

(set! effects-list (cons (lambda ()
			   (let ((new-label (format #f "Echo (~1,2F, ~1,2F)" echo-length echo-amount)))
			     (change-menu-label effects-menu echo-label new-label)
			     (set! echo-label new-label)))
			 effects-list))


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

(add-to-menu effects-menu "Flange" (lambda () (map-chan-with-sync (lambda () (flange)) "flange")))


;;; -------- reverberate (reverberation set by reverb-amount)
(define reverb-amount .05)
(define reverb-label "Reverberate")

(define (reverberate)
  "reverberate adds reverberation scaled by reverb-amount"
  (save-controls)
  (reset-controls)
  (set! (reverb-control?) #t)
  (set! (reverb-control-scale) reverb-amount)
  (apply-controls)
  (restore-controls))

(add-to-menu effects-menu reverb-label reverberate)

(set! effects-list (cons (lambda ()
			   (let ((new-label (format #f "Reverb (~1,2F)" reverb-amount)))
			     (change-menu-label effects-menu reverb-label new-label)
			     (set! reverb-label new-label)))
			 effects-list))


;;; -------- chorus (doesn't always work and needs speedup)
(define chorus-size 5)
(define chorus-time .05)
(define chorus-amount 20.0)
(define chorus-speed 10.0)

(define (chorus)
  (define (make-flanger)
    (let* ((ri (make-rand-interp :frequency chorus-speed :amplitude chorus-amount))
	   (len (inexact->exact (random (* 3.0 chorus-time (srate)))))
	   (gen (make-delay len :max-size (+ len chorus-amount 1))))
      (list gen ri)))
  (define (flanger dly inval)
    (+ inval 
       (delay (car dly)
	      inval
	      (rand-interp (cadr dly)))))
  (let ((dlys (make-vector chorus-size)))
    (do ((i 0 (1+ i)))
	((= i chorus-size))
      (vector-set! dlys i (make-flanger)))
    (lambda (inval)
      (do ((sum 0.0)
	   (i 0 (1+ i)))
	  ((= i chorus-size)
	   (* .25 sum))
	(set! sum (+ sum (flanger (vector-ref dlys i) inval)))))))

;(add-to-menu effects-menu "Chorus" (lambda () (map-chan-with-sync (lambda () (chorus)) "chorus")))


(add-to-menu effects-menu #f #f) ; separator

;;; -------- remove DC (from Perry Cook's physical modeling toolkit)

(define (block-dc)
  (let ((lastx 0.0)
	(lasty 0.0))
    (lambda (inval)
      (set! lasty (+ inval (- (* 0.999 lasty) lastx)))
      (set! lastx inval)
      lasty)))

(add-to-menu effects-menu "Remove DC" (lambda () (map-chan-with-sync (lambda () (block-dc)) "block-dc")))


;;; -------- pitch and time scaling by granular synthesis and sampling rate conversion

(define pitch-scale 1.0)
(define time-scale 1.0)
(define expsrc-label "Expsrc")

(define (cp-expsrc)
  (save-controls)
  (reset-controls)
  (set! (speed-control) pitch-scale)
  (let ((new-time (* pitch-scale time-scale)))
    (if (not (= new-time 1.0))
	(begin
	  (set! (expand-control?) #t)
	  (set! (expand-control) new-time))))
  (apply-controls)
  (restore-controls))
	    
(add-to-menu effects-menu expsrc-label cp-expsrc)

(set! effects-list (cons (lambda ()
			   (let ((new-label (format #f "Expsrc (~1,2F ~1,2F)" pitch-scale time-scale)))
			     (change-menu-label effects-menu expsrc-label new-label)
			     (set! expsrc-label new-label)))
			 effects-list))


;;; -------- notch filter

(define (make-butter-band-reject fq bw)
  (let* ((d  (* 2.0 (cos (/ (* 2.0 pi fq) (srate)))))
	 (c (tan (/ (* pi bw) (srate))))
	 (c1 (/ 1.0 (+ 1.0 c)))
	 (c2 (* (- d) c1))
	 (c3 c1)
	 (c4 c2)
	 (c5 (* (- 1.0 c) c1)))
    (make-filter 3
		 (list->vct (list c1 c2 c3))
		 (list->vct (list 0.0 c4 c5)))))

(define notch-freq 60.0)
(define notch-bw 100.0)
(define notch-label "Notch")

(add-to-menu effects-menu 
	     notch-label
	     (lambda () 
	       (filter-sound (make-butter-band-reject notch-freq notch-bw))))

(set! effects-list (cons (lambda ()
			   (let ((new-label (format #f "Notch (~1,2F ~1,2F)" notch-freq notch-bw)))
			     (change-menu-label effects-menu notch-label new-label)
			     (set! notch-label new-label)))
			 effects-list))

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

(add-to-menu effects-menu "Chordalize" (lambda () (map-chan-with-sync (lambda () (chordalize)) "chordalize")))


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

(add-to-menu effects-menu "Compand" (lambda () (map-chan-with-sync (lambda () (compand)) "compand")))

;;; -------- reverse
(add-to-menu effects-menu "Reverse" (lambda () (reverse-sound)))
			   

