;;; add an effects menu, as per request of Dave Phillips
;;;
;;; Has: reverse
;;;      normalize (normalization)
;;;      gain (gain-amount)
;;;      invert
;;;      compand
;;;      reverberate (reverb-amount)
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
;;;      flange
;;;      phaser
;;;      noise reduction -- how?
;;;      cut/copy to new: save-selection cut open-sound
;;;      increase/decrease file length -- meaning src or expand?
;;;      append (selection?)
;;;      mix/crossfade


(define effects-menu (add-to-main-menu "Effects"))

(add-to-menu effects-menu "reverse" (lambda () (reverse-sound)))

(define normalization 1.0)
(add-to-menu effects-menu "normalize" (lambda () (scale-to normalization)))

(add-to-menu effects-menu "invert" (lambda () (scale-by -1)))

(define gain-amount 1.0)
(add-to-menu effects-menu "gain" (lambda () (scale-by gain-amount)))


;;; next functions are taken from examp.scm

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

(define reverb-amount .1)

(define (reverberate)
  "reverberate adds reverberation scaled by reverb-amount"
  (save-control-panel)
  (reset-control-panel)
  (set! (reverbing) #t)
  (set! (reverb-scale) reverb-amount)
  (call-apply)
  (restore-control-panel))

(add-to-menu effects-menu "reverberate" reverberate)

(define echo-length .5)
(define echo-amount .2)

(define (echo)
  "echo adds echos spaced by echo-length seconds and scaled by echo-amount"
  (let ((del (make-delay (round (* echo-length (srate))))))
    (lambda (inval)
      (+ inval (delay del (* echo-amount (+ (tap del) inval)))))))

(add-to-menu effects-menu "echo" (lambda () (map-chan-with-sync (echo) "echo")))


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


(define map-silence
  (lambda (silence replacement)
    (let ((sum-of-squares 0.0)
          (buffer (make-vector 128 0.0))
          (position 0)
          (current-sample 0)
          (chan-samples (frames)))
      (lambda (y)
	(let ((old-y (vector-ref buffer position)))
	  (set! sum-of-squares (- (+ sum-of-squares (* y y)) (* old-y old-y)))
	  (vector-set! buffer position y)
	  (set! position (1+ position))
	  (if (= position 128) (set! position 0))
	  (set! current-sample (1+ current-sample))
	  (if (> sum-of-squares silence)
	      (if (= current-sample chan-samples)
		  ;; at end return trailing samples as long as it looks like sound
		  (let ((temp-buffer (make-vector 128 0.0)))
		    (do ((i 0 (1+ i)))
			((= i 128) temp-buffer)
		      (let ((final-y (vector-ref buffer position)))
			(vector-set! temp-buffer i (if (> sum-of-squares silence) final-y 0.0))
			(set! sum-of-squares (- sum-of-squares (* final-y final-y)))
			(set! position (1+ position))
			(if (= position 128) (set! position 0)))))
		  old-y)
	      replacement))))))

(define squelch-amount .003)
(add-to-menu effects-menu "squelch" (lambda () (map-chan-with-sync (map-silence squelch-amount 0.0) "squelch")))


(define trim-amount .01) ;looks for this as sign of sound

