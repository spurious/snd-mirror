;;; backwards compatibility for snd 7

(provide 'snd-snd7.scm)

(define free-mix-sample-reader free-sample-reader)
(define free-track-sample-reader free-sample-reader)
(define (inspect-sample-reader rd) (format #f "~A" rd))

(define enved-exp?
  (make-procedure-with-setter
   (lambda ()
     (= (enved-style envelope-exponential)))
   (lambda (val)
     (set! (enved-style) (if val envelope-exponential envelope-linear)))))

(define enved-active-env enved-envelope)
(define enved-selected-env enved-envelope)
(define filter-control-env filter-control-envelope)
(define filter-waveform-color filter-control-waveform-color)

(define (change-window-property w a v) (set! (window-property w a) v))

(define (recolor-widget w col)
  (if (and (provided? 'xm)
	   (provided? 'snd-motif))
      (XmChangeColor w col)
      (if (and (provided? 'xg)
	       (provided? 'snd-gtk))
	  (gtk_widget_modify_bg w GTK_STATE_NORMAL col))))

(define region-dialog view-regions-dialog)
(define edit-save-as-dialog save-selection-dialog)
(define file-dialog view-files-dialog)
(define region-dialog view-regions-dialog)
(define mix-dialog view-mixes-dialog)
(define track-dialog view-tracks-dialog)
(define file-save-as-dialog save-sound-dialog)


(define (back-or-forth-graph count)
  (let ((curpos 0)
	(cursnd (or (selected-sound) (car (sounds))))
	(curchn (or (selected-channel) 0)))
    (define (all-chans)
      (let ((sndlist '())
	    (chnlist '())
	    (pos 0))
	(for-each (lambda (snd)
		    (let ((chns (chans snd)))
		      (do ((i 0 (1+ i)))
			  ((= i chns))
			(if (and (= snd cursnd)
				 (= i curchn))
			    (set! curpos pos))
			(set! pos (1+ pos))
			(set! sndlist (cons snd sndlist))
			(set! chnlist (cons i chnlist)))))
		  (reverse (sounds)))
	(list (reverse sndlist) (reverse chnlist))))
  (if (not (null? (sounds)))
      (let* ((chanlist (all-chans))
	     (len (length (car chanlist)))
	     (newpos (modulo (+ curpos count) len)))
	(set! (selected-sound) (list-ref (car chanlist) newpos))
	(set! (selected-channel) (list-ref (cadr chanlist) newpos))
	(list (selected-sound) (selected-channel))))))

(define* (forward-graph #:optional (count 1))
  (back-or-forth-graph count))

(define* (backward-graph #:optional (count 1))
  (back-or-forth-graph (- count)))


(define (back-or-forth-mix count snd chn)
  (if (not (= count 0))
      (let ((mx (mixes snd chn)))
	(if (not (null? mx))
	    (let ((len (length mx)))
	      (if (= len 1)
		  (begin
		    (set! (cursor snd chn) (mix-position (car mx)))
		    (car mx))
		  (let ((sorted-mx (sort mx (lambda (a b) (< (mix-position a) (mix-position b)))))
			(pos (cursor snd chn))
			(curpos (if (> count 0) -1 0)))
		    (if (>= pos (mix-position (car sorted-mx)))
			(call-with-current-continuation
			 (lambda (break)
			   (for-each
			    (lambda (m)
			      (if (or (and (> count 0)
					   (< pos (mix-position m)))
				      (and (< count 0)
					   (<= pos (mix-position m))))
				  (break)
				  (set! curpos (1+ curpos))))
			    sorted-mx))))
		    (set! curpos (modulo (+ curpos count) len))
		    (set! (cursor snd chn) (mix-position (list-ref sorted-mx curpos)))
		    (list-ref sorted-mx curpos))))
	    #f))
      #f))
		
(define* (forward-mix #:optional (count 1) snd chn)
  (back-or-forth-mix count (or snd (selected-sound) (car (sounds))) (or chn (selected-channel) 0)))

(define* (backward-mix #:optional (count 1) snd chn)
  (back-or-forth-mix (- count) (or snd (selected-sound) (car (sounds))) (or chn (selected-channel) 0)))


(define (back-or-forth-mark count snd chn)
  (if (not (= count 0))
      (let ((mx (marks snd chn)))
	(if (not (null? mx))
	    (let ((len (length mx)))
	      (if (= len 1)
		  (begin
		    (set! (cursor snd chn) (mark-sample (car mx)))
		    (car mx))
		  (let ((sorted-mx (sort mx (lambda (a b) (< (mark-sample a) (mark-sample b)))))
			(pos (cursor snd chn))
			(curpos (if (> count 0) -1 0)))
		    (if (>= pos (mark-sample (car sorted-mx)))
			(call-with-current-continuation
			 (lambda (break)
			   (for-each
			    (lambda (m)
			      (if (or (and (> count 0)
					   (< pos (mark-sample m)))
				      (and (< count 0)
					   (<= pos (mark-sample m))))
				  (break)
				  (set! curpos (1+ curpos))))
			    sorted-mx))))
		    (set! curpos (modulo (+ curpos count) len))
		    (set! (cursor snd chn) (mark-sample (list-ref sorted-mx curpos)))
		    (list-ref sorted-mx curpos))))
	    #f))
      #f))
		
(define* (forward-mark #:optional (count 1) snd chn)
  (back-or-forth-mark count (or snd (selected-sound) (car (sounds))) (or chn (selected-channel) 0)))

(define* (backward-mark #:optional (count 1) snd chn)
  (back-or-forth-mark (- count) (or snd (selected-sound) (car (sounds))) (or chn (selected-channel) 0)))

(define mus-data-format-bytes-per-sample mus-bytes-per-sample)

(define mus-linear mus-interp-linear)
(define mus-sinusoidal mus-interp-sinusoidal)

(define color-map-black-and-white 0)
(define color-map-gray 1)
(define color-map-hot 2)
(define color-map-cool 3)
(define color-map-bone 4)
(define color-map-copper 5)
(define color-map-pink 6)
(define color-map-jet 7)
(define color-map-prism 8)
(define color-map-autumn 9)
(define color-map-winter 10)
(define color-map-spring 11)
(define color-map-summer 12)
(define color-map-rainbow 13)
(define color-map-flag 14)


(define mus-a0
  (make-procedure-with-setter
   (lambda (gen)
     (mus-xcoeff gen 0))
   (lambda (gen val)
     (set! (mus-xcoeff gen 0) val))))

(define mus-a1
  (make-procedure-with-setter
   (lambda (gen)
     (mus-xcoeff gen 1))
   (lambda (gen val)
     (set! (mus-xcoeff gen 1) val))))

(define mus-a2
  (make-procedure-with-setter
   (lambda (gen)
     (mus-xcoeff gen 2))
   (lambda (gen val)
     (set! (mus-xcoeff gen 2) val))))

(define mus-b1
  (make-procedure-with-setter
   (lambda (gen)
     (mus-ycoeff gen 1))
   (lambda (gen val)
     (set! (mus-ycoeff gen 1) val))))

(define mus-b2
  (make-procedure-with-setter
   (lambda (gen)
     (mus-ycoeff gen 2))
   (lambda (gen val)
     (set! (mus-ycoeff gen 2) val))))

(define pv-amp-increments phase-vocoder-amp-increments)
(define pv-amps phase-vocoder-amps)
(define pv-freqs phase-vocoder-freqs)
(define pv-outctr phase-vocoder-outctr)
(define pv-phase-increments phase-vocoder-phase-increments)
(define pv-phases phase-vocoder-phases)

(define mus-inspect mus-describe)
