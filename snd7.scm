;;; backwards compatibility for snd 7

(provide 'snd-snd7.scm)

(use-modules (ice-9 session)) ; for apropos

(define free-mix-sample-reader free-sample-reader)
(define free-track-sample-reader free-sample-reader)
(define (inspect-sample-reader rd) (format #f "~A" rd))

(define enved-exp?
  (make-procedure-with-setter
   (lambda ()
     (= (enved-style) envelope-exponential))
   (lambda (val)
     (set! (enved-style) (if val envelope-exponential envelope-linear)))))

(define enved-active-env enved-envelope)
(define enved-selected-env enved-envelope)
(define filter-control-env filter-control-envelope)
(define filter-waveform-color filter-control-waveform-color)

(define (change-window-property w a v)
  "obsolete way to set a window-property"
  (set! (window-property w a) v))

(define (recolor-widget w col)
  "(recolor-widget w color) tries to redraw the widget 'w' in the given color"
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
  "(back-or-forth-graph count) moves up or down in the channel list by 'count'"
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

(define* (forward-graph :optional (count 1))
  "(forward-graph :optional (count 1) moves forward 'count' channel graphs"
  (back-or-forth-graph count))

(define* (backward-graph :optional (count 1))
  "(backward-graph :optional (count 1) moves backward 'count' channel graphs"
  (back-or-forth-graph (- count)))


(define (back-or-forth-mix count snd chn)
  "(back-or-forth-mix count snd chn) moves the cursor 'count' mixes"
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
		
(define* (forward-mix :optional (count 1) snd chn)
  "(forward-mix :optional (count 1) snd chn) moves the cursor forward 'count' mixes in the given channel"
  (back-or-forth-mix count (or snd (selected-sound) (car (sounds))) (or chn (selected-channel) 0)))

(define* (backward-mix :optional (count 1) snd chn)
  "(backward-mix :optional (count 1) snd chn) moves the cursor backward 'count' mixes in the given channel"
  (back-or-forth-mix (- count) (or snd (selected-sound) (car (sounds))) (or chn (selected-channel) 0)))


(define (back-or-forth-mark count snd chn)
  "(back-or-forth-mark count snd chn) moves the cursor 'count' marks"
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
		
(define* (forward-mark :optional (count 1) snd chn)
  "(forward-mark :optional (count 1) snd chn) moves the cursor forward 'count' marks in the given channel"
  (back-or-forth-mark count (or snd (selected-sound) (car (sounds))) (or chn (selected-channel) 0)))

(define* (backward-mark :optional (count 1) snd chn)
  "(backward-mark :optional (count 1) snd chn) moves the cursor backward 'count' marks in the given channel"
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
     "obsolete way to access mus-xcoeff 0"
     (mus-xcoeff gen 0))
   (lambda (gen val)
     (set! (mus-xcoeff gen 0) val))))

(define mus-a1
  (make-procedure-with-setter
   (lambda (gen)
     "obsolete way to access mus-xcoeff 1"
     (mus-xcoeff gen 1))
   (lambda (gen val)
     (set! (mus-xcoeff gen 1) val))))

(define mus-a2
  (make-procedure-with-setter
   (lambda (gen)
     "obsolete way to access mus-xcoeff 2"
     (mus-xcoeff gen 2))
   (lambda (gen val)
     (set! (mus-xcoeff gen 2) val))))

(define mus-b1
  (make-procedure-with-setter
   (lambda (gen)
     "obsolete way to access mus-ycoeff 1"
     (mus-ycoeff gen 1))
   (lambda (gen val)
     (set! (mus-ycoeff gen 1) val))))

(define mus-b2
  (make-procedure-with-setter
   (lambda (gen)
     "obsolete way to access mus-ycoeff 2"
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

;;; -------- sound-data->list

(define+ (sound-data->list sd)
  "(sound-data->list sd chan) turns a sound-data object's data into a list of lists (one for each channel)"

  (define (sound-data-channel->list sd chan)
    (let ((ls '()))
      (do ((i (1- (sound-data-length sd)) (1- i)))
	  ((< i 0) ls)
	(set! ls (cons (sound-data-ref sd chan i) ls)))))

  (let ((lst '()))
    (do ((i (1- (sound-data-chans sd)) (1- i)))
	((< i 0) lst)
      (set! lst (cons (sound-data-channel->list sd i) lst)))))


(define (vct-convolve! r1 r2)
  "(vct-convolve! r1 r2) is a wrapper for convolution"
  (convolution r1 r2 (vct-length r1)))

(define* (old-map-chan func :optional start end edname snd chn edpos)
  (map-chan (lambda (y)
	      (let ((val (func y)))
		(if (vector? val)
		    (vector->vct val)
		    (if (list? val)
			(apply vct val)
			val))))
	    start end edname snd chn edpos))

(define* (old-map-channel func :optional beg dur snd chn edpos edname)
  "(old-map-channel func :optional beg dur snd chn edpos edname) is a wrapper for map-channel that accepts vector return values"
  (map-channel (lambda (y)
		 (let ((val (func y)))
		   (if (vector? val)
		       (vector->vct val)
		       (if (list? val)
			   (apply vct val)
			   val))))
	       beg dur snd chn edpos edname))

(define* (mus-bank gens amps1 :optional in1 in2)
  "(mus-bank gens amps1 :optional in1 in2) sums a vector of CLM generators ('gens') multiplied by 'amps' (a vct)"
  (let ((len (vector-length gens))
	(sum 0.0)
	(amps (if (vector? amps1) (vector->vct amps1) amps1))
	(inp1 (if (vector? in1) (vector->vct in1) (or in1 #f)))
	(inp2 (if (vector? in2) (vector->vct in2) (or in2 #f))))
    (do ((i 0 (1+ i)))
	((= i len))
      (set! sum (+ sum (* (vct-ref amps i)
			  (mus-run (vector-ref gens i) 
				   (if inp1 (vct-ref inp1 i) 0.0)
				   (if inp2 (vct-ref inp2 i) 0.0))))))
    sum))

(define* (oscil-bank amps1 gens :optional in1 in2)
  "(oscil-bank amps1 gens :optional in1 in2) sums a vector of oscils"
  (let ((len (vector-length gens))
	(sum 0.0)
	(amps (if (vector? amps1) (vector->vct amps1) amps1))
	(inp1 (if (vector? in1) (vector->vct in1) (or in1 #f)))
	(inp2 (if (vector? in2) (vector->vct in2) (or in2 #f))))
    (do ((i 0 (1+ i)))
	((= i len))
      (set! sum (+ sum (* (vct-ref amps i)
			  (oscil (vector-ref gens i) 
				 (if inp1 (vct-ref inp1 i) 0.0)
				 (if inp2 (vct-ref inp2 i) 0.0))))))
    sum))

(define* (old-formant-bank amps gens :optional (in1 0.0))
  "(old-formant-bank amps gens :optional (in1 0.0)) is a wrapper for formant-bank that accepts a vector of amps"
  (formant-bank (if (vector? amps) (vector->vct amps) amps) gens in1))

(define* (vct->samples samp samps data :optional snd chn) 
  "(vct->samples samp samps data :optional snd chn) is an old form of vct->channel"
  (vct->channel data samp samps snd chn))

(define* (samples->vct samp samps :optional snd chn v pos)
  "(samples->vct samp samps :optional snd chn v pos) is an old form of channel->vct"
  (if (not v)
      (channel->vct samp samps snd chn pos)
      (vct-subseq (channel->vct samp samps snd chn pos) 0 samps v)))

(define transform-samples->vct transform->vct)
(define transform-samples-size transform-frames)
(define region-samples->vct region->vct)

(define* (scale-sound-by scl :optional beg dur snd chn edpos)
  "(scale-sound-by scl :optional beg dur snd chn edpos) is an old form of scale-sound"
  (if (integer? chn)
      (scale-channel scl beg dur snd chn edpos)
      (do ((i 0 (1+ i)))
	  ((= i (chans snd)))
	(scale-channel scl beg dur snd i))))

(define* (scale-sound-to norm :optional beg dur snd chn)
  "(scale-sound-to norm :optional beg dur snd chn) is an old form of normalize-sound"
  (if (integer? chn)
      (let ((mx (maxamp snd chn)))
	(if (and (not (= mx 0.0))
		 (not (= mx norm)))
	    (scale-channel (/ norm mx) beg dur snd chn)))
      (let ((mx (apply max (maxamp snd #t))))
	(if (and (not (= mx 0.0))
		 (not (= mx norm)))
	    (do ((i 0 (1+ i)))
		((= i (chans snd)))
	      (scale-channel (/ norm mx) beg dur snd i))))))

(define (after-save-as-hook-replace-sound snd filename from-dialog)
  (if from-dialog
      (begin
	(revert-sound snd) ; avoid close-hook confusion
	(close-sound snd)
	(open-sound filename))))

(define emacs-style-save-as
  (make-procedure-with-setter
   (lambda ()
     (and (not (hook-empty? after-save-as-hook))
	  (member after-save-as-hook-replace-sound (hook->list after-save-as-hook))))
   (lambda (val)
     (if val
	 (if (not (member after-save-as-hook-replace-sound (hook->list after-save-as-hook)))
	     (add-hook! after-save-as-hook after-save-as-hook-replace-sound))
	 (if (member after-save-as-hook-replace-sound (hook->list after-save-as-hook))
	     (remove-hook! after-save-as-hook after-save-as-hook-replace-sound))))))

(define send-netscape send-mozilla)

(if (not (defined? 'in-hz)) (define in-hz hz->radians))
(if (not (defined? 'restart-env)) (define restart-env mus-reset))

(define mixer-scale mixer*)
(define mus-error-to-string mus-error-type->string)
(define save-options save-state)
(define delete-samples-with-origin delete-samples)

(if (not (defined? 'default-output-type)) (define default-output-type default-output-header-type))
(if (not (defined? 'default-output-format)) (define default-output-format default-output-data-format))

(define previous-files-sort view-files-sort)
;; (define previous-files-sort-procedure view-files-sort-procedure)
;; previous-files-select-hook -> view-files-select-hook
;;   but this doesn't actually work -- new version takes 2 args and does not return anything

(define preload-directory add-directory-to-view-files-list)
(define preload-file add-file-to-view-files-list)

(if (not (defined? 'recorder-in-format)) (define recorder-in-format recorder-in-data-format))
(if (not (defined? 'recorder-out-format)) (define recorder-out-format recorder-out-data-format))
(if (not (defined? 'recorder-out-type)) (define recorder-out-type recorder-out-header-type))

(define (snd-apropos val)
  "(snd-apropos val) is a wrapper for Scheme's apropos"
  (if (defined? 'apropos)
      (with-output-to-string
	(lambda ()
	  (apropos (if (string? val) val (object->string val)))))))

(define (make-iir-low-pass-1 fc)
  "(make-iir-low-pass-1 fc) makes an IIR low pass filter"
  (let* ((theta (/ (* 2 pi fc) (mus-srate)))
	 (gamma (/ (cos theta) (+ 1.0 (sin theta))))
	 (xc (/ (- 1.0 gamma) 2.0)))
    (make-filter 2 
		 (vct xc xc) 
		 (vct 0.0 (- gamma)))))

(define (make-iir-high-pass-1 fc)
  "(make-iir-high-pass-1 fc) makes an IIR high pass filter"
  (let* ((theta (/ (* 2 pi fc) (mus-srate)))
	 (gamma (/ (cos theta) (+ 1.0 (sin theta))))
	 (xc (/ (+ 1.0 gamma) 2.0)))
    (make-filter 2 
		 (vct xc (- xc)) 
		 (vct 0.0 (- gamma)))))

(define sort-files-by-name 0)
(define sort-files-by-date 2)
(define sort-files-by-size 4)
(define sort-files-by-entry -1) ; no longer implemented

(define mus-audio-sun-outputs mus-sun-set-outputs)
(define mus-audio-set-oss-buffers mus-oss-set-buffers)

;;; this was actually referring to the default (global) value, not file-specific
(if (not (defined? 'mus-file-data-clipped)) 
    (define mus-file-data-clipped mus-clipping))

(if (not (defined? 'data-clipped))
    (define data-clipped clipping))

(define (dac-is-running) (playing))

