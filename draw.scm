;;; examples of extensions to Snd's graphics

(define red (make-color 1 0 0))

#!
;;; this version uses draw-lines which is unnecessary
(define (display-samps-in-red-1 snd chn)
  "display samples 1000 to 2000 in red whenever they're in the current view"
  (let ((left (left-sample snd chn))
	(right (right-sample snd chn))
	(old-color (foreground-color snd chn)))
    (if (and (< left 2000)
	     (> right 1000))
	(let* ((data (make-graph-data snd chn)))
	  (if (vct? data)
	      (let* ((samps (- (min right 2000)
			       (max left 1000)))
		     (offset (max 0 (- 1000 left)))
		     (vals (* samps 2))
		     (srinc (/ 1.0 (srate snd)))
		     (lines (make-vector vals)))
		(do ((i 0 (+ i 2))
		     (j offset (1+ j))
		     (k (/ (max left 1000) (srate snd)) (+ k srinc)))
		    ((= i vals))
		  (vector-set! lines i (x->position k))
		  (vector-set! lines (+ i 1) (y->position (vct-ref data j) snd chn)))
		(set! (foreground-color snd chn) red)
		(draw-lines lines snd chn)
		(set! (foreground-color snd chn) old-color)))))))
!#

;;; a better version:
(define (display-samps-in-red snd chn)
  "(display-samples-in-red snd chn) displays samples 1000 to 2000 in red whenever they're in the current view. It \
is intended to be used as an after-graph-hook function."
  (let ((left (left-sample snd chn))
	(right (right-sample snd chn))
	(old-color (foreground-color snd chn)))
    (if (and (< left 2000)
	     (> right 1000))
	(let* ((data (make-graph-data snd chn)))
	  (if (vct? data)
	      (let* ((samps (- (min right 2000)
			       (max left 1000)))
		     (offset (max 0 (- 1000 left)))
		     (new-data (vct-subseq data offset (+ offset samps))))
		(set! (foreground-color snd chn) red)
		(graph-data new-data snd chn copy-context (max 1000 left) (min 2000 right))
		(set! (foreground-color snd chn) old-color))
	      (let* ((low-data (car data))
		     (high-data (cadr data))
		     (size (vct-length low-data))
		     (samps (- right left))
		     (left-offset (max 0 (- 1000 left)))
		     (left-bin (inexact->exact (/ (* size left-offset) samps)))
		     (right-offset (- (min 2000 right) left))
		     (right-bin (inexact->exact (/ (* size right-offset) samps)))
		     (new-low-data (vct-subseq low-data left-bin right-bin))
		     (new-high-data (vct-subseq high-data left-bin right-bin)))
		(set! (foreground-color snd chn) red)
		(graph-data (list new-low-data new-high-data) snd chn copy-context left-bin right-bin)
		(set! (foreground-color snd chn) old-color)))))))

;(add-hook! after-graph-hook display-samps-in-red)

(define (display-previous-edits snd chn)
  "(display-previous-edits snd chn) displays all edits of the current sound, with older versions gradually fading away"
  (let* ((edits (edit-position snd chn))
	 (old-color (foreground-color snd chn))
	 (clist (color->list old-color))
	 (r (car clist))
	 (g (cadr clist))
	 (b (caddr clist))
	 (rinc (/ (- 1.0 r) (+ edits 1)))
	 (ginc (/ (- 1.0 g) (+ edits 1)))
	 (binc (/ (- 1.0 b) (+ edits 1))))
    (if (> edits 0)
	(begin
	  (do ((pos 0 (1+ pos))
	       (re (- 1.0 rinc) (- re rinc))
	       (ge (- 1.0 ginc) (- ge ginc))
	       (be (- 1.0 binc) (- be binc)))
	      ((> pos edits))
	    (let ((data (make-graph-data snd chn pos)))
	      (set! (foreground-color snd chn) (make-color re ge be))
	      (graph-data data snd chn)))
	  (set! (foreground-color snd chn) old-color)))))


(define (overlay-sounds . args)
  "(overlay-sounds . args) overlays onto its first argument all subsequent arguments: (overlay-sounds 1 0 3)"
  (let ((base (car args)))
    (add-hook! after-graph-hook
	       (lambda (snd chn)
		 (if (and (sound? base)
			  (= snd base))
		     (for-each 
		      (lambda (snd)
			;; perhaps this should also set sync
			(if (and (sound? snd)
				 (> (channels snd) chn)
				 (> (channels base) chn))
			    (graph-data 
			     (make-graph-data snd chn) 
			     base chn copy-context #f #f graph-dots)))
		      (cdr args)))))))



(define (display-energy snd chn)
  "(display-energy snd chn) is a lisp-graph-hook function that displays the time domain data as energy in the lisp graph; \
the y-zoom-slider controls the graph amp"
  (let* ((ls (left-sample))
	 (rs (right-sample))
	 (datal (make-graph-data snd chn))
	 (data (if (vct? datal) datal (cadr datal)))
	 (len (vct-length data))
	 (sr (srate snd))
	 (y-max (y-zoom-slider snd chn)))
    (vct-multiply! data data)
    (graph data "energy" (/ ls sr) (/ rs sr) 0.0 (* y-max y-max) snd chn #f)))

;(add-hook! lisp-graph-hook display-energy)


(define (samples-via-colormap snd chn)
  "(samples-via-colormap snd chn) displays time domain graph using current colormap (just an example of colormap-ref)"
  (let* ((left (left-sample snd chn))
	 (right (right-sample snd chn))
	 (old-color (foreground-color snd chn))
	 (data (make-graph-data snd chn))
	 (samps (- right left))
	 (x0 (x->position (/ left (srate))))
	 (y0 (y->position (vct-ref data 0)))
	 (colors (make-vector colormap-size #f)))
    (do ((i (+ left 1) (1+ i))
	 (j 1 (1+ j)))
	((= i right))
      (let* ((x1 (x->position (/ i (srate))))
	     (y1 (y->position (vct-ref data j)))
	     (ref (inexact->exact (* colormap-size (abs (vct-ref data j)))))
	     (color (or (vector-ref colors ref)
			(let ((new-color (apply make-color (colormap-ref (colormap) ref))))
			  (vector-set! colors ref new-color)
			  new-color))))
	(set! (foreground-color snd chn) color)
	(draw-line x0 y0 x1 y1)
	(set! x0 x1)
	(set! y0 y1)))
    (set! (foreground-color snd chn) old-color)))



;;; -------- inset overall waveform; if click, move to that location

(use-modules (ice-9 common-list))

(define inset-width .2)
(define inset-height .25)

(define (make-current-window-display)

  (let ((envelopes '()))
    ;; keep track of already-computed envelopes

    (define (remove-sound-envelopes snd)
      (set! envelopes (remove-if 
		       (lambda (n) 
			 (= (car n) snd)) 
		       envelopes))
      #f)

    (define (remove-channel-envelope snd chn)
      (set! envelopes (remove-if 
		       (lambda (n) 
			 (and (= (car n) snd) 
			      (= (cadr n) chn))) 
		       envelopes)))

    (define (search-envelopes snd chn envs)
      (if (not (null? envs))
	  (let ((cur (car envs)))
	    (if (and (= (car cur) snd)
		     (= (cadr cur) chn))
		(caddr cur)
		(search-envelopes snd chn (cdr envs))))
	  #f))

    (define channel-envelope
      (make-procedure-with-setter
       (lambda (snd chn)
	 (search-envelopes snd chn envelopes))
       (lambda (snd chn new-env)
	 (remove-channel-envelope snd chn)
	 (set! envelopes (cons (list snd chn new-env) envelopes)))))

    (define (display-current-window-location snd chn)
      "display in upper right corner the overall current sound and where the current window fits in it"
      (if (graph-time? snd chn)
	  (let* ((axinf (axis-info snd chn))
		 (grf-width (list-ref axinf 12))
		 (width (inexact->exact (* inset-width grf-width)))
		 (x-offset (inexact->exact (- grf-width width)))
		 (grf-height (- (list-ref axinf 11) (list-ref axinf 13)))
		 (height (inexact->exact (* inset-height grf-height)))
		 (chan-offset (- (list-ref axinf 13) 10))
		 (y-offset (+ chan-offset (inexact->exact (/ height 2))))
		 (grf-chn (if (= (channel-style snd) channels-separate) chn 0))
		 (data0 #f)
		 (data1 #f))

	    (if (and (> width 10)
		     (> height 10)
		     (> (frames snd chn) 0)
		     (or (= chn 0)
			 (not (= (channel-style snd) channels-superimposed))))
		(begin
	      
		  ;; draw axes around the inset graph
		  (fill-rectangle x-offset (+ chan-offset height) width 2 snd grf-chn)
		  (fill-rectangle x-offset chan-offset 2 height snd grf-chn)
	      
		  ;; now show where the current window fits in this graph
		  (let ((rx (inexact->exact (* width (/ (right-sample snd chn) (frames snd chn)))))
			(lx (inexact->exact (* width (/ (left-sample snd chn) (frames snd chn))))))
		    (fill-rectangle (+ x-offset lx) chan-offset (max 1 (- rx lx)) height snd grf-chn selection-context))
	      
		  (let ((old-env (channel-envelope snd chn)))
		    (if (and old-env
			     (= width (car old-env))
			     (= height (cadr old-env))
			     (= y-offset (list-ref old-env 5))
			     (= (edit-position snd chn) (list-ref old-env 2)))
			(begin
			  (set! data0 (list-ref old-env 3))
			  (set! data1 (list-ref old-env 4)))
			(let* ((data (make-graph-data snd chn current-edit-position 0 (frames snd chn)))
			       (data-max (if (vct? data) (vct-peak data) (vct-peak (car data))))
			       (data-scaler (if (> data-max 0.0) (/ height (* 2 data-max)) 0.0))
			       (new-len (* width 2))
			       (data-len (if (vct? data) (vct-length data) (vct-length (car data))))
			       (step (/ data-len width)))
			  
		      (if (> data-len width)
			  (begin ; the normal case -- more samples to display than pixels available
			    (set! data0 (make-vector new-len))
			    (set! data1 (and (not (vct? data)) (make-vector new-len)))
			    
			    ;; now subsample the data to fit the number of pixels available
			    (let ((j 0)
				  (max-y (- data-max))
				  (min-y data-max)
				  (stepper 0.0))
			      (do ((i 0 (1+ i)))
				  ((or (= j new-len) (= i data-len)))
				(if data1
				    (begin
				      (set! max-y (max max-y (vct-ref (car data) i)))
				      (set! min-y (min min-y (vct-ref (cadr data) i))))
				    (set! max-y (max max-y (vct-ref data i))))
				(set! stepper (+ stepper 1.0))
				(if (>= stepper step)
				    (begin
				      (vector-set! data0 j x-offset) 
				      (vector-set! data0 (+ j 1) (inexact->exact (- y-offset (* max-y data-scaler))))
				      (set! max-y (- data-max))
				      (if data1
					  (begin
					    (vector-set! data1 j x-offset) 
					    (vector-set! data1 (+ j 1) (inexact->exact (- y-offset (* min-y data-scaler))))
					    (set! min-y data-max)))
				      (set! x-offset (+ x-offset 1))
				      (set! stepper (- stepper step))
				      (set! j (+ j 2)))))
			      
			      (while (< j new-len)
				     (vector-set! data0 j (vector-ref data0 (- j 2)))
				     (vector-set! data0 (+ j 1) (vector-ref data0 (- j 1)))
				     (if data1
					 (begin
					   (vector-set! data1 j (vector-ref data1 (- j 2)))
					   (vector-set! data1 (+ j 1) (vector-ref data1 (- j 1)))))
				     (set! j (+ j 2)))))
			  (let ((xstep (/ width data-len)))
			    ;; more pixels than samples
			    (set! data0 (make-vector (* data-len 2)))
			    (set! data1 (and (not (vct? data)) (make-vector (* data-len 2))))
			    (do ((i 0 (1+ i))
				 (j 0 (+ j 2))
				 (xj x-offset (+ xj xstep)))
				((= i data-len))
			      (vector-set! data0 j (inexact->exact xj))
			      (if (not data1)
				  (vector-set! data0 (+ j 1) (inexact->exact (- y-offset (* (vct-ref data i) data-scaler))))
				  (begin
				    (vector-set! data0 (+ j 1) (inexact->exact (- y-offset (* (vct-ref (car data) i) data-scaler))))
				    (vector-set! data1 j (inexact->exact xj))
				    (vector-set! data1 (+ j 1) (inexact->exact (- y-offset (* (vct-ref (cadr data) i) data-scaler)))))))))
		      (set! (channel-envelope snd chn) (list width height (edit-position snd chn) data0 data1 y-offset)))))
		  
		  (draw-lines data0 snd grf-chn)
		  (if data1 (draw-lines data1 snd grf-chn)))))))

    (define (click-current-window-location snd chn button state x y axis)
      (if (= axis time-graph)
	  (let* ((axinf (axis-info snd chn))
		 (grf-width (list-ref axinf 12))
		 (width (inexact->exact (* inset-width grf-width)))
		 (x-offset (inexact->exact (- grf-width width)))
		 (grf-height (- (list-ref axinf 11) (list-ref axinf 13)))
		 (height (inexact->exact (* inset-height grf-height)))
		 (chan-offset (- (list-ref axinf 13) 10))
		 (y-offset (+ chan-offset (inexact->exact (/ height 2)))))
	    (if (and (> width 0)
		     (>= x x-offset)
		     (<= x grf-width)
		     (>= y chan-offset)
		     (<= y (+ chan-offset height)))
		(let ((samp (inexact->exact (* (frames snd chn) (/ (- x x-offset) width)))))
		  (set! (cursor snd chn) samp)
		  (set! (right-sample snd chn) 
			(max 0 
			     (min (frames snd chn)
				  (- samp (inexact->exact (* .5 (- (left-sample snd chn) (right-sample snd chn))))))))
		  (update-time-graph)
		  #t)
	    #f))
	  #f))

    (add-hook! after-graph-hook display-current-window-location)
    (add-hook! mouse-click-hook click-current-window-location)
    (add-hook! close-hook remove-sound-envelopes)))

