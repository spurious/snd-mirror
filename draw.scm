;;; examples of extensions to Snd's graphics

(use-modules (ice-9 optargs))

(provide 'snd-draw.scm)
(if (not (provided? 'snd-extensions.scm)) (load-from-path "extensions.scm"))

;; these two are in dsp.scm
(if (not (defined? 'make-moving-rms))
    (define* (make-moving-rms :optional (size 128))
      (make-moving-average size)))

(if (not (defined? 'moving-rms))
    (define (moving-rms gen y)
      (sqrt (moving-average gen (* y y)))))

(define (overlay-rms-env snd chn)
  (let* ((red (make-color 1 0 0))            ; rms env displayed in red
	 (left (left-sample snd chn))
	 (right (right-sample snd chn))
	 (rms-size 128)                      ; this could be a parameter -- not sure what the "right" size is
	 (sr (/ 1.0 (srate snd)))
	 (old-color (foreground-color snd chn))
	 (axinf (axis-info snd chn))
	 (old-axinf (channel-property 'rms-axis-info snd chn)))

    ;; these functions are an optimization to speed up calculating the rms env graph.
    ;; ideally we'd use something like:
    ;;
    ;;   (let* ((x1 (x->position (/ i (srate)) snd chn))
    ;;          (y1 (y->position (moving-rms rms (reader)) snd chn)))
    ;;     (draw-line x0 y0 x1 y1)
    ;;
    ;; in the do-loop below that runs through the samples, but I haven't added x|y->position or draw-line
    ;; to the optimizer ("run"), and each would be looking up the graph axis info on each call even if
    ;; available to the optimizer -- this seems wasteful.  So, the grf-it function below is using the
    ;; axis info in axinf to get the pixel location for the envelope line segment break point.
    ;; Also, draw-lines takes a vector for some reason, so we need to tell "run" that it is an
    ;; integer vector (and preload it with 0).  We save the vector in the channel property 'rms-lines,
    ;; and the associated axis info in 'rms-axis-info.  Since redisplay is common in Snd, it reduces
    ;; "flashing" a lot to have this data instantly available.

    (define (pack-x-info axinf)
      (vct (list-ref axinf 2) ;  x0
	   (list-ref axinf 4) ;  x1
	   (list-ref axinf 10) ; x_axis_x0
	   (list-ref axinf 12) ; x_axis_x1
	   (list-ref axinf 15) ; scale
	   (- (list-ref axinf 10) (* (list-ref axinf 2) (list-ref axinf 15))))) ; base

    (define (pack-y-info axinf)
      (vct (list-ref axinf 3) ;  y0
	   (list-ref axinf 5) ;  y1
	   (list-ref axinf 11) ; y_axis_y0
	   (list-ref axinf 13) ; y_axis_y1
	   (list-ref axinf 16) ; scale
	   (- (list-ref axinf 11) (* (list-ref axinf 3) (list-ref axinf 16))))) ; base

    (define (grf-it val v)
      (inexact->exact 
       (round
	(if (>= val (vct-ref v 1))
	    (vct-ref v 3)
	    (if (<= val (vct-ref v 0))
		(vct-ref v 2)
		(+ (vct-ref v 5) (* val (vct-ref v 4))))))))

    (if (equal? axinf old-axinf)                    ; the previously calculated lines can be re-used
	(begin
	  (set! (foreground-color snd chn) red)
	  (draw-lines (channel-property 'rms-lines snd chn))
	  (set! (foreground-color snd chn) old-color))
	(let* ((xdata (pack-x-info axinf))
	       (ydata (pack-y-info axinf))
	       (start (max 0 (- left rms-size)))
	       (reader (make-sample-reader start snd chn))
	       (rms (make-moving-rms rms-size))
	       (x0 0)
	       (y0 0)
	       (line-ctr 2)
	       (lines (make-vector (* 2 (+ 1 (- (list-ref axinf 12) (list-ref axinf 10)))) 0)))
	  (dynamic-wind
	      (lambda ()
		(set! (foreground-color snd chn) red))
	      (lambda ()
		(run
		 (lambda ()
		   (declare (int-vector lines))
		   (if (< start left)                ; check previous samples to get first rms value
		       (do ((i start (1+ i)))
			   ((= i left))
			 (moving-rms rms (reader))))
		   (let ((first-sample (next-sample reader)))
		     (set! x0 (grf-it (* left sr) xdata))
		     (set! y0 (grf-it first-sample ydata))
		     (vector-set! lines 0 x0)        ; first graph point
		     (vector-set! lines 1 y0))
		   (do ((i (+ left 1) (1+ i)))       ; loop through all samples calling moving-rms
		       ((= i right))
		     (let* ((x1 (grf-it (* i sr) xdata))
			    (y (moving-rms rms (next-sample reader))))
		       (if (> x1 x0)                 ; very often many samples are represented by one pixel
			   (let ((y1 (grf-it y ydata)))
			     (vector-set! lines line-ctr x1)
			     (vector-set! lines (1+ line-ctr) y1)
			     (set! line-ctr (+ line-ctr 2))
			     (set! x0 x1)
			     (set! y0 y1)))))))      ; else should we do "max" here? or draw a vertical line from min to max?
		(if (< line-ctr (vector-length lines))
		    (do ((j line-ctr (+ j 2)))       ; off-by-one in vector size calc -- need to pad so we don't get a bogus line to (0, 0)
			((>= j (vector-length lines)))
		      (vector-set! lines j x0)
		      (vector-set! lines (+ j 1) y0)))
		(draw-lines lines snd chn)
		(set! (channel-property 'rms-lines snd chn) lines)  ; save current data for possible redisplay
		(set! (channel-property 'rms-axis-info snd chn) axinf))
	      (lambda ()
		(set! (foreground-color snd chn) old-color)))))))

;(add-hook! after-graph-hook overlay-rms-env)


(define* (display-colored-samples color beg dur :optional snd chn)
  "(display-colored-samples color beg dur snd chn) displays samples from beg for dur in color 
whenever they're in the current view."
  (let ((left (left-sample snd chn))
	(right (right-sample snd chn))
	(end (+ beg dur))
	(old-color (foreground-color snd chn)))
    (if (and (< left end)
	     (> right beg))
	(let* ((data (make-graph-data snd chn)))
	  (if (vct? data)
	      (let* ((samps (- (min right end) (max left beg)))
		     (offset (max 0 (- beg left)))
		     (new-data (vct-subseq data offset (+ offset samps))))
		(set! (foreground-color snd chn) color)
		(graph-data new-data snd chn copy-context (max beg left) (min end right))
		(set! (foreground-color snd chn) old-color))
	      (let* ((low-data (car data))
		     (high-data (cadr data))
		     (size (vct-length low-data))
		     (samps (- right left))
		     (left-offset (max 0 (- beg left)))
		     (left-bin (inexact->exact (floor (/ (* size left-offset) samps))))
		     (right-offset (- (min end right) left))
		     (right-bin (inexact->exact (floor (/ (* size right-offset) samps))))
		     (new-low-data (vct-subseq low-data left-bin right-bin))
		     (new-high-data (vct-subseq high-data left-bin right-bin)))
		(set! (foreground-color snd chn) color)
		(graph-data (list new-low-data new-high-data) snd chn copy-context left-bin right-bin)
		(set! (foreground-color snd chn) old-color)))))))

(define (display-samples-in-color snd chn)
  ;; intended as after-graph-hook member 
  ;; run through 'colored-samples lists passing each to display-colored-samples
  (let ((colors (channel-property 'colored-samples snd chn)))
    (if colors
	(for-each
	 (lambda (vals)
	   (apply display-colored-samples (append vals (list snd chn))))
	 colors))))

(define* (color-samples color :optional ubeg udur usnd uchn)
  "(color-samples color :optional beg dur snd chn) causes samples from beg to beg+dur to be displayed in color"
  (if (not (member display-samples-in-color (hook->list after-graph-hook)))
      (add-hook! after-graph-hook display-samples-in-color))
  (let* ((beg (or ubeg 0))
	 (snd (or usnd (selected-sound) (car (sounds))))
	 (chn (or uchn (selected-channel snd) 0))
	 (dur (or udur (- (frames snd chn) beg)))
	 (old-colors (or (channel-property 'colored-samples snd chn) '())))
    (set! (channel-property 'colored-samples snd chn) (cons (list color beg dur) old-colors))
    (update-time-graph snd chn)))

(define* (uncolor-samples :optional usnd uchn)
  "(uncolor-samples :optional snd chn) cancels sample coloring in the given channel"
  (let*	((snd (or usnd (selected-sound) (car (sounds))))
	 (chn (or uchn (selected-channel snd) 0)))
    (set! (channel-property 'colored-samples snd chn) '())
    (update-time-graph snd chn)))



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
			     base chn copy-context 
			     #f #f graph-dots)))
		      (cdr args)))))))

(define (samples-via-colormap snd chn)
  "(samples-via-colormap snd chn) displays time domain graph using current colormap (just an example of colormap-ref)"
  (let* ((left (left-sample snd chn))
	 (right (right-sample snd chn))
	 (old-color (foreground-color snd chn))
	 (data (make-graph-data snd chn)))
    (if (vct? data) ; could also be a list which we'll ignore since this is just a demo
	(let* ((x0 (x->position (/ left (srate snd))))
	       (y0 (y->position (vct-ref data 0)))
	       (colors (make-vector (colormap-size) #f)))
	  (do ((i (+ left 1) (1+ i))
	       (j 1 (1+ j)))
	      ((= i right))
	    (let* ((x1 (x->position (/ i (srate snd))))
		   (y1 (y->position (vct-ref data j)))
		   (x (abs (vct-ref data j)))
		   (ref (inexact->exact (floor (* (colormap-size) x))))
		   (color (or (vector-ref colors ref)
			      (let ((new-color (apply make-color (colormap-ref (colormap) x))))
				(vector-set! colors ref new-color)
				new-color))))
	      (set! (foreground-color snd chn) color)
	      (draw-line x0 y0 x1 y1)
	      (set! x0 x1)
	      (set! y0 y1)))
	  (set! (foreground-color snd chn) old-color)))))



;;; -------- inset overall waveform; if click, move to that location

(use-modules (ice-9 common-list))

(define inset-width .2)
(define inset-height .25)
(define current-window-display-is-running #f) ; a kludge for the preferences dialog

(define (make-current-window-display)

  (define (update-current-window-location snd)
    ;; this is called before the actual update -- we need to clear the notion of edit-position to force a re-read
    (if current-window-display-is-running
	(do ((i 0 (1+ i)))
	    ((= i (chans snd)))
	  (let ((vals (channel-property 'inset-envelope snd i)))
	    (if vals
		(list-set! vals 2 -2))))) ; set edit-position to impossible value
    #f)

  (define (display-current-window-location snd chn)
    "display in upper right corner the overall current sound and where the current window fits in it"
    (if (and current-window-display-is-running
	     (time-graph? snd chn))
	(let* ((axinf (axis-info snd chn))
	       (grf-width (list-ref axinf 12))
	       (width (inexact->exact (round (* inset-width grf-width))))
	       (x-offset (inexact->exact (- grf-width width)))
	       (grf-height (- (list-ref axinf 11) (list-ref axinf 13)))
	       (height (inexact->exact (round (* inset-height grf-height))))
	       (chan-offset (- (list-ref axinf 13) 10))
	       (y-offset (+ chan-offset (inexact->exact (round (/ height 2)))))
	       (grf-chn (if (= (channel-style snd) channels-separate) chn 0))
	       (new-peaks (list-ref axinf 19))
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
		(let ((rx (inexact->exact (round (* width (/ (right-sample snd chn) (frames snd chn))))))
		      (lx (inexact->exact (round (* width (/ (left-sample snd chn) (frames snd chn)))))))
		  (fill-rectangle (+ x-offset lx) chan-offset (max 1 (- rx lx)) height snd grf-chn selection-context))
		(let ((old-env (channel-property 'inset-envelope snd chn)))
		  (if (and old-env
			   (not new-peaks)
			   (= width (car old-env))
			   (= height (cadr old-env))
			   (= y-offset (list-ref old-env 5))
			   (= (edit-position snd chn) (list-ref old-env 2)))
		      (begin
			(set! data0 (list-ref old-env 3))
			(set! data1 (list-ref old-env 4)))
		      (let* ((data (make-graph-data snd chn current-edit-position 0 (frames snd chn)))
			     (data-max (if (vct? data) (vct-peak data)
					   (apply max (map vct-peak data))))
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
					(set! max-y (max max-y (vct-ref (cadr data) i)))
					(set! min-y (min min-y (vct-ref (car data) i))))
				      (set! max-y (max max-y (vct-ref data i))))
				  (set! stepper (+ stepper 1.0))
				  (if (>= stepper step)
				      (begin
					(vector-set! data0 j x-offset) 
					(vector-set! data0 (+ j 1) (inexact->exact (round (- y-offset (* max-y data-scaler)))))
					(set! max-y (- data-max))
					(if data1
					    (begin
					      (vector-set! data1 j x-offset) 
					      (vector-set! data1 (+ j 1) (inexact->exact (round (- y-offset (* min-y data-scaler)))))
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
				(vector-set! data0 j (inexact->exact (round xj)))
				(if (not data1)
				    (vector-set! data0 (+ j 1) (inexact->exact (round (- y-offset (* (vct-ref data i) data-scaler)))))
				    (begin
				      (vector-set! data0 (+ j 1) (inexact->exact (round (- y-offset (* (vct-ref (cadr data) i) data-scaler)))))
				      (vector-set! data1 j (inexact->exact (floor xj)))
				      (vector-set! data1 (+ j 1) (inexact->exact (round (- y-offset (* (vct-ref (car data) i) data-scaler))))))))))
			(set! (channel-property 'inset-envelope snd chn) 
			      (list width height (edit-position snd chn) data0 data1 y-offset)))))
		
		(draw-lines data0 snd grf-chn)
		(if data1 (draw-lines data1 snd grf-chn)))))))
  
  (define (click-current-window-location snd chn button state x y axis)
    (if (and current-window-display-is-running
	     (= axis time-graph))
	(let* ((axinf (axis-info snd chn))
	       (grf-width (list-ref axinf 12))
	       (width (inexact->exact (round (* inset-width grf-width))))
	       (x-offset (inexact->exact (- grf-width width)))
	       (grf-height (- (list-ref axinf 11) (list-ref axinf 13)))
	       (height (inexact->exact (round (* inset-height grf-height))))
	       (chan-offset (- (list-ref axinf 13) 10)))
	  (if (and (> width 0)
		   (>= x x-offset)
		   (<= x grf-width)
		   (>= y chan-offset)
		   (<= y (+ chan-offset height)))
	      (let* ((samp (inexact->exact (round (* (frames snd chn) (/ (- x x-offset) width)))))
		     (ls (left-sample snd chn))
		     (rs (right-sample snd chn)))
		(set! (cursor snd chn) samp)
		(if (or (< samp ls)
			(> samp rs))
		    (let ((rsamp (min (max 0 
					   (- samp (inexact->exact (round (* .5 (- ls rs))))))
				      (1- (frames snd chn)))))
		      (set! (right-sample snd chn) rsamp)))
		(update-time-graph)
		#t)
	      #f))
	#f))
  
  (set! current-window-display-is-running #t)
  (add-hook! after-open-hook 
    (lambda (s)
      (do ((i 0 (1+ i)))
	  ((= i (chans s)))
	(set! (channel-property 'save-state-ignore s i)
	      (cons 'inset-envelope 
		    (or (channel-property 'save-state-ignore s i) 
			(list 'save-state-ignore))))
	(add-hook! (undo-hook s i)
		   (lambda ()
		     (let ((vals (channel-property 'inset-envelope s i)))
		       (if vals
			   (list-set! vals 2 -2)))))))) ; set edit-position to impossible value
  (add-hook! after-graph-hook display-current-window-location)
  (add-hook! mouse-click-hook click-current-window-location)
  (add-hook! update-hook update-current-window-location))

(define (smart-line-cursor snd chn tracking)
  "smart-line-cursor is a cursor-style function that tries not to overwrite the thumbnail graph in the upper right corner"
  (let* ((point (cursor-position))
         (x (car point))
	 (ax (axis-info snd chn time-graph))
	 (y0 (list-ref ax 11))
	 (y1 (list-ref ax 13))
	 (x1 (list-ref ax 12))
	 (inset-x0 (* x1 (- 1.0 inset-width )))
	 (inset-y0 (+ (- y1 10) (* inset-height (- y0 y1)))))
    (if (> x (- inset-x0 5))
	(draw-line x y0 x (+ inset-y0 5) snd chn cursor-context)
	(draw-line x y0 x (- y1 5) snd chn cursor-context))))


;;; -------- click-for-listener-help

(define click-for-listener-help
  (let ((help-moved #f)
	(last-click-time 0))
    (lambda (pos)
  
      (define (char-quit? ch)
	(or (char-whitespace? ch)
	    (char=? ch #\()
	    (char=? ch #\))
	    (char=? ch #\')
	    (char=? ch #\")))

      ;; find nearest name
      ;; scan back and forth for non-alphanumeric
      ;; call snd-help on name and post in help-dialog
      
      (let* ((time (get-internal-real-time)) ; look for double-click
	     (click-time (- time last-click-time)))
	(set! last-click-time time)
	(if (< click-time 25) ; .25 secs -- could use XtGetMultiClickTime if xm loaded
	    (let* ((text (widget-text (list-ref (main-widgets) 4)))
		   (len (and (string? text) (string-length text)))
		   (happy #f))
	      (if (and len (>= pos len)) (set! pos (1- len))) ;click at very bottom of listener is beyond current string end
	      (if (and len (>= pos 1))                        ;not click in empty listener, etc
		  (begin
		    (if (char-quit? (string-ref text pos))
			(begin                      ;not currently in a name
			  ;; go looking for plausible name (look left first)
			  (do ((i (1- pos) (1- i)))
			      ((or happy (= i 0)))
			    (if (char-alphabetic? (string-ref text i))
				(begin
				  (set! pos i)
				  (set! happy #t))))
			  (if (not happy)           ;nothing to the left -- try to the right
			      (do ((i (1+ pos) (1+ i)))
				  ((or happy (= i len)))
				(if (char-alphabetic? (string-ref text i))
				    (begin
				      (set! pos i)
				      (set! happy #t))))))
			(set! happy #t))
		    (if happy
			;; found some portion of a name -- try to get the full thing
			(let ((start pos)
			      (end pos))
			  (set! happy #f)
			  (do ((i (1- pos) (1- i)))  ;look for start of name
			      ((or happy (= i 0)))
			    (if (char-quit? (string-ref text i))
				(begin
				  (set! start (+ i 1))
				  (set! happy #t))))
			  (set! happy #f)
			  (do ((i (1+ pos) (1+ i)))  ;look for end of name
			      ((or happy (and (= i len)
					      (let () 
						(set! end i) 
						(set! happy #t) 
						#t))))
			    (if (char-quit? (string-ref text i))
				(begin
				  (set! end i)
				  (set! happy #t))))
			  (if (and happy
				   (> end start))
			      ;; got a name -- look for snd-help, if any
			      (let* ((name (substring text start end))
				     (help (snd-help name #f)))
				(if (string=? name help)
				    (set! help (snd-help (string->symbol name))))
				(if (not (string=? name help))
				    (let ((dialog (help-dialog name help)))
				      (if (not help-moved)
					  ;; if this is the first call, try to position help dialog out of the way
					  ;;   this is clunky -- perhaps better would be a tooltip window at the bottom of the listener?
					  (let ((main-xy (widget-position (cadr (main-widgets))))
						(main-size (widget-size (cadr (main-widgets)))))
					    (set! (widget-position (list-ref (dialog-widgets) 14))
						  (list (+ (car main-xy) (car main-size))
							(cadr main-xy)))
					    (set! help-moved #t)))))))))))))))))
