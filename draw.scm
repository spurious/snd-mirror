;;; examples of extensions to Snd's graphics

(define red (make-color 1 0 0))

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

;;; a better version:
(define (display-samps-in-red snd chn)
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
  "display all edits of the current sound, with older versions gradually fading away"
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
  "overlay-sounds overlays onto its first argument all subsequent arguments: (overlay-sounds 1 0 3)"
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



(define display-energy
  ;; in this version, the y-zoom-slider controls the graph amp
  (lambda (snd chn)
    "(display-energy snd chn) is a lisp-graph-hook function to display the time domain data\n\
    as energy (squared)"
    (let* ((ls (left-sample))
           (rs (right-sample))
	   (datal (make-graph-data snd chn))
	   (data (if (vct? datal) datal (cadr datal)))
           (len (vct-length data))
           (sr (srate snd))
	   (y-max (y-zoom-slider snd chn)))
      (vct-multiply! data data)
      (graph data "energy" (/ ls sr) (/ rs sr) 0.0 (* y-max y-max) snd chn #f))))

;(add-hook! lisp-graph-hook display-energy)


(define (display-current-window-location snd chn xsize ysize)
  "display in upper right corner the overall current sound and where the current window fits in it"
  (let* ((data (make-graph-data snd chn current-edit-position 0 (frames snd chn)))
	 (grf-width (car (widget-size (car (channel-widgets snd chn)))))
	 (width (inexact->exact (* xsize grf-width)))
	 (x-offset (inexact->exact (- grf-width width)))
	 (grf-height (cadr (widget-size (car (channel-widgets snd chn)))))
	 (height (inexact->exact (* ysize grf-height)))
	 (y-offset (/ height 2))
	 (data-max (if (vct? data) (vct-peak data) (vct-peak (car data))))
	 (data-scaler (/ height (* 2 data-max)))
	 (new-len (* width 2))
	 (data0 (make-vector new-len))
	 (data1 (and (not (vct? data)) (make-vector new-len)))
	 (data-len (if (vct? data) (vct-length data) (vct-length (car data))))
	 (step (/ data-len width)))

    ;; draw a axis-like around the inset graph
    (fill-rectangle x-offset height width 2 snd chn)
    (fill-rectangle x-offset 0 2 height snd chn)

    ;; now show where the current window fits in this graph
    (let ((rx (inexact->exact (* width (/ (right-sample snd chn) (frames snd chn)))))
	  (lx (inexact->exact (* width (/ (left-sample snd chn) (frames snd chn))))))
      (fill-rectangle (+ x-offset lx) 0 (max 1 (- rx lx)) height snd chn selection-context))

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
	(set! j (+ j 2))))
      
    (draw-lines data0 snd chn)
    (if data1 (draw-lines data1 snd chn))))

;(add-hook! after-graph-hook (lambda (snd chn) (display-current-window-location snd chn .2 .15)))

