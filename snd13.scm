(define (clm-print . args) 
  "(clm-print . args) applies format to args and prints the result"
  (snd-print (apply format #f args)))

#|
;;; this is now moved to C 
;;; -------- envelope-interp

(define* (envelope-interp x e (base 1.0))   ;e is list of x y breakpoint pairs, interpolate at x returning y
;;  "(envelope-interp x e (base 1.0)) -> value of e at x; base controls connecting segment type: (envelope-interp .3 '(0 0 .5 1 1 0) -> .6"
  (cond ((null? e) 0.0)		        ;no data -- return 0.0
	((or (<= x (car e))	        ;we're sitting on x val (or if < we blew it)
	     (null? (cddr e)))	        ;or we're at the end of the list
	 (cadr e))		        ;so return current y value
	((> (caddr e) x)		;x <= next env x axis value
	 (if (or (= (cadr e) (cadddr e))
		 (= base 0.0))
	     (cadr e)		        ;y1=y0, so just return y0 (avoid endless calculations below)
	     (if (= base 1.0)
		 (+ (cadr e)	        ;y0+(x-x0)*(y1-y0)/(x1-x0)
		    (* (- x (car e))
		       (/ (- (cadddr e) (cadr e))
			  (- (caddr e) (car e)))))
		 (+ (cadr e) ; this does not exactly match xramp-channel
		    (* (/ (- (cadddr e) (cadr e))
			  (- base 1.0))
		       (- (expt base (/ (- x (car e))
					(- (caddr e) (car e))))
			  1.0))))))
	(else (envelope-interp x (cddr e) base)))) ;go on looking for x segment
|#


#|

;;; ---------------- waterfall spectrum ----------------
;;; this is obsolete

(define waterfall
  (let* ((drawer #f)
	 (input-port #f)
	 (input-proc 0)
	 (gl-list #f)
	 (slices 256) ; number of traces displayed
	 (slice 0)
	 (data (make-vector slices))
	 (bins 512) ; fft size
	 (input-data #f)
	 (scaler 1.0)  ; data scaler before GL turns it into colors
	 (cutoff 0.2)) ; 0.5 is full spectrum
    
    (define (redraw-graph)
      (let ((win (XtWindow drawer))
	    (dpy (XtDisplay drawer))
	    (cx (snd-glx-context)))
	(glXMakeCurrent dpy win cx)
	(if gl-list (glDeleteLists gl-list 1))
	(set! gl-list (glGenLists 1))
	(glEnable GL_DEPTH_TEST)
	(glShadeModel GL_SMOOTH)
	(glClearDepth 1.0)
	(glClearColor 1.0 1.0 1.0 1.0) ; todo: bg color here
	(glClear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
	;;gl_spectrogram(XEN data, XEN gl_list, XEN cutoff, XEN use_dB, XEN min_dB, XEN scale, XEN br, XEN bg, XEN bb)
	(glSpectrogram data gl-list cutoff #f -60.0 scaler 65535 65535 65535)
	(let ((vals (XtVaGetValues drawer (list XmNwidth 0 XmNheight 0))))
	  (glViewport 0 0 (list-ref vals 1) (list-ref vals 3)))
	(glMatrixMode GL_PROJECTION)
	(glLoadIdentity)
	(glRotatef (spectro-x-angle) 1.0 0.0 0.0)
	(glRotatef (spectro-y-angle) 0.0 1.0 0.0)
	(glRotatef (spectro-z-angle) 0.0 0.0 1.0)
	(glScalef (spectro-x-scale) (spectro-y-scale) (spectro-z-scale))
	(glCallList gl-list)
	;; todo: make axis
	(glXSwapBuffers dpy win)
	(glDrawBuffer GL_BACK)))
    
    (define (tick-audio id)
      ;; background process reads incoming audio data, creates spectrum, displays next trace
      (mus-audio-read input-port input-data (* bins 2))
      (let ((rl-data (copy (input-data 0) (data slice))))
	(snd-spectrum rl-data blackman2-window bins #t 0.0 #t #f)
	(redraw-graph))
      (set! slice (+ 1 slice))
      (if (>= slice slices)
	  (set! slice 0))
      #f)
    
    (define (stop-it)
      ;; turn off the waterfall display
      (if input-port
	  (begin
	    (mus-audio-close input-port)
	    (set! input-port #f)))
      (if (XtWorkProcId? input-proc)
	  (begin
	    (XtRemoveWorkProc input-proc)
	    (set! input-proc 0)))
      (do ((i 0 (+ 1 i)))
	  ((= i slices))
	(float-vector-scale! (data i) 0.0)))
    
    (define (start-it)
      (define (add-main-pane name type args)
	(XtCreateManagedWidget name type (list-ref (main-widgets) 3) args))
      (if (not drawer)
	  (let ((outer (add-main-pane "Waterfall" xmFormWidgetClass
				      (list XmNbackground (basic-color)
					    XmNpaneMinimum 320))))
	    (set! drawer (XtCreateManagedWidget "draw" xmDrawingAreaWidgetClass outer
						(list XmNbackground       (graph-color)
						      XmNforeground       (data-color)
						      XmNleftAttachment   XmATTACH_FORM
						      XmNtopAttachment    XmATTACH_FORM
						      XmNbottomAttachment XmATTACH_FORM
						      XmNrightAttachment  XmATTACH_FORM)))
	    (XtAddCallback drawer XmNresizeCallback (lambda (w context info) (redraw-graph)))
	    (XtAddCallback drawer XmNexposeCallback (lambda (w context info) (redraw-graph)))
	    (hook-push orientation-hook (lambda (hook) (redraw-graph)))
	    (hook-push color-hook (lambda (hook) (redraw-graph)))))
      ;; start the waterfall display
      (if (not (or input-port (XtWorkProcId? input-proc)))
	  (begin
	    (set! input-port (mus-audio-open-input mus-audio-default 22050 1 mus-lshort 512))
	    (set! input-proc (XtAppAddWorkProc (car (main-widgets)) tick-audio)))))
    
    ;; turn display with orientation dialog
    ;;  for example: x-angle 290, y angle: 60
    
    (lambda* (start scl pc-spectrum fft-size)
	     (if start
		 (begin
		   (set! cutoff pc-spectrum)
		   (set! scaler scl)
		   (set! bins fft-size)
		   (set! input-data (make-vector (list 1 (* bins 2)) 0.0 #t))
		   (do ((i 0 (+ 1 i)))
		       ((= i slices))
		     (set! (data i) (make-float-vector bins)))
		   (start-it))
		 (stop-it)))))

  
(define* (start-waterfall (scl 1.0) (pc-spectrum 0.2) (fft-size 512))
  "(start-waterfall (scl 1.0) (pc-spectrum 0.2) (fft-size 512)) starts a 'waterfall' spectrum display of the incoming audio data"
  (waterfall #t scl pc-spectrum fft-size))

(define (stop-waterfall)
  "(stop-waterfall) stops a waterfall display"
  (waterfall #f))

|#



#|
;;; -------- "vector synthesis"
;;; also obsolete
;;; this idea (and the weird name) from linux-audio-development mailing list discussion
;;;   apparently some commercial synths (or software?) provide this

(define (vector-synthesis driver files read-even-when-not-playing)

  "(vector-synthesis driver files read-even-when-not-playing) uses 'driver', a 
function of two args (the number of files, and the number of samples between calls) to decide which file to play.  If 
'read-even-when-not-playing' is #t (default is #f), the input files are constantly 
read, even if not playing.  'files' is a list of files to be played."
  
  (let ((files-len (length files)))
    (if (> files-len 0)
	(let* ((bufsize 256)
	       (srate (srate (car files)))
	       (chans (apply max (map channels files)))
	       (data (make-vector (list chans bufsize) 0.0 #t))
	       (readers (map make-file->frame files))
	       (locs (make-vector files-len 0))
	       (pframes (make-vector files-len 0))
	       (current-file 0)
	       (reading #t)
	       (out-port (mus-audio-open-output 0 srate chans mus-lshort (* bufsize 2))))
	  (if (< out-port 0)
	      (format #t "can't open audio port! ~A" out-port)
	      (begin
		(do ((i 0 (+ i 1)))
		    ((= i files-len))
		  (set! (pframes i) (frames (files i))))
		(catch #t
		       (lambda ()
			 (while reading
				(let ((next-file (driver files-len bufsize)))
				  (if (not (= next-file current-file))
				      (let ((ramp-down 1.0)
					    (ramp (/ 1.0 bufsize))
					    (current (readers current-file))
					    (current-loc (locs current-file))
					    (next (readers next-file))
					    (next-loc (locs next-file))
					    (up (make-frame chans))
					    (down (make-frame chans)))
					(do ((i 0 (+ i 1)))
					    ((= i bufsize))
					  (file->frame next (+ next-loc i) up)
					  (file->frame current (+ current-loc i) down)
					  (do ((j 0 (+ 1 j)))
					      ((= j chans))
					    (vector-set! data j i 
							     (+ (* ramp-down (frame-ref down j))
								(* (- 1.0 ramp-down) (frame-ref up j)))))
					  (set! ramp-down (- ramp-down ramp)))
					(if read-even-when-not-playing
					    (do ((i 0 (+ i 1)))
						((= i files-len))
					      (set! (locs i) (+ (locs i) bufsize)))
					    (begin
					      (set! (locs current-file) (+ (locs current-file) bufsize))
					      (set! (locs next-file) (+ (locs next-file) bufsize))))
					(set! current-file next-file))
				      (let ((current (readers current-file))
					    (current-loc (locs current-file))
					    (on (make-frame chans)))
					(do ((i 0 (+ i 1)))
					    ((= i bufsize))
					  (file->frame current (+ current-loc i) on)
					  (do ((k 0 (+ 1 k)))
					      ((= k chans))
					    (vector-set! data k i (frame-ref on k))))
					(if read-even-when-not-playing
					    (do ((i 0 (+ i 1)))
						((= i files-len))
					      (set! (locs i) (+ (locs i) bufsize)))
					    (set! (locs current-file) (+ (locs current-file) bufsize)))))
				  (mus-audio-write out-port data bufsize)
				  (set! reading (letrec ((any-data-left 
							  (lambda (f)
							    (if (= f files-len)
								#f
								(or (< (locs f) (pframes f))
								    (any-data-left (+ 1 f)))))))
						  (any-data-left 0))))))
		       (lambda args (begin (snd-print (format #f "error ~A" args)) (car args))))
		(mus-audio-close out-port)))))))
|#
#|
(vector-synthesis (let ((ctr 0) (file 0)) 
		    (lambda (files bufsize)
		      (if (> ctr 4)
			  (begin
			    (set! file (+ 1 file))
			    (set! ctr 0)
			    (if (>= file files)
				(set! file 0)))
			  (set! ctr (+ ctr 1)))
		      file))
		  (list "oboe.snd" "pistol.snd") #t)
|#

