;;; examples of using the GL bindings

(use-modules (ice-9 format))

;;; ---------------- gl-info ----------------
(define (gl-info)
  ;; taken loosely from glxvisuals.c
  "(gl-info) prints out GL-related info"
  (define (class-of n)
    (cond ((= n StaticGray) "static-gray")
	  ((= n GrayScale) "gray-scale")
	  ((= n StaticColor) "static-color")
	  ((= n PseudoColor) "pseudo-color")
	  ((= n TrueColor) "true-color")
	  ((= n DirectColor) "direct-color")
	  (#t "??")))
  (let* ((cx (snd-glx-context))
	 (dpy (XtDisplay (cadr (main-widgets))))
	 (version (glXQueryVersion dpy 0 0)))
    (if (car version)
	(let* ((scr (DefaultScreen dpy))
	       (visuals (XGetVisualInfo dpy 0 (list 'XVisualInfo 0))))
	  (glXMakeCurrent dpy (XtWindow (cadr (main-widgets))) cx)
	  (snd-print (format #f "GL version: ~A.~A, (~A ~A ~A)~%"
			     (cadr version) (caddr version)
			     (glGetString GL_VENDOR) (glGetString GL_RENDERER) (glGetString GL_VERSION)))
	  (snd-print (format #f "  with: ~A~A~%"
			     (glXQueryExtensionsString dpy (XScreenNumberOfScreen (DefaultScreenOfDisplay dpy)))
			     (if (glXIsDirect dpy cx) ", direct rendering support" "")))
	  (for-each 
	   (lambda (visual)
	     (if (= (cadr (glXGetConfig dpy visual GLX_USE_GL)) 1)
		 ;; found a visual that can support GL
		 (let ((buffersize (cadr (glXGetConfig dpy visual GLX_BUFFER_SIZE)))
		       (level (cadr (glXGetConfig dpy visual GLX_LEVEL)))
		       (rgba (cadr (glXGetConfig dpy visual GLX_RGBA)))
		       (doublebuffer (cadr (glXGetConfig dpy visual GLX_DOUBLEBUFFER)))
		       (stereo (cadr (glXGetConfig dpy visual GLX_STEREO)))
		       (auxbuffers (cadr (glXGetConfig dpy visual GLX_AUX_BUFFERS)))
		       (redsize (cadr (glXGetConfig dpy visual GLX_RED_SIZE)))
		       (bluesize (cadr (glXGetConfig dpy visual GLX_BLUE_SIZE)))
		       (greensize (cadr (glXGetConfig dpy visual GLX_GREEN_SIZE)))
		       (alphasize (cadr (glXGetConfig dpy visual GLX_ALPHA_SIZE)))
		       (depthsize (cadr (glXGetConfig dpy visual GLX_DEPTH_SIZE)))
		       (stencilsize (cadr (glXGetConfig dpy visual GLX_STENCIL_SIZE)))
		       (acredsize (cadr (glXGetConfig dpy visual GLX_ACCUM_RED_SIZE)))
		       (acgreensize (cadr (glXGetConfig dpy visual GLX_ACCUM_GREEN_SIZE)))
		       (acbluesize (cadr (glXGetConfig dpy visual GLX_ACCUM_BLUE_SIZE)))
		       (acalphasize (cadr (glXGetConfig dpy visual GLX_ACCUM_ALPHA_SIZE))))
		   (snd-print (format #f "  id: #x~X depth: ~D class: ~S~%" (.visualid visual) (.depth visual) (class-of (.class visual))))
		   (snd-print (format #f "      buffersize: ~D, level: ~D, rgba: ~A, doublebuffer: ~A, stereo: ~A~%"
				      buffersize level
				      (if (= rgba 1) "#t" "#f")
				      (if (= doublebuffer 1) "#t" "#f")
				      (if (= stereo 1) "#t" "#f")))
		   (snd-print (format #f "      r: ~A, g: ~D, b: ~D, alpha: ~D, accum-r: ~D, accum-g: ~D, accum-b: ~D, accum-alpha: ~D~%"
				      redsize greensize bluesize alphasize 
				      acredsize acgreensize acbluesize acalphasize))
		   (snd-print (format #f "      auxbuffs: ~D, depth: ~D, acalpha: ~D~%"
				      auxbuffers depthsize stencilsize))

		   
		   )))
	   visuals))
	(snd-print "no GL found!"))))


;;; ---------------- waterfall spectrum ----------------
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
      (let* ((win (XtWindow drawer))
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
      (let ((rl-data (sound-data->vct input-data 0 (vector-ref data slice))))
	(snd-spectrum rl-data blackman2-window bins #t 0.0 #t #f)
	(redraw-graph))
      (set! slice (1+ slice))
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
      (do ((i 0 (1+ i)))
	  ((= i slices))
	(vct-scale! (vector-ref data i) 0.0)))
    
    (define (start-it)
      (define (add-main-pane name type args)
	(XtCreateManagedWidget name type (list-ref (main-widgets) 3) args))
      (if (not drawer)
	  (let* ((outer (add-main-pane "Waterfall" xmFormWidgetClass
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
	    (add-hook! orientation-hook (lambda () (redraw-graph)))
	    (add-hook! color-hook (lambda () (redraw-graph)))))
      ;; start the waterfall display
      (if (not (or input-port (XtWorkProcId? input-proc)))
	  (begin
	    (set! input-port (mus-audio-open-input mus-audio-default 22050 1 mus-lshort 512))
	    (set! input-proc (XtAppAddWorkProc (car (main-widgets)) tick-audio)))))
    
    ;; turn display with orientation dialog
    ;;  for example: x-angle 290, y angle: 60
    
    (lambda* (start #:optional scl pc-spectrum fft-size)
	     (if start
		 (begin
		   (set! cutoff pc-spectrum)
		   (set! scaler scl)
		   (set! bins fft-size)
		   (set! input-data (make-sound-data 1 (* bins 2)))
		   (do ((i 0 (1+ i)))
		       ((= i slices))
		     (vector-set! data i (make-vct bins)))
		   (start-it))
		 (stop-it)))))

  
(define* (start-waterfall #:optional (scl 1.0) (pc-spectrum 0.2) (fft-size 512))
  (waterfall #t scl pc-spectrum fft-size))

(define (stop-waterfall)
  (waterfall #f))
