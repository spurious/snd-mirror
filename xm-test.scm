(use-modules (ice-9 format))

(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
	  (snd-error (format #f "snd-motif.scm needs the xm module: ~A" hxm))
	  (dlinit hxm "init_xm"))))


(define (snd-display . args)
  (let ((str (apply format #f args)))
    (display str)
    (if (not (provided? 'snd-nogui))
	(begin
	  (snd-print "\n")
	  (snd-print str)))))

(define (for-each-child w func)
  (func w)
  (if (|XtIsComposite w)
      (for-each 
       (lambda (n)
	 (for-each-child n func))
       (cadr (|XtGetValues w (list |XmNchildren 0) 1)))))

(define c1 #f)

(define scale-dialog #f)
(define current-scaler 1.0)

(define (create-scale-dialog parent)
  (if (not (|Widget? scale-dialog))
      (let ((xdismiss (|XmStringCreate "Dismiss" |XmFONTLIST_DEFAULT_TAG))
	    (xhelp (|XmStringCreate "Help" |XmFONTLIST_DEFAULT_TAG))
	    (titlestr (|XmStringCreate "Scaling" |XmFONTLIST_DEFAULT_TAG)))
	(set! scale-dialog 
	      (|XmCreateTemplateDialog parent "Scaling"
                (list |XmNcancelLabelString   xdismiss
		      |XmNhelpLabelString     xhelp
		      |XmNautoUnmanage        #f
		      |XmNdialogTitle         titlestr
		      |XmNresizePolicy        |XmRESIZE_GROW
	              |XmNnoResize            #f
		      |XmNtransient           #f) ))

	(|XtVaSetValues (|XmMessageBoxGetChild scale-dialog |XmDIALOG_HELP_BUTTON) 
			(list |XmNarmColor (|WhitePixelOfScreen 
					     (|DefaultScreenOfDisplay 
					       (|XtDisplay (|Widget (cadr (main-widgets))))))))

	(|XtAddCallback scale-dialog 
			|XmNcancelCallback (lambda (w context info)
					     (|XtUnmanageChild scale-dialog)))
	(|XtAddCallback scale-dialog 
			|XmNhelpCallback (lambda (w context info)
					   (snd-print "move the slider to affect the volume")))
	(|XmStringFree xhelp)
	(|XmStringFree xdismiss)
	(|XmStringFree titlestr)

	(let* ((mainform 
		(|XtCreateManagedWidget "formd" |xmFormWidgetClass scale-dialog
                  (list |XmNleftAttachment      |XmATTACH_FORM
		        |XmNrightAttachment     |XmATTACH_FORM
		        |XmNtopAttachment       |XmATTACH_FORM
		        |XmNbottomAttachment    |XmATTACH_WIDGET
		        |XmNbottomWidget        (|XmMessageBoxGetChild scale-dialog |XmDIALOG_SEPARATOR))))
	       (scale
		(|XtCreateManagedWidget "" |xmScaleWidgetClass mainform
		  (list |XmNorientation |XmHORIZONTAL
			|XmNshowValue   #t
			|XmNvalue       100
			|XmNmaximum     500
			|XmNdecimalPoints 2))))

      (|XtAddCallback scale 
		      |XmNvalueChangedCallback (lambda (w context info)
						 (set! current-scaler (/ (|value info) 100.0))))
      (|XtAddCallback scale |XmNdragCallback (lambda (w context info)
						 (set! current-scaler (/ (|value info) 100.0)))))))
  (|XtManageChild scale-dialog))

(let* ((wids (main-widgets))
       (app (|XtAppContext (car wids)))
       (shell (|Widget (cadr wids)))
       (sound-pane (|Widget (cadddr wids)))
       (listener (|Widget (list-ref (dialog-widgets) 21)))
       (dpy (|XtDisplay shell))
       (black (|BlackPixelOfScreen 
		(|DefaultScreenOfDisplay 
		  dpy)))
       (white (|WhitePixelOfScreen 
		(|XDefaultScreenOfDisplay 
		  dpy))))
  (if (not (|XtIsApplicationShell shell))
      (snd-display ";~A not appshell?" shell))
  (if (not (= (|XScreenCount dpy) (|ScreenCount dpy)))
      (snd-display ";ScreenCount: ~D ~D~%" (|XScreenCount dpy) (|ScreenCount dpy)))
  (if (not (= (|XScreenNumberOfScreen (|DefaultScreenOfDisplay dpy)) 0))
      (snd-display ";ScreenNumberOfScreen: ~D~%" (|XScreenNumberOfScreen (|DefaultScreenOfDisplay dpy))))
  (if (not (|Screen? (|ScreenOfDisplay dpy 0)))
      (snd-display ";ScreenOfDisplay: ~A?" (|ScreenOfDisplay dpy 0)))
  (if (not (|Screen? (|XScreenOfDisplay dpy 0)))
      (snd-display ";XScreenOfDisplay: ~A?" (|XScreenOfDisplay dpy 0)))
  (if (not (equal? (|XScreenOfDisplay dpy 0) (|ScreenOfDisplay dpy 0)))
      (snd-display ";ScreenOfDisplay: ~A ~A~%" (|XScreenOfDisplay dpy 0) (|ScreenOfDisplay dpy 0)))
  (let ((scrn (|XScreenNumberOfScreen (|DefaultScreenOfDisplay dpy))))
    (if (not (= (|DefaultDepth dpy scrn) 24))
	(snd-display ";DefaultDepth: ~A?" (|DefaultDepth dpy scrn) 24))
    (if (not (= (|XDefaultDepth dpy scrn) (|DefaultDepth dpy scrn)))
	(snd-display ";default depth ~D ~D~%" (|XDefaultDepth dpy scrn) (|DefaultDepth dpy scrn)))
    (if (not (= (|XDefaultScreen dpy) (|DefaultScreen dpy)))
	(snd-display ";default screen number: ~D ~D~%" (|XDefaultScreen dpy) (|DefaultScreen dpy)))
    (if (not (|Screen?  (|DefaultScreenOfDisplay dpy)))
	(snd-display ";defaultscreenofdisplay: ~A" (|DefaultScreenOfDisplay dpy)))
    (if (not (equal? (|DefaultScreenOfDisplay dpy) (|XDefaultScreenOfDisplay dpy))) 
	(snd-display ";default screen of display: ~A ~A~%" (|XDefaultScreenOfDisplay dpy) (|DefaultScreenOfDisplay dpy)))


    (|XtSetValues shell (list |XmNtitle "Hi!") 1)
    (let* ((main-pane 
	    (|XtVaCreateManagedWidget 
	      "main-pane" |xmFormWidgetClass sound-pane
	      (list |XmNbackground       black
		    |XmNtopAttachment    |XmATTACH_FORM
		    |XmNbottomAttachment |XmATTACH_FORM
		    |XmNleftAttachment   |XmATTACH_FORM
		    |XmNrightAttachment  |XmATTACH_FORM
		    |XmNallowResize      #t)))
	   (button (|XtCreateManagedWidget 
		     "push me" |xmPushButtonWidgetClass main-pane 
		     (list |XmNleftAttachment   |XmATTACH_FORM
			   |XmNbottomAttachment |XmATTACH_NONE
			   |XmNrightAttachment  |XmATTACH_NONE
			   |XmNtopAttachment    |XmATTACH_FORM)))
	   (toggle (|XtCreateManagedWidget
		     "toggle" |xmToggleButtonWidgetClass main-pane
		     (list |XmNleftAttachment   |XmATTACH_WIDGET
			   |XmNleftWidget       button
                           |XmNrightAttachment  |XmATTACH_NONE
			   |XmNbottomAttachment |XmATTACH_NONE
			   |XmNtopAttachment    |XmATTACH_FORM)))
	   (slider (|XtCreateManagedWidget 
		     "slider" |xmScrollBarWidgetClass main-pane
		     (list |XmNleftAttachment   |XmATTACH_WIDGET
			   |XmNleftWidget       toggle
                           |XmNrightAttachment  |XmATTACH_FORM
			   |XmNbottomAttachment |XmATTACH_NONE
			   |XmNtopAttachment    |XmATTACH_FORM
			   |XmNorientation      |XmHORIZONTAL
			   |XmNdragCallback (list (lambda (widget context info)
						    (display (|value info)))
						  #f
						  (lambda (widget context info)
						    (display context))
						  "ha"))))
	   (drawer (|XtCreateManagedWidget
		     "drawer" |xmDrawingAreaWidgetClass main-pane
		     (list |XmNleftAttachment   |XmATTACH_FORM
			   |XmNbottomAttachment |XmATTACH_FORM
			   |XmNrightAttachment  |XmATTACH_FORM
			   |XmNtopAttachment    |XmATTACH_WIDGET
			   |XmNtopWidget        button
			   |XmNbackground       white)))
	   (scr (|DefaultScreen dpy))
	   (cmap (|DefaultColormap dpy scr))
	   (col (|XColor))
	   (gv (|XGCValues)))
      
      (if (= (|XAllocNamedColor dpy cmap "red" col col) 0)
	  (display "oops"))

      (set! c1 main-pane)

      (set! (|foreground gv) (|pixel col))
      (let ((gc (|XCreateGC dpy (|XtWindow shell) |GCForeground gv))
	    (draw-window (|XtWindow drawer))
	    (draw-display (|XtDisplay drawer)))
	
;	(|XtSetValues button (list |XmNbackground (|pixel col)) 1)
	(|XtSetValues button (list |XmNbackground (|Pixel (snd-pixel (basic-color)))) 1)

	(|XtAppAddWorkProc app (let ((ctr 0))
				 (lambda (n)
				   (if (= ctr 3)
				       #t
				       (begin
					 (display ctr)
					 (set! ctr (+ ctr 1))
					 #f)))))

	(|XtAddCallback button |XmNactivateCallback 
			(lambda (widget context event-info)
			  (display (|event event-info))
			  (display (|reason event-info))
			  (display (|state (|event event-info)))
			  (|XtAppAddTimeOut app 1000 (lambda (me id)
						       (display me))
					    "ho!"))
			123)
	(|XtAddCallback toggle |XmNvalueChangedCallback
			(lambda (widget context info)
			  (display info)
			  (|XDrawLine draw-display draw-window gc 0 0 120 100)
			  (display (|set info))))

	(|XtAddEventHandler drawer |EnterWindowMask #f
			    (lambda (w context ev flag)
			      (display "hi")))


	(create-scale-dialog shell)
    
      ))))

;;; ex: change pos/range scrollbar algorithm, change action of ypos, add wave to vu

(define mouse_width 32)
(define mouse_height 32)
(define mouse_bits (list
   #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
   #x80 #xff #xff #x01 #x80 #x00 #x01 #x01 #x80 #x00 #x01 #x01
   #x80 #x00 #x01 #x01 #x80 #x00 #x01 #x01 #x80 #x00 #x01 #x01
   #x80 #x00 #x01 #x01 #x80 #xff #xff #x01 #x80 #x00 #x00 #x01
   #x80 #x00 #x00 #x01 #x80 #x00 #x00 #x01 #x80 #x00 #x00 #x01
   #x80 #x00 #x00 #x01 #x80 #x00 #x00 #x01 #x80 #x00 #x00 #x01
   #x80 #x00 #x00 #x01 #x00 #x01 #x80 #x00 #x00 #x01 #x80 #x00
   #x00 #x06 #x60 #x00 #x00 #xf8 #x1f #x00 #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
   #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00))

(define rb (list
   #x00 #x04 #x10 #x08 #x00 #x10 #x04 #x20 #x00 #x40 #xa5 #xbf
   #x00 #x40 #x04 #x20 #x00 #x10 #x10 #x08 #x00 #x04 #x00 #x00))


(define (bitmap->pixmap widget bits width height)
  (|XCreateBitmapFromData (|XtDisplay widget) (|XtWindow widget) bits width height))

;;;   (|XtSetValues (|Widget (list-ref (sound-widgets) 8)) (list |XmNlabelPixmap (cadr pix))))

(define iconw (|Widget (list-ref (sound-widgets) 8)))
(define speedr (bitmap->pixmap iconw rb 16 12))
(define m (bitmap->pixmap iconw mouse_bits mouse_width mouse_height))
