(use-modules (ice-9 format) (ice-9 optargs))
(provide 'snd-oscope.scm)

(define audio-srate 22050) ; change this to 44100 to change the graph's sampling rate

(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
	  (snd-error (format #f "snd-motif.scm needs the xm module: ~A" hxm))
	  (dlinit hxm "init_xm"))))

(if (not (defined? 'red-pixel))
    (define red-pixel
      (let ((pix #f))
	(lambda ()
	  (if (not pix)
	      (let* ((shell (cadr (main-widgets)))
		     (dpy (XtDisplay shell))
		     (scr (DefaultScreen dpy))
		     (cmap (DefaultColormap dpy scr))
		     (col (XColor)))
		(if (= (XAllocNamedColor dpy cmap "red" col col) 0)
		    (snd-error "can't allocate red!")
		    (set! pix (.pixel col)))))
	  pix))))


(define oscope-dialog #f)
(define in-data (make-sound-data 1 256))
(define cycle-length 1024)
(define cycle-start 0)
(define power #f)
(define frozen #f)


(define (make-oscope)
  (let ((xdismiss (XmStringCreate "Dismiss" XmFONTLIST_DEFAULT_TAG))
	(titlestr (XmStringCreate "Oscilloscope" XmFONTLIST_DEFAULT_TAG))
	(in-port #f)
	(pbutton #f))
    (set! oscope-dialog (XmCreateTemplateDialog (cadr (main-widgets)) "oscilloscope"
						(list XmNokLabelString       xdismiss
						      XmNautoUnmanage        #f
						      XmNdialogTitle         titlestr
						      XmNresizePolicy        XmRESIZE_GROW
						      XmNnoResize            #f
						      XmNtransient           #f
						      XmNheight              400
						      XmNwidth               400
						      XmNbackground          (basic-color))))
    (XtVaSetValues (XmMessageBoxGetChild oscope-dialog XmDIALOG_OK_BUTTON)
		   (list XmNarmColor   (pushed-button-color)
			 XmNbackground (quit-button-color)))
    (XtAddCallback oscope-dialog 
		   XmNokCallback (lambda (w context info)
				   (if power (set! power #f))
				   (XtUnmanageChild oscope-dialog)))
    (XmStringFree xdismiss)
    (XmStringFree titlestr)
    (XtManageChild oscope-dialog)

    (let* ((toppane (XtCreateManagedWidget "oscope-pane" xmFormWidgetClass oscope-dialog
					    (list XmNleftAttachment      XmATTACH_FORM
						  XmNrightAttachment     XmATTACH_FORM
						  XmNtopAttachment       XmATTACH_FORM
						  XmNbottomAttachment    XmATTACH_WIDGET
						  XmNbottomWidget        (XmMessageBoxGetChild oscope-dialog XmDIALOG_SEPARATOR)
						  )))
	   (bottom-row (XtCreateManagedWidget "oscope-row" xmRowColumnWidgetClass toppane
					    (list XmNleftAttachment      XmATTACH_FORM
						  XmNrightAttachment     XmATTACH_FORM
						  XmNtopAttachment       XmATTACH_NONE
						  XmNbottomAttachment    XmATTACH_FORM
						  XmNorientation         XmVERTICAL
						  )))
	   (prow (XtCreateManagedWidget "oscope-row" xmRowColumnWidgetClass bottom-row
					    (list XmNleftAttachment      XmATTACH_FORM
						  XmNrightAttachment     XmATTACH_FORM
						  XmNtopAttachment       XmATTACH_FORM
						  XmNbottomAttachment    XmATTACH_NONE
						  XmNorientation         XmHORIZONTAL
						  XmNbackground          (basic-color)
						  )))
	   (power-button (XtCreateManagedWidget "power" xmToggleButtonWidgetClass prow 
						(list    XmNbackground          (basic-color)
							 XmNselectColor         (red-pixel)
							 )))
	   (freeze-button (XtCreateManagedWidget "freeze" xmToggleButtonWidgetClass prow
						 (list    XmNbackground          (basic-color)
							  XmNselectColor         (red-pixel)
							  )))
	   (cycle-title (XmStringCreate "cycle length" XmFONTLIST_DEFAULT_TAG))
	   (cycle (XtCreateManagedWidget "oscope-cycle" xmScaleWidgetClass bottom-row 
					 (list XmNorientation   XmHORIZONTAL
					       XmNshowValue     #t
					       XmNminimum       32
					       XmNmaximum       8192
					       XmNvalue         cycle-length
					       XmNdecimalPoints 0
					       XmNtitleString   cycle-title
					       XmNbackground    (basic-color)
					       )))
	   (mainform (XtCreateManagedWidget "oscope-form" xmFormWidgetClass toppane
					    (list XmNleftAttachment      XmATTACH_FORM
						  XmNrightAttachment     XmATTACH_FORM
						  XmNtopAttachment       XmATTACH_FORM
						  XmNbottomAttachment    XmATTACH_WIDGET
						  XmNbottomWidget        bottom-row
						  XmNbackground          (basic-color)
						  )))
    	   (graph (make-variable-graph mainform "input" 8192 audio-srate))
	   (data (channel-data graph 0)))
      (set! pbutton power-button)

      (set! (max-transform-peaks graph 0) 10)
      (XtAddCallback cycle XmNvalueChangedCallback (lambda (w context info) (set! cycle-length (.value info))))
      (XtAddCallback cycle XmNdragCallback (lambda (w context info) 
					     (let ((old-length cycle-length))
					       (set! cycle-length (.value info))
					       (if (< cycle-length old-length)
						   (do ((i cycle-length (1+ i)))
						       ((= i old-length))
						     (sound-data-set! data 0 i 0.0))))))
      (XtAddCallback freeze-button XmNvalueChangedCallback 
		     (lambda (w context info) 
		       (set! frozen (not frozen))
		       (if frozen
			   (begin
			     (if (time-graph? graph 0) (update-time-graph graph 0))
			     (if (transform-graph? graph 0) (update-transform-graph graph 0))))))
      (XtAddCallback power-button XmNvalueChangedCallback 
		     (lambda (w context info) 
		       (set! power (not power))
		       (if power
			   (begin
			     (set! in-port (mus-audio-open-input mus-audio-microphone audio-srate 1 mus-lshort 512))
			     (if (not (= in-port -1))
				 (begin
				   (do ()
				       ((or (not power) (c-g?)))
				     (mus-audio-read in-port in-data 256)
				     (if (not frozen)
					 (begin
					   (set! cycle-start (sound-data->sound-data in-data data cycle-start 256 cycle-length))
					   (if (< cycle-start 256)
					       (begin
						 (if (time-graph? graph 0) (update-time-graph graph 0))
						 (if (transform-graph? graph 0) (update-transform-graph graph 0)))))))
				   (if power ; C-g?
				       (begin
					 (XmToggleButtonSetValue power-button XmUNSET #f)
					 (set! power #f)))
				   (mus-audio-close in-port))
				 (snd-print ";can't open audio input?"))))))
      (list graph data))))

(define oscope (make-oscope))

;;; TODO: sine tone out
;;; TODO: gl checks ("waterfall"), gtk version, rb cases
