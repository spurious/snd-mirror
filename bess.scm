#!/usr/local/bin/guile -s
!#

(use-modules (ice-9 format))

;;;   for the Ruby version, see contrib/bess.rb by Michael Scholz

;;; load sndlib and xmlib
(let ((sndlib (dynamic-link "sndlib.so")))
  (if (not (dynamic-object? sndlib))
      (error "can't find sndlib.so")
      (dynamic-call (dynamic-func "init_sndlib" sndlib) #f)))

(let ((libxm (dynamic-link "libxm.so")))
  (if (not (dynamic-object? libxm))
      (error "can't find libxm")
      (dynamic-call (dynamic-func "init_xm" libxm) #f)))

;;; if these fail, first strace bess.scm and see where it failed
;;; if it actually did find the library, try running Snd and (dlopen "sndlib.so")
;;;   Snd's dlopen will report a truthful error message (libtool lies)


;;; set up our user-interface
(let* ((shell-app (XtVaOpenApplication 
                    "FM Forever!" 0 '() applicationShellWidgetClass
                    (list XmNallowShellResize #t)))
       (app (cadr shell-app))
       (shell (car shell-app))
       (dpy (XtDisplay shell))
       (screen (DefaultScreenOfDisplay dpy))
       (cmap (DefaultColormap dpy (DefaultScreen dpy)))
       (black (BlackPixelOfScreen screen))
       (white (WhitePixelOfScreen screen)))

  (define (get-color color)
    (let ((col (XColor)))
      (if (= (XAllocNamedColor dpy cmap color col col) 0)
	  (error (format #f "can't allocate ~A" color))
	  (.pixel col))))

  (define (set-flabel label value)
    (let ((s1 (XmStringCreate (format #f "~,3F" value) XmFONTLIST_DEFAULT_TAG)))
      (XtVaSetValues label (list XmNlabelString s1))
      (XmStringFree s1)))

  (define (set-ilabel label value)
    (let ((s1 (XmStringCreate (format #f "~D" value) XmFONTLIST_DEFAULT_TAG)))
      (XtVaSetValues label (list XmNlabelString s1))
      (XmStringFree s1)))

  (XtSetValues shell (list XmNtitle "FM Forever!"))

  (let* ((light-blue (get-color "lightsteelblue"))
	 (form (XtCreateManagedWidget "form" xmFormWidgetClass shell 
		 (list XmNbackground white
		       XmNforeground black
		       XmNresizePolicy XmRESIZE_GROW)))
	 ;; toggle named "play"
	 (play-button (XtCreateManagedWidget "play" xmToggleButtonWidgetClass form
                        (list XmNleftAttachment   XmATTACH_FORM
			      XmNbottomAttachment XmATTACH_NONE
			      XmNtopAttachment    XmATTACH_FORM
			      XmNrightAttachment  XmATTACH_NONE
			      XmNbackground       white)))
	 ;; carrier freq
	 (carrier (XtCreateManagedWidget "carrier freq:" xmLabelWidgetClass form
                    (list XmNleftAttachment   XmATTACH_FORM
			  XmNbottomAttachment XmATTACH_NONE
			  XmNtopAttachment    XmATTACH_WIDGET
			  XmNtopWidget        play-button
			  XmNrightAttachment  XmATTACH_NONE
			  XmNrecomputeSize    #f
			  XmNbackground       white)))
	 (freq-label (XtCreateManagedWidget "label" xmLabelWidgetClass form
                       (list XmNleftAttachment   XmATTACH_WIDGET
			     XmNleftWidget       carrier
			     XmNbottomAttachment XmATTACH_NONE
			     XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
			     XmNtopWidget        carrier
			     XmNrightAttachment  XmATTACH_NONE
			     XmNbackground       white)))
	 (freq-scale (XtCreateManagedWidget "carrier freq" xmScaleWidgetClass form
                       (list XmNleftAttachment   XmATTACH_WIDGET
			     XmNleftWidget       freq-label
			     XmNbottomAttachment XmATTACH_NONE
			     XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
			     XmNtopWidget        freq-label
			     XmNrightAttachment  XmATTACH_FORM
			     XmNshowValue        #f
			     XmNorientation      XmHORIZONTAL
			     XmNbackground       light-blue)))
	 ;; amp
	 (amp (XtCreateManagedWidget "amp:" xmLabelWidgetClass form
                (list XmNleftAttachment   XmATTACH_FORM
		      XmNbottomAttachment XmATTACH_NONE
		      XmNtopAttachment    XmATTACH_WIDGET
		      XmNtopWidget        carrier
		      XmNrightAttachment  XmATTACH_NONE
		      XmNrecomputeSize    #f
		      XmNbackground       white)))
	 (amp-label (XtCreateManagedWidget "label" xmLabelWidgetClass form
                      (list XmNleftAttachment   XmATTACH_WIDGET
			    XmNleftWidget       amp
			    XmNbottomAttachment XmATTACH_NONE
			    XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
			    XmNtopWidget        amp
			    XmNrightAttachment  XmATTACH_NONE
			    XmNbackground       white)))
	 (amp-scale (XtCreateManagedWidget "amp" xmScaleWidgetClass form
                      (list XmNleftAttachment   XmATTACH_WIDGET
			    XmNleftWidget       amp-label
			    XmNbottomAttachment XmATTACH_NONE
			    XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
			    XmNtopWidget        amp-label
			    XmNrightAttachment  XmATTACH_FORM
			    XmNshowValue        #f
			    XmNorientation      XmHORIZONTAL
			    XmNbackground       light-blue)))
	 ;; fm index
	 (fm-index (XtCreateManagedWidget "fm index:" xmLabelWidgetClass form
                     (list XmNleftAttachment   XmATTACH_FORM
			   XmNbottomAttachment XmATTACH_NONE
			   XmNtopAttachment    XmATTACH_WIDGET
			   XmNtopWidget        amp-scale
			   XmNrightAttachment  XmATTACH_NONE
			   XmNrecomputeSize    #f
			   XmNbackground       white)))
	 (fm-label (XtCreateManagedWidget "label" xmLabelWidgetClass form
                     (list XmNleftAttachment   XmATTACH_WIDGET
			   XmNleftWidget       fm-index
			   XmNbottomAttachment XmATTACH_NONE
			   XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
			   XmNtopWidget        fm-index
			   XmNrightAttachment  XmATTACH_NONE
			   XmNbackground       white)))
	 (fm-scale (XtCreateManagedWidget "fm index" xmScaleWidgetClass form
                     (list XmNleftAttachment   XmATTACH_WIDGET
			   XmNleftWidget       fm-label
			   XmNbottomAttachment XmATTACH_NONE
			   XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
			   XmNtopWidget        fm-label
			   XmNrightAttachment  XmATTACH_FORM
			   XmNshowValue        #f
			   XmNorientation      XmHORIZONTAL
			   XmNbackground       light-blue)))
	 ;; c/m ratio
	 (cm-ratio (XtCreateManagedWidget "c/m ratio:" xmLabelWidgetClass form
                     (list XmNleftAttachment   XmATTACH_FORM
			   XmNbottomAttachment XmATTACH_NONE
			   XmNtopAttachment    XmATTACH_WIDGET
			   XmNtopWidget        fm-scale
			   XmNrightAttachment  XmATTACH_NONE
			   XmNrecomputeSize    #f
			   XmNbackground       white)))
	 (cm-label (XtCreateManagedWidget "label" xmLabelWidgetClass form
                     (list XmNleftAttachment   XmATTACH_WIDGET
			   XmNleftWidget       cm-ratio
			   XmNbottomAttachment XmATTACH_NONE
			   XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
			   XmNtopWidget        cm-ratio
			   XmNrightAttachment  XmATTACH_NONE
			   XmNbackground       white)))
	 (cm-scale (XtCreateManagedWidget "cm ratio" xmScaleWidgetClass form
                     (list XmNleftAttachment   XmATTACH_WIDGET
			   XmNleftWidget       cm-label
			   XmNbottomAttachment XmATTACH_NONE
			   XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
			   XmNtopWidget        cm-label
			   XmNrightAttachment  XmATTACH_FORM
			   XmNshowValue        #f
			   XmNorientation      XmHORIZONTAL
			   XmNbackground       light-blue)))
	 (frequency 220.0)
	 (low-frequency 40.0)
	 (high-frequency 2000.0)
	 (amplitude 0.5)
	 (index 1.0)
	 (high-index 3.0)
	 (ratio 1)
	 (high-ratio 10)

	 (playing 0.0)

	 (carosc (make-oscil 0.0))
	 (modosc (make-oscil 0.0)))

    (define (freq-callback w c i)
      (set! frequency (+ low-frequency (* (.value i) (/ (- high-frequency low-frequency) 100.0))))
      (set-flabel freq-label frequency))

    (define (amp-callback w c i)
      (set! amplitude (/ (.value i) 100.0))
      (set-flabel amp-label amplitude))

    (define (fm-callback w c i)
      (set! index (* (.value i) (/ high-index 100.0)))
      (set-flabel fm-label index))

    (define (ratio-callback w c i)
      (set! ratio (inexact->exact (* (.value i) (/ high-ratio 100.0))))
      (set-ilabel cm-label ratio))

    ;; add scale-change (drag and value-changed) callbacks
    (XtAddCallback freq-scale XmNdragCallback freq-callback)
    (XtAddCallback freq-scale XmNvalueChangedCallback freq-callback)

    (XtAddCallback amp-scale XmNdragCallback amp-callback)
    (XtAddCallback amp-scale XmNvalueChangedCallback amp-callback)

    (XtAddCallback fm-scale XmNdragCallback fm-callback)
    (XtAddCallback fm-scale XmNvalueChangedCallback fm-callback)

    (XtAddCallback cm-scale XmNdragCallback ratio-callback)
    (XtAddCallback cm-scale XmNvalueChangedCallback ratio-callback)

    (XtAddCallback play-button XmNvalueChangedCallback (lambda (w c i) (set! playing (if (.set i) 1.0 0.0))))
    
    ;; set initial values
    (set-flabel freq-label frequency)
    (set-flabel amp-label amplitude)
    (set-flabel fm-label index)
    (set-ilabel cm-label ratio)

    (XmScaleSetValue freq-scale (inexact->exact (* 100 (/ (- frequency low-frequency) (- high-frequency low-frequency)))))
    (XmScaleSetValue amp-scale (inexact->exact (* 100 amplitude)))
    (XmScaleSetValue fm-scale (inexact->exact (* 100 (/ index high-index))))
    (XmScaleSetValue cm-scale (inexact->exact (* ratio (/ 100 high-ratio))))

    (XtRealizeWidget shell)

    ;; send fm data to dac
    (mus-audio-set-oss-buffers 4 12) ; a no-op except in OSS/Linux
    (let* ((bufsize 256)
	   (srate 22050)
	   (chans 1)
	   (data (make-sound-data chans bufsize))
	   (proc #f)
	   (port (mus-audio-open-output mus-audio-default srate chans mus-lshort (* bufsize 2))))
      (if (< port 0) (display (format #f "can't open DAC!")))
      (XmAddWMProtocolCallback shell 
			       (XmInternAtom dpy "WM_DELETE_WINDOW" #f)
			       (lambda (w c i)
				 (XtRemoveWorkProc proc) ; odd that there's no XtAppRemoveWorkProc
				 (mus-audio-close port))
			       #f)
      (set! proc (XtAppAddWorkProc 
		  app 
		  (lambda (ignored-arg)
		    (do ((i 0 (1+ i)))
			((= i bufsize))
		      (sound-data-set! 
		       data 0 i
		       (* amplitude playing
			  (oscil carosc 
				 (+ (in-hz frequency)
				    (* index 
				       (oscil modosc 
					      (in-hz (* ratio frequency)))))))))
		    (mus-audio-write port data bufsize)
		    #f))))
    (XtAppMainLoop app)))
