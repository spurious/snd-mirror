(use-modules (ice-9 format) (ice-9 optargs))
(provide 'snd-oscope.scm)

(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
	  (snd-error (format #f "snd-motif.scm needs the xm module: ~A" hxm))
	  (dlinit hxm "init_xm"))))

(define oscope-dialog #f)

(define (make-oscope)
  (let ((xdismiss (XmStringCreate "Dismiss" XmFONTLIST_DEFAULT_TAG))
	(titlestr (XmStringCreate "Oscilloscope" XmFONTLIST_DEFAULT_TAG)))
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
				   (XtUnmanageChild oscope-dialog)))
    (XmStringFree xdismiss)
    (XmStringFree titlestr)
    (XtManageChild oscope-dialog)
    ;; TODO: protect here against no-window display
    (let* ((mainform (XtCreateManagedWidget "oscope-form" xmFormWidgetClass oscope-dialog
					    (list XmNleftAttachment      XmATTACH_FORM
						  XmNrightAttachment     XmATTACH_FORM
						  XmNtopAttachment       XmATTACH_FORM
						  XmNbottomAttachment    XmATTACH_WIDGET
						  XmNbottomWidget        (XmMessageBoxGetChild oscope-dialog XmDIALOG_SEPARATOR)
						  XmNbackground          (basic-color))))
    	   (graph (make-variable-graph mainform "input" 8192 (mus-srate))))
      (list graph (channel-data graph 0)))))

(define oscope (make-oscope))
(define in-data (make-sound-data 1 256))
(define cycle-length 256)
(define cycle-start 0)

;;; TODO: longer buffer, on button, cycle length setter, freeze/unfreeze, cursor if long window?
;;; TODO: sine tone out, fewer peaks default, off/on button to start stop input
;;; TODO: refresh rate


  (let* ((in-port (mus-audio-open-input mus-audio-microphone 22050 1 mus-lshort 512))
	 (snd (car oscope))
	 (data (cadr oscope)))
    (do ()
	((c-g?)) ; run until C-g stops us
      (mus-audio-read in-port in-data 256)
      (set! cycle-start (sound-data->sound-data in-data data cycle-start 256 cycle-length))
      (if (time-graph? snd 0) (update-time-graph snd 0))
      (if (transform-graph? snd 0) (update-transform-graph snd 0)))
    (mus-audio-close in-port))

#!
(define* (open-play-output #:optional out-chans out-srate out-format out-bufsize)
  ;; returns (list audio-fd chans frames)
  (let* ((outchans (or out-chans 1))
	 (cur-srate (or out-srate (and (not (null? (sounds))) (srate)) 22050))
	 (pframes (or out-bufsize 256))
	 (frm (or out-format (if (little-endian?) mus-lshort mus-bshort)))
	 (outbytes (* pframes 2))     ; 2 here since we'll first try to send short (16-bit) data to the DAC
	 (audio-fd ;; ALSA throws an error where the rest of the audio cases simply report failure
	           ;;   so we turn off the "error" printout, catch the error itself, and toss it
	  (let ((no-error (hook-empty? mus-error-hook)))
	    (if no-error
		(add-hook! mus-error-hook (lambda (typ msg) #t)))
	    (let ((val (catch #t
			      (lambda ()
				(mus-audio-open-output mus-audio-default cur-srate outchans frm outbytes))
			      (lambda args -1)))) ; -1 returned in case of error
	      (if no-error 
		  (reset-hook! mus-error-hook))
	      val))))
    (if (= audio-fd -1)
	;; ask card what it wants -- ALSA with some cards, for example, insists on 10 (virtual) channels and mus-lintn data!
	(let ((vals (make-vct 32)))
	  (mus-audio-mixer-read mus-audio-default mus-audio-format 32 vals)
	  (let ((fmt (inexact->exact (vct-ref vals 1))))
	    (mus-audio-mixer-read mus-audio-default mus-audio-channel 32 vals)
	    (set! outchans (inexact->exact (vct-ref vals 0)))
	    (let ((err (mus-audio-mixer-read mus-audio-default mus-audio-samples-per-channel 2 vals)))
	      (if (not (= err -1))
		  (set! pframes (inexact->exact (vct-ref vals 0))))
	      (let* ((bps (mus-bytes-per-sample fmt)))
		(set! outbytes (* bps pframes outchans))
		(set! audio-fd (catch #t
				      (lambda ()
					(mus-audio-open-output mus-audio-default cur-srate outchans fmt outbytes))
				      (lambda args -1))))))))
    (if (not (= audio-fd -1))
	(set! (dac-size) outbytes))
    (list audio-fd outchans pframes)))
!#
