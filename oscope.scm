;;; a software oscilloscope using the standard Snd channel display interface

(use-modules (ice-9 format) (ice-9 optargs))
(provide 'snd-oscope.scm)

;;; TODO: sonogram -> slices, not one at a time

(define audio-srate 44100) ; graph's sampling rate
(define max-cycle 8192)    ; maximum size in samples of the displayed buffer
(define cycle-length 1024) ; initial cycle length


(if (provided? 'snd-motif)
    (if (not (provided? 'xm))
	(let ((hxm (dlopen "xm.so")))
	  (if (string? hxm)
	      (snd-error (format #f "oscope.scm needs the xm module: ~A" hxm))
	      (dlinit hxm "Init_libxm"))))
    (if (provided? 'snd-gtk)
	(if (not (provided? 'xg))
	    (let ((hxm (dlopen "xg.so")))
	      (if (string? hxm)
		  (snd-error (format #f "oscope.scm needs the xg module: ~A" hxm))
		  (dlinit hxm "Init_libxg"))))))

(if (and (not (defined? 'red-pixel)) 
	 (provided? 'snd-motif))
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
(define oscope-input-data (make-sound-data 1 256))
(define cycle-start 0)
(define oscope-power #f)
(define oscope-frozen #f)
(define oscope-input-port #f)
(define oscope-graph-data #f)
(define oscope-input-frames #f)
(define oscope-graph #f)

(define (power-func off-func)
  (set! oscope-power (not oscope-power))
  (if oscope-power
      (begin
	(set! oscope-input-port (mus-audio-open-input mus-audio-microphone audio-srate 1 mus-lshort 512))
	(if (= oscope-input-port -1)
	    ;; ask card what it wants
	    (let ((vals (make-vct 32))
		  (chans 1)
		  (bytes 512))
	      (mus-audio-mixer-read mus-audio-line-in mus-audio-format 32 vals)
	      (let ((fmt (inexact->exact (vct-ref vals 1))))
		(mus-audio-mixer-read mus-audio-line-in mus-audio-channel 32 vals)
		(set! chans (inexact->exact (vct-ref vals 0)))
		(let ((err (mus-audio-mixer-read mus-audio-line-in mus-audio-samples-per-channel 2 vals)))
		  (if (not (= err -1))
		      (set! oscope-input-frames (inexact->exact (vct-ref vals 0))))
		  (let* ((bps (mus-bytes-per-sample fmt)))
		    (set! bytes (* bps oscope-input-frames chans))
		    (set! oscope-input-port (catch #t
					 (lambda ()
					   (mus-audio-open-input mus-audio-line-in audio-srate chans fmt bytes))
					 (lambda args -1)))
		    (if (not (= oscope-input-port -1))
			(begin
			  (if (or (not (= (sound-data-chans oscope-input-data) chans))
				  (not (= (sound-data-length oscope-input-data) oscope-input-frames)))
			      (set! oscope-input-data (make-sound-data chans oscope-input-frames))))))))))
	(if (not (= oscope-input-port -1))
	    (begin
	      (do ()
		  ((or (not oscope-power) 
		       (c-g?)))
		(mus-audio-read oscope-input-port oscope-input-data oscope-input-frames)
		(if (not oscope-frozen)
		    (begin
		      (set! cycle-start (sound-data->sound-data oscope-input-data oscope-graph-data cycle-start oscope-input-frames cycle-length))
		      (if (< cycle-start oscope-input-frames)
			  (begin
			    (if (time-graph? oscope-graph 0) (update-time-graph oscope-graph 0))
			    (if (transform-graph? oscope-graph 0) (update-transform-graph oscope-graph 0)))))))
	      (if oscope-power ; C-g?
		  (begin
		    (off-func)
		    (set! oscope-power #f)))
	      (mus-audio-close oscope-input-port))
	    (snd-print ";can't open audio input?"))))
  #f)

(define (freeze-func)
  (set! oscope-frozen (not oscope-frozen))
  (if oscope-frozen
      (begin
	(if (time-graph? oscope-graph 0) (update-time-graph oscope-graph 0))
	(if (transform-graph? oscope-graph 0) (update-transform-graph oscope-graph 0))))
  #f)

(define (cycle-func size)
  (let ((old-length cycle-length)
	(old-frozen oscope-frozen))
    (set! oscope-frozen #t)
    (set! cycle-length size)
    (set! cycle-start 0)
    (if (< cycle-length old-length)
	(do ((i cycle-length (1+ i)))
	    ((>= i old-length))
	  (sound-data-set! oscope-graph-data 0 i 0.0)))
    (set! oscope-frozen old-frozen)
    #f))


(if (provided? 'snd-motif)
    ;; -------- motif case --------
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
							  XmNheight              600
							  XmNwidth               800
							  XmNbackground          (basic-color))))
	(XtVaSetValues (XmMessageBoxGetChild oscope-dialog XmDIALOG_OK_BUTTON)
		       (list XmNarmColor   (pushed-button-color)
			     XmNbackground (quit-button-color)))
	(XtAddCallback oscope-dialog 
		       XmNokCallback (lambda (w context info)
				       (set! oscope-power #f)
				       (XtUnmanageChild oscope-dialog)))
	(XmStringFree xdismiss)
	(XmStringFree titlestr)
	(XtManageChild oscope-dialog)
	(let* ((toppane (XtCreateManagedWidget "oscope-pane" xmFormWidgetClass oscope-dialog
					       (list XmNleftAttachment      XmATTACH_FORM
						     XmNrightAttachment     XmATTACH_FORM
						     XmNtopAttachment       XmATTACH_FORM
						     XmNbottomAttachment    XmATTACH_WIDGET
						     XmNbottomWidget        (XmMessageBoxGetChild oscope-dialog XmDIALOG_SEPARATOR))))
	       (bottom-row (XtCreateManagedWidget "oscope-row" xmRowColumnWidgetClass toppane
						  (list XmNleftAttachment      XmATTACH_FORM
							XmNrightAttachment     XmATTACH_FORM
							XmNtopAttachment       XmATTACH_NONE
							XmNbottomAttachment    XmATTACH_FORM
							XmNorientation         XmVERTICAL)))
	       (prow (XtCreateManagedWidget "oscope-row" xmRowColumnWidgetClass bottom-row
					    (list XmNleftAttachment      XmATTACH_FORM
						  XmNrightAttachment     XmATTACH_FORM
						  XmNtopAttachment       XmATTACH_FORM
						  XmNbottomAttachment    XmATTACH_NONE
						  XmNorientation         XmHORIZONTAL
						  XmNbackground          (basic-color))))
	       (power-button (XtCreateManagedWidget "power" xmToggleButtonWidgetClass prow 
						    (list    XmNbackground          (basic-color)
							     XmNselectColor         (red-pixel))))
	       (freeze-button (XtCreateManagedWidget "freeze" xmToggleButtonWidgetClass prow
						     (list    XmNbackground          (basic-color)
							      XmNselectColor         (red-pixel))))
	       (cycle-title (XmStringCreate "cycle length" XmFONTLIST_DEFAULT_TAG))
	       (cycle (XtCreateManagedWidget "oscope-cycle" xmScaleWidgetClass bottom-row 
					     (list XmNorientation   XmHORIZONTAL
						   XmNshowValue     #t
						   XmNminimum       32
						   XmNmaximum       max-cycle
						   XmNvalue         cycle-length
						   XmNdecimalPoints 0
						   XmNtitleString   cycle-title
						   XmNbackground    (basic-color))))
	       (mainform (XtCreateManagedWidget "oscope-form" xmFormWidgetClass toppane
						(list XmNleftAttachment      XmATTACH_FORM
						      XmNrightAttachment     XmATTACH_FORM
						      XmNtopAttachment       XmATTACH_FORM
						      XmNbottomAttachment    XmATTACH_WIDGET
						      XmNbottomWidget        bottom-row
						      XmNbackground          (basic-color)))))
	  (set! oscope-graph (make-variable-graph mainform "input" max-cycle audio-srate))
	  (set! oscope-graph-data (channel-data oscope-graph 0))
	  (set! oscope-input-frames 256)
	  (set! (right-sample oscope-graph 0) cycle-length)
	  (set! (max-transform-peaks oscope-graph 0) 10)
	  (XtAddCallback cycle XmNvalueChangedCallback (lambda (w context info) (set! cycle-length (.value info))))
	  (XtAddCallback cycle XmNdragCallback (lambda (w context info) (cycle-func (.value info))))
	  (XtAddCallback freeze-button XmNvalueChangedCallback (lambda (w context info) (freeze-func)))
	  (XtAddCallback power-button XmNvalueChangedCallback 
			 (lambda (w context info) 
			   (power-func (lambda () 
					 (XmToggleButtonSetValue power-button XmUNSET #f)))))
	  (list oscope-graph oscope-graph-data))))

    ;; -------- gtk case --------
    (define (make-oscope)
      (let ((dismiss-button (gtk_button_new_with_label "Dismiss")))
	(gtk_widget_set_name dismiss-button "quit_button")
	(set! oscope-dialog (gtk_dialog_new))
	(gtk_window_set_title (GTK_WINDOW oscope-dialog) "Oscilloscope")
	(gtk_container_set_border_width (GTK_CONTAINER oscope-dialog) 10)
	(gtk_window_set_default_size (GTK_WINDOW oscope-dialog) 800 600)
	(gtk_window_set_resizable (GTK_WINDOW oscope-dialog) #t)
	(gtk_widget_realize oscope-dialog)
	(g_signal_connect oscope-dialog "delete_event" (lambda (w ev data) 
							 (set! oscope-power #f)
							 (gtk_widget_hide oscope-dialog) 
							 #t) 
			  #f)
	(gtk_box_pack_start (GTK_BOX (.action_area (GTK_DIALOG oscope-dialog))) dismiss-button #t #t 10)
	(g_signal_connect dismiss-button "clicked" (lambda (w data)
						     (set! oscope-power #f)
						     (gtk_widget_hide oscope-dialog)) 
			  #f)
	(gtk_widget_show dismiss-button)
	;; to change button color:   gtk_widget_modify_base(w, GTK_STATE_NORMAL, col);
	;;                           gtk_widget_modify_base(w, GTK_STATE_PRELIGHT, col);
	(let ((mainform (.vbox (GTK_DIALOG oscope-dialog))))
	  (set! oscope-graph (make-variable-graph mainform "input" max-cycle audio-srate))
	  (let* ((hbox (gtk_hbox_new #f 0))
		 (power-button (gtk_toggle_button_new_with_label "power"))
		 (freeze-button (gtk_toggle_button_new_with_label "freeze")))
	    (set! oscope-graph-data (channel-data oscope-graph 0))
	    (set! oscope-input-frames 256)
	    (gtk_box_pack_start (GTK_BOX mainform) hbox #f #f 4)
	    (gtk_widget_show hbox)
	    (gtk_box_pack_start (GTK_BOX hbox) power-button #f #f 6)
	    (gtk_box_pack_start (GTK_BOX hbox) freeze-button #f #f 6)
	    (gtk_widget_show power-button)
	    (gtk_widget_show freeze-button)
	    (let* ((adj (gtk_adjustment_new cycle-length 32 max-cycle 1.0 10.0 1.0))
		   (scale (gtk_hscale_new (GTK_ADJUSTMENT adj)))
		   (label (gtk_label_new "cycle length")))
	      (gtk_range_set_update_policy (GTK_RANGE (GTK_SCALE scale)) GTK_UPDATE_CONTINUOUS)
	      (gtk_scale_set_digits (GTK_SCALE scale) 0)
	      (gtk_scale_set_value_pos (GTK_SCALE scale) GTK_POS_TOP)
	      (gtk_scale_set_draw_value (GTK_SCALE scale) #t)
	      (gtk_box_pack_start (GTK_BOX mainform) scale #f #f 0)
	      (gtk_widget_show scale)
	      (gtk_box_pack_start (GTK_BOX mainform) label #f #f 0)
	      (gtk_widget_show label)
	      (g_signal_connect adj "value_changed" (lambda (w d) (cycle-func (inexact->exact (.value (GTK_ADJUSTMENT adj)))) #f))
	      (set! (right-sample oscope-graph 0) cycle-length)
	      (set! (max-transform-peaks oscope-graph 0) 10)
	      (g_signal_connect freeze-button "toggled" (lambda (w d) (freeze-func) #f))
	      (g_signal_connect power-button "toggled" (lambda (w d) (power-func (lambda () #f)) #f))
	      (gtk_widget_show_all oscope-dialog)
	      (list oscope-graph oscope-graph-data)))))))
    
(define oscope (make-oscope))
    
