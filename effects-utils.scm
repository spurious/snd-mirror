(use-modules (ice-9 format) (ice-9 optargs))

(provide 'snd-effects-utils.scm)

(if (not (provided? 'snd-motif)) (snd-error "effects-utils.scm is Motif-specific"))

(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
	  (snd-error (format #f "new-effects.scm needs the xm module: ~A" hxm))
	  (dlinit hxm "Init_libxm"))))

(if (not (defined? 'raise-dialog))
    (define (raise-dialog w)
      "(raise-dialog w) tries to put 'w' on top of any widgets that are obscuring it"
      (if (and (Widget? w) 
	       (XtIsManaged w))
	  (let ((parent (XtParent w)))
	    (if (and (Widget? parent)
		     (XtIsSubclass parent xmDialogShellWidgetClass))
		(XtPopup parent XtGrabNone))))))

(define (activate-dialog dialog)
  "(activate-dialog dialog) makes 'dialog' active and brings it to the top of the currently displayed widgets"
  (if (not (XtIsManaged dialog))
      (XtManageChild dialog)
      (raise-dialog dialog)))

(if (not (defined? 'for-each-child))
    (define (for-each-child w func)
      "(for-each-child w func) applies 'func' to 'w' and to its descendents"
      (func w)
      (if (XtIsComposite w)
	  (for-each 
	   (lambda (n)
	     (for-each-child n func))
	   (cadr (XtGetValues w (list XmNchildren 0) 1))))))

(define use-combo-box-for-fft-size #f) ; cross-synthesis fft size: radio-buttons or combo-box choice

(define (current-screen)
  "(current-screen) returns the current X screen number of the current display"
  (DefaultScreenOfDisplay 
    (XtDisplay (cadr (main-widgets)))))

(define (all-chans)
  "(all-chans) returns a list of all current sound indices and channel numbers"
  (let ((sndlist '())
	(chnlist '()))
    (for-each (lambda (snd)
		(do ((i (1- (channels snd)) (1- i)))
		    ((< i 0))
		  (set! sndlist (cons snd sndlist))
		  (set! chnlist (cons i chnlist))))
	      (sounds))
    (list sndlist chnlist)))

(define (update-label effects)
  "(update-label effects) evaluates the elements of the list 'effects'"
  (if (not (null? effects))
      (begin
	((car effects))
	(update-label (cdr effects)))))

(define (effect-target-ok target)
  "(effect-target-ok target) returns #t if the current effect's chosen target is ready"
  (if (eq? target 'sound) 
      (not (null? (sounds)))
      (if (eq? target 'selection) 
	  (selection?)
	  (and (selected-sound)
	       (>= (length (marks (selected-sound) (selected-channel))) 2)))))

(define* (make-effect-dialog label ok-callback help-callback reset-callback :optional target-ok-callback)
  "(make-effect-dialog label ok-callback help-callback reset-callback :optional target-ok-callback) makes a standard effects dialog"
  ;; make a standard dialog
  (let* ((xdismiss (XmStringCreate "Dismiss" XmFONTLIST_DEFAULT_TAG))
	 (xhelp (XmStringCreate "Help" XmFONTLIST_DEFAULT_TAG))
	 (xok (XmStringCreate "DoIt" XmFONTLIST_DEFAULT_TAG))
	 (titlestr (XmStringCreate label XmFONTLIST_DEFAULT_TAG))
	 (new-dialog (XmCreateTemplateDialog
		       (cadr (main-widgets)) label
		       (list XmNcancelLabelString   xdismiss
			     XmNhelpLabelString     xhelp
			     XmNokLabelString       xok
			     XmNautoUnmanage        #f
			     XmNdialogTitle         titlestr
			     XmNresizePolicy        XmRESIZE_GROW
			     XmNnoResize            #f
			     XmNbackground          (basic-color)
			     XmNtransient           #f))))
    (for-each
     (lambda (button color)
       (XtVaSetValues
	 (XmMessageBoxGetChild new-dialog button)
	 (list XmNarmColor   (pushed-button-color)
	       XmNbackground color)))
     (list XmDIALOG_HELP_BUTTON XmDIALOG_CANCEL_BUTTON XmDIALOG_OK_BUTTON)
     (list (help-button-color) (quit-button-color) (doit-button-color)))
    
    (XtAddCallback new-dialog XmNcancelCallback (lambda (w c i) (XtUnmanageChild new-dialog)))
    (XtAddCallback new-dialog XmNhelpCallback help-callback)  ; "Help"
    (XtAddCallback new-dialog XmNokCallback ok-callback)      ; "DoIt"

    (if reset-callback
	;; add a Reset button
	(let ((reset-button (XtCreateManagedWidget "Reset" xmPushButtonWidgetClass new-dialog
			      (list XmNbackground (reset-button-color)
				    XmNforeground (BlackPixelOfScreen (current-screen))
				    XmNarmColor   (pushed-button-color)))))
	  (XtAddCallback reset-button XmNactivateCallback reset-callback)))

    (XmStringFree xhelp)
    (XmStringFree xok)
    (XmStringFree xdismiss)
    (XmStringFree titlestr)

    (if target-ok-callback
	(begin
	  (XtSetSensitive (XmMessageBoxGetChild new-dialog XmDIALOG_OK_BUTTON) (target-ok-callback))
	  (add-watcher (lambda () (XtSetSensitive (XmMessageBoxGetChild new-dialog XmDIALOG_OK_BUTTON) (target-ok-callback)))))
	(begin
	  (XtSetSensitive (XmMessageBoxGetChild new-dialog XmDIALOG_OK_BUTTON) (not (null? (sounds))))
	  (add-watcher (lambda () (XtSetSensitive (XmMessageBoxGetChild new-dialog XmDIALOG_OK_BUTTON) (not (null? (sounds))))))))

    new-dialog))


;;; replacement for change-menu-label
(define (change-label widget new-label)
  "(change-label widget new-label) changes the label of 'widget' to be 'new-label'"
  (if (provided? 'xg)
      (gtk_label_set_text (GTK_LABEL (gtk_bin_get_child (GTK_BIN widget))) new-label)
      (if (provided? 'xm)
         (let ((str (XmStringCreateLocalized new-label)))
           (XtSetValues widget (list XmNlabelString str))
           (XmStringFree str)))))


;;; -------- log scaler widget

(define log-scale-ticks 500) ; sets precision (to some extent) of slider 

(define (scale-log->linear lo val hi)
  "(scale-log->linear lo val hi) given user-relative low..val..hi returns val as scale-relative (0..log-scale-ticks)"
  (let* ((log2 (log 2.0)) ; using log 2 here to get equally spaced octaves
	 (log-lo (/ (log (max lo 1.0)) log2))
	 (log-hi (/ (log hi) log2))
	 (log-val (/ (log val) log2)))
    (inexact->exact (floor (* log-scale-ticks (/ (- log-val log-lo) (- log-hi log-lo)))))))
  
(define (scale-linear->log lo val hi)
  "(scale-linear->log lo val hi) given user-relative lo..hi and scale-relative val, returns the user-relative val"
  ;; since log-scale widget assumes 0..log-scale-ticks, val can be used as ratio (log-wise) between lo and hi
  (let* ((log2 (log 2.0))
	 (log-lo (/ (log (max lo 1.0)) log2))
	 (log-hi (/ (log hi) log2))
	 (log-val (+ log-lo (* (/ val log-scale-ticks) (- log-hi log-lo)))))
    (expt 2.0 log-val)))

(define (scale-log-label lo val hi)
  "(scale-log-label lo val hi) makes a log scale label"
  (format #f "~,2F" (scale-linear->log lo val hi)))
	  
(define (create-log-scale-widget parent title low initial high callback scale)
  "(create-log-scale-widget parent title low initial high callback scale) returns a log scale widget"
  (let* ((label (XtCreateManagedWidget (format #f "~,2F" initial) xmLabelWidgetClass parent
	   (list XmNbackground          (basic-color))))
	 (scale (XtCreateManagedWidget "scale" xmScaleWidgetClass parent
                  (list XmNorientation   XmHORIZONTAL
			XmNshowValue     #f
			XmNminimum       0
			XmNmaximum       log-scale-ticks
			XmNvalue         (inexact->exact (floor (scale-log->linear low initial high)))
			XmNdecimalPoints 0
			XmNtitleString   title
			XmNbackground    (basic-color)))))
    (XtAddCallback scale XmNvalueChangedCallback
		    (lambda (widget context info)
		      (change-label label (scale-log-label low (.value info) high))))
    (XtAddCallback scale XmNdragCallback
		    (lambda (widget context info)
		      (change-label label (scale-log-label low (.value info) high))))
    scale))


;;; -------- semitone scaler widget
;;; 
;;; set up like log scale (use 'semi in place of 'log),
;;;   to get the ratio from the semitones, use (expt 2.0 (/ value 12.0)) -- semitones->ratio below					 

(define semi-range 24) ; 2 octaves either way

(define (semi-scale-label val)
  "(semi-scale-label val) makes a semitone label"
  (format #f "semitones: ~D" (- val semi-range)))

(define (semitones->ratio val)
  "(semitones->ratio val) takes a semitone number 'val' and returns the corresponding float ratio"
  (expt 2.0 (/ val 12.0)))

(define (ratio->semitones ratio)
  "(ratio->semitones ratio) takes a float ratio and returns the corresponding number of semitones"
  (inexact->exact (round (* 12 (/ (log ratio) (log 2.0))))))
	  
(define (create-semi-scale-widget parent title initial callback)
  "(create-semi-scale-widget parent title initial callback) returns a semitone scale widget"
  (let* ((label (XtCreateManagedWidget (format #f "semitones: ~D" (ratio->semitones initial)) xmLabelWidgetClass parent
	   (list XmNbackground          (basic-color))))
	 (scale (XtCreateManagedWidget "scale" xmScaleWidgetClass parent
                  (list XmNorientation   XmHORIZONTAL
			XmNshowValue     #f
			XmNminimum       0
			XmNmaximum       (* 2 semi-range)
			XmNvalue         (+ semi-range (ratio->semitones initial))
			XmNdecimalPoints 0
			XmNtitleString   title
			XmNbackground    (basic-color)))))
    (XtAddCallback scale XmNvalueChangedCallback
		    (lambda (widget context info)
		      (change-label label (semi-scale-label (.value info)))))
    (XtAddCallback scale XmNdragCallback
		    (lambda (widget context info)
		      (change-label label (semi-scale-label (.value info)))))
    scale))

(define* (add-sliders dialog sliders)
  "(add-sliders dialog sliders) takes 'sliders', a list of lists, each inner list being (title low initial high callback scale ['log]) \
and returns a list of widgets (for reset callbacks)"
  (let* ((mainfrm (XtCreateManagedWidget "formd" xmFormWidgetClass dialog
                  (list XmNleftAttachment      XmATTACH_FORM
                        XmNrightAttachment     XmATTACH_FORM
                        XmNtopAttachment       XmATTACH_FORM
                        XmNbottomAttachment    XmATTACH_WIDGET
                        XmNbottomWidget        (XmMessageBoxGetChild dialog XmDIALOG_SEPARATOR)
                        XmNbackground          (highlight-color))))
         (mainform (XtCreateManagedWidget "formd" xmRowColumnWidgetClass mainfrm
                  (list XmNleftAttachment      XmATTACH_FORM
                        XmNrightAttachment     XmATTACH_FORM
                        XmNbackground          (highlight-color)
                        XmNorientation         XmVERTICAL))))
    (map
     (lambda (slider-data)
       (let* ((title (XmStringCreate (list-ref slider-data 0) XmFONTLIST_DEFAULT_TAG))
	      (low (list-ref slider-data 1))
	      (initial (list-ref slider-data 2))
	      (high (list-ref slider-data 3))
	      (func (list-ref slider-data 4))
	      (scale (list-ref slider-data 5))
	      (new-slider (if (= (length slider-data) 7)
			      (if (eq? (list-ref slider-data 6) 'log)
				  (create-log-scale-widget mainform title low initial high func scale)
				  (create-semi-scale-widget mainform title initial func))
			      (XtCreateManagedWidget (car slider-data) xmScaleWidgetClass mainform
			        (list XmNorientation   XmHORIZONTAL
				      XmNshowValue     #t
				      XmNminimum       (inexact->exact (floor (* low scale)))
				      XmNmaximum       (inexact->exact (floor (* high scale)))
				      XmNvalue         (inexact->exact (floor (* initial scale)))
				      XmNdecimalPoints (if (= scale 10000) 4 (if (= scale 1000) 3 (if (= scale 100) 2 (if (= scale 10) 1 0))))
				      XmNtitleString   title
				      XmNleftAttachment XmATTACH_FORM
				      XmNrightAttachment XmATTACH_FORM
				      XmNbackground    (basic-color))))))
	 (XmStringFree title)
	 (XtAddCallback new-slider XmNvalueChangedCallback func)
	 new-slider))
     sliders)))

