;;; snd-motif.scm -- Motif-related procedures (all use xm.so)
;;;
;;; (install-searcher proc) -- use proc as File Selection Box filter
;;; (zync) and (unzync) -- cause y-zoom sliders to move together
;;; (for-each-child w func) -- apply func to w and all its children
;;; (make-hidden-controls-dialog) -- add Options menu "Hidden controls" item that creates dialog to control these variables
;;; (create-fmv-dialog) for "real-time" control of the fm-violin in fmv.scm
;;; (make-pixmap strs) turns xpm-style description into pixmap
;;; (display-scanned-synthesis) opens a scanned-synthesis viewer
;;; (disable-control-panel) does away with the control panel


(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
	  (snd-error (format #f "snd-motif.scm needs the xm module: ~A" hxm))
	  (dlinit hxm "init_xm"))))


;;; -------- install-searcher --------
;;;
;;; replaces the current file search procedure in the File Selection Box
;;;
;;;    (install-searcher (lambda (file) (= (mus-sound-srate file) 44100)))
;;;    (install-searcher (lambda (file) (= (mus-sound-chans file) 4)))

(define (install-searcher proc)
  (define match-sound-files
    (lambda args
      "(match-sound-files func &optional dir) applies func to each sound file in dir and returns a list of files for which func does not return #f"
      (let* ((func (car args))
	     (matches '()))
	(for-each
	 (lambda (file)
	   (if (func file)
	       (set! matches (cons file matches))))
	 (sound-files-in-directory (if (null? (cdr args)) "." (cadr args))))
	matches)))
  (define (XmString->string str)
    (cadr (|XmStringGetLtoR str |XmFONTLIST_DEFAULT_TAG)))
  (define (XmStringTable->list st len)
    (|XmStringTableUnparse st len #f |XmCHARSET_TEXT |XmCHARSET_TEXT #f 0 |XmOUTPUT_ALL))
  (define (list->XmStringTable strs)
    (|XmStringTableParseStringArray strs (length strs) #f |XmCHARSET_TEXT #f 0 #f))
  (|XtSetValues (let ((m (open-file-dialog #f)))
		  (|Widget (list-ref (dialog-widgets) 6)))
		(list |XmNfileSearchProc
		       (lambda (widget info)
			 (let* ((dir (XmString->string (|dir info)))
				(files (match-sound-files proc dir))
				(fileTable (list->XmStringTable 
					    (map (lambda (n) 
						   (string-append dir n)) 
						 files))))
			   (|XtSetValues widget
					 (list |XmNfileListItems fileTable
					       |XmNfileListItemCount (length files)
					       |XmNlistUpdated #t))
			   (|XmStringTableFree fileTable (length files)))))))



;;; -------- zync and unzync: start or stop y-zoom slider sync --------
;;; 
;;; (i.e. when one y-zoom-slider changes position, all other channels in the sound change in parallel)
;;; (zync) to start and (unzync) to stop

(define (sync-y-zooms snd)
  (let ((calls '()))
    (do ((chn 0 (1+ chn)))
	((= chn (chans snd)))
      (let* ((zy (|Widget (list-ref (channel-widgets snd chn) 6)))
	     (zy-div (- 100.0 (cadr (|XtGetValues zy (list |XmNsliderSize 0))))))
	(set! calls
	      (cons (|XtAddCallback zy
		       |XmNdragCallback 
		         (lambda (w data info)
			   (let ((v (/ (|value info) zy-div)))
			     (do ((i 0 (1+ i)))
				 ((= i (chans snd)))
			       (if (not (= i chn))
				   (begin
				     (set! (y-zoom-slider snd i) (* v v))
				     (set! (y-position-slider snd i) (y-position-slider snd chn))))))))
			  calls))))
    (reverse calls)))

(define (unsync-y-zooms snd calls)
  (do ((chn 0 (1+ chn)))
      ((= chn (chans snd)))
    (let ((zy (|Widget (list-ref (channel-widgets snd chn) 6))))
      (|XtRemoveCallback zy |XmNdragCallback (car calls))
      (set! calls (cdr calls)))))

(define draggers '())

(define (dragger snd)
  (find-if (lambda (n) (= (car n) snd)) draggers))

(define (remove-dragger snd)
  (if (dragger snd) (apply unsync-y-zooms (dragger snd)))
  (set! draggers (remove-if (lambda (n) (= (car n) snd)) draggers)))

(define (add-dragger snd)
  (set! draggers (cons (list snd (sync-y-zooms snd)) draggers)))

(add-hook! close-hook remove-dragger)

(define (zync)
  (add-hook! after-open-hook add-dragger)
  (for-each
   (lambda (n)
     (if (not (dragger n))
	 (add-dragger n)))
   (sounds)))

(define (unzync)
  (remove-hook! after-open-hook add-dragger)
  (for-each
   (lambda (n)
     (if (dragger n)
	 (remove-dragger n)))
   (sounds)))



;;; -------- add our own pane to the channel section --------

(define (add-channel-pane snd chn name type args)
  (|XtCreateManagedWidget name type (|XtParent (|XtParent (|Widget (list-ref (channel-widgets snd chn) 7)))) args))

;;; -------- add our own pane to the sound section (underneath the controls in this case) --------

(define (add-sound-pane snd name type args)
  (|XtCreateManagedWidget name type (|Widget (car (sound-widgets snd))) args))

;;; -------- add our own pane to the overall Snd window (underneath the listener in this case) --------

(define (add-main-pane name type args)
  (|XtCreateManagedWidget name type (|Widget (list-ref (main-widgets) 3)) args))

;(add-channel-pane "new-pane" 
;		  |xmDrawingAreaWidgetClass 
;		  (list |XmNbackground (|Pixel (snd-pixel (graph-color)))
;			|XmNforeground (|Pixel (snd-pixel (data-color)))))

(define (remove-menu-bar-menu which)
  (|XtUnmanageChild (|Widget (list-ref (menu-widgets) which))))


;;; -------- apply func to every widget belonging to w (and w) --------

(define (for-each-child w func)
  (func w)
  (if (|XtIsComposite w)
      (for-each 
       (lambda (n)
	 (for-each-child n func))
       (cadr (|XtGetValues w (list |XmNchildren 0) 1)))))



;;; -------- disable control panel --------

(define (disable-control-panel snd)
  (let ((swc (|Widget (caddr (sound-widgets snd)))))
    (for-each-child 
     swc 
     (lambda (n) 
       (if (and (not (string=? (|XtName n) "snd-name-form")) 
		(not (string=? (|XtName (|XtParent n)) "snd-name-form")))
	   (|XtUnmanageChild n))))
    (|XtSetValues swc (list |XmNpaneMaximum 18 
			    |XmNpaneMinimum 18))
    (remove-from-menu 2 "Show controls")
    (|XtManageChild swc)))



;;; -------- bring possibly-obscured dialog to top

(define (raise-dialog w)
  (if (and (|Widget? w) 
	   (|XtIsManaged w))
      (let ((parent (|XtParent w)))
	(if (and (|Widget? parent)
		 (|XtIsSubclass parent |xmDialogShellWidgetClass))
	    (|XtPopup parent |XtGrabNone)))))



;;; -------- hidden controls panel --------

(define hidden-controls-dialog #f)
(define hidden-controls '())

(define hidden-controls-help 
"Expand-hop sets the time in seconds between successive grains.\n\
Expand-length sets the length of each grain.\n\
Expand-ramp sets the ramp-time in the grain envelope.\n\
Contrast-amp sets the prescaler for contrast-enhancement.\n\
Reverb-lowpass sets the feedback lowpass filter coeficient.\n\
Reverb-feedback sets the scaler on the feedback.\n\
")

(define (make-hidden-controls-dialog)
  (define (reset-all-sliders)
    (for-each
     (lambda (ctl)
       (set! ((caddr ctl) #t) (cadr ctl))
       (|XtSetValues (car ctl) 
		     (list |XmNvalue (inexact->exact (* (cadr ctl) 100)))))
     hidden-controls))
  (if (not (|Widget? hidden-controls-dialog))
      (let ((xdismiss (|XmStringCreate "Dismiss" |XmFONTLIST_DEFAULT_TAG))
	    (xhelp (|XmStringCreate "Help" |XmFONTLIST_DEFAULT_TAG))
	    (titlestr (|XmStringCreate "More Controls" |XmFONTLIST_DEFAULT_TAG))
	    (xreset (|XmStringCreate "Reset" |XmFONTLIST_DEFAULT_TAG)))
	(set! hidden-controls-dialog 
	      (|XmCreateTemplateDialog (|Widget (cadr (main-widgets))) "More Controls"
                (list |XmNcancelLabelString   xreset
		      |XmNokLabelString       xdismiss
		      |XmNhelpLabelString     xhelp
		      |XmNautoUnmanage        #f
		      |XmNdialogTitle         titlestr
		      |XmNresizePolicy        |XmRESIZE_GROW
	              |XmNnoResize            #f
		      |XmNtransient           #f
		      |XmNbackground          (|Pixel (snd-pixel (basic-color))))))

	(for-each
	 (lambda (button)
	   (|XtVaSetValues (|XmMessageBoxGetChild hidden-controls-dialog button)
			   (list |XmNarmColor   (|Pixel (snd-pixel (pushed-button-color)))
				 |XmNbackground (|Pixel (snd-pixel (basic-color))))))
	 (list |XmDIALOG_HELP_BUTTON |XmDIALOG_CANCEL_BUTTON |XmDIALOG_OK_BUTTON))

	(|XtAddCallback hidden-controls-dialog 
			|XmNokCallback (lambda (w context info)
					 (|XtUnmanageChild hidden-controls-dialog)))
	(|XtAddCallback hidden-controls-dialog 
			|XmNhelpCallback (lambda (w context info)
					   (help-dialog "More Controls" hidden-controls-help)))
	(|XtAddCallback hidden-controls-dialog
			|XmNcancelCallback (lambda (w context info)
					     (reset-all-sliders)))
	(|XmStringFree xhelp)
	(|XmStringFree xdismiss)
	(|XmStringFree titlestr)
	(|XmStringFree xreset)

	(let* ((mainform 
		(|XtCreateManagedWidget "formd" |xmRowColumnWidgetClass hidden-controls-dialog
                  (list |XmNleftAttachment      |XmATTACH_FORM
		        |XmNrightAttachment     |XmATTACH_FORM
		        |XmNtopAttachment       |XmATTACH_FORM
		        |XmNbottomAttachment    |XmATTACH_WIDGET
		        |XmNbottomWidget        (|XmMessageBoxGetChild hidden-controls-dialog |XmDIALOG_SEPARATOR)
                        |XmNorientation         |XmVERTICAL))))
	  (for-each
	   (lambda (lst)
	     (let* ((name (car lst))
		    (low (cadr lst))
		    (high (caddr lst))
		    (initial (list-ref lst 3))
		    (func (list-ref lst 4))
		    (title (|XmStringCreate name |XmFONTLIST_DEFAULT_TAG))
		    (slider (|XtCreateManagedWidget name |xmScaleWidgetClass mainform
			       (list |XmNorientation   |XmHORIZONTAL
				     |XmNshowValue     #t
				     |XmNminimum       (inexact->exact (* low 100))
				     |XmNmaximum       (inexact->exact (* high 100))
				     |XmNvalue         (inexact->exact (* initial 100))
				     |XmNdecimalPoints 2
				     |XmNtitleString   title
				     |XmNborderWidth   1
				     |XmNbackground    (|Pixel (snd-pixel (basic-color)))))))
	       (|XmStringFree title)
	       (set! hidden-controls (cons (list slider initial func) hidden-controls))
	       (|XtAddCallback slider
			       |XmNvalueChangedCallback 
				(lambda (w context info)
				  (set! (func #t) (/ (|value info) 100.0))))
	       (|XtAddCallback slider
			       |XmNdragCallback 
				(lambda (w context info)
				  (set! (func #t) (/ (|value info) 100.0))))))
	   (list (list "expand-hop" 0.01 1.0 0.05  expand-control-hop)
		 (list "expand-length" 0.01 .5 0.15 expand-control-length)
		 (list "expand-ramp" 0.01 .5 0.4 expand-control-ramp)
		 (list "contrast-amp" 0.0 2.0 1.0 contrast-control-amp)
		 (list "reverb-lowpass" 0.0 1.0 0.7 reverb-control-lowpass)
		 (list "reverb-feedback" 0.0 1.25 1.09 reverb-control-feedback))))
	(add-to-menu 3 "Hidden controls"
		     (lambda ()
		       (if (not (|XtIsManaged hidden-controls-dialog))
			   (|XtManageChild hidden-controls-dialog)
			   (raise-dialog hidden-controls-dialog)))))))


;;; -------- create-fmv-dialog --------

(define fmv-dialog #f)
(load "fmv.scm")

(define (create-fmv-dialog)
  ;; TODO: ramp the amp changes, add index and frequency controls, add spectrum display
  (define amplitude 0.2)
  (define running #f)
  (define audio-fd #f)
  (if (not (|Widget? fmv-dialog))
      (let ((xdismiss (|XmStringCreate "Dismiss" |XmFONTLIST_DEFAULT_TAG))
	    (xhelp (|XmStringCreate "Help" |XmFONTLIST_DEFAULT_TAG))
	    (titlestr (|XmStringCreate "Scaling" |XmFONTLIST_DEFAULT_TAG)))
	(set! fmv-dialog 
	      (|XmCreateTemplateDialog (|Widget (cadr (main-widgets))) "fm-violin"
                (list |XmNcancelLabelString   xdismiss
		      |XmNhelpLabelString     xhelp
		      |XmNautoUnmanage        #f
		      |XmNdialogTitle         titlestr
		      |XmNresizePolicy        |XmRESIZE_GROW
	              |XmNnoResize            #f
		      |XmNbackground          (|Pixel (snd-pixel (basic-color)))
		      |XmNtransient           #f) ))
	(|XtAddCallback fmv-dialog 
			|XmNcancelCallback (lambda (w context info)
					     (if running
						 (set! running #f))
					     (|XtUnmanageChild fmv-dialog)))
	(|XtAddCallback fmv-dialog 
			|XmNhelpCallback (lambda (w context info)
					   (snd-print "move the slider to affect the volume")))
	(|XmStringFree xhelp)
	(|XmStringFree xdismiss)
	(|XmStringFree titlestr)
	(let* ((mainform 
		(|XtCreateManagedWidget "formd" |xmRowColumnWidgetClass fmv-dialog
                  (list |XmNleftAttachment      |XmATTACH_FORM
		        |XmNrightAttachment     |XmATTACH_FORM
		        |XmNtopAttachment       |XmATTACH_FORM
		        |XmNbottomAttachment    |XmATTACH_WIDGET
		        |XmNbottomWidget        (|XmMessageBoxGetChild fmv-dialog |XmDIALOG_SEPARATOR)
			|XmNbackground          (|Pixel (snd-pixel (basic-color)))
			|XmNorientation         |XmHORIZONTAL)))
	       (button 
		(|XtCreateManagedWidget "play" |xmToggleButtonWidgetClass mainform
		  (list |XmNbackground  (|Pixel (snd-pixel (basic-color))))))
					
	       (scale
		(|XtCreateManagedWidget "" |xmScaleWidgetClass mainform
		  (list |XmNorientation |XmHORIZONTAL
			|XmNshowValue   #t
			|XmNbackground  (|Pixel (snd-pixel (basic-color)))
			|XmNvalue       (inexact->exact (* amplitude 100))
			|XmNmaximum     100
			|XmNdecimalPoints 2))))
      (|XtAddCallback scale 
		      |XmNvalueChangedCallback 
		       (lambda (w context info)
			 (set! amplitude (/ (|value info) 100.0))))
      (|XtAddCallback scale |XmNdragCallback 
		      (lambda (w context info)
			(set! amplitude (/ (|value info) 100.0))))
      (|XtAddCallback button |XmNvalueChangedCallback 
		      (lambda (w context info)
			(if running
			    (set! running #f)
			    (let* ((size 128)
				   (data (make-sound-data 1 size))
				   (v (make-fm-violin 440 amplitude :amp-env (lambda () amplitude)))
				   (bytes (* size 2))
				   (audio-fd (mus-audio-open-output mus-audio-default 22050 1 mus-lshort bytes)))
			      (set! running #t)
			      (if (not (= audio-fd -1))
				  (do ()
				      ((or (c-g?) (not running))
				       (begin
					 (set! running #f)
					 (mus-audio-close audio-fd)))
				    (do ((k 0 (1+ k)))
					((= k size))
				      (sound-data-set! data 0 k (v)))
				    (mus-audio-write audio-fd data size))
				  (set! running #f)))))))))
  (|XtManageChild fmv-dialog))



;;; -------- make-pixmap --------

(define arrow-strs (list
"16 12 6 1"
" 	c None s None"
".	c gray50"
"X	c black"
"o	c white"
"O	c yellow"
"-      c ivory2 s basiccolor"
"--------X---------"
"---------X--------"
"----------X-------"
"-----------X------"
"------------X-----"
"XXXXXXXXXXXXXX----"
"------------X-----"
"-----------X------"
"----------X-------"
"---------X--------"
"--------X---------"
"-------X----------"))

(define (make-pixmap widget strs) ; strs is list of strings as in arrow-strs above
  (let* ((attr (|XpmAttributes))
	 (symb (|XpmColorSymbol "basiccolor" #f (|Pixel (snd-pixel (basic-color)))))
	 (dpy (|XtDisplay widget))
	 (win (|XtWindow widget))
	 (scr (|DefaultScreen dpy))
	 (depth (cadr (|XtGetValues widget (list |XmNdepth 0))))
	 (colormap (cadr (|XtGetValues widget (list |XmNcolormap 0)))))
    (set! (|depth attr) depth)
    (set! (|colormap attr) colormap)
    (set! (|visual attr) (|DefaultVisual dpy scr))
    (set! (|colorsymbols attr) symb)
    (set! (|numsymbols attr) 1)
    (set! (|valuemask attr) (logior |XpmColorSymbols |XpmDepth |XpmColormap |XpmVisual))
    (|XpmCreatePixmapFromData dpy win strs attr)))

; (|XtSetValues (|Widget (list-ref (sound-widgets) 8)) (list |XmNlabelPixmap (make-pixmap (|Widget (cadr (main-widgets))) arrow-strs)))

(define right-arrow (list
   #x00 #x04 #x10 #x08 #x00 #x10 #x04 #x20 #x00 #x40 #xa5 #xbf
   #x00 #x40 #x04 #x20 #x00 #x10 #x10 #x08 #x00 #x04 #x00 #x00))

(define (bitmap->pixmap widget bits width height)
  (|XCreateBitmapFromData (|XtDisplay widget) (|XtWindow widget) bits width height))

; (|XtSetValues (|Widget (list-ref (sound-widgets) 8)) (list |XmNlabelPixmap (bitmap->pixmap (|Widget (list-ref (sound-widgets) 8)) iconw right-arrow 16 12)))



;;; -------- display-scanned-synthesis --------
;;;
;;; open a new main pane below the listener, with two sections
;;;  on the left various controls, on the right a graph
;;;  push 'start' to start the scanned synthesis display
;;;  if spring > mass, you'll get overflows
;;;
;;; TODO: add audio output

(define (display-scanned-synthesis)

  (define (add-main-pane name type args)
    (|XtCreateManagedWidget name type (|Widget (list-ref (main-widgets) 3)) args))

  (define compute-uniform-circular-string
    ;; copied from dsp.scm to simplify life
    (lambda (size x0 x1 x2 mass xspring damp)
      (define circle-vct-ref 
	(lambda (v i)
	  (if (< i 0)
	      (vct-ref v (+ size i))
	      (if (>= i size)
		  (vct-ref v (- i size))
		  (vct-ref v i)))))
      (let* ((dm (/ damp mass))
	     (km (/ xspring mass))
	     (denom (+ 1.0 dm))
	     (p1 (/ (+ 2.0 (- dm (* 2.0 km))) denom))
	     (p2 (/ km denom))
	     (p3 (/ -1.0 denom)))
	(do ((i 0 (1+ i)))
	    ((= i size))
	  (vct-set! x0 i (min (+ (* p1 (vct-ref x1 i))
				 (* p2 (+ (circle-vct-ref x1 (- i 1)) (circle-vct-ref x1 (+ i 1))))
				 (* p3 (vct-ref x2 i)))
			      1000.0)))
	(vct-fill! x2 0.0)
	(vct-add! x2 x1)
	(vct-fill! x1 0.0)
	(vct-add! x1 x0))))

  (let* ((mass 1.0)
	 (xspring 0.1)
	 (damp 0.0)
	 (bounds '())
	 (pts0 #f)
	 (pts1 #f)
	 (ax0 0) (ax1 0) (ay0 0) (ay1 0)
	 (gc (|GC (car (snd-gcs))))
	 (egc (|GC (list-ref (snd-gcs) 7)))
	 (app (|XtAppContext (car (main-widgets))))

	 ;; now set up a paned window in the main Snd window with controllers on the left and the graph on the right
	 (scan-outer (add-main-pane "Scanned Synthesis" |xmFormWidgetClass
				    (list |XmNbackground (|Pixel (snd-pixel (basic-color)))
					  |XmNpaneMinimum 320)))
	 (scan-row (|XtCreateManagedWidget "row" |xmRowColumnWidgetClass scan-outer
					   (list |XmNbackground       (|Pixel (snd-pixel (basic-color)))
						 |XmNorientation      |XmVERTICAL
						 |XmNleftAttachment   |XmATTACH_FORM
						 |XmNtopAttachment    |XmATTACH_FORM
						 |XmNbottomAttachment |XmATTACH_FORM
						 |XmNrightAttachment  |XmATTACH_POSITION
						 |XmNrightPosition    32)))

	 ;; the graph
	 (scan-pane (|XtCreateManagedWidget "draw" |xmDrawingAreaWidgetClass scan-outer
					    (list |XmNbackground       (|Pixel (snd-pixel (graph-color)))
						  |XmNforeground       (|Pixel (snd-pixel (data-color)))
						  |XmNleftAttachment   |XmATTACH_WIDGET
						  |XmNleftWidget       scan-row
						  |XmNtopAttachment    |XmATTACH_FORM
						  |XmNbottomAttachment |XmATTACH_FORM
						  |XmNrightAttachment  |XmATTACH_FORM)))

	 ;; the controllers
	 (scan-start (|XtCreateManagedWidget "Start" |xmPushButtonWidgetClass scan-row
					     (list |XmNbackground (|Pixel (snd-pixel (basic-color)))
						   |XmNarmColor   (|Pixel (snd-pixel (pushed-button-color))))))
	 (scan-continue (|XtCreateManagedWidget "Continue" |xmPushButtonWidgetClass scan-row
						(list |XmNbackground (|Pixel (snd-pixel (basic-color)))
						      |XmNarmColor   (|Pixel (snd-pixel (pushed-button-color))))))
	 (scan-stop (|XtCreateManagedWidget "Stop" |xmPushButtonWidgetClass scan-row
					    (list |XmNbackground (|Pixel (snd-pixel (basic-color)))
						  |XmNarmColor   (|Pixel (snd-pixel (pushed-button-color))))))
	 
	 (size 128)
	 (gx0 (make-vct size))	   
	 (gx1 (make-vct size))	   
	 (gx2 (make-vct size))
	 (vect (make-vector (* 2 size)))
	 (work-proc 0))

    (define (y->grfy y range)
      (min ay1
	   (max ay0
		(inexact->exact
		 (+ ay0
		    (* range (- 10.0 y)))))))

    (define (draw-graph)
      (if (and (> ax1 ax0)
	       (> ay1 ay0))
	  (let ((diff (* 0.05 (- ay1 ay0))) ; assuming -10 to 10 
		(dpy (|XtDisplay scan-pane))
		(wn (|XtWindow scan-pane))
		(xincr (/ (- ax1 ax0) size)))
	    (if pts1
		(|XDrawLinesDirect dpy wn egc pts1 size 0)
		(|XFillRectangle dpy wn egc ; erase previous graph
				 (+ ax0 2)
				 ay0
				 (- ax1 ax0 2)
				 (- ay1 ay0)))
	    (do ((i 0 (1+ i))
		 (j 0 (+ j 2))
		 (xi ax0 (+ xi xincr)))
		((= i size))
	      (vector-set! vect j (inexact->exact xi))
	      (vector-set! vect (+ j 1) (y->grfy (vct-ref gx0 i) diff)))
	    (if pts1 (|freeXPoints pts1))
	    (set! pts0 (|vector->XPoints vect))
	    (set! pts1 pts0)
	    (|XDrawLinesDirect dpy wn gc pts0 size 0))))

    (define (redraw-graph)
      (set! bounds (draw-axes scan-pane gc "scanned synthesis" 0.0 1.0 -10.0 10.0))
      (set! ax0 (+ (car bounds) 2))
      (set! ax1 (caddr bounds))
      (set! ay1 (cadr bounds))
      (set! ay0 (cadddr bounds))
      (draw-graph))

    (define (tick-synthesis n)
      ;; background process
      (compute-uniform-circular-string size gx0 gx1 gx2 mass xspring damp)
      (draw-graph)
      #f)

    (define (stop-synthesis)
      (if (|XtWorkProcId? work-proc)
	  (|XtRemoveWorkProc work-proc))
      (set! work-proc 0))

    (define (start-synthesis)
      (stop-synthesis)
      (vct-fill! gx0 0.0)
      (vct-fill! gx1 0.0)
      (vct-fill! gx2 0.0)
      (do ((i 0 (1+ i)))
	  ((= i 12))
	(let ((val (sin (/ (* 2 pi i) 12.0))))
	  (vct-set! gx1 (+ i (- (/ size 4) 6)) val)))
      (set! work-proc (|XtAppAddWorkProc app tick-synthesis)))

    (define (continue-synthesis)
      (stop-synthesis)
      (set! work-proc (|XtAppAddWorkProc app tick-synthesis)))

    ;; controller callbacks
    (for-each 
     (lambda (data)
       (let* ((title (|XmStringCreate (car data) |XmFONTLIST_DEFAULT_TAG))
	      (button (|XtCreateManagedWidget (car data) |xmScaleWidgetClass scan-row
					      (list |XmNbackground    (|Pixel (snd-pixel (basic-color)))
						    |XmNorientation   |XmHORIZONTAL
						    |XmNshowValue     #t
						    |XmNminimum       (list-ref data 1)
						    |XmNmaximum       (list-ref data 2)
						    |XmNvalue         (list-ref data 3)
						    |XmNdecimalPoints (list-ref data 4)
						    |XmNtitleString   title))))
	 (|XtAddCallback button |XmNdragCallback (lambda (w c i) ((list-ref data 5) (|value i))))
	 (|XtAddCallback button |XmNvalueChangedCallback (lambda (w c i) ((list-ref data 5) (|value i))))
	 (|XmStringFree title)))
     (list (list "mass" 1 200 100 2 (lambda (val) (set! mass (/ val 100.0))))
	   (list "spring" 1 100 10 2 (lambda (val) (set! xspring (/ val 100.0))))
	   (list "damping" 0 100 0 4 (lambda (val) (set! damp (/ val 10000.0))))))

    (let* ((scan-size (|XtCreateManagedWidget "srow" |xmFormWidgetClass scan-row
					      (list  |XmNbackground (|Pixel (snd-pixel (basic-color))))))
	   (scan-label (|XtCreateManagedWidget "Size:" |xmLabelWidgetClass scan-size
					       (list |XmNbackground       (|Pixel (snd-pixel (basic-color)))
						     |XmNleftAttachment   |XmATTACH_FORM
						     |XmNtopAttachment    |XmATTACH_FORM
						     |XmNbottomAttachment |XmATTACH_FORM
						     |XmNrightAttachment  |XmATTACH_NONE)))
	   (scan-text (|XtCreateManagedWidget "stext" |xmTextFieldWidgetClass scan-size
					      (list |XmNbackground       (|Pixel (snd-pixel (basic-color)))
						    |XmNvalue            (number->string size)
						    |XmNleftAttachment   |XmATTACH_WIDGET
						    |XmNleftWidget       scan-label
						    |XmNtopAttachment    |XmATTACH_FORM
						    |XmNbottomAttachment |XmATTACH_FORM
						    |XmNrightAttachment  |XmATTACH_FORM))))

      (|XtAddEventHandler scan-text |EnterWindowMask #f
			  (lambda (w context ev flag)
			    (|XmProcessTraversal w |XmTRAVERSE_CURRENT)
			    (|XtSetValues w (list |XmNbackground (|WhitePixelOfScreen 
								   (|DefaultScreenOfDisplay 
								     (|XtDisplay w)))))))
      (|XtAddEventHandler scan-text |LeaveWindowMask #f
			  (lambda (w context ev flag)
			    (|XtSetValues w (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
      (|XtAddCallback scan-text |XmNactivateCallback 
		      (lambda (w c i)
			(stop-synthesis)
			(set! size (string->number (cadr (|XtGetValues scan-text (list |XmNvalue 0)))))
			(set! gx0 (make-vct size))	   
			(set! gx1 (make-vct size))	   
			(set! gx2 (make-vct size))
			(set! vect (make-vector (* size 2))))))

    (|XtAddCallback scan-pane |XmNresizeCallback (lambda (w context info) (redraw-graph)))
    (|XtAddCallback scan-pane |XmNexposeCallback (lambda (w context info) (redraw-graph)))
    (|XtAddEventHandler scan-pane |ButtonPressMask #f
			(lambda (w context ev flag)
			  (if (not (|XtWorkProcId? work-proc))
			      (if (= (|button ev) 2)
				  (continue-synthesis)
				  (start-synthesis))
			      (stop-synthesis))))

    (|XtAddCallback scan-start |XmNactivateCallback (lambda (w c i) (start-synthesis)))
    (|XtAddCallback scan-continue |XmNactivateCallback (lambda (w c i) (continue-synthesis)))
    (|XtAddCallback scan-stop |XmNactivateCallback (lambda (w c i) (stop-synthesis)))
    #t ; for slightly prettier listener output
    ))



;;; -------- run-spectro-display -------- 
;;; running spectrogram display, but ugly -- using XFillPolygon for hidden-line removal was flashing too much
(define spectro-micro-drawer #f)

(define (run-spectro-display fft-size traces)

  (define (add-main-pane name type args)
    (|XtCreateManagedWidget name type (|Widget (list-ref (main-widgets) 3)) args))

  (let ((ffts (make-vector traces #f))
	(vf (make-vct fft-size))
	(vect (make-vector (/ fft-size 2)))
	(gc (|GC (car (snd-gcs))))
	(egc (|GC (list-ref (snd-gcs) 7)))
	(app (|XtAppContext (car (main-widgets))))
	(x0 0)
	(x1 0)
	(y0 0)
	(y1 0)
	(yhop 2)
	(num (inexact->exact (/ 22050 (* 10 fft-size))))
	(numctr 0)
	(drawer #f))

    (define (redraw-graph)
      (set! x1 (- (cadr (|XtGetValues drawer (list |XmNwidth 0))) 10))
      (set! y0 (- (cadr (|XtGetValues drawer (list |XmNheight 0))) 10))
      (do ((i 0 (1+ i)))
	  ((= i traces))
	(if (vector-ref ffts i)
	    (|freeXPoints (vector-ref ffts i))))
      (set! traces (inexact->exact (/ (- y0 y1) yhop)))
      (set! ffts (make-vector traces #f)))

    (define (y->grfy y)
      (min y0
	   (max y1
		(inexact->exact
		 (- y0 (* y 20))))))

    (if (not spectro-micro-drawer)
	(set! spectro-micro-drawer 
	      (add-main-pane "Scanned Synthesis" |xmDrawingAreaWidgetClass
			     (list |XmNbackground    (|Pixel (snd-pixel (graph-color)))
				   |XmNforeground    (|Pixel (snd-pixel (data-color)))
				   |XmNpaneMinimum   200))))
    
    (set! drawer spectro-micro-drawer)

    (|XtAddCallback drawer |XmNresizeCallback (lambda (w context info) (redraw-graph)))
    (|XtAddCallback drawer |XmNexposeCallback (lambda (w context info) (redraw-graph)))

    (set! x0 10)
    (set! x1 (- (cadr (|XtGetValues drawer (list |XmNwidth 0))) 10))
    (set! y1 10)
    (set! y0 (- (cadr (|XtGetValues drawer (list |XmNheight 0))) 10))

    (let* ((in-port (mus-audio-open-input mus-audio-default 22050 1 mus-lshort (* 2 fft-size)))
	   (data (make-sound-data 1 fft-size))
	   (ind 0)
	   (dpy (|XtDisplay drawer))
	   (wn (|XtWindow drawer))
	   (xincr (/ (* 4 (- x1 x0 traces)) fft-size)))
      (do ()
	  ((c-g?))
	(mus-audio-read in-port data fft-size)
	(set! numctr (+ numctr 1))
	(if (>= numctr num)
	    (begin
	      (sound-data->vct data 0 vf)
	      (snd-spectrum vf blackman2-window fft-size #t)
	      (vct-scale! vf fft-size)
	      (do ((i 0 (1+ i))
		   (xi x0 (+ xi xincr))
		   (j 0 (+ j 2)))
		  ((= j (/ fft-size 2)))
		(vector-set! vect j (inexact->exact xi))
		(vector-set! vect (+ j 1) (y->grfy (vct-ref vf i))))
	      (if (vector-ref ffts 0) 
		  (begin
		    (|XDrawLinesDirect dpy wn egc (vector-ref ffts 0) (/ fft-size 4) 0)
		    (|freeXPoints (vector-ref ffts 0))))
	      (do ((i 0 (1+ i))
		   (yy0 (- y0 2) (- yy0 2)))
		  ((= i (- traces 1)))
		(vector-set! ffts i (vector-ref ffts (+ i 1)))
		(if (vector-ref ffts i)
		    (begin
		      (|XDrawLinesDirect dpy wn egc (vector-ref ffts i) (/ fft-size 4) 0)
		      (|moveXPoints (vector-ref ffts i) (/ fft-size 4) 1 -2)
		      (|XDrawLinesDirect dpy wn gc (vector-ref ffts i) (/ fft-size 4) 0))))
	      (vector-set! ffts (- traces 1) (|vector->XPoints vect))
	      (|XDrawLinesDirect dpy wn gc (vector-ref ffts (- traces 1)) (/ fft-size 4) 0)
	      (set! numctr 0))))
      (mus-audio-close in-port))))







;;; animated pixmaps
;;; spectral editing in new window
;;; panel of effects-buttons[icons] on right? (normalize, reverse, etc)
;;; panel of icons at top (cut/paste/undo/redo/save/play/play-selection
;;; bess-translations
;;; separate chan amp controls (from snd-gtk.scm)
