;;; snd-motif.scm -- Motif-related procedures (all use xm.so, most assume Motif 2)
;;;
;;; (install-searcher proc) -- use proc as File Selection Box filter, also install-searcher-with-colors
;;; (zync) and (unzync) -- cause y-zoom sliders to move together
;;; (for-each-child w func) -- apply func to w and all its children
;;; (make-hidden-controls-dialog) -- add Options menu "Hidden controls" item that creates dialog to control these variables
;;; (create-fmv-dialog) for "real-time" control of the fm-violin in fmv.scm
;;; (make-pixmap strs) turns xpm-style description into pixmap
;;; (display-scanned-synthesis) opens a scanned-synthesis viewer
;;; (disable-control-panel) does away with the control panel
;;; (add-mark-pane) adds a pane of mark locations to each channel that has marks
;;; (snd-clock-icon snd hour) show an animated clock icon
;;; (make-sound-box name parent select-func peak-func sounds args) makes a box of sound icons
;;; (show-smpte-label on-or-off) shows the current SMPTE frame number
;;; (make-level-meter parent width height args), (display-level data), (with-level-meters n) -- VU meters
;;; (make-channel-drop-site snd chn) -- add a drop site
;;; (select-file func &optional title dir filter help) starts a Snd-like File Selection Dialog running func if a file is selected
;;; (show-disk-space) adds a label to the minibuffer area showing the current free space 
;;; (keep-file-dialog-open-upon-ok) changes File:Open so that clicking "ok" does not "unmanage" the dialog
;;; (add-amp-control) adds another amp slider to the control panel
;;; (add-very-useful-icons) adds some very useful icons


(use-modules (ice-9 common-list))

(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
	  (snd-error (format #f "snd-motif.scm needs the xm module: ~A" hxm))
	  (dlinit hxm "init_xm"))))

(define (current-display)
  (|DefaultScreenOfDisplay 
    (|XtDisplay (|Widget (cadr (main-widgets))))))

(define (white-pixel)  (|WhitePixelOfScreen (current-display)))
(define (black-pixel)  (|BlackPixelOfScreen (current-display)))
(define (screen-depth) (|DefaultDepthOfScreen (current-display)))

;;; -------- apply func to every widget belonging to w (and w) --------

(define (for-each-child w func)
  (func w)
  (if (|XtIsComposite w)
      (for-each 
       (lambda (n)
	 (for-each-child n func))
       (cadr (|XtGetValues w (list |XmNchildren 0) 1)))))

(define (find-child widget name)
  (call-with-current-continuation
   (lambda (return)
     (for-each-child
      widget
      (lambda (child)
	(if (string=? (|XtName child) name)
	    (return child)))))))

(define (set-main-color-of-widget w)
  (for-each-child 
   w
   (lambda (n)
     (if (|XtIsWidget n)
	 (if (|XmIsScrollBar n)
	     (|XmChangeColor n (|Pixel (snd-pixel (position-color))))
	     (|XmChangeColor n (|Pixel (snd-pixel (basic-color)))))))))




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
			   (for-each (lambda (n) (|XmStringFree n)) fileTable))))))


;;; here's a fancier version that gets rid of the useless directory list,
;;;   and shows multi-channel files in color

(define install-searcher-with-colors
  (let* ((dialog (let ((m (open-file-dialog #f)))
		   (|Widget (list-ref (dialog-widgets) 6))))
	 ;; (|XtGetValues dialog (|XmNfileSearchProc 0)) to get the default
	 (shell (|Widget (cadr (main-widgets))))
	 (tags (list "one" "two" "three" "four"))
	 (colors (list "black" "red" "blue" "orange"))
	 (pixels (let* ((dpy (|XtDisplay shell))
			(scr (|DefaultScreen dpy))
			(cmap (|DefaultColormap dpy scr)))
		   (map
		    (lambda (color)
		      (let ((col (|XColor)))
			(if (= (|XAllocNamedColor dpy cmap color col col) 0)
			    (snd-error "can't allocate ~S" color)
			    (|pixel col))))
		    colors)))
	 (rendertable (|XmRenderTableAddRenditions 
			#f 
			(map (lambda (tag pix)
			       (|XmRenditionCreate 
				 (|Widget (cadr (main-widgets)))
				 tag
				 (list |XmNrenditionForeground pix
				       |XmNfontName "9x15"
 				       |XmNfontType |XmFONT_IS_FONT)))
			     tags pixels)
			(length tags)
			|XmMERGE_NEW)))
    (lambda (proc)
      (define match-sound-files
	(lambda args
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
      (|XtSetValues dialog
		(list |XmNfileSearchProc
		       (lambda (widget info)
			 (let* ((dir (XmString->string (|dir info)))  ; may need filter text here?
				(files (sort (map 
					      (lambda (n) 
						(string-append dir n)) 
					      (match-sound-files proc dir))
					     string<?))               ; alphabetical order
				(fileTable (map
					    (lambda (n)
					      (|XmStringGenerate 
						n #f |XmCHARSET_TEXT 
						(if (= (mus-sound-chans n) 1)
						    "one"
						    (if (= (mus-sound-chans n) 2)
							"two"
							(if (= (mus-sound-chans n) 4)
							    "four"
							    "three")))))
					    files)))
			   (|XtSetValues widget
					 (list |XmNfileListItems fileTable
					       |XmNfileListItemCount (length files)
					       |XmNlistUpdated #t))
			   (for-each (lambda (n) (|XmStringFree n)) fileTable)))))
      (|XtUnmanageChild (|XmFileSelectionBoxGetChild dialog |XmDIALOG_DIR_LIST))
      (|XtUnmanageChild (|XmFileSelectionBoxGetChild dialog |XmDIALOG_DIR_LIST_LABEL))
      (|XtSetValues (|XmFileSelectionBoxGetChild dialog |XmDIALOG_LIST)
		    (list |XmNrenderTable rendertable))
      (|XmFileSelectionDoSearch dialog #f))))
    
;(install-searcher-with-colors (lambda (file) #t))


;;; -------- keep-file-dialog-open-upon-ok
;;;
;;; change File:Open so that clicking "ok" does not "unmanage" the dialog

(define keep-file-dialog-open-upon-ok
  (let* ((dialog (let ((m (open-file-dialog #f)))
		   (|Widget (list-ref (dialog-widgets) 6)))))
    (lambda ()
      (|XtRemoveAllCallbacks dialog |XmNokCallback) ; remove built-in version
      (|XtAddCallback dialog |XmNokCallback
		      (lambda (widget context info)
			;; same as built-in "ok" callback, but does not "unmanage" the dialog
			(let ((filename (cadr (|XmStringGetLtoR (|value info) |XmFONTLIST_DEFAULT_TAG))))
			  (if (not (file-is-directory? filename))
			      (let ((snd (open-sound filename)))
				(select-channel 0))
			      (snd-error (format #f "~S is a directory" filename))))))
      'ok))) ; prettier in listener than printing out a callback procedure



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
    (cadr (|XpmCreatePixmapFromData dpy win strs attr))))

; (|XtSetValues (|Widget (list-ref (sound-widgets) 8)) (list |XmNlabelPixmap (make-pixmap (cadr (|Widget (cadr (main-widgets))) arrow-strs))))

;;; if you have a nice background pixmap, you can map it over all of Snd with:

;;; (load "backgrounds.scm")
;;; (define wd (make-pixmap (|Widget (cadr (main-widgets))) wood))
;;; (for-each-child (|Widget (cadr (main-widgets))) (lambda (w) (|XtSetValues w (list |XmNbackgroundPixmap wd))))


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
			    (|XtSetValues w (list |XmNbackground (white-pixel)))))
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


(define (close-scanned-synthesis-pane)
  (for-each-child 
   (|Widget (cadr (main-widgets)))  ; this is Snd's outermost shell
   (lambda (n)
     (if (string=? (|XtName n) "Scanned Synthesis")
	 (|XtUnmanageChild n)))))




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




;;; -------- add-mark-pane --------
;;;
;;; adds a pane to each channel giving the current mark locations (sample values)
;;;   these can be edited to move the mark, or deleted to delete the mark

(define (add-mark-pane)

  (define (find-mark-list snd chn dats)
    (if (not (null? dats))
	(let ((cur (car dats)))
	  (if (and (= (car cur) snd)
		   (= (cadr cur) chn))
	      (caddr cur)
	      (find-mark-list snd chn (cdr dats))))
	#f))

  (define mark-list-length
    (let ((mark-list-lengths '()))
      (define (remove-mark-list snd chn)
	(set! mark-list-lengths (remove-if 
				 (lambda (n) 
				   (and (= (car n) snd) 
					(= (cadr n) chn))) 
				 mark-list-lengths)))
      (make-procedure-with-setter
       (lambda (snd chn)
	 (or (find-mark-list snd chn mark-list-lengths)
	     0))
       (lambda (snd chn len)
	 (remove-mark-list snd chn)
	 (set! mark-list-lengths (cons (list snd chn len) mark-list-lengths))))))

  (define mark-list
    (let ((mark-lists '()))
      (make-procedure-with-setter
       (lambda (snd chn)
	 (let ((dat (find-mark-list snd chn mark-lists)))
	   (and dat
		(caddr dat))))
       (lambda (snd chn wid)
	 (set! mark-lists (cons (list snd chn wid) mark-lists))))))

  (define (deactivate-channel snd chn)
    (let ((current-mark-list-length (mark-list-length snd chn)))
      (if (and (> current-mark-list-length 0)
	       (|Widget? (mark-list snd chn)))
	  (for-each
	   (lambda (n)
	     (|XtUnmanageChild n))
	   (cadr (|XtGetValues (mark-list snd chn) (list |XmNchildren 0) 1))))))
  
  (define (make-mark-list snd chn)
    (let ((current-mark-list-length (mark-list-length snd chn)))
      (deactivate-channel snd chn)
      (if (not (|Widget? (mark-list snd chn)))
	  (let* ((mark-box (add-channel-pane snd chn "mark-box" |xmFormWidgetClass
					     (list |XmNbackground       (|Pixel (snd-pixel (basic-color)))
						   |XmNorientation      |XmVERTICAL
						   |XmNpaneMinimum      100
						   |XmNbottomAttachment |XmATTACH_FORM)))
		 (mark-label (|XtCreateManagedWidget "Marks" |xmLabelWidgetClass mark-box
						    (list |XmNbackground      (|Pixel (snd-pixel (highlight-color)))
							  |XmNleftAttachment  |XmATTACH_FORM
							  |XmNrightAttachment |XmATTACH_FORM
							  |XmNalignment       |XmALIGNMENT_CENTER
							  |XmNtopAttachment   |XmATTACH_FORM)))
		 (mark-scroller (|XtCreateManagedWidget "mark-scroller" |xmScrolledWindowWidgetClass mark-box
							(list |XmNbackground             (|Pixel (snd-pixel (basic-color)))
							      |XmNscrollingPolicy        |XmAUTOMATIC
							      |XmNscrollBarDisplayPolicy |XmSTATIC
							      |XmNleftAttachment         |XmATTACH_FORM
							      |XmNrightAttachment        |XmATTACH_FORM
							      |XmNtopAttachment          |XmATTACH_WIDGET
							      |XmNtopWidget              mark-label
							      |XmNbottomAttachment       |XmATTACH_FORM)))
		 (mlist (|XtCreateManagedWidget "mark-list" |xmRowColumnWidgetClass mark-scroller
						(list |XmNorientation      |XmVERTICAL
						      |XmNtopAttachment    |XmATTACH_FORM
						      |XmNbottomAttachment |XmATTACH_FORM
						      |XmNspacing          0))))
	    (set-main-color-of-widget mark-scroller)
	    (|XtSetValues mark-box (list |XmNpaneMinimum 1))
	    (set! (mark-list snd chn) (list snd chn mlist))))
      (let ((new-marks (marks snd chn)))
	(if (> (length new-marks) current-mark-list-length)
	    (let* ((lst (mark-list snd chn)))
	      (do ((i current-mark-list-length (1+ i)))
		  ((= i (length new-marks)))
		(let ((tf (|XtCreateWidget "field" |xmTextFieldWidgetClass lst
					   (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
		  (|XtAddCallback tf |XmNfocusCallback
				  (lambda (w c i)
				    (|XtSetValues w (list |XmNbackground (white-pixel)))))
		  (|XtAddCallback tf |XmNlosingFocusCallback
				  (lambda (w c i)
				    (|XtSetValues w (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
		  (|XtAddCallback tf |XmNactivateCallback
				  (lambda (w c i)
				    (let* ((id (cadr (|XtGetValues w (list |XmNuserData 0))))
					   (txt (cadr (|XtGetValues w (list |XmNvalue 0))))
					   (samp (if (and (string? txt) 
							  (> (string-length txt) 0))
						     (string->number txt)
						     #f)))
				      (if samp
					  (set! (mark-sample id) samp)
					  (delete-mark id))
				      (|XtSetValues w (list |XmNbackground (|Pixel (snd-pixel (basic-color))))))))))))
	(set! (mark-list-length snd chn) (length new-marks))
	(let* ((lst (mark-list snd chn)))
	  (call-with-current-continuation
	   (lambda (quit)
	     (for-each
	      (lambda (n)
		(if (null? new-marks) (quit #f))
		(if (|XmIsTextField n)
		    (begin
		      (|XtSetValues n (list |XmNvalue (number->string (mark-sample (car new-marks)))
					    |XmNuserData (car new-marks)))
		      (|XtManageChild n)
		      (set! new-marks (cdr new-marks)))))
	      (cadr (|XtGetValues lst (list |XmNchildren 0) 1)))))))
      #f))

  (define (remark id snd chn reason)
    (make-mark-list snd chn))

  (define (unremark snd)
    (do ((i 0 (1+ i)))
	((= i (chans snd)))
      (deactivate-channel snd i)))

  (define (open-remarks snd)
    (do ((i 0 (1+ i)))
	((= i (chans snd)))
      (add-hook! (edit-hook snd i) (lambda () make-mark-list snd i))
      (add-hook! (undo-hook snd i) (lambda () make-mark-list snd i))))

  (add-hook! mark-hook remark)
  (add-hook! close-hook unremark)
  (add-hook! after-open-hook open-remarks))


;;; -------- select-file --------
;;;
;;; (select-file func &optional title dir filter help)
;;;   starts a Snd-like File Selection Dialog running func if a file is selected
;;;
;;; (add-to-menu 0 "Insert File" 
;;;   (lambda () 
;;;     (select-file 
;;;       (lambda (filename)
;;;         (insert-sound filename))
;;;       "Insert File" "." "*" "file will be inserted at cursor")))

(define select-file

  (let ((file-selector-dialogs '()))
    ;; (list (list widget inuse func title help) ...)
    (define (find-free-dialog ds)
      (if (null? ds)
	  #f
	  (if (not (cadr (car ds)))
	      (begin
		(list-set! (car ds) 1 #t)
		(caar ds))
	      (find-free-dialog (cdr ds)))))
    (define (find-dialog wid ds)
      (if (null? ds)
	  #f
	  (if (equal? wid (caar ds))
	      (car ds)
	      (find-dialog wid (cdr ds)))))
    (lambda args
      ;; (file-select func title dir filter help)
      (let* ((func (if (> (length args) 0) (list-ref args 0) #f))
	     (title (if (> (length args) 1) (list-ref args 1) "select file"))
	     (dir (if (> (length args) 2) (list-ref args 2) "."))
	     (filter (if (> (length args) 3) (list-ref args 3) "*"))
	     (help (if (> (length args) 4) (list-ref args 4) #f))
	     (dialog (or (find-free-dialog file-selector-dialogs)
		 	 (let ((new-dialog (|XmCreateFileSelectionDialog 
					     (|Widget (cadr (main-widgets))) 
					     title
					     (list |XmNbackground (|Pixel (snd-pixel (basic-color)))))))
			   (|XtAddCallback new-dialog |XmNhelpCallback
					    (lambda (w c i)
					      (let ((lst (find-dialog w file-selector-dialogs)))
						(if (list-ref lst 4)
						    (help-dialog (list-ref lst 3) (list-ref lst 4))))))
			   (|XtAddCallback new-dialog |XmNokCallback 
					   (lambda (w c i)
					     (let ((lst (find-dialog w file-selector-dialogs)))
					       ((list-ref lst 2) (cadr (|XmStringGetLtoR (|value i) |XmFONTLIST_DEFAULT_TAG)))
					       (list-set! lst 1 #f)
					       (|XtUnmanageChild w))))
			   (|XtAddCallback new-dialog |XmNcancelCallback
					   (lambda (w c i)
					     (let ((lst (find-dialog w file-selector-dialogs)))
					       (list-set! lst 1 #f)
					       (|XtUnmanageChild w))))
			  (set! file-selector-dialogs (cons (list new-dialog #t func title help) file-selector-dialogs))
			  (set-main-color-of-widget new-dialog)
			  (|XtSetValues (|XmFileSelectionBoxGetChild new-dialog |XmDIALOG_DIR_LIST) 
					(list |XmNbackground (white-pixel)))
			  (|XtSetValues (|XmFileSelectionBoxGetChild new-dialog |XmDIALOG_LIST) 
					(list |XmNbackground (white-pixel)))
			  (|XtSetValues (|XtNameToWidget new-dialog "Cancel")
					(list |XmNarmColor (|Pixel (snd-pixel (pushed-button-color)))))
			  (|XtSetValues (|XtNameToWidget new-dialog "Help")
					(list |XmNarmColor (|Pixel (snd-pixel (pushed-button-color)))))
			  (|XtSetValues (|XtNameToWidget new-dialog "OK")
					(list |XmNarmColor (|Pixel (snd-pixel (pushed-button-color)))))
			  new-dialog))))
	(if (not help)
	    (|XtUnmanageChild (|XmFileSelectionBoxGetChild dialog |XmDIALOG_HELP_BUTTON))
	    (|XtManageChild (|XmFileSelectionBoxGetChild dialog |XmDIALOG_HELP_BUTTON)))
	(let ((dirstr (|XmStringCreateLocalized dir))
	      (patstr (|XmStringCreateLocalized filter))
	      (titlestr (|XmStringCreateLocalized title)))
	  (|XtSetValues dialog
			(list |XmNdirectory dirstr
			      |XmNpattern patstr
			      |XmNdialogTitle titlestr))
	  (|XmStringFree dirstr)
	  (|XmStringFree patstr)
	  (|XmStringFree titlestr)
	  (|XtManageChild dialog))))))

; (select-file (lambda (n) (snd-print n)))



;;; -------- snd-clock-icon --------
;;;
;;; a clock icon to replace Snd's hourglass
;;;   call from a work proc or whatever with hour going from 0 to 12 then #f

(define snd-clock-icon
  (let* ((shell (|Widget (list-ref (main-widgets) 1)))
	 (dpy (|XtDisplay shell))
	 (win (|XtWindow shell))
	 (clock-pixmaps (make-vector 12))
	 (dgc (|GC (car (snd-gcs)))))
    (do ((i 0 (1+ i)))
	((= i 12))
      ;; it's actually possible to simply redraw on one pixmap, but updates are unpredictable
      (let* ((pix (|XCreatePixmap dpy win 16 16 (screen-depth)))
	     (pixwin (list 'Window (cadr pix)))) ; C-style cast to Window for X graphics procedures
	(vector-set! clock-pixmaps i pix)
	(|XSetForeground dpy dgc (|Pixel (snd-pixel (basic-color))))
	(|XFillRectangle dpy pixwin dgc 0 0 16 16)
	(|XSetForeground dpy dgc (white-pixel))
	(|XFillArc dpy pixwin dgc 1 1 14 14 0 (* 64 360))
	(|XSetForeground dpy dgc (black-pixel))
	(|XDrawArc dpy pixwin dgc 1 1 14 14 0 (* 64 360))
	(|XDrawLine dpy pixwin dgc 8 8
			(+ 8 (inexact->exact (* 7 (sin (* i (/ 3.1416 6.0))))))
			(- 8 (inexact->exact (* 7 (cos (* i (/ 3.1416 6.0)))))))))
    (|XSetBackground dpy dgc (|Pixel (snd-pixel (graph-color))))
    (|XSetForeground dpy dgc (|Pixel (snd-pixel (data-color))))
    (lambda (snd hour)
      (if hour
	  (|XtSetValues (|Widget (list-ref (sound-widgets snd) 8))
			(list |XmNlabelPixmap (vector-ref clock-pixmaps hour)))
	  (bomb snd #f))))) ; using bomb simply to clear the icon



;;; -------- make-sound-box --------
;;;
;;; make-sound-box makes a container of sound file icons, each icon
;;;   containing a little sketch of the waveform, the length of the
;;;   file, and the filename.  What happens when an icon is selected
;;;   is up to caller-supplied procedure.

(define (thumbnail-graph dpy wn gc pts width height)
  ;; make a little graph of the data
  (let* ((top-margin 2)
	 (bottom-margin 6)
	 (left-margin 2)
	 (right-margin 2)
	 (ay1 top-margin)
	 (ay0 (- height bottom-margin))
	 (range (/ (- height top-margin bottom-margin) 2)))
    (define (y->grfy y height)
      (min ay0
	   (max ay1
		(inexact->exact
		 (+ ay1
		    (* height (- 1.0 y)))))))
    (let* ((ly (y->grfy (vector-ref pts 0) range))
	   (lx left-margin)
	   (len (vector-length pts))
	   (xinc (/ (- width left-margin right-margin) len))
	   (y 0))
      (do ((i 1 (1+ i))
	   (x lx (+ x xinc)))
	  ((= i len))
	(set! y (y->grfy (vector-ref pts i) range))
	(|XDrawLine dpy wn gc lx ly (inexact->exact x) y)
	(set! lx (inexact->exact x))
	(set! ly y)))))

(define make-sound-box 
  ;; make box of sound file icons

  ;; graphics stuff (fonts etc)
  (let*  ((gv (|XGCValues))
	  (shell (|Widget (list-ref (main-widgets) 1)))
	  (button-fontstruct (|XLoadQueryFont (|XtDisplay shell) "6x12"))
	  (button-fontlist
	   (let* ((e1 (|XmFontListEntryCreate "smallest" |XmFONT_IS_FONT button-fontstruct))
		  (f1 (|XmFontListAppendEntry #f e1)))
	     (|XmFontListEntryFree e1)
	     f1)))
    (set! (|foreground gv) (|Pixel (snd-pixel (data-color))))
    (set! (|background gv) (|Pixel (snd-pixel (basic-color))))
    (set! (|font gv) (|fid button-fontstruct))
    (let ((gc (|XCreateGC (|XtDisplay shell) 
			  (|XtWindow shell) 
			  (logior |GCForeground |GCBackground |GCFont) gv))
	  (sound-buttons '()))

      ;; button data list handlers
      (define sound-button-gc
	(make-procedure-with-setter
	 (lambda (data) (list-ref data 0))
	 (lambda (data val) (list-set! data 0 val))))

      (define sound-button-filename
	(make-procedure-with-setter
	 (lambda (data) (list-ref data 1))
	 (lambda (data val) (list-set! data 1 val))))

      (define sound-button
	(make-procedure-with-setter
	 (lambda (data) (list-ref data 2))
	 (lambda (data val) (list-set! data 2 val))))

      (define sound-button-peaks
	(make-procedure-with-setter
	 (lambda (data) (list-ref data 3))
	 (lambda (data val) (list-set! data 3 val))))

      (define (sound-button-data button)
	(define (sb-data lst)
	  (if (null? lst)
	      #f
	      (if (equal? button (sound-button (car lst)))
		  (car lst)
		  (sb-data (cdr lst)))))
	(sb-data sound-buttons))

      (define (make-sound-button-pixmap dpy wn data width height)
	(if (list? (sound-button-peaks data))
	    (let* ((mins (car (sound-button-peaks data)))
		   (maxes (cadr (sound-button-peaks data)))
		   (gc (sound-button-gc data))
		   (name (sound-button-filename data)))	     
	      (let* ((secs (format #f "~,1F" (mus-sound-duration (sound-button-filename data))))
		     (size (|XTextWidth button-fontstruct secs (string-length secs))))
		(if (<= size width) 
		    (|XDrawString dpy wn gc (- width size) height secs (string-length secs))))
	      (thumbnail-graph dpy wn gc mins width height)
	      (thumbnail-graph dpy wn gc maxes width height))))

      (define (make-sound-icon filename parent peak-func gc width height args)
	(define (cast-to-window n) (list 'Window (cadr n)))
	(let* ((dpy (|XtDisplay parent))
	       (win (|XtWindow parent))
	       (pix (|XCreatePixmap dpy win width height (screen-depth)))
	       (str (|XmStringCreateLocalized filename))
	       (data (list gc filename #f (channel-amp-envs filename 0 width peak-func))))
	  (|XSetForeground dpy gc (|Pixel (snd-pixel (basic-color))))
	  (|XFillRectangle dpy (cast-to-window pix) gc 0 0 width height)
	  (|XSetForeground dpy gc (|Pixel (snd-pixel (data-color))))
	  (make-sound-button-pixmap dpy (cast-to-window pix) data width height)
	  (let ((icon (|XtCreateManagedWidget filename |xmIconGadgetClass parent
			(append (list |XmNbackground      (|Pixel (snd-pixel (basic-color)))
				      |XmNforeground      (|Pixel (snd-pixel (data-color)))
				      |XmNlabelString     str
				      |XmNfontList        button-fontlist
				      |XmNlargeIconPixmap pix
				      |XmNsmallIconPixmap pix)
				args))))
	    (set! (sound-button data) icon)
	    (set! sound-buttons (cons data sound-buttons))
	    icon)))

      ;; now the actual sound-box maker
      ;; TODO: multi-channel thumbnail sketches, filled polygon rather than lines, small/large icon distinction
      (lambda (name parent select-func peak-func sounds args)
	;; select-func called when sound selected and passed sound file name
	;; peak-func (if any) tells icon where to find peak-env-info-file (if any)
	;; sounds is list of sound file names
	;; args is list of resource settings for each icon
	(let ((container (|XtCreateManagedWidget name |xmContainerWidgetClass parent
			   (list |XmNlayoutType     |XmSPATIAL
				 |XmNbackground     (white-pixel)
				 |XmNentryViewType  |XmANY_ICON
				 |XmNlargeCellWidth 80))))
	  (|XtAddCallback container |XmNselectionCallback 
	    (lambda (w c i)
	      (if (= (|auto_selection_type i) |XmAUTO_BEGIN) ; just click to select for now
		  (select-func (|XtName (car (|selected_items i)))))))
	  (for-each
	   (lambda (file)
	     (make-sound-icon file
			      container
			      peak-func
			      gc
			      64 32
			      args))
	   sounds)
	  container)))))

#!
(make-sound-box "sounds"
		(|Widget (list-ref (main-widgets) 3))
		(lambda (file) 
		  (mix file))
		(lambda (file chn)
		  (define (without-directories filename)
		    (call-with-current-continuation
		     (lambda (return)
		       (do ((i (- (string-length filename) 1) (1- i)))
			   ((= 0 i) filename)
			 (if (char=? (string-ref filename i) #\/)
			     (return (substring filename (+ i 1))))))))
		  (format #f "~~/peaks/~A-peaks-~D" 
			  (without-directories (mus-expand-filename file)) 
			  chn))
		(list "oboe.snd" "pistol.snd" "cardinal.snd" "storm.snd")
		'())
!#

    

;;; -------- show-smpte-label
;;;
;;; (show-smpte-label #:optional on-or-off)
;;;   turns on/off a label in the time-domain graph showing the current smpte frame of the leftmost sample

(define smpte-frames-per-second 24.0)

(define draw-smpte-label
  (let* ((dpy (|XtDisplay (|Widget (cadr (main-widgets)))))
	 (fs (|XLoadQueryFont dpy (axis-numbers-font)))
	 (width (+ 8 (|XTextWidth fs "00:00:00:00" 11)))
	 (height (+ 8 (caddr (|XTextExtents fs "0" 1)))))

    (define (smpte-label samp sr)
      (define (round-down val) (inexact->exact (truncate val)))
      (let* ((seconds (/ samp sr))
	     (frames (* seconds smpte-frames-per-second))
	     (minutes (round-down (/ seconds 60)))
	     (hours (round-down (/ minutes 60))))
	(format #f "~2,'0D:~2,'0D:~2,'0D:~2,'0D"
		hours
		(- minutes (* hours 60))
		(round-down (- seconds (* minutes 60)))
		(round-down (- frames (* (round-down seconds) smpte-frames-per-second))))))
	    
    (lambda (snd chn)
      (let* ((axinf (axis-info snd chn))
	     (x (list-ref axinf 10))
	     (y (list-ref axinf 13))
	     (grf-width (- (list-ref axinf 12) x))
	     (grf-height (- (list-ref axinf 11) y)))
	(if (and (> grf-height (* 2 height))
		 (> grf-width (* 1.5 width))
		 (graph-time? snd chn))
	    (let* ((smpte (smpte-label (car axinf) (srate snd)))
		   (samp (car axinf)))
	      (fill-rectangle x y width 2 snd chn)
	      (fill-rectangle x (+ y height) width 2 snd chn)
	      (fill-rectangle x y 2 height snd chn)
	      (fill-rectangle (+ x width -2) y 2 height snd chn)
	      (|XSetFont dpy
			 (if (= chn (selected-channel snd))
			     (|GC (cadr (snd-gcs)))
			     (|GC (car (snd-gcs))))
			 (|fid fs))
	      (draw-string smpte (+ x 4) (+ y height -4) snd chn)))))))

(define show-smpte-label
  (lambda arg
    (if (or (null? arg)
	    (car arg))
      (if (not (member draw-smpte-label (hook->list after-graph-hook)))
	  (begin
	    (add-hook! after-graph-hook draw-smpte-label)
	    (update-time-graph #t #t)))
      (begin
	(remove-hook! after-graph-hook draw-smpte-label)
	(update-time-graph #t #t)))))




;;; -------- with-level-meters, make-level-meter, display-level

(define red-pixel
  (let ((pix #f))
    (lambda ()
      (if (not pix)
	  (let* ((shell (|Widget (cadr (main-widgets))))
		 (dpy (|XtDisplay shell))
		 (scr (|DefaultScreen dpy))
		 (cmap (|DefaultColormap dpy scr))
		 (col (|XColor)))
	       (if (= (|XAllocNamedColor dpy cmap "red" col col) 0)
		   (snd-error "can't allocate red!")
		   (set! pix (|pixel col)))))
      pix)))

(define (make-level-meter parent width height args)
  (let* ((frame (|XtCreateManagedWidget "meter-frame" |xmFrameWidgetClass parent
		  (append (list |XmNshadowType       |XmSHADOW_ETCHED_IN
				|XmNwidth            width
				|XmNheight           height
				|XmNshadowThickness  (if (> width 500) 6 3))
			  args)))
	 (meter (|XtCreateManagedWidget "meter" |xmDrawingAreaWidgetClass frame
		  (list |XmNbackground       (white-pixel)
			|XmNforeground       (black-pixel)
			|XmNtopAttachment    |XmATTACH_FORM
			|XmNbottomAttachment |XmATTACH_FORM
			|XmNleftAttachment   |XmATTACH_FORM
			|XmNrightAttachment  |XmATTACH_FORM)))
	 (context (list meter 0.0 1.0 0.0 0.0 width height)))
    (|XtAddCallback meter |XmNexposeCallback 
		    (lambda (w c i) 
		      (display-level c)) 
		    context)
    (|XtAddCallback meter |XmNresizeCallback 
		    (lambda (w c i) 
		      (list-set! c 5 (cadr (|XtGetValues w (list |XmNwidth 0))))
		      (list-set! c 6 (cadr (|XtGetValues w (list |XmNheight 0))))
		      (display-level c))
		    context)
    context))

(define (display-level meter-data)
  (let* ((meter (car meter-data))
	 (level (list-ref meter-data 1))
	 (last-level (list-ref meter-data 3))
	 (red-deg (list-ref meter-data 4))
	 (width (list-ref meter-data 5))
	 (height (list-ref meter-data 6))
	 (size (list-ref meter-data 2))
	 (dpy (|XtDisplay meter))
	 (win (|XtWindow meter))
	 (major-tick (inexact->exact (/ width 24)))
	 (minor-tick (inexact->exact (* major-tick .6)))
	 (ang0 (* 45 64))
	 (ang1 (* 90 64))
	 (wid2 (inexact->exact (/ width 2)))
	 (gc (|GC (car (snd-gcs))))
	 (top (inexact->exact (/ height 3.2)))) ; distance of label from top of meter
    (|XSetForeground dpy gc (white-pixel))
    (|XFillRectangle dpy win gc 0 0 width height)
    (|XSetForeground dpy gc (black-pixel))
    (|XDrawArc dpy win gc 0 top width width ang0 ang1)
    (|XDrawArc dpy win gc 0 (- top 1) width width ang0 ang1)
    (if (> width 100)
	(|XDrawArc dpy win gc 0 (- top 2) width width ang0 ang1))
    (|XDrawArc dpy win gc 4 (+ top 4) (- width 8) (- width 8) ang0 ang1)
    (do ((i 0 (1+ i)))
	((= i 5))
      (let* ((rdeg (degrees->radians (- 45 (* i 22.5))))
	     (sinr (sin rdeg))
	     (cosr (cos rdeg))
	     (x0 (inexact->exact (+ wid2 (* wid2 sinr))))
	     (y0 (inexact->exact (- (+ wid2 top) (* wid2 cosr))))
	     (x1 (inexact->exact (+ wid2 (* (+ wid2 major-tick) sinr))))
	     (y1 (inexact->exact (- (+ wid2 top) (* (+ wid2 major-tick) cosr)))))
	(|XDrawLine dpy win gc x0 y0 x1 y1)
	(|XDrawLine dpy win gc (+ x0 1) y0 (+ x1 1) y1)
	(if (< i 4)
	    (do ((j 1 (1+ j)))
		((= j 6))
	      (let* ((rdeg (degrees->radians (- 45 (* i 22.5) (* j (/ 90.0 20.0)))))
		     (sinr (sin rdeg))
		     (cosr (cos rdeg))
		     (x0 (inexact->exact (* wid2 (+ 1.0 sinr))))
		     (y0 (inexact->exact (- (+ wid2 top) (* wid2 cosr))))
		     (x1 (inexact->exact (+ wid2 (* (+ wid2 minor-tick) sinr))))
		     (y1 (inexact->exact (- (+ wid2 top) (* (+ wid2 minor-tick) cosr)))))
		(|XDrawLine dpy win gc x0 y0 x1 y1))))))
    (let* ((needle-speed 0.25)
	   (bubble-speed 0.025)
	   (bubble-size (* 15 64))
	   (val (+ (* level needle-speed) (* last-level (- 1.0 needle-speed))))
	   (deg (- (* val 90.0) 45.0))
	   (rdeg (degrees->radians deg))
	   (nx1 (inexact->exact (+ wid2 (* (+ wid2 major-tick) (sin rdeg)))))
	   (ny1 (inexact->exact (- (+ wid2 top) (* (+ wid2 major-tick) (cos rdeg))))))
      (|XDrawLine dpy win gc wid2 wid2 nx1 ny1)
      (list-set! meter-data 3 val)
      (if (> val red-deg)
	  (list-set! meter-data 4 val)
	  (list-set! meter-data 4 (+ (* val bubble-speed) (* red-deg (- 1.0 bubble-speed)))))
      (if (> (list-ref meter-data 4) .01)
	  (begin
	    (|XSetForeground dpy gc (red-pixel))
	    (let* ((redx (inexact->exact (* (list-ref meter-data 4) 90 64)))
		   (redy (min redx bubble-size)))
	      (do ((i 0 (1+ i)))
		  ((= i 4))
		(|XDrawArc dpy win gc i (+ top i) (- width (* i 2)) (- width (* i 2)) (- (* 135 64) redx) redy))
	      (|XSetForeground dpy gc (black-pixel))))))))

(define (with-level-meters n)
  ;; add n level meters to a pane at the top of the Snd window
  (let* ((parent (|Widget (list-ref (main-widgets) 3)))
	 (height 70)
	 (width (inexact->exact (/ (cadr (|XtGetValues parent (list |XmNwidth 0))) n)))
	 (meters (|XtCreateManagedWidget "meters" |xmFormWidgetClass parent
	 	   (list |XmNpositionIndex 0  ; top pane
			 |XmNbackground    (|Pixel (snd-pixel (basic-color)))
			 |XmNfractionBase  (* n 10)
			 |XmNpaneMinimum   height)))
	 (meter-list '()))
    (do ((i 0 (1+ i)))
	((= i n))
      (set! meter-list 
	 (cons (make-level-meter meters width height
				 (list |XmNtopAttachment    |XmATTACH_FORM
				       |XmNbottomAttachment |XmATTACH_FORM
				       |XmNleftAttachment   |XmATTACH_POSITION
				       |XmNleftPosition     (* i 10)
				       |XmNrightAttachment  |XmATTACH_POSITION
				       |XmNrightPosition    (* (+ 1 i) 10))) 
	       meter-list)))
    (add-hook! dac-hook 
	       (lambda (sdobj)
		 (let* ((maxes (sound-data-maxamp sdobj)))
		   (for-each
		    (lambda (meter)
		      (if (null? maxes)
			  (list-set! meter 1 0.0)
			  (begin
			    (list-set! meter 1 (car maxes))
			    (display-level meter)
			    (set! maxes (cdr maxes)))))
		    (reverse meter-list)))))
    (add-hook! stop-dac-hook
	       (lambda () ; drain away the bubble
		 (|XtAppAddWorkProc (|XtAppContext (car (main-widgets)))
				    (let ((ctr 0))
				      (lambda (ignored)
					(for-each 
					 (lambda (meter)
					   (list-set! meter 1 0.0)
					   (display-level meter))
					 meter-list)
					(set! ctr (+ ctr 1))
					(> ctr 200))))))
    (|XtSetValues meters (list |XmNpaneMinimum 1))
    meter-list))



;;; -------- add a drop site
;;;
;;; this adds a pane to the current channel which can respond to drag-and-drop operations
;;;   (this is a Motif 1.2 style drop -- I've had trouble getting the new style to work at all)

(define make-channel-drop-site
  (lambda args
    (let* ((snd (if (> (length args) 0) (car args) (selected-sound)))
	   (chn (selected-channel snd))
	   (widget (add-channel-pane snd chn "drop here" |xmDrawingAreaWidgetClass
		     (list |XmNbackground (white-pixel)
                           |XmNleftAttachment      |XmATTACH_FORM
		           |XmNrightAttachment     |XmATTACH_FORM
		           |XmNtopAttachment       |XmATTACH_FORM
		           |XmNbottomAttachment    |XmATTACH_FORM))))
      (|XmDropSiteRegister
	widget 
	(list |XmNdropSiteOperations |XmDROP_COPY
	      |XmNimportTargets      (list |XA_STRING) ; list of Atoms we can deal with -- in this case, just strings
	      |XmNnumImportTargets   1
	      |XmNdropProc 
	       (lambda (w c i)
		 ;; i is the callback data (XmDropProcCallbackStruct), c is always #f
		 (if (or (not (= (|dropAction i) |XmDROP))
			 (not (= (|operation i) |XmDROP_COPY)))
		     (set! (|dropSiteStatus i) |XmINVALID_DROP_SITE)
		     (begin
		       (set! (|operation i) |XmDROP_COPY) ; tell system drop has succeeded
		       (|XmDropTransferStart 
			 (|dragContext i)
			 (list |XmNdropTransfers (list (list |XA_STRING w)) ; list of lists of Atoms/our-data
			       |XmNnumDropTransfers 1
			       |XmNtransferProc 
				(lambda (w context selection type val len fmt)
				  ;; the actual in-coming string (properly terminated in xm.c) is 'value'
				  (snd-print (format #f "got: ~A ~A ~A ~A ~A ~A ~A~%"
						     w context selection type val len fmt)))))))))))))



;;; -------- show-disk-space
;;;
;;; adds a label to the minibuffer area showing the current free space 

(define show-disk-space
  (let ((labelled-snds '()))
    (define (kmg num)
      (if (<= num 0)
	  "disk full!"
	  (if (> num 1024)
	      (if (> num (* 1024 1024))
		  (format #f "space: ~6,3FG" (/ num (* 1024 1024)))
		  (format #f "space: ~6,3FM" (/ num 1024.0)))
	      (format #f "space: ~10DK" num))))
    (define (show-label data id)
      (if (sound? (car data))
	  (let* ((space (kmg (disk-kspace (file-name (car data)))))
		 (str (|XmStringCreateLocalized space)))
	    (|XtSetValues (cadr data) (list |XmNlabelString str))
	    (|XmStringFree str)
	    (|XtAppAddTimeOut (caddr data) 10000 show-label data))))
    (lambda (snd)
      (let ((previous-label (find-if (lambda (n) (= (car n) snd)) labelled-snds)))
	(if (not previous-label)
	    (let* ((app (|XtAppContext (car (main-widgets))))
		   (widgets (sound-widgets snd))
		   (minibuffer (|Widget (list-ref widgets 3)))
		   (name-form (|XtParent minibuffer))
		   (space (kmg (disk-kspace (file-name snd))))
		   (str (|XmStringCreateLocalized space))
		   (new-label (|XtCreateManagedWidget "space:" |xmLabelWidgetClass name-form 
				(list |XmNbackground      (|Pixel (snd-pixel (basic-color)))
				      |XmNleftAttachment  |XmATTACH_WIDGET
				      |XmNleftWidget      minibuffer
				      |XmNlabelString     str
				      |XmNrightAttachment |XmATTACH_NONE
				      |XmNtopAttachment   |XmATTACH_FORM))))
	      (|XmStringFree str)
	      (|XtSetValues minibuffer (list |XmNrightAttachment |XmATTACH_WIDGET
					     |XmNrightWidget new-label))
	      (set! previous-label (list snd new-label app))
	      (set! labelled-snds (cons previous-label labelled-snds))))
	(|XtAppAddTimeOut (caddr previous-label) 10000 show-label previous-label)))))

;(add-hook! after-open-hook show-disk-space)



;;; -------- add another amp slider in control panel
;;;
;;; just step one of this section
;;; TODO: after-open callback to reflect current channel number
;;; TODO: tie new scrollers into Play/Apply (this will require support from Snd)
;;; TODO: handle n>2 scrollers
;;; TODO: don't remanage controls unless it's previously open

(define (amp-scroller->amp val)
  ;; same as Snd's built-in amp scroller callbacks
  (if (= val 0)
      0.0
      (if (< val 15)
	  (* val 0.011584929) ; linear section
	  (exp (/ (- val 50) 20.0)))))

(define (amp-callback w c i)
  (let* ((amp (amp-scroller->amp (|value i)))
	 (ampstr (|XmStringCreateLocalized (format #f "~,2F" amp))))
    (|XtSetValues c (list |XmNlabelString ampstr))
    (|XmStringFree ampstr)))

(define (make-amp-package parent)
  (let* ((s1 (|XmStringCreateLocalized "amp:"))
	 (label (|XtCreateManagedWidget "amp-label" |xmPushButtonWidgetClass parent
	 	  (list |XmNbackground (|Pixel (snd-pixel (basic-color)))
			|XmNalignment |XmALIGNMENT_BEGINNING
			|XmNtopAttachment |XmATTACH_FORM
			|XmNbottomAttachment |XmATTACH_NONE
			|XmNleftAttachment |XmATTACH_FORM
			|XmNrightAttachment |XmATTACH_NONE
			|XmNlabelString s1
			|XmNmarginHeight 1
			|XmNrecomputeSize #f
			|XmNshadowThickness 0
			|XmNhighlightThickness 0
			|XmNfillOnArm #f)))
	 (s2 (|XmStringCreateLocalized "1.00"))
	 (number (|XtCreateManagedWidget "amp-number" |xmLabelWidgetClass parent
	 	  (list |XmNbackground (|Pixel (snd-pixel (basic-color)))
			|XmNalignment |XmALIGNMENT_BEGINNING
			|XmNtopAttachment |XmATTACH_OPPOSITE_WIDGET
			|XmNtopWidget label
			|XmNbottomAttachment |XmATTACH_NONE
			|XmNleftAttachment |XmATTACH_WIDGET
			|XmNleftWidget label
			|XmNrightAttachment |XmATTACH_NONE
			|XmNlabelString s2
			|XmNmarginHeight 1
			|XmNrecomputeSize #f)))
	 (scroll (|XtCreateManagedWidget "amp" |xmScrollBarWidgetClass parent
	 	  (list |XmNbackground (|Pixel (snd-pixel (position-color)))
			|XmNtopAttachment |XmATTACH_OPPOSITE_WIDGET
			|XmNtopWidget label
			|XmNbottomAttachment |XmATTACH_NONE
			|XmNheight 16
			|XmNleftAttachment |XmATTACH_WIDGET
			|XmNleftWidget number
			|XmNrightAttachment |XmATTACH_FORM
			|XmNorientation |XmHORIZONTAL
			|XmNmaximum 100
			|XmNvalue 50
			|XmNdragCallback (list amp-callback number)
			|XmNvalueChangedCallback (list amp-callback number)))))
	 (|XmStringFree s1)
	 (|XmStringFree s2)
	 label
	 ))

(define (add-amp-control)
  (let* ((wids (sound-widgets))
	 (ctrls (|Widget (list-ref wids 2)))
	 (snd-amp (find-child ctrls "snd-amp"))
	 (amplabel (find-child snd-amp "amp-label")))
    (if (|Widget? amplabel)
	(begin
	  (|XtUnmanageChild ctrls)
	  
	  (let ((new-amp (make-amp-package snd-amp)))
	    (|XtSetValues amplabel (list |XmNtopAttachment |XmATTACH_WIDGET
					 |XmNtopWidget new-amp))
	    (|XtManageChild ctrls))))))



;;; -------- add-very-useful-icons adds some very useful icons

(define (add-very-useful-icons)
  (let ((tools (add-main-pane "tools" |xmRowColumnWidgetClass
		  (list |XmNbackground (black-pixel)
			|XmNpaneMinimum 48
			|XmNpaneMaximum 48
			|XmNorientation |XmHORIZONTAL))))
    (load "icons.scm")
    (for-each 
     (lambda (icon)
       (let ((button
	      (|XtCreateManagedWidget "button" |xmPushButtonWidgetClass tools
		(list |XmNlabelPixmap (make-pixmap tools icon)
		      |XmNlabelType   |XmPIXMAP
		      |XmNwidth       32
		      |XmNheight      32))))
	 (|XtAddCallback button |XmNactivateCallback (lambda (w c i) (snd-print "aren't icons grand?")))))
     (list burger syringe media tut fortune bob1 caesar xmas1 umbrela chess3 compress xdbx icl8))))



;;; drawnbutton+workproc sound-button example
;;; bess-translations
;;; midi trigger
;;; save/restore -separate window details
;;; chan-grf
;;; marks menu
