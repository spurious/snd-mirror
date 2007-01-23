;;; snd-motif.scm -- Motif-related procedures (all use xm.so, most assume Motif 2)
;;;
;;; (install-searcher proc) -- use proc as File Selection Box filter, also install-searcher-with-colors
;;; (zync) and (unzync) -- cause y-zoom sliders to move together
;;; (for-each-child w func) -- apply func to w and all its children, similarly find-child and display-widget-tree
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
;;; (set-channel-drop drop snd chn) -- change given graph drop callback to drop
;;; (select-file func :optional title dir filter help) starts a Snd-like File Selection Dialog running func if a file is selected
;;; (show-disk-space snd) adds a label to the minibuffer area showing the current free space 
;;; (keep-file-dialog-open-upon-ok) changes File:Open so that clicking "ok" does not "unmanage" the dialog
;;; (use-pan-mix-in-mix-menu) changes the File:Mix menu Ok callback to use pan-mix rather than mix
;;; (add-amp-controls) adds amp sliders to the control panel for multi-channel sounds
;;; (remove-main-menu menu) removes a top-level menu
;;; add delete and rename options to the file menu (add-delete-option) (add-rename-option)
;;; (mark-sync-color new-color) sets the color of syncd marks
;;; (add-tooltip widget tip) adds tooltip tip to widget
;;; (menu-option name) to access menu items
;;; (show-all-atoms) shows all X atoms
;;; show-font-name shows the Snd-related name and the X-related name of a font
;;; show-widget-font shows what fonts are associated with a widget
;;; add-find-to-listener enables C-s and C-r in the listener
;;; add a function to be called when the window manager sends us a "save yourself" message
;;; add-text-to-status-area puts a text widget in the notebook status area
;;; make-variable-display displays an arbitrary set of expressions/variables in a notebook widget
;;; with-minmax-button adds an open/close button to each sound pane
;;; set-root-window-color
;;; notebook-with-top-tabs (for Xemacs-like list of open files across the top of the window)
;;; create-audit-dialog

(use-modules (ice-9 common-list) (ice-9 format))

(if (not (provided? 'snd-motif)) (snd-error "snd-motif.scm is Motif-specific"))

(if (not (defined? 'find-if))
    (define (find-if pred l)
      (cond ((null? l) #f)
	    ((pred (car l)) (car l))
	    (else (find-if pred (cdr l))))))

(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
	  (snd-error (format #f "snd-motif.scm needs the xm module: ~A" hxm))
	  (dlinit hxm "Init_libxm"))))

(provide 'snd-snd-motif.scm)

(if (not (provided? 'snd-extensions.scm)) (load-from-path "extensions.scm"))
(if (not (provided? 'snd-play.scm)) (load-from-path "play.scm"))

(define (load-font name)
  "(load-font name) loads the font 'name', returning the font id"
  (let ((fs (XLoadQueryFont (XtDisplay (cadr (main-widgets))) name)))
    (and (XFontStruct? fs) (.fid fs))))

(define (current-screen)
  "(current-screen) returns the current X screen number of the current display"
  (DefaultScreenOfDisplay 
    (XtDisplay (cadr (main-widgets)))))

(define (white-pixel)
  "(white-pixel) returns a white pixel"
  (WhitePixelOfScreen (current-screen)))

(define (black-pixel)
  "(black-pixel) returns a black pixel"
  (BlackPixelOfScreen (current-screen)))

(define (screen-depth)
  "(screen-depth) returns the current screen depth"
  (DefaultDepthOfScreen (current-screen)))

(define (clean-string str)
  "(clean-string str) changes slash to underbar in the filename 'str' (for the peak env file)"
  ;; full file name should be unique, so I think we need only fix it up to look like a flat name
  (let* ((len (string-length str))
	 (new-str (make-string len #\.)))
    (do ((i 0 (1+ i)))
	((= i len) new-str)
      (let ((c (string-ref str i)))
	(if (or (char=? c #\\)
		(char=? c #\/))
	    (string-set! new-str i #\_)
	    (string-set! new-str i c))))))


;;; -------- apply func to every widget belonging to w (and w) --------

(define+ (for-each-child w func)
  "(for-each-child w func) applies func to w and its descendents"
  (func w)
  (if (XtIsComposite w)
      (for-each 
       (lambda (n)
	 (for-each-child n func))
       (cadr (XtGetValues w (list XmNchildren 0) 1)))))

(define+ (find-child widget name)
  "(find-child widget name) returns a widget named 'name', if one can be found in the widget hierarchy beneath 'widget'"
  ;; unfortunately, if the widget's name has been set for some non-English locale, this
  ;;   won't work -- we need to add gettext support (see snd.c for an example)
  (call-with-current-continuation
   (lambda (return)
     (for-each-child
      widget
      (lambda (child)
	(if (string=? (XtName child) name)
	    (return child))))
     (throw 'no-such-widget (list "find-child" name)))))

(define+ (display-widget-tree widget)
  "(display-widget-tree widget) displays the hierarchy of widgets beneath 'widget'"
  (define (display-widget w spaces)
    (let ((name (XtName w)))
      (if (or (not (string? name))
	      (= (string-length name) 0))
	  (set! name "<unnamed>"))
      (display (format #f "~A~A~%" spaces name))
      (if (XtIsComposite w)
	  (for-each 
	   (lambda (n)
	     (display-widget n (string-append spaces "  ")))
	   (cadr (XtGetValues w (list XmNchildren 0) 1))))))
  (display-widget widget ""))

(define (set-main-color-of-widget w)
  "(set-main-color-of-widget w) sets the background color of widget 'w'"
  (for-each-child 
   w
   (lambda (n)
     (if (XtIsWidget n)
	 (if (XmIsScrollBar n)
	     (XmChangeColor n (position-color))
	     (XmChangeColor n (basic-color)))))))

(define (host-name)
  "(host-name) -> name of current machine"
  (let* ((dpy (XtDisplay (cadr (main-widgets))))
	 (win (XtWindow (cadr (main-widgets))))
	 (host (XGetWindowProperty dpy win (XInternAtom (XtDisplay (cadr (main-widgets))) "WM_CLIENT_MACHINE" #f) 0 32 #f XA_STRING)))
    (and host (list-ref host 5))))


;;; -------- install-searcher --------
;;;
;;; replaces the current file search procedure in the File Selection Box
;;;
;;;    (install-searcher (lambda (file) (= (mus-sound-srate file) 44100)))
;;;    (install-searcher (lambda (file) (= (mus-sound-chans file) 4)))
;;;
;;; this is obsolete -- use the file-filter mechanism instead

(define+ (install-searcher proc)
  "(install-searcher proc) replaces the current file search procedure in the File Selection 
Box: (install-searcher (lambda (file) (= (mus-sound-srate file) 44100)))"
  (define match-sound-files
    (lambda args
      "(match-sound-files func :optional dir) applies func to each sound file in dir and returns a list of files for which func does not return #f"
      (let* ((func (car args))
	     (matches '()))
	(for-each
	 (lambda (file)
	   (if (func file)
	       (set! matches (cons file matches))))
	 (sound-files-in-directory (if (null? (cdr args)) "." (cadr args))))
	matches)))
  (define (XmString->string str)
    (cadr (XmStringGetLtoR str XmFONTLIST_DEFAULT_TAG)))
  (define (XmStringTable->list st len)
    (XmStringTableUnparse st len #f XmCHARSET_TEXT XmCHARSET_TEXT #f 0 XmOUTPUT_ALL))
  (define (list->XmStringTable strs)
    (XmStringTableParseStringArray strs (length strs) #f XmCHARSET_TEXT #f 0 #f))
  (XtSetValues (open-file-dialog #f)
	       (list XmNfileSearchProc
		     (lambda (widget info)
		       (let* ((dir (XmString->string (.dir info)))
			      (files (match-sound-files proc dir))
			      (fileTable (list->XmStringTable 
					  (map (lambda (n) 
						 (string-append dir n)) 
					       files))))
			 (XtSetValues widget
				      (list XmNfileListItems fileTable
					    XmNfileListItemCount (length files)
					    XmNlistUpdated #t))
			 (for-each (lambda (n) (XmStringFree n)) fileTable))))))


;;; here's a fancier version that gets rid of the useless directory list,
;;;   and shows multi-channel files in color

(define (install-searcher-with-colors proc)

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
    (cadr (XmStringGetLtoR str XmFONTLIST_DEFAULT_TAG)))

  (let* ((dialog (open-file-dialog #f))
	 ;; (XtGetValues dialog (XmNfileSearchProc 0)) to get the default
	 (shell (cadr (main-widgets)))
	 (tags (list "one" "two" "three" "four"))
	 (colors (list "black" "red" "blue" "orange"))
	 (pixels (let* ((dpy (XtDisplay shell))
			(scr (DefaultScreen dpy))
			(cmap (DefaultColormap dpy scr)))
		   (map
		    (lambda (color)
		      (let ((col (XColor)))
			(if (= (XAllocNamedColor dpy cmap color col col) 0)
			    (snd-error (format #f "can't allocate ~A" color))
			    (.pixel col))))
		    colors)))
	 (rendertable (XmRenderTableAddRenditions 
			#f 
			(map (lambda (tag pix)
			       (XmRenditionCreate 
				(cadr (main-widgets))
				tag
				(list XmNrenditionForeground pix
				      XmNfontName "9x15"
				      XmNfontType XmFONT_IS_FONT)))
			     tags pixels)
			(length tags)
			XmMERGE_NEW)))

    (XtSetValues dialog
		 (list XmNfileSearchProc
		       (lambda (widget info)
			 (let* ((dir (XmString->string (.dir info)))  ; may need filter text here?
				(files (sort (map 
					      (lambda (n) 
						(string-append dir n)) 
					      (match-sound-files proc dir))
					     string<?))               ; alphabetical order
				(fileTable (map
					    (lambda (n)
					      (XmStringGenerate 
					       n #f XmCHARSET_TEXT 
					       (if (= (mus-sound-chans n) 1)
						   "one"
						   (if (= (mus-sound-chans n) 2)
						       "two"
						       (if (= (mus-sound-chans n) 4)
							   "four"
							   "three")))))
					    files)))
			   (XtSetValues widget
					(list XmNfileListItems fileTable
					      XmNfileListItemCount (length files)
					      XmNlistUpdated #t))
			   (for-each (lambda (n) (XmStringFree n)) fileTable)))))
    (XtUnmanageChild (XmFileSelectionBoxGetChild dialog XmDIALOG_DIR_LIST))
    (XtUnmanageChild (XmFileSelectionBoxGetChild dialog XmDIALOG_DIR_LIST_LABEL))
    (XtSetValues (XmFileSelectionBoxGetChild dialog XmDIALOG_LIST)
		 (list XmNrenderTable rendertable))
    (XmFileSelectionDoSearch dialog #f)))
    
;(install-searcher-with-colors (lambda (file) #t))


;;; -------- keep-file-dialog-open-upon-ok
;;;
;;; change File:Open (or File:Mix) so that clicking "ok" does not "unmanage" the dialog

(define (keep-file-dialog-open-upon-ok)
  "(keep-file-dialog-open-upon-ok) changes the File:Open menu so that clicking 'ok' does not close the dialog"
  (let ((dialog (open-file-dialog #f)))
    (XtRemoveAllCallbacks dialog XmNokCallback) ; remove built-in version
    (XtAddCallback dialog XmNokCallback
		   (lambda (widget context info)
		     ;; same as built-in "ok" callback, but does not "unmanage" the dialog
		     (let ((filename (cadr (XmStringGetLtoR (.value info) XmFONTLIST_DEFAULT_TAG))))
		       (if (file-exists? filename)
			   (if (not (file-is-directory? filename))
			       (let ((snd (open-sound filename)))
				 (select-channel 0))
			       (snd-error (format #f "~S is a directory" filename)))
			   (snd-error (format #f "no such file: ~A" filename))))))
    'ok)) ; prettier in listener than printing out a callback procedure

(define (use-pan-mix-in-mix-menu)
  "(use-pan-mix-in-mix-menu) causes the File:Mix menu to use pan-mix rather than mix"
  (let ((dialog (mix-file-dialog #f)))
    (XtRemoveAllCallbacks dialog XmNokCallback) ; remove built-in version
    (XtAddCallback dialog XmNokCallback
		   (lambda (widget context info)
		     ;; same as built-in "ok" callback, but uses pan-mix, not mix
		     (let ((filename (cadr (XmStringGetLtoR (.value info) XmFONTLIST_DEFAULT_TAG))))
		       (if (file-exists? filename)
			   (if (not (file-is-directory? filename))
			       (pan-mix filename (or (cursor) 0))
			       (snd-error (format #f "~S is a directory" filename)))
			   (snd-error (format #f "no such file: ~A" filename))))))
    'ok))




;;; -------- zync and unzync: start or stop y-zoom slider sync --------
;;; 
;;; (i.e. when one y-zoom-slider changes position, all other channels in the sound change in parallel)
;;; (zync) to start and (unzync) to stop

(define (remove-dragger snd)
  "(remove-dragger snd) undoes an earlier add-dragger which syncs together all the y-zoom sliders"
  (let ((calls (sound-property 'dragger snd)))
    (if calls
	(do ((chn 0 (1+ chn)))
	    ((= chn (chans snd)))
	  (let ((zy (list-ref (channel-widgets snd chn) 6)))
	    (XtRemoveCallback zy XmNdragCallback (car calls))
	    (set! calls (cdr calls))))))
  (set! (sound-property 'dragger snd) #f)
  #f)

(define (add-dragger snd)
  "(add-dragger snd) syncs together y-zoom sliders"
  (set! (sound-property 'save-state-ignore snd)
	(cons 'dragger
	      (or (sound-property 'save-state-ignore snd)
		  (list 'save-state-ignore))))
  (set! (sound-property 'dragger snd)
	(let ((calls '()))
	  (do ((chn 0 (1+ chn)))
	      ((= chn (chans snd)))
	    (let* ((zy (list-ref (channel-widgets snd chn) 6))
		   (slider-size (cadr (XtGetValues zy (list XmNsliderSize 0)))) ; this is relative to max size
		   (max-size (cadr (XtGetValues zy (list XmNmaximum 0))))
		   (zy-div (max 10 (- max-size slider-size))))
	      (set! calls
		    (cons (XtAddCallback zy
					 XmNdragCallback 
					 (lambda (w data info)
					   (let ((v (/ (.value info) zy-div)))
					     (do ((i 0 (1+ i)))
						 ((= i (chans snd)))
					       (if (not (= i chn))
						   (begin
						     (set! (y-zoom-slider snd i) (* v v))
						     (set! (y-position-slider snd i) (y-position-slider snd chn))))))))
			  calls))))
	  (reverse calls))))

(define (zync)
  "(zync) ties each sound's y-zoom sliders together so that all change in parallel if one changes"
  (add-hook! after-open-hook add-dragger)
  (for-each
   (lambda (n)
     (if (not (sound-property 'dragger n))
	 (add-dragger n)))
   (sounds)))

(define (unzync)
  "(unzync) undoes a previous (zync) -- subsequently each sound's y-zoom sliders are independent"
  (remove-hook! after-open-hook add-dragger)
  (for-each
   (lambda (n)
     (if (sound-property 'dragger n)
	 (remove-dragger n)))
   (sounds)))



;;; -------- add our own pane to the channel section --------

(define* (add-channel-pane snd chn name type :optional (args '()))
  "(add-channel-pane snd chn name type :optional (args '())) adds a pane to the channel section"
  (XtCreateManagedWidget name type (XtParent (XtParent (list-ref (channel-widgets snd chn) 7))) args))


;;; -------- add our own pane to the sound section (underneath the controls in this case) --------

(define* (add-sound-pane snd name type :optional (args '()))
  "(add-sound-pane snd name type :optional (args '())) adds a pane to the sound section (underneath the control panel)"
  (XtCreateManagedWidget name type (car (sound-widgets snd)) args))


;;; -------- add our own pane to the overall Snd window (underneath the listener in this case) --------

(define* (add-main-pane name type :optional (args '()))
  "(add-main-pane name type :optional (args '())) adds a pane to Snd (underneath the listener)"
  (XtCreateManagedWidget name type (or (list-ref (main-widgets) 5) (list-ref (main-widgets) 3)) args))


;;; -------- add a widget at the top of the listener

(define (add-listener-pane name type args)
  "(add-listener-pane name type args) adds a widget at the top of the listener"
  (let* ((listener (find-child (cadr (main-widgets)) "lisp-listener"))
	 ;; this is the listener text widget, hopefully
	 ;;   its parent is the scrolled window, its parent is the form widget filling the listener pane
	 (listener-scroll (XtParent listener))
	 (listener-form (XtParent listener-scroll)))
    ;; to insert the new widget at the top of the listener pane we need to detach the
    ;;   listener scrolled window etc -- assume here that the "args" list does not
    ;;   include any ATTACH_* arguments
    (XtUnmanageChild listener-scroll)
    (let ((top-widget (XtCreateManagedWidget name type listener-form
					     (append 
					      (list XmNleftAttachment   XmATTACH_FORM
						    XmNrightAttachment  XmATTACH_FORM
						    XmNtopAttachment    XmATTACH_FORM)
					      args))))
      (XtVaSetValues listener-scroll (list XmNtopAttachment XmATTACH_WIDGET
					   XmNtopWidget     top-widget))
      (XtManageChild listener-scroll)
      top-widget)))

;(add-channel-pane "new-pane" 
;		  xmDrawingAreaWidgetClass 
;		  (list XmNbackground (graph-color)
;			XmNforeground (data-color)))

(define (remove-menu-bar-menu which)
  "(remove-menu-bar-menu which) removes a top-level menu; 'which' can be 0: top-level-menu-bar, 1: file-menu, \
2: edit-menu, 3: view-menu, 4: options-menu, 5: help-menu, 6: default popup menu"
  (XtUnmanageChild (list-ref (menu-widgets) which)))

#|
(define (add-second-row)
  ;; adds a row-column widget just under the menu bar
  (let ((menu-bar (car (menu-widgets)))
	(main-pane (caddr (main-widgets)))
	(sound-pane (cadddr (main-widgets))))
    (XtUnmanageChild sound-pane)
    (let* ((second-row (XtCreateManagedWidget "second-row" xmRowColumnWidgetClass main-pane
					      (list XmNleftAttachment    XmATTACH_FORM
						    XmNrightAttachment  XmATTACH_FORM
						    XmNbottomAttachment XmATTACH_NONE
						    XmNtopAttachment    XmATTACH_WIDGET
						    XmNtopWidget        menu-bar
						    XmNbackground       (highlight-color))))
	   (sep (XtCreateManagedWidget "sep" xmSeparatorWidgetClass main-pane
				              (list XmNleftAttachment    XmATTACH_FORM
						    XmNrightAttachment  XmATTACH_FORM
						    XmNbottomAttachment XmATTACH_NONE
						    XmNtopAttachment    XmATTACH_WIDGET
						    XmNtopWidget        second-row
						    XmNbackground       (highlight-color)
						    XmNorientation      XmHORIZONTAL
						    XmNseparatorType    XmSHADOW_ETCHED_OUT))))
      (XtVaSetValues sound-pane (list XmNtopAttachment XmATTACH_WIDGET
				      XmNtopWidget sep
				      XmNbackground       (highlight-color)))
      (XtManageChild sound-pane)
      (XtCreateManagedWidget "a name" xmPushButtonWidgetClass second-row
			     (list XmNbackground       (highlight-color))))))
|#

;;; -------- disable control panel --------

(define (disable-control-panel snd)
  "(disable-control-panel snd) disables the control panel for the sound 'snd'"
  (let ((swc (caddr (sound-widgets snd))))
    (for-each-child 
     swc 
     (lambda (n) 
       (if (and (not (string=? (XtName n) "snd-name-form")) 
		(not (string=? (XtName (XtParent n)) "snd-name-form")))
	   (XtUnmanageChild n))))
    (XtSetValues swc (list XmNpaneMaximum 1 
			   XmNpaneMinimum 1))
    (remove-from-menu 2 "Show controls")
    (XtManageChild swc)))



;;; -------- bring possibly-obscured dialog to top

(if (not (defined? 'raise-dialog))
    (define (raise-dialog w)
      "(raise-dialog w) tries to bring 'w' to the top of the widget heirarchy"
      (if (and (Widget? w) 
	       (XtIsManaged w))
	  (let ((parent (XtParent w)))
	    (if (and (Widget? parent)
		     (XtIsSubclass parent xmDialogShellWidgetClass))
		(XtPopup parent XtGrabNone))))))



;;; -------- hidden controls panel --------

(define hidden-controls-dialog #f)
(define hidden-controls '())

(define hidden-controls-help 
"Expand-hop sets the time in seconds between successive grains.
Expand-length sets the length of each grain.
Expand-ramp sets the ramp-time in the grain envelope.
Expand-jitter sets the grain timing jitter.
Contrast-amp sets the prescaler for contrast-enhancement.
Reverb-lowpass sets the feedback lowpass filter coeficient.
Reverb-feedback sets the scaler on the feedback.
")

(define (make-hidden-controls-dialog)
  
  (define (reset-all-sliders)
    (for-each
     (lambda (ctl)
       (set! ((caddr ctl) #t) (cadr ctl))
       (XtSetValues (car ctl) 
		    (list XmNvalue (inexact->exact (floor (* (cadr ctl) 100))))))
     hidden-controls))

  (if (not (Widget? hidden-controls-dialog))
      (let ((xdismiss (XmStringCreate "Dismiss" XmFONTLIST_DEFAULT_TAG))
	    (xhelp (XmStringCreate "Help" XmFONTLIST_DEFAULT_TAG))
	    (titlestr (XmStringCreate "More Controls" XmFONTLIST_DEFAULT_TAG))
	    (xreset (XmStringCreate "Reset" XmFONTLIST_DEFAULT_TAG)))
	(set! hidden-controls-dialog 
	      (XmCreateTemplateDialog (cadr (main-widgets)) "More Controls"
                (list XmNcancelLabelString   xreset
		      XmNokLabelString       xdismiss
		      XmNhelpLabelString     xhelp
		      XmNautoUnmanage        #f
		      XmNdialogTitle         titlestr
		      XmNresizePolicy        XmRESIZE_GROW
	              XmNnoResize            #f
		      XmNtransient           #f
		      XmNbackground          (basic-color))))

	(for-each
	 (lambda (button color)
	   (XtVaSetValues (XmMessageBoxGetChild hidden-controls-dialog button)
			  (list XmNarmColor   (pushed-button-color)
				XmNbackground color)))
	 (list XmDIALOG_HELP_BUTTON XmDIALOG_CANCEL_BUTTON XmDIALOG_OK_BUTTON)
	 (list (help-button-color) (reset-button-color) (quit-button-color)))

	(XtAddCallback hidden-controls-dialog 
		       XmNokCallback (lambda (w context info)
				       (XtUnmanageChild hidden-controls-dialog)))
	(XtAddCallback hidden-controls-dialog 
		       XmNhelpCallback (lambda (w context info)
					 (help-dialog "More Controls" hidden-controls-help)))
	(XtAddCallback hidden-controls-dialog
		       XmNcancelCallback (lambda (w context info)
					   (reset-all-sliders)))
	(XmStringFree xhelp)
	(XmStringFree xdismiss)
	(XmStringFree titlestr)
	(XmStringFree xreset)

	(let* ((mainform 
		(XtCreateManagedWidget "formd" xmRowColumnWidgetClass hidden-controls-dialog
                  (list XmNleftAttachment      XmATTACH_FORM
		        XmNrightAttachment     XmATTACH_FORM
		        XmNtopAttachment       XmATTACH_FORM
		        XmNbottomAttachment    XmATTACH_WIDGET
		        XmNbottomWidget        (XmMessageBoxGetChild hidden-controls-dialog XmDIALOG_SEPARATOR)
                        XmNorientation         XmVERTICAL))))
	  (for-each
	   (lambda (lst)
	     (let* ((name (car lst))
		    (low (cadr lst))
		    (high (caddr lst))
		    (initial (list-ref lst 3))
		    (func (list-ref lst 4))
		    (title (XmStringCreate name XmFONTLIST_DEFAULT_TAG))
		    (slider (XtCreateManagedWidget name xmScaleWidgetClass mainform
			       (list XmNorientation   XmHORIZONTAL
				     XmNshowValue     #t
				     XmNminimum       (inexact->exact (floor (* low 1000)))
				     XmNmaximum       (inexact->exact (floor (* high 1000)))
				     XmNvalue         (inexact->exact (floor (* initial 1000)))
				     XmNdecimalPoints 3
				     XmNtitleString   title
				     XmNborderWidth   1
				     XmNbackground    (basic-color)))))
	       (XmStringFree title)
	       (set! hidden-controls (cons (list slider initial func) hidden-controls))
	       (XtAddCallback slider
			      XmNvalueChangedCallback 
			      (lambda (w context info)
				(set! (func) (/ (.value info) 1000.0))))
	       (XtAddCallback slider
			      XmNdragCallback 
			      (lambda (w context info)
				(set! (func) (/ (.value info) 1000.0))))))
	   (list (list "expand-hop" 0.001 0.3 0.05  expand-control-hop)
		 (list "expand-length" 0.01 .5 0.15 expand-control-length)
		 (list "expand-ramp" 0.01 .5 0.4 expand-control-ramp)
		 (list "expand-jitter" 0.0 2.0 1.0 expand-control-jitter)
		 (list "contrast-amp" 0.0 2.0 1.0 contrast-control-amp)
		 (list "reverb-lowpass" 0.0 1.0 0.7 reverb-control-lowpass)
		 (list "reverb-feedback" 0.0 1.25 1.09 reverb-control-feedback))))
	(add-to-menu 3 "Hidden controls"
		     (lambda ()
		       (if (not (XtIsManaged hidden-controls-dialog))
			   (XtManageChild hidden-controls-dialog)
			   (raise-dialog hidden-controls-dialog)))))))


;;; -------- create-fmv-dialog --------

(define fmv-dialog #f)

(define (create-fmv-dialog)
  "(create-fmv-dialog) makes a dialog that runs the fm-violin instrument with various controls"
  (if (not (Widget? fmv-dialog))
      (let ((xdismiss (XmStringCreate "Dismiss" XmFONTLIST_DEFAULT_TAG))
	    (xhelp (XmStringCreate "Help" XmFONTLIST_DEFAULT_TAG))
	    (titlestr (XmStringCreate "FM Violin" XmFONTLIST_DEFAULT_TAG))
	    (running #f))
	(set! fmv-dialog 
	      (XmCreateTemplateDialog (cadr (main-widgets)) "fm-violin"
                (list XmNcancelLabelString   xdismiss
		      XmNhelpLabelString     xhelp
		      XmNautoUnmanage        #f
		      XmNdialogTitle         titlestr
		      XmNresizePolicy        XmRESIZE_GROW
	              XmNnoResize            #f
		      XmNbackground          (basic-color)
		      XmNwidth               400
		      XmNtransient           #f) ))
	(XtAddCallback fmv-dialog 
		       XmNcancelCallback (lambda (w context info)
					   (if running (set! running #f))
					   (XtUnmanageChild fmv-dialog)))
	(XtAddCallback fmv-dialog XmNhelpCallback (lambda (w context info) (snd-print "set 'play' and move the sliders!")))
	(XmStringFree xhelp)
	(XmStringFree xdismiss)
	(XmStringFree titlestr)
	
	(let* ((frequency 440.0)
	       (amplitude 0.1)
	       (fm-index 1.0)
	       (periodic-vibrato-rate 5.0) 
	       (random-vibrato-rate 16.0)
	       (periodic-vibrato-amplitude 0.0025) 
	       (random-vibrato-amplitude 0.005)
	       (noise-amount 0.0) 
	       (noise-freq 1000.0)
	       (fm1-rat 1.0) 
	       (fm2-rat 3.0)	 
	       (fm3-rat 4.0)                    
	       (frq-scl (hz->radians frequency))
	       (maxdev (* frq-scl fm-index))
	       (logfreq (log frequency))
	       (index1 (min pi (* maxdev (/ 5.0 logfreq))))
	       (index2 (min pi (* maxdev 3.0 (/ (- 8.5 logfreq) (+ 3.0 (* frequency .001))))))
	       (index3 (min pi (* maxdev (/ 4.0 (sqrt frequency)))))
	       (carrier (make-oscil frequency))
	       (fmosc1 (make-oscil (* fm1-rat frequency)))
	       (fmosc2 (make-oscil (* fm2-rat frequency)))
	       (fmosc3 (make-oscil (* fm3-rat frequency)))
	       (pervib (make-triangle-wave periodic-vibrato-rate (* periodic-vibrato-amplitude frq-scl)))
	       (ranvib (make-rand-interp random-vibrato-rate (* random-vibrato-amplitude frq-scl)))
	       (fm-noi (make-rand noise-freq (* pi noise-amount))))
	  (letrec ((v (lambda ()
			(let ((vib (+ (triangle-wave pervib) (rand-interp ranvib)))
			      (fuzz (rand fm-noi)))
			  (* amplitude
			     (oscil carrier 
				    (+ vib 
				       (* index1 (oscil fmosc1 (+ (* fm1-rat vib) fuzz)))
				       (* index2 (oscil fmosc2 (+ (* fm2-rat vib) fuzz)))
				       (* index3 (oscil fmosc3 (+ (* fm3-rat vib) fuzz)))))))))
		   (set-frequency (lambda (frq)
				    (set! frequency frq)
				    (set! frq-scl (hz->radians frequency))
				    (set! maxdev (* frq-scl fm-index))
				    (set! logfreq (log frequency))
				    (set! index1 (min pi (* maxdev (/ 5.0 logfreq))))
				    (set! index2 (min pi (* maxdev 3.0 (/ (- 8.5 logfreq) (+ 3.0 (* frequency .001))))))
				    (set! index3 (min pi (* maxdev (/ 4.0 (sqrt frequency)))))
				    (set! (mus-frequency carrier) frequency)
				    (set! (mus-frequency fmosc1) (* fm1-rat frequency))
				    (set! (mus-frequency fmosc2) (* fm2-rat frequency))
				    (set! (mus-frequency fmosc3) (* fm3-rat frequency))))
		   (set-index (lambda (ind)
				(set! fm-index ind)
				(set! maxdev (* frq-scl fm-index))
				(set! index1 (min pi (* maxdev (/ 5.0 logfreq))))
				(set! index2 (min pi (* maxdev 3.0 (/ (- 8.5 logfreq) (+ 3.0 (* frequency .001))))))
				(set! index3 (min pi (* maxdev (/ 4.0 (sqrt frequency)))))))
		   (set-noise (lambda (noi)
				(set! noise-amount noi)
				(set! (mus-scaler fm-noi) (* pi noise-amount)))))
	    (let* ((mainform 
		    (XtCreateManagedWidget "formd" xmRowColumnWidgetClass fmv-dialog
					   (list XmNleftAttachment      XmATTACH_FORM
						 XmNrightAttachment     XmATTACH_FORM
						 XmNtopAttachment       XmATTACH_FORM
						 XmNbottomAttachment    XmATTACH_WIDGET
						 XmNbottomWidget        (XmMessageBoxGetChild fmv-dialog XmDIALOG_SEPARATOR)
						 XmNbackground          (basic-color)
						 XmNorientation         XmVERTICAL)))
		   (button 
		    (XtCreateManagedWidget "play" xmToggleButtonWidgetClass mainform
					   (list XmNbackground  (basic-color))))
		   (ampstr (XmStringCreate "amp" XmFONTLIST_DEFAULT_TAG))
		   (amp-scale
		    (XtCreateManagedWidget "amp" xmScaleWidgetClass mainform
					   (list XmNorientation XmHORIZONTAL
						 XmNshowValue   #t
						 XmNbackground  (basic-color)
						 XmNvalue       (inexact->exact (floor (* amplitude 100)))
						 XmNmaximum     100
						 XmNtitleString ampstr
						 XmNdecimalPoints 2)))
		   (freqstr (XmStringCreate "freq" XmFONTLIST_DEFAULT_TAG))
		   (freq-scale
		    (XtCreateManagedWidget "freq" xmScaleWidgetClass mainform
					   (list XmNorientation XmHORIZONTAL
						 XmNshowValue   #t
						 XmNbackground  (basic-color)
						 XmNvalue       (inexact->exact frequency)
						 XmNmaximum     1000
						 XmNtitleString freqstr
						 XmNdecimalPoints 0)))
		   (indexstr (XmStringCreate "index" XmFONTLIST_DEFAULT_TAG))
		   (index-scale
		    (XtCreateManagedWidget "index" xmScaleWidgetClass mainform
					   (list XmNorientation XmHORIZONTAL
						 XmNshowValue   #t
						 XmNbackground  (basic-color)
						 XmNvalue       (inexact->exact (* 10 fm-index))
						 XmNmaximum     100
						 XmNtitleString indexstr
						 XmNdecimalPoints 1)))
		   (noisestr (XmStringCreate "noise" XmFONTLIST_DEFAULT_TAG))
		   (noise-scale
		    (XtCreateManagedWidget "noise" xmScaleWidgetClass mainform
					   (list XmNorientation XmHORIZONTAL
						 XmNshowValue   #t
						 XmNbackground  (basic-color)
						 XmNvalue       (inexact->exact (* 100 noise-amount))
						 XmNmaximum     100
						 XmNtitleString noisestr
						 XmNdecimalPoints 3))))
	      (XmStringFree ampstr)
	      (XmStringFree freqstr)
	      (XmStringFree indexstr)
	      (XmStringFree noisestr)
	      (XtAddCallback amp-scale XmNvalueChangedCallback (lambda (w context info) (set! amplitude (* .01 (.value info)))))
	      (XtAddCallback amp-scale XmNdragCallback (lambda (w context info) (set! amplitude (* .01 (.value info)))))
	      (XtAddCallback freq-scale XmNvalueChangedCallback (lambda (w context info) (set-frequency (.value info))))
	      (XtAddCallback freq-scale XmNdragCallback (lambda (w context info) (set-frequency (.value info))))
	      (XtAddCallback index-scale XmNvalueChangedCallback (lambda (w context info) (set-index (* .1 (.value info)))))
	      (XtAddCallback index-scale XmNdragCallback (lambda (w context info) (set-index (* .1 (.value info)))))
	      (XtAddCallback noise-scale XmNvalueChangedCallback (lambda (w context info) (set-noise (* .001 (.value info)))))
	      (XtAddCallback noise-scale XmNdragCallback (lambda (w context info) (set-noise (* .001 (.value info)))))
	      (XtAddCallback button XmNvalueChangedCallback 
			     (lambda (w context info)
			       (if running
				   (set! running #f)
				   (let* ((audio-info (open-play-output 1 22050 #f 128))
					  (audio-fd (car audio-info))
					  (outchans (cadr audio-info))
					  (frames (caddr audio-info))
					  (data (make-sound-data outchans frames)))
				     (if (not (= audio-fd -1))
					 (begin
					   (set! running #t)
					   (do ()
					       ((or (c-g?) (not running))
						(begin
						  (set! running #f)
						  (mus-audio-close audio-fd)))
					     (do ((k 0 (1+ k)))
						 ((= k frames))
					       (sound-data-set! data 0 k (v)))
					     (mus-audio-write audio-fd data frames)))))))))))))
  (XtManageChild fmv-dialog))



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
  "(make-pixmap w strs) creates a pixmap using the X/Xpm string-based pixmap description"
  (if (defined? 'XpmAttributes)
      (let* ((attr (XpmAttributes))
	     (symb (XpmColorSymbol "basiccolor" #f (basic-color)))
	     (dpy (XtDisplay widget))
	     (win (XtWindow widget))
	     (scr (DefaultScreen dpy))
	     (depth (cadr (XtGetValues widget (list XmNdepth 0))))
	     (colormap (cadr (XtGetValues widget (list XmNcolormap 0)))))
	(set! (.depth attr) depth)
	(set! (.colormap attr) colormap)
	(set! (.visual attr) (DefaultVisual dpy scr))
	(set! (.colorsymbols attr) (list symb))
	(set! (.numsymbols attr) 1)
	(set! (.valuemask attr) (logior XpmColorSymbols XpmDepth XpmColormap XpmVisual))
	(cadr (XpmCreatePixmapFromData dpy win strs attr)))
      #f))

; (XtSetValues (list-ref (sound-widgets) 8) (list XmNlabelPixmap (make-pixmap (cadr (main-widgets)) arrow-strs))))

;;; if you have a nice background pixmap, you can map it over all of Snd with:
#|
(load-from-path "new-backgrounds.scm")
(define wd (make-pixmap (cadr (main-widgets)) wood))

(define (paint-all widget)
  (for-each-child 
    widget
    (lambda (w) 
      (XtSetValues w (list XmNbackgroundPixmap wd))
      (if (XmIsLabel w)
	  (let ((val (cadr (XtVaGetValues w (list XmNlabelType 0)))))
	    (if (= val XmPIXMAP)
		(XtVaSetValues w (list XmNlabelPixmap wd))))))))

(paint-all (cadr (main-widgets)))
(for-each
 (lambda (w)
   (if w
       (paint-all w)))
 (dialog-widgets))
 
(add-hook! new-widget-hook paint-all)
|#

(define right-arrow (list
   #x00 #x04 #x10 #x08 #x00 #x10 #x04 #x20 #x00 #x40 #xa5 #xbf
   #x00 #x40 #x04 #x20 #x00 #x10 #x10 #x08 #x00 #x04 #x00 #x00))

(define (bitmap->pixmap widget bits width height)
  "(bitmap->pixmap widget bits width height) takes an X-style bitmap and turns it into a pixmap"
  (XCreateBitmapFromData (XtDisplay widget) (XtWindow widget) bits width height))

; (XtSetValues (list-ref (sound-widgets) 8) (list XmNlabelPixmap (bitmap->pixmap (list-ref (sound-widgets) 8) iconw right-arrow 16 12)))



;;; -------- display-scanned-synthesis --------
;;;
;;; open a new main pane below the listener, with two sections
;;;  on the left various controls, on the right a graph
;;;  push 'start' to start the scanned synthesis display
;;;  if spring > mass, you'll get overflows

(define (display-scanned-synthesis)
  
  (define (add-main-pane name type args)
    (XtCreateManagedWidget name type (list-ref (main-widgets) 3) args))
  
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
  
  (if (< (window-height) 520) (set! (window-height) 520))
  (set! (mus-srate) 22050.0)
  
  (let* ((mass 1.0)
	 (xspring 0.1)
	 (damp 0.0)
	 (bounds '())
	 (pts0 #f)
	 (pts1 #f)
	 (playing #f)
	 ;; (frequency 440.0)
	 (amplitude 0.02)
	 (ax0 0) (ax1 0) (ay0 0) (ay1 0)
	 (gc (car (snd-gcs)))
	 (egc (list-ref (snd-gcs) 7))
	 (app (car (main-widgets)))
	 
	 ;; now set up a paned window in the main Snd window with controllers on the left and the graph on the right
	 (scan-outer (add-main-pane "Scanned Synthesis" xmFormWidgetClass
				    (list XmNbackground (basic-color)
					  XmNpaneMinimum 520)))
	 (scan-row (XtCreateManagedWidget "row" xmRowColumnWidgetClass scan-outer
					  (list XmNbackground       (basic-color)
						XmNorientation      XmVERTICAL
						XmNleftAttachment   XmATTACH_FORM
						XmNtopAttachment    XmATTACH_FORM
						XmNbottomAttachment XmATTACH_FORM
						XmNrightAttachment  XmATTACH_POSITION
						XmNrightPosition    32)))
	 
	 ;; the graph
	 (scan-pane (XtCreateManagedWidget "draw" xmDrawingAreaWidgetClass scan-outer
					   (list XmNbackground       (graph-color)
						 XmNforeground       (data-color)
						 XmNleftAttachment   XmATTACH_WIDGET
						 XmNleftWidget       scan-row
						 XmNtopAttachment    XmATTACH_FORM
						 XmNbottomAttachment XmATTACH_FORM
						 XmNrightAttachment  XmATTACH_FORM)))
	 
	 ;; the controllers
	 (scan-start (XtCreateManagedWidget "Start" xmPushButtonWidgetClass scan-row
					    (list XmNbackground (basic-color)
						  XmNarmColor   (pushed-button-color))))
	 (scan-continue (XtCreateManagedWidget "Continue" xmPushButtonWidgetClass scan-row
					       (list XmNbackground (basic-color)
						     XmNarmColor   (pushed-button-color))))
	 (scan-stop (XtCreateManagedWidget "Stop" xmPushButtonWidgetClass scan-row
					   (list XmNbackground (basic-color)
						 XmNarmColor   (pushed-button-color))))
	 (size 128)
	 (tbl (make-table-lookup :size size))
	 (gx0 (mus-data tbl))
	 (gx1 (make-vct size))	   
	 (gx2 (make-vct size))
	 (vect (make-vector (* 2 size)))
	 (work-proc 0))
    
    (define (y->grfy y range)
      (min ay1
	   (max ay0
		(inexact->exact
		 (round (+ ay0
			   (* range (- 10.0 y))))))))
    
    (define (draw-graph)
      (if (and (> ax1 ax0)
	       (> ay1 ay0))
	  (let ((diff (* 0.05 (- ay1 ay0))) ; assuming -10 to 10 
		(dpy (XtDisplay scan-pane))
		(wn (XtWindow scan-pane))
		(xincr (exact->inexact (/ (- ax1 ax0) size))))
	    (if pts1
		(XDrawLinesDirect dpy wn egc pts1 size 0)
		(XFillRectangle dpy wn egc ; erase previous graph
				(+ ax0 2)
				ay0
				(- ax1 ax0 2)
				(- ay1 ay0)))
	    (do ((i 0 (1+ i))
		 (j 0 (+ j 2))
		 (xi ax0 (+ xi xincr)))
		((= i size))
	      (vector-set! vect j (inexact->exact (floor xi)))
	      (vector-set! vect (+ j 1) (y->grfy (vct-ref gx0 i) diff)))
	    (if pts1 (freeXPoints pts1))
	    (set! pts0 (vector->XPoints vect))
	    (set! pts1 pts0)
	    (XDrawLinesDirect dpy wn gc pts0 size 0))))
    
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
      (if (XtWorkProcId? work-proc)
	  (XtRemoveWorkProc work-proc))
      (set! work-proc 0)
      (set! playing #f))
    
    (define (start-synthesis)
      (stop-synthesis)
      (vct-fill! gx0 0.0)
      (vct-fill! gx1 0.0)
      (vct-fill! gx2 0.0)
      (do ((i 0 (1+ i)))
	  ((= i 12))
	(let ((val (sin (/ (* 2 pi i) 12.0))))
	  (vct-set! gx1 (+ i (- (/ size 4) 6)) val)))
      (set! work-proc (XtAppAddWorkProc app tick-synthesis)))
    
    (define (continue-synthesis)
      (stop-synthesis)
      (set! work-proc (XtAppAddWorkProc app tick-synthesis)))
    
    ;; controller callbacks
    (for-each 
     (lambda (data)
       (let* ((title (XmStringCreate (car data) XmFONTLIST_DEFAULT_TAG))
	      (button (XtCreateManagedWidget (car data) xmScaleWidgetClass scan-row
					     (list XmNbackground    (basic-color)
						   XmNorientation   XmHORIZONTAL
						   XmNshowValue     #t
						   XmNminimum       (list-ref data 1)
						   XmNmaximum       (list-ref data 2)
						   XmNvalue         (list-ref data 3)
						   XmNdecimalPoints (list-ref data 4)
						   XmNtitleString   title))))
	 (XtAddCallback button XmNdragCallback (lambda (w c i) ((list-ref data 5) (.value i))))
	 (XtAddCallback button XmNvalueChangedCallback (lambda (w c i) ((list-ref data 5) (.value i))))
	 (XmStringFree title)))
     (list (list "mass" 1 200 100 2 (lambda (val) (set! mass (/ val 100.0))))
	   (list "spring" 1 100 10 2 (lambda (val) (set! xspring (/ val 100.0))))
	   (list "damping" 0 100 0 4 (lambda (val) (set! damp (/ val 10000.0))))))
    
    (let* ((scan-size (XtCreateManagedWidget "srow" xmFormWidgetClass scan-row
					     (list  XmNbackground (basic-color))))
	   (scan-label (XtCreateManagedWidget "Size:" xmLabelWidgetClass scan-size
					      (list XmNbackground       (basic-color)
						    XmNleftAttachment   XmATTACH_FORM
						    XmNtopAttachment    XmATTACH_FORM
						    XmNbottomAttachment XmATTACH_FORM
						    XmNrightAttachment  XmATTACH_NONE)))
	   (scan-text (XtCreateManagedWidget "stext" xmTextFieldWidgetClass scan-size
					     (list XmNbackground       (basic-color)
						   XmNvalue            (number->string size)
						   XmNleftAttachment   XmATTACH_WIDGET
						   XmNleftWidget       scan-label
						   XmNtopAttachment    XmATTACH_FORM
						   XmNbottomAttachment XmATTACH_FORM
						   XmNrightAttachment  XmATTACH_FORM)))
	   (play-button (XtCreateManagedWidget "play" xmToggleButtonWidgetClass scan-row
					       (list XmNbackground  (basic-color)
						     XmNselectColor (pushed-button-color))))
	   (freq-str (XmStringCreate "frequency" XmFONTLIST_DEFAULT_TAG))
	   (freq-scale (XtCreateManagedWidget "frequency" xmScaleWidgetClass scan-row
					      (list XmNbackground    (basic-color)
						    XmNorientation   XmHORIZONTAL
						    XmNshowValue     #t
						    XmNminimum       20
						    XmNmaximum       1000
						    XmNvalue         440
						    XmNdecimalPoints 0
						    XmNtitleString   freq-str)))
	   (amp-str (XmStringCreate "amplitude" XmFONTLIST_DEFAULT_TAG))
	   (amp-scale (XtCreateManagedWidget "amplitude" xmScaleWidgetClass scan-row
					     (list XmNbackground    (basic-color)
						   XmNorientation   XmHORIZONTAL
						   XmNshowValue     #t
						   XmNminimum       0
						   XmNmaximum       100
						   XmNvalue         10
						   XmNdecimalPoints 3
						   XmNtitleString   amp-str))))
      (XmStringFree freq-str)
      (XmStringFree amp-str)
      
      (XtAddEventHandler scan-text EnterWindowMask #f
			 (lambda (w context ev flag)
			   (XmProcessTraversal w XmTRAVERSE_CURRENT)
			   (XtSetValues w (list XmNbackground (white-pixel)))))
      (XtAddEventHandler scan-text LeaveWindowMask #f
			 (lambda (w context ev flag)
			   (XtSetValues w (list XmNbackground (basic-color)))))
      (XtAddCallback scan-text XmNactivateCallback 
		     (lambda (w c i)
		       (stop-synthesis)
		       (set! size (string->number (cadr (XtGetValues scan-text (list XmNvalue 0)))))
		       (set! tbl (make-table-lookup :size size))
		       (set! gx0 (mus-data tbl))
		       (set! gx1 (make-vct size))	   
		       (set! gx2 (make-vct size))
		       (set! vect (make-vector (* size 2)))))
      
      (XtAddCallback freq-scale XmNdragCallback (lambda (w c i) (set! (mus-frequency tbl) (.value i))))
      (XtAddCallback freq-scale XmNvalueChangedCallback (lambda (w c i) (set! (mus-frequency tbl) (.value i))))
      (XtAddCallback amp-scale XmNdragCallback (lambda (w c i) (set! amplitude (* .001 (.value i)))))
      (XtAddCallback amp-scale XmNvalueChangedCallback (lambda (w c i) (set! amplitude (* .001 (.value i)))))
      
      (XtAddCallback play-button XmNvalueChangedCallback 
		     (lambda (w context info)
		       (if playing
			   (set! playing #f)
			   (let* ((audio-info (open-play-output 1 22050 #f 128))
				  (audio-fd (car audio-info))
				  (outchans (cadr audio-info))
				  (frames (caddr audio-info))
				  (data (make-sound-data outchans frames)))
			     (set! playing #t)
			     (if (not (= audio-fd -1))
				 (do ()
				     ((or (c-g?) (not playing)) ; can also happen if top Stop button pressed
				      (begin
					(set! playing #f)
					(XmToggleButtonSetValue play-button 0 #f) ; don't send event
					(mus-audio-close audio-fd)))
				   (tick-synthesis work-proc)
				   (do ((k 0 (1+ k)))
				       ((= k frames))
				     (sound-data-set! data 0 k (* amplitude (table-lookup tbl))))
				   (mus-audio-write audio-fd data frames))
				 (set! playing #f)))))))
    
    (XtAddCallback scan-pane XmNresizeCallback (lambda (w context info) (redraw-graph)))
    (XtAddCallback scan-pane XmNexposeCallback (lambda (w context info) (redraw-graph)))
    (XtAddEventHandler scan-pane ButtonPressMask #f
		       (lambda (w context ev flag)
			 (if (not (XtWorkProcId? work-proc))
			     (if (= (.button ev) 2)
				 (continue-synthesis)
				 (start-synthesis))
			     (stop-synthesis))))
    
    (XtAddCallback scan-start XmNactivateCallback (lambda (w c i) (start-synthesis)))
    (XtAddCallback scan-continue XmNactivateCallback (lambda (w c i) (continue-synthesis)))
    (XtAddCallback scan-stop XmNactivateCallback (lambda (w c i) (stop-synthesis)))
    
    #t ; for slightly prettier listener output
    ))

(define (close-scanned-synthesis-pane)
  "(close-scanned-synthesis-pane) closes the Scanned Sythesis sound pane"
  (for-each-child 
   (cadr (main-widgets))  ; this is Snd's outermost shell
   (lambda (n)
     (if (string=? (XtName n) "Scanned Synthesis")
	 (XtUnmanageChild n)))))



;;; -------- add-mark-pane --------
;;;
;;; adds a pane to each channel giving the current mark locations (sample values)
;;;   these can be edited to move the mark, or deleted to delete the mark
;;;   can't use channel-property here because the widget lists are permanent (just unmanaged)

(define including-mark-pane #f) ; for prefs

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
	       (Widget? (mark-list snd chn)))
	  (for-each
	   (lambda (n)
	     (XtUnmanageChild n))
	   (cadr (XtGetValues (mark-list snd chn) (list XmNchildren 0) 1))))))
  
  (define (make-mark-list snd chn)
    (let ((current-mark-list-length (mark-list-length snd chn)))
      (deactivate-channel snd chn)
      (if (not (Widget? (mark-list snd chn)))
	  (let* ((mark-box (add-channel-pane snd chn "mark-box" xmFormWidgetClass
			          (list XmNbackground       (basic-color)
				        XmNorientation      XmVERTICAL
				        XmNpaneMinimum      100
				        XmNbottomAttachment XmATTACH_FORM)))
		 (mark-label (XtCreateManagedWidget "Marks" xmLabelWidgetClass mark-box
			          (list XmNbackground       (highlight-color)
				        XmNleftAttachment   XmATTACH_FORM
				        XmNrightAttachment  XmATTACH_FORM
				        XmNalignment        XmALIGNMENT_CENTER
				        XmNtopAttachment    XmATTACH_FORM)))
		 (mark-scroller (XtCreateManagedWidget "mark-scroller" xmScrolledWindowWidgetClass mark-box
			          (list XmNbackground       (basic-color)
				        XmNscrollingPolicy  XmAUTOMATIC
				        XmNscrollBarDisplayPolicy XmSTATIC
				        XmNleftAttachment   XmATTACH_FORM
				        XmNrightAttachment  XmATTACH_FORM
				        XmNtopAttachment    XmATTACH_WIDGET
				        XmNtopWidget        mark-label
				        XmNbottomAttachment XmATTACH_FORM)))
		 (mlist (XtCreateManagedWidget "mark-list"  xmRowColumnWidgetClass mark-scroller
			          (list XmNorientation      XmVERTICAL
				        XmNtopAttachment    XmATTACH_FORM
				        XmNbottomAttachment XmATTACH_FORM
				        XmNspacing          0))))
	    (set-main-color-of-widget mark-scroller)
	    (XtSetValues mark-box (list XmNpaneMinimum 1))
	    (set! (mark-list snd chn) (list snd chn mlist))))
      (let ((new-marks (marks snd chn)))
	(if (> (length new-marks) current-mark-list-length)
	    (let* ((lst (mark-list snd chn)))
	      (do ((i current-mark-list-length (1+ i)))
		  ((= i (length new-marks)))
		(let ((tf (XtCreateWidget "field" xmTextFieldWidgetClass lst
					   (list XmNbackground (basic-color)))))
		  (XtAddCallback tf XmNfocusCallback
				 (lambda (w c i)
				   (XtSetValues w (list XmNbackground (white-pixel)))))
		  (XtAddCallback tf XmNlosingFocusCallback
				 (lambda (w c i)
				   (XtSetValues w (list XmNbackground (basic-color)))))
		  (XtAddCallback tf XmNactivateCallback
				 (lambda (w c i)
				   (let* ((id (cadr (XtGetValues w (list XmNuserData 0))))
					  (txt (cadr (XtGetValues w (list XmNvalue 0))))
					  (samp (if (and (string? txt) 
							 (> (string-length txt) 0))
						    (string->number txt)
						    #f)))
				     (if samp
					 (set! (mark-sample id) samp)
					 (delete-mark id))
				     (XtSetValues w (list XmNbackground (basic-color))))))))))
	(set! (mark-list-length snd chn) (length new-marks))
	(let* ((lst (mark-list snd chn)))
	  (call-with-current-continuation
	   (lambda (quit)
	     (for-each
	      (lambda (n)
		(if (null? new-marks) (quit #f))
		(if (XmIsTextField n)
		    (begin
		      (XtSetValues n (list XmNvalue (number->string (mark-sample (car new-marks)))
					   XmNuserData (car new-marks)))
		      (XtManageChild n)
		      (set! new-marks (cdr new-marks)))))
	      (cadr (XtGetValues lst (list XmNchildren 0) 1)))))))
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
      (add-hook! (after-edit-hook snd i) (lambda () (if (Widget? (mark-list snd i)) (make-mark-list snd i))))
      (add-hook! (undo-hook snd i) (lambda () (if (Widget? (mark-list snd i)) (make-mark-list snd i))))))

  (set! including-mark-pane #t)
  (add-hook! mark-hook remark)
  (add-hook! close-hook unremark)
  (add-hook! after-open-hook open-remarks)
  (add-hook! update-hook (lambda (snd) 
			   ;; update-sound (called if header is changed, for example), calls open-sound
			   ;;   which restores our channel-local mark-pane hooks, but doesn't re-activate
			   ;;   the mark pane itself. So, we return a procedure from the update-hook
			   ;;   evaluation that will recreate our pane immediately upon update completion.
			   (lambda (updated-snd) 
			     ;; this is the procedure to be called when the update is done
			     (do ((i 0 (1+ i)))
				 ((= i (chans updated-snd)))
			       (make-mark-list updated-snd i)))))
  )


;;; -------- select-file --------
;;;
;;; (select-file func :optional title dir filter help)
;;;   starts a Snd-like File Selection Dialog, runs func if a file is selected
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
    (define (find-dialog-widget wid ds)
      (if (null? ds)
	  #f
	  (if (equal? wid (caar ds))
	      (car ds)
	      (find-dialog-widget wid (cdr ds)))))
    (lambda args
      ;; (file-select func title dir filter help)
      (let* ((func (if (> (length args) 0) (list-ref args 0) #f))
	     (title (if (> (length args) 1) (list-ref args 1) "select file"))
	     (dir (if (> (length args) 2) (list-ref args 2) "."))
	     (filter (if (> (length args) 3) (list-ref args 3) "*"))
	     (help (if (> (length args) 4) (list-ref args 4) #f))
	     (dialog (or (find-free-dialog file-selector-dialogs)
		 	 (let ((new-dialog (XmCreateFileSelectionDialog 
					     (cadr (main-widgets)) 
					     title
					     (list XmNbackground (basic-color)))))
			   (XtAddCallback new-dialog XmNhelpCallback
					    (lambda (w c i)
					      (let ((lst (find-dialog-widget w file-selector-dialogs)))
						(if (list-ref lst 4)
						    (help-dialog (list-ref lst 3) (list-ref lst 4))))))
			   (XtAddCallback new-dialog XmNokCallback 
					   (lambda (w c i)
					     (let ((lst (find-dialog-widget w file-selector-dialogs)))
					       ((list-ref lst 2) (cadr (XmStringGetLtoR (.value i) XmFONTLIST_DEFAULT_TAG)))
					       (list-set! lst 1 #f)
					       (XtUnmanageChild w))))
			   (XtAddCallback new-dialog XmNcancelCallback
					   (lambda (w c i)
					     (let ((lst (find-dialog-widget w file-selector-dialogs)))
					       (list-set! lst 1 #f)
					       (XtUnmanageChild w))))
			  (set! file-selector-dialogs (cons (list new-dialog #t func title help) file-selector-dialogs))
			  (set-main-color-of-widget new-dialog)
			  (XtSetValues (XmFileSelectionBoxGetChild new-dialog XmDIALOG_DIR_LIST) 
					(list XmNbackground (white-pixel)))
			  (XtSetValues (XmFileSelectionBoxGetChild new-dialog XmDIALOG_LIST) 
					(list XmNbackground (white-pixel)))
			  (XtSetValues (XtNameToWidget new-dialog "Cancel")
					(list XmNarmColor (pushed-button-color)))
			  (XtSetValues (XtNameToWidget new-dialog "Help")
					(list XmNarmColor (pushed-button-color)))
			  (XtSetValues (XtNameToWidget new-dialog "OK")
					(list XmNarmColor (pushed-button-color)))
			  new-dialog))))
	(if (not help)
	    (XtUnmanageChild (XmFileSelectionBoxGetChild dialog XmDIALOG_HELP_BUTTON))
	    (XtManageChild (XmFileSelectionBoxGetChild dialog XmDIALOG_HELP_BUTTON)))
	(let ((dirstr (XmStringCreateLocalized dir))
	      (patstr (XmStringCreateLocalized filter))
	      (titlestr (XmStringCreateLocalized title)))
	  (XtSetValues dialog
		       (list XmNdirectory dirstr
			     XmNpattern patstr
			     XmNdialogTitle titlestr))
	  (XmStringFree dirstr)
	  (XmStringFree patstr)
	  (XmStringFree titlestr)
	  (XtManageChild dialog))))))

; (select-file (lambda (n) (snd-print n)))



;;; -------- snd-clock-icon --------
;;;
;;; a clock icon to replace Snd's hourglass
;;;   call from a work proc or whatever with hour going from 0 to 12 then #f

(define snd-clock-icon
  (let* ((shell (list-ref (main-widgets) 1))
	 (dpy (XtDisplay shell))
	 (win (XtWindow shell))
	 (clock-pixmaps (make-vector 12))
	 (dgc (car (snd-gcs))))
    (do ((i 0 (1+ i)))
	((= i 12))
      ;; it's actually possible to simply redraw on one pixmap, but updates are unpredictable
      (let* ((pix (XCreatePixmap dpy win 16 16 (screen-depth)))
	     (pixwin (list 'Window (cadr pix)))) ; C-style cast to Window for X graphics procedures
	(vector-set! clock-pixmaps i pix)
	(XSetForeground dpy dgc (basic-color))
	(XFillRectangle dpy pixwin dgc 0 0 16 16)
	(XSetForeground dpy dgc (white-pixel))
	(XFillArc dpy pixwin dgc 1 1 14 14 0 (* 64 360))
	(XSetForeground dpy dgc (black-pixel))
	(XDrawArc dpy pixwin dgc 1 1 14 14 0 (* 64 360))
	(XDrawLine dpy pixwin dgc 8 8
		   (+ 8 (inexact->exact (round (* 7 (sin (* i (/ 3.1416 6.0)))))))
		   (- 8 (inexact->exact (round (* 7 (cos (* i (/ 3.1416 6.0))))))))))
    (XSetBackground dpy dgc (graph-color))
    (XSetForeground dpy dgc (data-color))
    (lambda (snd hour)
      (if hour
	  (XtSetValues (list-ref (sound-widgets snd) 8)
		       (list XmNlabelPixmap (vector-ref clock-pixmaps hour)))
	  (bomb snd #f))))) ; using bomb to clear the icon



;;; -------- make-sound-box --------
;;;
;;; make-sound-box makes a container of sound file icons, each icon
;;;   containing a little sketch of the waveform, the length of the
;;;   file, and the filename.  What happens when an icon is selected
;;;   is up to caller-supplied procedure.  However, if you drag (via 
;;;   button 2) the icon to the menubar, that sound will be opened,
;;;   and if you drag it to a channel graph, it will be mixed at the
;;;   drag point in that channel.


(define (thumbnail-graph dpy wn gc pts width height)
  "(thumbnail-graph dpy wn gc pts width height) makes a little graph of the data"
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
		 (round (+ ay1
		    (* height (- 1.0 y))))))))
    (let* ((ly (y->grfy (vct-ref pts 0) range))
	   (lx left-margin)
	   (len (vct-length pts))
	   (xinc (/ (- width left-margin right-margin) len))
	   (y 0))
      (do ((i 1 (1+ i))
	   (x lx (+ x xinc)))
	  ((= i len))
	(set! y (y->grfy (vct-ref pts i) range))
	(XDrawLine dpy wn gc lx ly (inexact->exact (round x)) y)
	(set! lx (inexact->exact (round x)))
	(set! ly y)))))

(define make-sound-box 
  ;; graphics stuff (fonts etc)
  (let*  ((gv (XGCValues))
	  (shell (list-ref (main-widgets) 1))
	  (button-fontstruct (XLoadQueryFont (XtDisplay shell) "6x12"))
	  (button-fontlist
	   (let* ((e1 (XmFontListEntryCreate "smallest" XmFONT_IS_FONT button-fontstruct))
		  (f1 (XmFontListAppendEntry #f e1)))
	     (XmFontListEntryFree e1)
	     f1)))
    (set! (.foreground gv) (data-color))
    (set! (.background gv) (basic-color))
    (set! (.font gv) (.fid button-fontstruct))
    (let ((gc (XCreateGC (XtDisplay shell) 
			 (XtWindow shell) 
			 (logior GCForeground GCBackground GCFont) gv))
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
		     (size (XTextWidth button-fontstruct secs (string-length secs))))
		(if (<= size width) 
		    (XDrawString dpy wn gc (- width size) height secs (string-length secs))))
	      (thumbnail-graph dpy wn gc mins width height)
	      (thumbnail-graph dpy wn gc maxes width height))))

      (define (make-sound-icon filename parent peak-func gc width height args)
	(define (cast-to-window n) (list 'Window (cadr n)))
	(let* ((dpy (XtDisplay parent))
	       (win (XtWindow parent))
	       (pix (XCreatePixmap dpy win width height (screen-depth)))
	       (str (XmStringCreateLocalized filename))
	       (data (list gc filename #f (channel-amp-envs filename 0 width peak-func))))
	  (XSetForeground dpy gc (basic-color))
	  (XFillRectangle dpy (cast-to-window pix) gc 0 0 width height)
	  (XSetForeground dpy gc (data-color))
	  (make-sound-button-pixmap dpy (cast-to-window pix) data width height)
	  (let ((icon (XtCreateManagedWidget filename xmIconGadgetClass parent
			(append (list XmNbackground      (basic-color)
				      XmNforeground      (data-color)
				      XmNlabelString     str
				      XmNfontList        button-fontlist
				      XmNlargeIconPixmap pix
				      XmNsmallIconPixmap pix)
				args))))
	    (set! (sound-button data) icon)
	    (set! sound-buttons (cons data sound-buttons))
	    icon)))

      ;; now the actual sound-box maker
      (lambda (name parent select-func peak-func sounds args)
	;; select-func called when sound selected and passed sound file name
	;; peak-func (if any) tells icon where to find peak-env-info-file (if any)
	;; sounds is list of sound file names
	;; args is list of resource settings for each icon
	"(make-sound-box name parent select-func peak-func sounds args) makes a box of sound icons"
	(let ((container (XtCreateManagedWidget name xmContainerWidgetClass parent
			   (list XmNlayoutType         XmSPATIAL
				 XmNspatialResizeModel XmGROW_BALANCED
				 XmNbackground         (white-pixel)
				 XmNentryViewType      XmANY_ICON
				 XmNlargeCellWidth     80))))
	  (XtVaSetValues parent (list XmNworkWindow container))
	  (XtAddCallback container XmNselectionCallback 
	    (lambda (w c i)
	      (if (and (= (.auto_selection_type i) XmAUTO_BEGIN) ; just click to select for now
		       (list? (.selected_items i))
		       (not (null? (.selected_items i))))
		  (select-func (XtName (car (.selected_items i)))))))
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

(define* (show-sounds-in-directory :optional (dir "."))
  "(show-sounds-in-directory :optional (dir \".\")) calls make-sound-box with the given directory"

  (make-sound-box
   "sounds"
   (XtCreateManagedWidget "scrolled-window" xmScrolledWindowWidgetClass (list-ref (main-widgets) 3)
			  (list XmNscrollBarDisplayPolicy XmAS_NEEDED
				XmNbackground             (basic-color)
				XmNvisualPolicy           XmVARIABLE
				XmNscrollingPolicy        XmAUTOMATIC))
   (lambda (file) 
     (open-sound file))
   (lambda (file chn)
     (format #f "~~/peaks/~A-peaks-~D"                              
	     (clean-string (mus-expand-filename file))
	     chn))
   (sound-files-in-directory dir)
   '()))


    

;;; -------- show-smpte-label
;;;
;;; (show-smpte-label :optional on-or-off)
;;;   turns on/off a label in the time-domain graph showing the current smpte frame of the leftmost sample

(define smpte-frames-per-second 24.0)

(define draw-smpte-label
  (let* ((dpy (XtDisplay (cadr (main-widgets))))
	 (fs (XLoadQueryFont dpy (axis-numbers-font)))
	 (width (+ 8 (XTextWidth fs "00:00:00:00" 11)))
	 (height (+ 8 (caddr (XTextExtents fs "0" 1)))))

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
      "(draw-smpte-label snd chn) draws a SMPTE time stamp in a box on a graph"
      (let* ((axinf (axis-info snd chn))
	     (x (list-ref axinf 10))
	     (y (list-ref axinf 13))
	     (grf-width (- (list-ref axinf 12) x))
	     (grf-height (- (list-ref axinf 11) y)))
	(if (and (> grf-height (* 2 height))
		 (> grf-width (* 1.5 width))
		 (time-graph? snd chn))
	    (let* ((smpte (smpte-label (car axinf) (srate snd)))
		   (samp (car axinf)))
	      (fill-rectangle x y width 2 snd chn)
	      (fill-rectangle x (+ y height) width 2 snd chn)
	      (fill-rectangle x y 2 height snd chn)
	      (fill-rectangle (+ x width -2) y 2 height snd chn)
	      (XSetFont dpy
			(if (= chn (selected-channel snd))
			    (cadr (snd-gcs))
			    (car (snd-gcs)))
			(.fid fs))
	      (draw-string smpte (+ x 4) (+ y 4) snd chn)))))))

(define (show-smpte-label . arg)
  "(show-smpte-label :optional on-or-off) turns on/off a label in the time-domain graph showing the current smpte frame of the leftmost sample"
  (if (or (null? arg)
	  (car arg))
      (if (not (member draw-smpte-label (hook->list after-graph-hook)))
	  (begin
	    (add-hook! after-graph-hook draw-smpte-label)
	    (update-time-graph #t #t)))
      (begin
	(remove-hook! after-graph-hook draw-smpte-label)
	(update-time-graph #t #t))))

(define (smpte-is-on) ; for prefs dialog
  "(smpte-is-on) is #t if we are drawing SMPTE time stamps"
  (member draw-smpte-label (hook->list after-graph-hook)))


;;; -------- with-level-meters, make-level-meter, display-level

(define red-pixel
  (let ((pix #f))
    (lambda ()
      "(red-pixel) returns a red pixel"
      (if (not pix)
	  (let* ((shell (cadr (main-widgets)))
		 (dpy (XtDisplay shell))
		 (scr (DefaultScreen dpy))
		 (cmap (DefaultColormap dpy scr))
		 (col (XColor)))
	       (if (= (XAllocNamedColor dpy cmap "red" col col) 0)
		   (snd-error "can't allocate red!")
		   (set! pix (.pixel col)))))
      pix)))

(define* (make-level-meter parent width height args :optional (resizable #t))
  "(make-level-meter parent width height args :optional (resizable #t)) makes a VU level meter"
  (let* ((frame (XtCreateManagedWidget "meter-frame" xmFrameWidgetClass parent
		  (append (list XmNshadowType       XmSHADOW_ETCHED_IN
				XmNwidth            width
				XmNheight           height
				XmNshadowThickness  (if (> width 500) 6 3))
			  args)))
	 (meter (XtCreateManagedWidget "meter" xmDrawingAreaWidgetClass frame
                  (if resizable				       
		      (list XmNbackground       (white-pixel)
			    XmNforeground       (black-pixel)
			    XmNtopAttachment    XmATTACH_FORM
			    XmNbottomAttachment XmATTACH_FORM
			    XmNleftAttachment   XmATTACH_FORM
			    XmNrightAttachment  XmATTACH_FORM)
		      (list XmNbackground       (white-pixel)
			    XmNforeground       (black-pixel)
			    XmNwidth            width
			    XmNheight           height
			    XmNresizePolicy     XmRESIZE_NONE
			    XmNtopAttachment    XmATTACH_FORM
			    XmNbottomAttachment XmATTACH_FORM
			    XmNleftAttachment   XmATTACH_FORM
			    XmNrightAttachment  XmATTACH_FORM))))
	 (context (list meter 0.0 1.0 0.0 0.0 width height)))
    (XtAddCallback meter XmNexposeCallback 
		    (lambda (w c i) 
		      (display-level c)) 
		    context)
    (if resizable
	(XtAddCallback meter XmNresizeCallback 
		       (lambda (w c i) 
			 (list-set! c 5 (cadr (XtGetValues w (list XmNwidth 0))))
			 (list-set! c 6 (cadr (XtGetValues w (list XmNheight 0))))
			 (display-level c))
		       context))
    context))

(define (display-level meter-data)
  "(display-level meter-data) displays a VU level meter"
  (let* ((meter (car meter-data))
	 (level (list-ref meter-data 1))
	 (last-level (list-ref meter-data 3))
	 (red-deg (list-ref meter-data 4))
	 (width (list-ref meter-data 5))
	 (height (list-ref meter-data 6))
	 ;; (size (list-ref meter-data 2))
	 (dpy (XtDisplay meter))
	 (win (XtWindow meter))
	 (major-tick (inexact->exact (round (/ width 24))))
	 (minor-tick (inexact->exact (round (* major-tick .6))))
	 (ang0 (* 45 64))
	 (ang1 (* 90 64))
	 (wid2 (inexact->exact (floor (/ width 2))))
	 (gc (car (snd-gcs)))
	 (top (inexact->exact (round (/ height 3.2))))) ; distance of label from top of meter
    (if (and (> top 10)
	     (> width 10)
	     (> height 10))
	(begin
	  (XSetForeground dpy gc (white-pixel))
	  (XFillRectangle dpy win gc 0 0 width height)
	  (XSetForeground dpy gc (black-pixel))
	  (XDrawArc dpy win gc 0 top width width ang0 ang1)
	  (XDrawArc dpy win gc 0 (- top 1) width width ang0 ang1)
	  (if (> width 100)
	      (XDrawArc dpy win gc 0 (- top 2) width width ang0 ang1))
	  (XDrawArc dpy win gc 4 (+ top 4) (- width 8) (- width 8) ang0 ang1)
	  (do ((i 0 (1+ i)))
	      ((= i 5))
	    (let* ((rdeg (degrees->radians (- 45 (* i 22.5))))
		   (sinr (sin rdeg))
		   (cosr (cos rdeg))
		   (x0 (inexact->exact (round (+ wid2 (* wid2 sinr)))))
		   (y0 (inexact->exact (round (- (+ wid2 top) (* wid2 cosr)))))
		   (x1 (inexact->exact (round (+ wid2 (* (+ wid2 major-tick) sinr)))))
		   (y1 (inexact->exact (round (- (+ wid2 top) (* (+ wid2 major-tick) cosr))))))
	      (XDrawLine dpy win gc x0 y0 x1 y1)
	      (XDrawLine dpy win gc (+ x0 1) y0 (+ x1 1) y1)
	      (if (< i 4)
		  (do ((j 1 (1+ j)))
		      ((= j 6))
		    (let* ((rdeg (degrees->radians (- 45 (* i 22.5) (* j (/ 90.0 20.0)))))
			   (sinr (sin rdeg))
			   (cosr (cos rdeg))
			   (x0 (inexact->exact (round (* wid2 (+ 1.0 sinr)))))
			   (y0 (inexact->exact (round (- (+ wid2 top) (* wid2 cosr)))))
			   (x1 (inexact->exact (round (+ wid2 (* (+ wid2 minor-tick) sinr)))))
			   (y1 (inexact->exact (round (- (+ wid2 top) (* (+ wid2 minor-tick) cosr))))))
		      (XDrawLine dpy win gc x0 y0 x1 y1))))))
	  (let* ((needle-speed 0.25)
		 (bubble-speed 0.025)
		 (bubble-size (* 15 64))
		 (val (+ (* level needle-speed) (* last-level (- 1.0 needle-speed))))
		 (deg (- (* val 90.0) 45.0))
		 (rdeg (degrees->radians deg))
		 (nx1 (inexact->exact (round (+ wid2 (* (+ wid2 major-tick) (sin rdeg))))))
		 (ny1 (inexact->exact (round (- (+ wid2 top) (* (+ wid2 major-tick) (cos rdeg)))))))
	    (XDrawLine dpy win gc wid2 (+ top wid2) nx1 ny1)
	    (list-set! meter-data 3 val)
	    (if (> val red-deg)
		(list-set! meter-data 4 val)
		(list-set! meter-data 4 (+ (* val bubble-speed) (* red-deg (- 1.0 bubble-speed)))))
	    (if (> (list-ref meter-data 4) .01)
		(begin
		  (XSetForeground dpy gc (red-pixel))
		  (let* ((redx (inexact->exact (floor (* (list-ref meter-data 4) 90 64))))
			 (redy (min redx bubble-size)))
		    (do ((i 0 (1+ i)))
			((= i 4))
		      (XDrawArc dpy win gc i (+ top i) (- width (* i 2)) (- width (* i 2)) (- (* 135 64) redx) redy))
		    (XSetForeground dpy gc (black-pixel))))))))))

(define (with-level-meters n)
  "(with-level-meters n) adds 'n' level meters to a pane at the top of the Snd window"
  (let* ((parent (list-ref (main-widgets) 3))
	 (height 70)
	 (width (inexact->exact (floor (/ (cadr (XtGetValues parent (list XmNwidth 0))) n))))
	 (meters (XtCreateManagedWidget "meters" xmFormWidgetClass parent
	 	   (list XmNpositionIndex 0  ; top pane
			 XmNbackground    (basic-color)
			 XmNfractionBase  (* n 10)
			 XmNpaneMinimum   height)))
	 (meter-list '()))
    (do ((i 0 (1+ i)))
	((= i n))
      (set! meter-list 
	 (cons (make-level-meter meters width height
				 (list XmNtopAttachment    XmATTACH_FORM
				       XmNbottomAttachment XmATTACH_FORM
				       XmNleftAttachment   XmATTACH_POSITION
				       XmNleftPosition     (* i 10)
				       XmNrightAttachment  XmATTACH_POSITION
				       XmNrightPosition    (* (+ 1 i) 10))) 
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
		 (XtAppAddWorkProc (car (main-widgets))
				   (let ((ctr 0))
				     (lambda (ignored)
				       (for-each 
					(lambda (meter)
					  (list-set! meter 1 0.0)
					  (display-level meter))
					meter-list)
				       (set! ctr (+ ctr 1))
				       (> ctr 200))))))
    (XtSetValues meters (list XmNpaneMinimum 1))
    meter-list))



;;; -------- add a drop site
;;;
;;; this adds a pane to the current channel which can respond to drag-and-drop operations
;;;   (this is a Motif 1.2 style drop -- I've had trouble getting the new style to work at all)

(define make-channel-drop-site
  (lambda args
    "(make-channel-drop-site :optional snd) adds a drop site pane to the current channel"
    (let* ((snd (if (> (length args) 0) (car args) (selected-sound)))
	   (chn (selected-channel snd))
	   (widget (add-channel-pane snd chn "drop here" xmDrawingAreaWidgetClass
		     (list XmNbackground (white-pixel)
                           XmNleftAttachment      XmATTACH_FORM
		           XmNrightAttachment     XmATTACH_FORM
		           XmNtopAttachment       XmATTACH_FORM
		           XmNbottomAttachment    XmATTACH_FORM))))
      (XmDropSiteRegister
	widget 
	(list XmNdropSiteOperations XmDROP_COPY
	      XmNimportTargets      (list XA_STRING) ; list of Atoms we can deal with -- in this case, just strings
	      XmNnumImportTargets   1
	      XmNdropProc 
	       (lambda (w c i)
		 ;; i is the callback data (XmDropProcCallbackStruct), c is always #f
		 (if (or (not (= (.dropAction i) XmDROP))
			 (not (= (.operation i) XmDROP_COPY)))
		     (set! (.dropSiteStatus i) XmINVALID_DROP_SITE)
		     (begin
		       (set! (.operation i) XmDROP_COPY) ; tell system drop has succeeded
		       (XmDropTransferStart 
			 (.dragContext i)
			 (list XmNdropTransfers (list XA_STRING)
			       XmNnumDropTransfers 1
			       XmNtransferProc 
			       (lambda (w context selection type val len fmt)
				 ;; the actual in-coming string (properly terminated in xm.c) is 'value'
				 (snd-print (format #f "got: ~A ~A ~A ~A ~A ~A ~A~%"
						    w context selection type val len fmt)))))))))))))


;;; -------- change a drop callback
;;;
;;; drop arg is 3-arg func: filename snd chn

(define (set-channel-drop drop snd chn)
  "(set-channel-drop drop snd chn) changes a drop callback function; 'drop' is function of 3 args (filename snd chn)"
  (XmDropSiteUpdate
   (car (channel-widgets snd chn))
   (list XmNdropProc
	 (lambda (w c i)
	   (if (or (not (= (.dropAction i) XmDROP))
		   (not (= (.operation i) XmDROP_COPY)))
	       (set! (.dropSiteStatus i) XmINVALID_DROP_SITE)
	       (begin
		 (set! (.operation i) XmDROP_COPY)
		 (XmDropTransferStart 
		  (.dragContext i)
		  (list XmNdropTransfers (list (XInternAtom (XtDisplay (cadr (main-widgets))) "FILE_NAME" #f))

			;; this is saying that the in-coming drag-and-drop is expected to pass us a FILE_NAME atom
			;; to find out what Atoms the selection translator can handle, use
			;;   (XtVaGetValues (.dragContext i) (list XmNexportTargets 0))
			;; which will return a list of acceptable Atoms

			XmNnumDropTransfers 1
			XmNtransferProc 
			(lambda (w context selection type val len fmt)
			  (drop val snd chn))))))))))


;;; -------- show-disk-space
;;;
;;; adds a label to the minibuffer area showing the current free space 

(define showing-disk-space #f) ; for prefs dialog

(define show-disk-space
  (let ((labelled-snds '()))
    (define (kmg num)
      (if (<= num 0)
	  "disk full!"
	  (if (> num 1024)
	      (if (> num (* 1024 1024))
		  (format #f "space: ~6,3FG" (/ num (* 1024.0 1024.0)))
		  (format #f "space: ~6,3FM" (/ num 1024.0)))
	      (format #f "space: ~10DK" num))))
    (define (show-label data id)
      (if (sound? (car data))
	  (let* ((space (kmg (disk-kspace (file-name (car data)))))
		 (str (XmStringCreateLocalized space)))
	    (XtSetValues (cadr data) (list XmNlabelString str))
	    (XmStringFree str)
	    (XtAppAddTimeOut (caddr data) 10000 show-label data))))
    (lambda* (:optional snd-arg)
      "(show-disk-space) adds a label to the minibuffer area showing the current free space (for use with after-open-hook)"
      (let* ((snd (or snd-arg (selected-sound)))
	     (previous-label (find-if (lambda (n) (= (car n) snd)) labelled-snds)))
	(if (not previous-label)
	    (if (not snd)
		(snd-error "no sound found for disk space label")
		(let* ((app (car (main-widgets)))
		       (widgets (sound-widgets snd))
		       (minibuffer (list-ref widgets 3))
		       (unite-button (list-ref widgets 6))
		       (sync-button (list-ref widgets 9))
		       (name-form (XtParent minibuffer)) ; "snd-name-form"
		       (space (kmg (disk-kspace (file-name snd))))
		       (str (XmStringCreateLocalized space)))
		  (set! showing-disk-space #t)
		  (XtUnmanageChild minibuffer)
		  (XtVaSetValues minibuffer (list XmNrightAttachment XmATTACH_NONE))
		  (let ((new-label (XtCreateManagedWidget "space:" xmLabelWidgetClass name-form 
							  (list XmNbackground      (basic-color)
								XmNleftAttachment  XmATTACH_NONE
								XmNlabelString     str
								XmNrightAttachment XmATTACH_WIDGET
								XmNrightWidget     (if (XtIsManaged unite-button)
										       unite-button
										       sync-button)
								XmNtopAttachment   XmATTACH_FORM))))
		    (XtVaSetValues minibuffer (list XmNrightWidget new-label XmNrightAttachment XmATTACH_WIDGET))
		    (XtManageChild minibuffer)
		    (XmStringFree str)
		    (set! previous-label (list snd new-label app))
		    (set! labelled-snds (cons previous-label labelled-snds))
		    (XtAppAddTimeOut (caddr previous-label) 10000 show-label previous-label)))))))))



;;; -------- add amp sliders in control panel for multi-channel sounds
;;;
;;; use control-button to move all at once

(define+ (add-amp-controls)
  "(add-amp-controls) adds amplitude sliders to the control panel for each channel in multi-channel sounds"

  (define (label-name chan) (if (= chan 0) "amp-label" (format #f "amp-label-~D" chan)))
  (define (number-name chan) (if (= chan 0) "amp-number" (format #f "amp-number-~D" chan)))
  (define (scroller-name chan) (if (= chan 0) "amp" (format #f "amp-~D" chan)))

  (define (amp->scroll minval val maxval)
    (if (<= val minval) 0
	(if (>= val maxval) 900
	    (if (>= val 1.0)
		(inexact->exact (* 450 (+ 1.0 (/ (- val 1.0) (- maxval 1.0)))))
		(inexact->exact (* 450 (/ (- val minval) (- 1.0 minval))))))))
  
  (define (scroll->amp snd val)
    (if (<= val 0)
	(car (amp-control-bounds snd))
	(if (>= val 900)
	    (cadr (amp-control-bounds snd))
	    (if (> val 450)
		(+ (* (- (/ val 450.0) 1.0) (- (cadr (amp-control-bounds snd)) 1.0)) 1.0)
		(+ (* val (/ (- 1.0 (car (amp-control-bounds snd))) 450.0)) (car (amp-control-bounds snd)))))))

  (define (amp-callback w c info)
    ;; c is (list number-widget snd chan)
    (let* ((snd (cadr c))
	   (amp (scroll->amp snd (.value info)))
	   (ampstr (XmStringCreateLocalized (format #f "~,2F" amp)))
	   (top-chn (- (chans snd) 1))
	   (chn (- top-chn (caddr c)))
	   (ctrl (and (.event info) (not (= (logand (.state (.event info)) ControlMask) 0)))))
      (XtSetValues (car c) (list XmNlabelString ampstr))
      (XmStringFree ampstr)
      (if ctrl
	  (let* ((wids (sound-widgets snd))
		 (ctrls (list-ref wids 2))
		 (snd-amp (find-child ctrls "snd-amp"))
		 (chns (chans snd)))
	    (do ((i 0 (1+ i)))
		((= i chns))
	      (let* ((ampscr (find-child snd-amp (scroller-name i)))
		     (ampvals (XmScrollBarGetValues ampscr)))
		(XmScrollBarSetValues ampscr (.value info) (cadr ampvals) (caddr ampvals) (cadddr ampvals) #t)
		(set! (amp-control snd i) amp))))
	  (set! (amp-control snd chn) amp))))

  (define (reset-to-one scroller number)
    (XtSetValues scroller (list XmNvalue 450))
    (let ((ampstr (XmStringCreateLocalized "1.00")))
      (XtSetValues number (list XmNlabelString ampstr))
      (XmStringFree ampstr)))
  
  (define (make-amp-control snd chan parent)
    (let* ((s1 (XmStringCreateLocalized "amp:"))
	   (label (XtCreateManagedWidget (label-name chan) xmPushButtonWidgetClass parent
					  (list XmNbackground       (basic-color)
						XmNalignment        XmALIGNMENT_BEGINNING
						XmNtopAttachment    XmATTACH_FORM
						XmNbottomAttachment XmATTACH_NONE
						XmNleftAttachment   XmATTACH_FORM
						XmNrightAttachment  XmATTACH_NONE
					        XmNlabelString      s1
						XmNmarginHeight     1
						XmNrecomputeSize    #f
						XmNshadowThickness  0
						XmNhighlightThickness 0
						XmNfillOnArm        #f)))
	   (s2 (XmStringCreateLocalized "1.00"))
	   (number (XtCreateManagedWidget (number-name chan) xmLabelWidgetClass parent
					   (list XmNbackground       (basic-color)
						 XmNalignment        XmALIGNMENT_BEGINNING
						 XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
						 XmNtopWidget        label
						 XmNbottomAttachment XmATTACH_NONE
						 XmNleftAttachment   XmATTACH_WIDGET
						 XmNleftWidget       label
						 XmNrightAttachment  XmATTACH_NONE
						 XmNlabelString      s2
						 XmNmarginHeight     1
						 XmNrecomputeSize    #f)))
	   (scroll (XtCreateManagedWidget (scroller-name chan) xmScrollBarWidgetClass parent
					   (list XmNbackground       (position-color)
						 XmNtopAttachment    XmATTACH_OPPOSITE_WIDGET
						 XmNtopWidget        label
						 XmNbottomAttachment XmATTACH_NONE
						 XmNheight           16
						 XmNleftAttachment   XmATTACH_WIDGET
						 XmNleftWidget       number
						 XmNrightAttachment  XmATTACH_FORM
						 XmNorientation      XmHORIZONTAL
						 XmNmaximum          1000
						 XmNvalue            450
						 XmNdragCallback     (list amp-callback (list number snd chan))
						 XmNvalueChangedCallback (list amp-callback (list number snd chan))))))
      (XtOverrideTranslations scroll
			      (XtParseTranslationTable "c<Btn1Down>: Select()
                                                        c<Btn1Motion>: Moved()
						        c<Btn1Up>:   Release()"))

      (XtAddCallback label XmNactivateCallback (lambda (w c i)
						 (reset-to-one scroll number)))
      (XmStringFree s1)
      (XmStringFree s2)
      label))
  
  (define (amp-controls-reflect-chans snd)
    (let* ((wids (sound-widgets snd))
	   (ctrls (list-ref wids 2))
	   (snd-amp (find-child ctrls "snd-amp"))
	   (chns (chans snd)))
      
      (if (Widget? snd-amp)
	  (let ((height (cadr (XtGetValues ctrls (list XmNheight 0))))
		(panemin (cadr (XtGetValues ctrls (list XmNpaneMinimum 0))))
		(panemax (cadr (XtGetValues ctrls (list XmNpaneMaximum 0)))))
	    (XtUnmanageChild ctrls)

	    (if (not (sound-property 'amp-controls snd))
		(let ((orig-amp (find-child snd-amp "amp")))
		  (XtOverrideTranslations orig-amp
					  (XtParseTranslationTable "c<Btn1Down>: Select()
                                                                    c<Btn1Motion>: Moved()
						                    c<Btn1Up>:   Release()"))
		  (XtAddCallback orig-amp XmNdragCallback
				 (lambda (w c info)
				   (if (and (.event info) (not (= (logand (.state (.event info)) ControlMask) 0)))
				       (do ((i 1 (1+ i)))
					   ((= i chns))
					 (let* ((ampscr (find-child snd-amp (scroller-name i)))
						(ampvals (XmScrollBarGetValues ampscr)))
					   (XmScrollBarSetValues ampscr (.value info) (cadr ampvals) (caddr ampvals) (cadddr ampvals) #t))))))))
	    (let ((existing-controls (or (sound-property 'amp-controls snd) 1)))
	      (if (< existing-controls chns)
		  (begin
		    (if (> height 20)
			(set! height (+ height (* 18 (- chns existing-controls)))))
		    (do ((i existing-controls (1+ i)))
			((= i chns))
		      (make-amp-control snd i snd-amp))
		    (set! (sound-property 'amp-controls snd) chns)
		    (set! existing-controls chns)))
	      (do ((i 0 (1+ i)))
		  ((= i existing-controls))
		(let ((ampc (find-child snd-amp (label-name i)))
		      (ampn (find-child snd-amp (number-name i)))
		      (amp (find-child snd-amp (scroller-name i))))
		  (XtUnmanageChild ampc)
		  (XtUnmanageChild ampn)
		  (XtUnmanageChild amp)))
	      (do ((i 0 (1+ i)))
		  ((= i chns))
		(let ((ampc (find-child snd-amp (label-name i)))
		      (ampn (find-child snd-amp (number-name i)))
		      (amp (find-child snd-amp (scroller-name i)))
		      (next-amp (if (< i (1- chns))
				    (find-child snd-amp (label-name (1+ i)))
				    #f)))
		  (reset-to-one amp ampn)
		  (if next-amp
		      (XtSetValues ampc (list XmNtopAttachment XmATTACH_WIDGET
					      XmNtopWidget     next-amp))
		      (XtSetValues ampc (list XmNtopAttachment XmATTACH_FORM)))
		  (XtManageChild ampc)
		  (XtManageChild ampn)
		  (XtManageChild amp))))
	    
	    (XtSetValues ctrls (list XmNpaneMinimum height XmNpaneMaximum height))
	    (XtManageChild ctrls)
	    (XtSetValues ctrls (list XmNpaneMinimum panemin XmNpaneMaximum panemax))))))
  
  (define (amp-controls-clear snd)
    (if (> (chans snd) 1)
	(let* ((wids (sound-widgets snd))
	       (ctrls (list-ref wids 2))
	       (snd-amp (find-child ctrls "snd-amp"))
	       (top (1- (chans snd))))
	  (do ((i 1 (1+ i)))
	      ((= i (chans snd)))
	    (let ((ampn (find-child snd-amp (number-name i)))
		  (amp (find-child snd-amp (scroller-name i))))
	      (reset-to-one amp ampn)
	      (set! (amp-control snd (- top i)) 1.0))))))
  
  (add-hook! after-open-hook amp-controls-reflect-chans)
  (add-hook! after-apply-controls-hook amp-controls-clear))

;(add-amp-controls)


;;; -------- remove top level menu
;;;
;;; (remove-main-menu 5) removes the Help menu

(define (remove-main-menu menu)
  "(remove-main-menu menu) removes the specified top-level menu: (remove-main-menu 5) removes the Help menu"
  (let* ((cascade (list-ref (menu-widgets) menu))
	 (top (cadr (XtGetValues cascade (list XmNsubMenuId 0)))))
    (XtUnmanageChild cascade)
    (XtUnmanageChild top)))


;;; -------- add delete and rename options to the file menu

(define (add-delete-option)
  "(add-delete-option) adds a delete (file) option to the File menu"
  (add-to-menu 0 "Delete" ; add Delete option to File menu
	       (lambda ()
		 ;; close current sound and delete it
		 (if (selected-sound)
		     (let ((filename (file-name)))
		       (close-sound)
		       (delete-file filename))))
	       8)) ; place after File:New

(define (add-rename-option)
  "(add-rename-option) adds a rename (file) option to the File menu"
  (let ((rename-dialog #f)
	(rename-text #f))
    (add-to-menu 0 "Rename" 
      (lambda ()
	;; open dialog to get new name, save-as that name, open
	(if (not rename-dialog)
	    ;; make a standard dialog
	    (let* ((xdismiss (XmStringCreate "Dismiss" XmFONTLIST_DEFAULT_TAG))
		   (xhelp (XmStringCreate "Help" XmFONTLIST_DEFAULT_TAG))
		   (xok (XmStringCreate "DoIt" XmFONTLIST_DEFAULT_TAG))
		   (titlestr (XmStringCreate "Rename" XmFONTLIST_DEFAULT_TAG))
		   (new-dialog (XmCreateTemplateDialog
				 (cadr (main-widgets)) "Rename"
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
    
	      (XtAddCallback new-dialog XmNcancelCallback 
			     (lambda (w c i) (XtUnmanageChild w)))
	      
	      (XtAddCallback new-dialog XmNhelpCallback 
			     (lambda (w c i)
			       (help-dialog "Rename" "give a new file name to rename the currently selected sound")))
	      
	      (XtAddCallback new-dialog XmNokCallback 
			     (lambda (w c i)
			       (let ((new-name (XmTextFieldGetString rename-text)))
				 (if (and (string? new-name)
					  (> (string-length new-name) 0)
					  (selected-sound))
				     (let ((current-name (file-name)))
				       (save-sound-as new-name)
				       (close-sound)
				       (rename-file current-name new-name)
				       (open-sound new-name)
				       (XtUnmanageChild w))))))

	      (XmStringFree xhelp)
	      (XmStringFree xok)
	      (XmStringFree xdismiss)
	      (XmStringFree titlestr)
	      (set! rename-dialog new-dialog)

	      (let* ((mainform (XtCreateManagedWidget "formd" xmRowColumnWidgetClass rename-dialog
				     (list XmNleftAttachment   XmATTACH_FORM
					   XmNrightAttachment  XmATTACH_FORM
					   XmNtopAttachment    XmATTACH_FORM
					   XmNbottomAttachment XmATTACH_WIDGET
					   XmNbottomWidget     (XmMessageBoxGetChild rename-dialog XmDIALOG_SEPARATOR)
					   XmNorientation      XmVERTICAL
					   XmNbackground       (basic-color))))
		     (label (XtCreateManagedWidget "new name:" xmLabelWidgetClass mainform
				     (list XmNleftAttachment   XmATTACH_FORM
					   XmNrightAttachment  XmATTACH_NONE
					   XmNtopAttachment    XmATTACH_FORM
					   XmNbottomAttachment XmATTACH_FORM
					   XmNbackground       (basic-color)))))
		(set! rename-text 
		      (XtCreateManagedWidget "newname" xmTextFieldWidgetClass mainform
				     (list XmNleftAttachment   XmATTACH_WIDGET
					   XmNleftWidget       label
					   XmNrightAttachment  XmATTACH_FORM
					   XmNtopAttachment    XmATTACH_FORM
					   XmNbottomAttachment XmATTACH_FORM
					   XmNbackground       (basic-color))))
		(XtAddEventHandler rename-text EnterWindowMask #f
				   (lambda (w context ev flag)
				     (XmProcessTraversal w XmTRAVERSE_CURRENT)
				     (XtSetValues w (list XmNbackground (white-pixel)))))
		(XtAddEventHandler rename-text LeaveWindowMask #f
				   (lambda (w context ev flag)
				     (XtSetValues w (list XmNbackground (basic-color))))))))
	(if (not (XtIsManaged rename-dialog))
	    (XtManageChild rename-dialog)
	    (raise-dialog rename-dialog)))
      8)))


(define (change-label widget new-label)
  "(change-label widget new-label) changes widget's label to new-label"
  (let ((str (XmStringCreateLocalized new-label)))
    (XtSetValues widget (list XmNlabelString str))
    (XmStringFree str)))

;;; this deletes the play button
;(add-hook! after-open-hook
;	   (lambda (snd)
;	     (let* ((ctrls (list-ref (sound-widgets snd) 2))
;		    (play-button (find-child ctrls "play"))
;		    (sync-button (find-child ctrls "sync")))
;	     (XtUnmanageChild play-button)
;	     (XtVaSetValues sync-button (list XmNrightAttachment XmATTACH_FORM)))))


;;; -------- mark-sync-color
;;;
;;; (mark-sync-color "blue")

(define+ (mark-sync-color new-color)
  "(mark-sync-color new-color) sets the color for sync'd marks"

  (define get-color
    (lambda (color-name)
      (let* ((col (XColor))
	     (dpy (XtDisplay (cadr (main-widgets))))
	     (scr (DefaultScreen dpy))
	     (cmap (DefaultColormap dpy scr)))
	(if (= (XAllocNamedColor dpy cmap color-name col col) 0)
	    (snd-error (format #f "can't allocate ~A" color-name))
	    (.pixel col)))))

  (let* ((mark-gc (list-ref (snd-gcs) 9))
	 (selected-mark-gc (list-ref (snd-gcs) 10))
	 (dpy (XtDisplay (cadr (main-widgets))))
	 (original-mark-color (list 'Pixel (logxor (cadr (mark-color)) 
						   (cadr (graph-color)))))
	 (original-selected-mark-color (list 'Pixel (logxor (cadr (mark-color)) 
							    (cadr (selected-graph-color)))))
	 (new-mark-color (list 'Pixel (logxor (cadr (graph-color)) 
					      (cadr (get-color new-color)))))
	 (new-selected-mark-color (list 'Pixel (logxor (cadr (selected-graph-color))
						       (cadr (get-color new-color))))))
    (if (not (hook-empty? draw-mark-hook)) 
	(reset-hook! draw-mark-hook))
    (add-hook! draw-mark-hook
	       (lambda (id)
		 (if (> (mark-sync id) 0)
		     (begin
		       (XSetForeground dpy mark-gc new-mark-color)
		       (XSetForeground dpy selected-mark-gc new-selected-mark-color))
		     (begin
		       (XSetForeground dpy mark-gc original-mark-color)
		       (XSetForeground dpy selected-mark-gc original-selected-mark-color)))
		 #f))))



;;; -------- add "tooltip" to a widget
;;;
;;; (add-tooltip (cadr (channel-widgets)) "the w button")

(define with-tooltips #t) ; set to #f to turn these off
(define tooltip-shell #f)
(define tooltip-label #f)

(define (add-tooltip widget tip)
  "(add-tooltip widget tip) adds the tooltip 'tip' to the widget"
  (let ((tool-proc #f)
	(quit-proc #f)
	(timeout 500)   ; millisecs after mouse enters widget to tip display 
	(quittime 3000) ; millisecs to show tip (if pointer not already moved out of widget)
	(last-time 0))  ; try to squelch "fluttering"

    (define (stop-tooltip)
      (if tool-proc
	  (begin
	    (XtRemoveTimeOut tool-proc)
	    (set! tool-proc #f)))
      (if quit-proc
	  (begin
	    (XtRemoveTimeOut quit-proc)
	    (set! quit-proc #f)))
      (if (and tooltip-shell (XtIsManaged tooltip-shell))
	  (XtUnmanageChild tooltip-shell)))

    (define (start-tooltip ev)
      (if (and with-tooltips 
	       (not tool-proc))
	  (set! tool-proc (XtAppAddTimeOut 
			    (car (main-widgets))
			    timeout 
			    (lambda (data id)
			      (if (not tooltip-shell)
				  (begin
				    (set! tooltip-shell (XtCreatePopupShell 
							 tip 
							 overrideShellWidgetClass 
							 (cadr (main-widgets)) 
							 (list XmNallowShellResize #t)))
				    (set! tooltip-label
					  (XtCreateManagedWidget 
					   tip
					   xmLabelWidgetClass 
					   tooltip-shell
					   (list XmNrecomputeSize #t
					         XmNbackground (highlight-color)))))
				  (change-label tooltip-label tip))
			      (let ((loc (XtTranslateCoords widget (.x ev) (.y ev))))
				(XtVaSetValues tooltip-shell (list XmNx (car loc) XmNy (cadr loc))))
			      (XtManageChild tooltip-shell)
			      (set! quit-proc (XtAppAddTimeOut
					       (car (main-widgets))
					       quittime
					       (lambda (data id)
						 (XtUnmanageChild tooltip-shell)
						 (set! quit-proc #f)))))))))
    
    (XtAddEventHandler widget EnterWindowMask #f 
      (lambda (w c ev flag)
	(if (> (- (cadr (.time ev)) last-time) 50)
	    (start-tooltip ev))
	(set! last-time (cadr (.time ev)))))
    (XtAddEventHandler widget LeaveWindowMask #f 
      (lambda (w c ev flag) 
	(set! last-time (cadr (.time ev)))
        (stop-tooltip)))))

(define (menu-option name)
  "(menu-option name) finds the widget associated with a given menu item name"
  (call-with-current-continuation
   (lambda (return)
     (for-each
      (lambda (top-menu)
	(for-each-child
	 top-menu
	 (lambda (w)
	   (let ((option-holder (cadr (XtGetValues w (list XmNsubMenuId 0)))))
	     (for-each-child
	      option-holder
	      (lambda (menu)
		(if (string=? name (XtName menu))
		    (return menu)
		    (if (XmIsCascadeButton menu)
			(let ((options (cadr (XtGetValues menu (list XmNsubMenuId 0)))))
			  (for-each-child
			   options
			   (lambda (inner-menu)
			     (if (string=? name (XtName inner-menu))
				 (return inner-menu)))))))))))))
      (cdr (menu-widgets)))
     (throw 'no-such-menu (list "menu-option" name)))))

(define (show-all-atoms)
  "(show-all-atoms) displays all current X atom names"
  (let ((i 1)
	(dpy (XtDisplay (cadr (main-widgets))))
	(happy #t))
    (XSetErrorHandler (lambda (dpy err)
			(set! happy #f)
			#f))
    (while happy
      (let ((name (XGetAtomName dpy (list 'Atom i))))
	(if (string? name)
	    (display (format #f "~D: ~A~%" i name))
	    (set! happy #f)))
      (set! i (1+ i)))
    (XSetErrorHandler #f)))


(define+ (show-font-name font)
  "(show-font-name font-list) shows the Snd-related name and the X-related name of each font in a font list"
  (define (show-next-font context)
    (let ((next-font (XmFontListGetNextFont context)))
      (if (and next-font (car next-font))
	  (begin
	    (if (XFontStruct? (caddr next-font))
		(let ((name (XGetFontProperty (caddr next-font) XA_FULL_NAME)))
		  (if (not (car name))
		      (set! name (XGetFontProperty (caddr next-font) XA_FAMILY_NAME)))
		  (snd-print 
		   (format #f "~A: ~A~%"
			   (cadr next-font)
			   (XGetAtomName 
			    (XtDisplay (cadr (main-widgets)))
			    (list 'Atom (cadr name))))))
		(snd-print (format #f "no font found!~%")))
	    (show-next-font context)))))
  (let ((context (XmFontListInitFontContext font)))
    (if context
	(begin
	  (show-next-font context)
	  (XmFontListFreeFontContext context))
	"no fonts?")))

(define (show-widget-font widget)
  "(show-widget-font widget) shows what fonts are associated with a widget"
  (show-font-name (cadr (XtVaGetValues widget (list XmNfontList 0)))))


;;; -------- enable C-s and C-r in listener

(define add-find-to-listener
  (let ((dialog #f)
	(find-text #f)
	(find-forward #t)
	(find-new #t)
	(listener-text (list-ref (main-widgets) 4))
	(shell (cadr (main-widgets)))
	(snd-app (car (main-widgets))))
    (lambda ()

      (define (start-dialog)
	(if (not dialog)
	    (let ((xdismiss (XmStringCreate "Dismiss" XmFONTLIST_DEFAULT_TAG))
		  (xhelp (XmStringCreate "Help" XmFONTLIST_DEFAULT_TAG))
		  (xfind (XmStringCreate "Find" XmFONTLIST_DEFAULT_TAG)))
	      (set! dialog (XmCreateMessageDialog shell
						  "Find"
						  (list XmNcancelLabelString   xdismiss
							XmNokLabelString       xfind
							XmNhelpLabelString     xhelp
							XmNautoUnmanage        #f
							XmNresizePolicy        XmRESIZE_GROW
							XmNnoResize            #f
							XmNtransient           #f
							XmNbackground          (basic-color))))
	      (for-each
	       (lambda (button color)
		 (XtVaSetValues (XmMessageBoxGetChild dialog button)
				(list XmNarmColor   (pushed-button-color)
				      XmNbackground color)))
	       (list XmDIALOG_HELP_BUTTON XmDIALOG_CANCEL_BUTTON XmDIALOG_OK_BUTTON)
	       (list (help-button-color) (quit-button-color) (doit-button-color)))
	      (XtAddCallback dialog XmNcancelCallback (lambda (w context info) (XtUnmanageChild dialog)))
	      (XtAddCallback dialog XmNhelpCallback (lambda (w context info) (help-dialog "Find" "no help yet")))
	      (XtAddCallback dialog XmNokCallback (lambda (w context info) (find-it)))
	      (XmStringFree xhelp)
	      (XmStringFree xdismiss)
	      (XmStringFree xfind)
	      (set! find-text (XtCreateManagedWidget "text" xmTextFieldWidgetClass dialog
						     (list XmNleftAttachment      XmATTACH_FORM
							   XmNrightAttachment     XmATTACH_FORM
							   XmNtopAttachment       XmATTACH_FORM
							   XmNbottomAttachment    XmATTACH_WIDGET
							   XmNbottomWidget        (XmMessageBoxGetChild dialog XmDIALOG_SEPARATOR)
							   XmNbackground          (basic-color))))
	      (XtAddCallback find-text XmNfocusCallback 
			     (lambda (w c i)
			       (XtVaSetValues w (list XmNbackground (WhitePixelOfScreen (DefaultScreenOfDisplay (XtDisplay shell)))))))
	      (XtAddCallback find-text XmNlosingFocusCallback (lambda (w c i) (XtSetValues w (list XmNbackground (basic-color)))))
	      (XtAddCallback find-text XmNvalueChangedCallback (lambda (w c i) (set! find-new #t)))))
	(XtManageChild dialog))

      (define (find-it)
	(let* ((search-str (XmTextFieldGetString find-text))
	       (len (string-length search-str))
	       (pos (XmTextFindString listener-text
				      (+ (XmTextGetCursorPosition listener-text)
					 (if find-new 0 (if find-forward 1 -1)))
				      search-str
				      (if find-forward XmTEXT_FORWARD XmTEXT_BACKWARD))))
	  (if (not pos)
	      (set! pos (XmTextFindString listener-text
					  (if find-forward 0 (XmTextGetLastPosition listener-text))
					  search-str
					  (if find-forward XmTEXT_FORWARD XmTEXT_BACKWARD))))
	  (if (number? pos)
	      (begin
		(XmTextSetInsertionPosition listener-text pos)
		(XmTextSetHighlight listener-text pos (+ pos len) XmHIGHLIGHT_SELECTED) ; flash the string briefly
		(XtAppAddTimeOut snd-app 200 
				 (lambda (context id) 
				   (XmTextSetHighlight listener-text pos (+ pos len) XmHIGHLIGHT_NORMAL)))))
	  (set! find-new #f)))
	  
      (XtAppAddActions snd-app
		       (list (list "search-forward" 
				   (lambda args 
				     (set! find-forward #t)
				     (start-dialog)))
			     (list "search-backward"
				   (lambda args
				     (set! find-forward #f)
				     (start-dialog)))))
      (XtOverrideTranslations listener-text
			      (XtParseTranslationTable "Ctrl <Key>s: search-forward()
						        Ctrl <Key>r: search-backward()")))))
  
  
;;; -------- add a function to be called when the window manager sends us a "save yourself" or "take focus" message

(define (upon-save-yourself thunk)
  "(upon-save-yourself thunk) causes 'thunk' to be called if a 'save yourself' message is received"
  (XmAddWMProtocolCallback 
   (cadr (main-widgets))
   (XmInternAtom (XtDisplay (cadr (main-widgets))) "WM_SAVE_YOURSELF" #f)
   (lambda (w c i)
     (thunk))
   #f))

;;; similarly for "take focus"

(define (upon-take-focus thunk)
  "(upon-take-focus thunk) causes 'thunk' to be called if a 'take focus' message is received"
  (XmAddWMProtocolCallback 
   (cadr (main-widgets))
   (XmInternAtom (XtDisplay (cadr (main-widgets))) "WM_TAKE_FOCUS" #f)
   (lambda (w c i)
     (thunk))
   #f))


;;; -------- add text widget to notebook "status" area --------

(define (add-text-to-status-area)
  "(add-text-to-status-area) adds a text widget to the notebook status area"
  ;; it might be a better use of this space to put dlp's icon row in it
  (let ((notebook (list-ref (main-widgets) 3)))
    (if (XmIsNotebook notebook)
	(let ((text (XtCreateManagedWidget "notebook-text" xmTextFieldWidgetClass notebook
	              (list XmNbackground (basic-color)))))
	  (XtAddCallback text XmNfocusCallback
			 (lambda (w c i)
			   (XtSetValues w (list XmNbackground (white-pixel)))))
	  (XtAddCallback text XmNlosingFocusCallback
			 (lambda (w c i)
			   (XtSetValues w (list XmNbackground (basic-color)))))
	  text)
	#f)))
	  

;;; -------- variable display panel --------

(define variables-dialog #f)
(define variables-notebook #f)
(define variables-pages '())

(define (make-variables-dialog)
  "(make-variables-dialog) makes a variable-display dialog"
  (let ((xdismiss (XmStringCreate "Dismiss" XmFONTLIST_DEFAULT_TAG))
	(titlestr (XmStringCreate "Variables" XmFONTLIST_DEFAULT_TAG)))
    (set! variables-dialog 
	  (XmCreateTemplateDialog (cadr (main-widgets)) "variables-dialog"
				  (list XmNokLabelString       xdismiss
					XmNautoUnmanage        #f
					XmNdialogTitle         titlestr
					XmNresizePolicy        XmRESIZE_GROW
					XmNnoResize            #f
					XmNtransient           #f
					XmNheight              400
					XmNwidth               400
					XmNbackground          (basic-color))))
    
    (XtVaSetValues (XmMessageBoxGetChild variables-dialog XmDIALOG_OK_BUTTON)
		   (list XmNarmColor   (pushed-button-color)
			 XmNbackground (quit-button-color)))
    (XtAddCallback variables-dialog 
		   XmNokCallback (lambda (w context info)
				   (XtUnmanageChild variables-dialog)))
    (XmStringFree xdismiss)
    (XmStringFree titlestr)
    
    (set! variables-notebook
	  (XtCreateManagedWidget "variables-notebook" xmNotebookWidgetClass variables-dialog
				 (list XmNleftAttachment      XmATTACH_FORM
				       XmNrightAttachment     XmATTACH_FORM
				       XmNtopAttachment       XmATTACH_FORM
				       XmNbottomAttachment    XmATTACH_WIDGET
				       XmNbottomWidget        (XmMessageBoxGetChild variables-dialog XmDIALOG_SEPARATOR)
				       XmNbackground          (basic-color)
				       XmNframeBackground     (zoom-color)
				       XmNbindingWidth        14)))
    (XtManageChild variables-dialog)
    variables-dialog))

(define* (make-variable-display page-name variable-name :optional (type 'text) (range (list 0.0 1.0)))
"(make-variable-display page-name variable-name :optional (type 'text) (range (list 0.0 1.0))) makes a variable \
display widget; type = 'text, 'meter, 'graph, 'spectrum, 'scale"
  (if (not (Widget? variables-dialog)) (make-variables-dialog))
  ;; rowColumn widget gets confused by drawingareas, so I'll split them out into separate panes
  (let ((page-info (assoc page-name variables-pages)))
    (if (not page-info)
	(let* ((panes (XtCreateManagedWidget page-name xmPanedWindowWidgetClass variables-notebook '()))
	       (simple-cases (XtCreateManagedWidget page-name xmRowColumnWidgetClass panes
						    (list XmNorientation XmVERTICAL
							  XmNpaneMinimum 30
							  XmNbackground  (basic-color)))))
	  (set! page-info (cons page-name (list panes simple-cases)))
	  (XtCreateManagedWidget page-name xmPushButtonWidgetClass variables-notebook
				 (list XmNnotebookChildType XmMAJOR_TAB
				       XmNbackground        (basic-color)))
	  (set! variables-pages (cons page-info variables-pages))))
    (let* ((row-pane (caddr page-info))
	   (pane (cadr page-info))
	   (var-label (string-append variable-name ":")))
      (case type
	((text)
	 ;; add a horizontal pair: label text
	 (let* ((row (XtCreateManagedWidget (string-append variable-name "-row") xmRowColumnWidgetClass row-pane
					    (list XmNorientation XmHORIZONTAL
						  XmNbackground  (basic-color))))
		(label (XtCreateManagedWidget var-label xmLabelWidgetClass row
					      (list XmNbackground  (basic-color)))))
	   (XtCreateManagedWidget (string-append variable-name "-value") xmTextFieldWidgetClass row
				  (list XmNeditable #f
					XmNresizeWidth #t
					XmNbackground (WhitePixelOfScreen (DefaultScreenOfDisplay (XtDisplay variables-dialog)))))))
	((scale)
	 ;; scale bar with red "thermometer"
	 (let* ((title (XmStringCreate var-label XmFONTLIST_DEFAULT_TAG))
		(scl (XtCreateManagedWidget variable-name xmScaleWidgetClass row-pane
					    (list XmNbackground  (basic-color)
						  XmNslidingMode XmTHERMOMETER
						  XmNminimum (inexact->exact (floor (* 100 (car range))))
						  XmNmaximum (inexact->exact (floor (* 100 (cadr range))))
						  XmNdecimalPoints 2
						  XmNtitleString title
						  XmNorientation XmHORIZONTAL
						  XmNshowValue XmNEAR_BORDER))))
	   (XtVaSetValues (find-child scl "Scrollbar") (list XmNtroughColor (red-pixel)))
	   (XmStringFree title)
	   scl))
	((meter)
	 ;; using the level meters in snd-motif.scm
	 (let* ((height 70)
		(width 210)
		(label (XtCreateManagedWidget var-label xmLabelWidgetClass row-pane
					      (list XmNbackground  (basic-color)))))
	   (make-level-meter row-pane width height '() #f)))
	((graph)
	 (let* ((form (XtCreateManagedWidget var-label xmFormWidgetClass pane 
					     (list XmNpaneMinimum 100)))
		(snd (make-variable-graph form (string-append variable-name ": time") 2048 (mus-srate))))
	   (list snd (channel-data snd 0))))
	((spectrum)
	 (let* ((form (XtCreateManagedWidget var-label xmFormWidgetClass pane
					     (list XmNpaneMinimum 100)))
		(snd (make-variable-graph form variable-name 2048 (inexact->exact (mus-srate)))))
	   (set! (time-graph? snd 0) #f)
	   (set! (transform-graph? snd 0) #t)
	   (set! (x-axis-label snd 0 transform-graph) (string-append variable-name ": frequency"))
	   (list snd (channel-data snd 0))))
	(else #f)))))

(define (variable-display var widget)
  "(variable-display var widget) displays the value of 'var' in 'widget'"
  (if (Widget? widget)
      (if (XmIsTextField widget)
	  ;; text representation
	  (let ((old-str (XmTextFieldGetString widget))
		(new-str (object->string var)))
	    (if (not (string=? old-str new-str))
		(begin
		  (XmTextFieldSetString widget new-str)
		  (if (XtIsManaged widget)
		      (XmUpdateDisplay widget)))))
	  (if (XmIsScale widget)
	      ;; thermometer
	      (XmScaleSetValue widget (inexact->exact (floor (* 100 var))))))
      (if (list? widget)
	  (if (number? (car widget))
	      ;; graph/spectrum -- does this need an explicit update?
	      (let* ((snd (car widget))
		     (data (cadr widget))
		     (frames (sound-data-length data))
		     (loc (cursor snd 0)))
		(sound-data-set! data 0 loc var)
		(if (time-graph? snd) (update-time-graph snd))
		(if (transform-graph? snd) (update-transform-graph snd))
		(if (= (+ loc 1) frames)
		    (set! (cursor snd 0) 0)
		    (set! (cursor snd 0) (+ loc 1))))
	      (if (XmIsDrawingArea (car widget))
		  ;; level meter
		  (begin
		    (list-set! widget 1 var)
		    (display-level widget)
		    (XmUpdateDisplay (car widget)))))))
  var)

(define (variable-display-reset widget)
  "(variable-display-reset widget) restarts the variable graphs -- this is intended for the start (or perhaps end) of a note"
  (if (list? widget)
      (if (number? (car widget))
	  ;; graph/spectrum
	  (let* ((snd (car widget))
		 (data (cadr widget))
		 (frames (sound-data-length data)))
	    (set! (cursor snd 0) 0)
	    (do ((i 0 (1+ i)))
		((= i frames))
	      (sound-data-set! data 0 i 0.0))))))


#|
(define wid (make-variable-display "do-loop" "i*2" 'text))
(define wid1 (make-variable-display "do-loop" "i" 'text))
(do ((i 0 (1+ i)))
    ((= i 10))
  (variable-display (* (variable-display i wid1) 2) wid))

(define wid2 (make-variable-display "a-loop" "k*2" 'meter))
;(define wid3 (make-variable-display "a-loop" "k" 'scale '(0 40)))
(do ((k 0 (1+ k)))
    ((= k 11))
  (variable-display (* k .02) wid2))
|#

(define with-minmax-button
  (let ((maxed-snds '()))
    (lambda (snd)
      (let ((previous-minmax (find-if (lambda (n) (= (car n) snd)) maxed-snds)))
	(if (not previous-minmax)
	    (let* ((widgets (sound-widgets snd))
		   (minibuffer (list-ref widgets 3))
		   (play-button (list-ref widgets 4))
		   (cur-size (cadr (XtVaGetValues (car widgets) (list XmNheight 0)))))
	      (XtUnmanageChild play-button)
	      (let* ((name-form (XtParent minibuffer)) ; "snd-name-form"
		     (new-minmax (XtCreateManagedWidget "." xmPushButtonWidgetClass name-form 
							(list XmNbackground      (basic-color)
							      XmNrightAttachment XmATTACH_FORM
							      XmNtopAttachment   XmATTACH_FORM
							      XmNmarginWidth 2
							      XmNmarginHeight 0
							      XmNshadowThickness 0
							      ))))
		(XtVaSetValues play-button (list XmNrightAttachment XmATTACH_WIDGET
						 XmNrightWidget new-minmax))
		(XtManageChild play-button)
		(XtAddCallback 
		 new-minmax XmNactivateCallback 
		 (lambda (w c i)
		   (let ((mv (find-if (lambda (n) (= (car n) c)) maxed-snds)))
		     (if mv
			 (let ((maxed (caddr mv)))
			   (if maxed
			       (begin
				 (list-set! mv 3 (cadr (XtVaGetValues (car (sound-widgets c)) (list XmNheight 0))))
				 (list-set! mv 4 (show-controls c))
				 (do ((i 0 (1+ i)))
				     ((= i (chans c)))
				   (XtUnmanageChild (list-ref (channel-widgets c i) 10)))
				 (set! (show-controls c) #f)
				 (XmChangeColor new-minmax (make-color 1 1 0))
				 (XtVaSetValues (car (sound-widgets c)) (list XmNpaneMaximum 25)))
			       (let ((prev-size (list-ref mv 3)))
				 (do ((i 0 (1+ i)))
				     ((= i (chans c)))
				   (XtManageChild (list-ref (channel-widgets c i) 10)))
				 (if (list-ref mv 4) (set! (show-controls c) #t))
				 (XmChangeColor new-minmax (basic-color))
				 (XtVaSetValues (car (sound-widgets c)) (list XmNpaneMaximum prev-size XmNpaneMinimum (1- prev-size)))
				 (XtVaSetValues (car (sound-widgets c)) (list XmNpaneMaximum 1000 XmNpaneMinimum 1))))
			   (list-set! mv 2 (not maxed))))))
			       snd)

		(set! previous-minmax (list snd new-minmax #t cur-size (show-controls snd)))
		(set! maxed-snds (cons previous-minmax maxed-snds)))))
	#f))))


;(add-hook! after-open-hook with-minmax-button)


(define (set-root-window-color color)
  "(set-root-window-color color) sets the color of the overall X background"
  (let* ((dpy (XtDisplay (cadr (main-widgets))))
	 (root-window (DefaultRootWindow dpy)))
    (XSetWindowBackground dpy root-window color)
    (XClearWindow dpy root-window)))


;;; you can get a different scrollbar style with:
;;; (XtVaSetValues (XmGetXmDisplay (XtDisplay (cadr (main-widgets)))) (list XmNenableThinThickness #t))


;;; get open file list across top of window (like Xemacs): use -notebook, then:
;;; this is now the default
(define (notebook-with-top-tabs)
  "(notebook-with-top-tabs) posts the list of open sounds across the top of the Snd window (like the Emacs buffer list)"
  (let ((nb (list-ref (main-widgets) 3)))
    (XtVaSetValues nb (list XmNorientation XmVERTICAL
                            XmNbindingType XmNONE
                            XmNbackPagePlacement XmTOP_RIGHT))))


;;; -------- create-ssb-dialog --------
;;;
;;; this needs auto-pitch detection

(define ssb-dialog #f)

(define (create-ssb-dialog)
  "(create-ssb-dialog) creates a dialog for testing the ssb-am stuff"
  (if (not (Widget? ssb-dialog))
      (let ((xdismiss (XmStringCreate "Dismiss" XmFONTLIST_DEFAULT_TAG))
	    (xhelp (XmStringCreate "Help" XmFONTLIST_DEFAULT_TAG))
	    (titlestr (XmStringCreate "SSB-Expand" XmFONTLIST_DEFAULT_TAG))
	    (running #f))
	(set! ssb-dialog 
	      (XmCreateTemplateDialog (cadr (main-widgets)) "ssb-expand"
                (list XmNcancelLabelString   xdismiss
		      XmNhelpLabelString     xhelp
		      XmNautoUnmanage        #f
		      XmNdialogTitle         titlestr
		      XmNresizePolicy        XmRESIZE_GROW
	              XmNnoResize            #f
		      XmNbackground          (basic-color)
		      XmNwidth               400
		      XmNtransient           #f) ))
	(XtAddCallback ssb-dialog 
		       XmNcancelCallback (lambda (w context info)
					   (if running (set! running #f))
					   (XtUnmanageChild ssb-dialog)))
	(XtAddCallback ssb-dialog XmNhelpCallback (lambda (w context info) (snd-print "set 'play' and move the sliders!")))
	(XmStringFree xhelp)
	(XmStringFree xdismiss)
	(XmStringFree titlestr)

	(let* ((ratio 1.0)
	       (old-freq 550.0)
	       (new-freq 550.0)
	       (hilbert-order 40)
	       (ssb-pairs 10)
	       (bw 50.0)
	       (ssbs (make-vector 512))
	       (bands (make-vector 512))
	       (reader #f))

    (letrec ((ssb-expand 
	      (lambda ()
		(mus-ssb-bank ssbs bands (reader) ssb-pairs))) ; clm2xen.c -- an experiment
	     (set-freq 
	      (lambda (nfreq)
		(set! old-freq nfreq)
		(set! ratio (/ (- new-freq old-freq) old-freq))
		(if running
		    (do ((i 0 (1+ i)))
			((= i ssb-pairs))
		      (set! (mus-frequency (vector-ref ssbs i)) (* (1+ i) ratio old-freq))))))
	     (set-ratio
	      (lambda (nfreq)
		(set! new-freq nfreq)
		(set! ratio (/ (- new-freq old-freq) old-freq))
		(if running
		    (do ((i 0 (1+ i)))
			((= i ssb-pairs))
		      (set! (mus-frequency (vector-ref ssbs i)) (* (1+ i) ratio old-freq))))))
	     (set-pairs 
	      (lambda (pairs)
		(set! ssb-pairs pairs)))
	     (set-order 
	      (lambda (order)
		(set! hilbert-order order))))
      (let* ((mainform 
	      (XtCreateManagedWidget "formd" xmRowColumnWidgetClass ssb-dialog
				     (list XmNleftAttachment      XmATTACH_FORM
					   XmNrightAttachment     XmATTACH_FORM
					   XmNtopAttachment       XmATTACH_FORM
					   XmNbottomAttachment    XmATTACH_WIDGET
					   XmNbottomWidget        (XmMessageBoxGetChild ssb-dialog XmDIALOG_SEPARATOR)
					   XmNbackground          (basic-color)
					   XmNorientation         XmVERTICAL)))
	     (button 
	      (XtCreateManagedWidget "play" xmToggleButtonWidgetClass mainform
				     (list XmNbackground  (basic-color))))
	     (freqstr (XmStringCreate "original freq" XmFONTLIST_DEFAULT_TAG))
	     (freq-scale
	      (XtCreateManagedWidget "frq" xmScaleWidgetClass mainform
				     (list XmNorientation XmHORIZONTAL
					   XmNshowValue   #t
					   XmNbackground  (basic-color)
					   XmNvalue       (inexact->exact old-freq)
					   XmNmaximum     1000
					   XmNtitleString freqstr
					   XmNdecimalPoints 0)))
	     (ratiostr (XmStringCreate "new freq" XmFONTLIST_DEFAULT_TAG))
	     (ratio-scale
	      (XtCreateManagedWidget "nfrq" xmScaleWidgetClass mainform
				     (list XmNorientation XmHORIZONTAL
					   XmNshowValue   #t
					   XmNbackground  (basic-color)
					   XmNvalue       (inexact->exact new-freq)
					   XmNmaximum     1000
					   XmNtitleString ratiostr
					   XmNdecimalPoints 0)))
	     (orderstr (XmStringCreate "order" XmFONTLIST_DEFAULT_TAG))
	     (order-scale
	      (XtCreateManagedWidget "order" xmScaleWidgetClass mainform
				     (list XmNorientation XmHORIZONTAL
					   XmNshowValue   #t
					   XmNbackground  (basic-color)
					   XmNvalue       hilbert-order
					   XmNmaximum     100
					   XmNtitleString orderstr
					   XmNdecimalPoints 0)))
	     (pairsstr (XmStringCreate "ssbs" XmFONTLIST_DEFAULT_TAG))
	     (pairs-scale
	      (XtCreateManagedWidget "pairs" xmScaleWidgetClass mainform
				     (list XmNorientation XmHORIZONTAL
					   XmNshowValue   #t
					   XmNbackground  (basic-color)
					   XmNvalue       ssb-pairs
					   XmNmaximum     100
					   XmNtitleString pairsstr
					   XmNdecimalPoints 0))))
	(XmStringFree freqstr)
	(XmStringFree ratiostr)
	(XmStringFree orderstr)
	(XmStringFree pairsstr)
	(XtAddCallback freq-scale XmNvalueChangedCallback (lambda (w context info) (set-freq (.value info))))
	(XtAddCallback freq-scale XmNdragCallback (lambda (w context info) (set-freq (.value info))))
	(XtAddCallback ratio-scale XmNvalueChangedCallback (lambda (w context info) (set-ratio (.value info))))
	(XtAddCallback ratio-scale XmNdragCallback (lambda (w context info) (set-ratio (.value info))))
	(XtAddCallback order-scale XmNvalueChangedCallback (lambda (w context info) (set-order (.value info))))
	(XtAddCallback order-scale XmNdragCallback (lambda (w context info) (set-order (.value info))))
	(XtAddCallback pairs-scale XmNvalueChangedCallback (lambda (w context info) (set-pairs (.value info))))
	(XtAddCallback pairs-scale XmNdragCallback (lambda (w context info) (set-pairs (.value info))))
	(XtAddCallback button XmNvalueChangedCallback 
		       (lambda (w context info)
			 (if running
			     (set! running #f)
			     (let* ((audio-info (open-play-output 1 22050 #f 128))
				    (audio-fd (car audio-info))
				    (outchans (cadr audio-info))
				    (frames (caddr audio-info))
				    (data (make-sound-data outchans frames)))
			       (if (not (= audio-fd -1))
				   (begin
				     (do ((i 1 (1+ i)))
					 ((> i ssb-pairs))
				       (let* ((aff (* i old-freq))
					      (bwf (* bw (+ 1.0 (/ i (* 2 ssb-pairs))))))
					 (vector-set! ssbs (1- i) (make-ssb-am (* i ratio old-freq)))
					 (vector-set! bands (1- i) (make-bandpass (hz->2pi (- aff bwf)) 
										  (hz->2pi (+ aff bwf)) 
										  hilbert-order))))
    				     (set! reader (make-sample-reader 0))
				     (set! running #t)
				     (do ()
					 ((or (c-g?) 
					      (not running)
					      (sample-reader-at-end? reader))
					  (begin
					    (XmToggleButtonSetValue button 0 #f)
					    (set! running #f)
					    (free-sample-reader reader)
					    (mus-audio-close audio-fd)))
				       (do ((k 0 (1+ k)))
					   ((= k frames))
					 (sound-data-set! data 0 k (ssb-expand)))
				       (mus-audio-write audio-fd data frames)))))))))))))
  (XtManageChild ssb-dialog))


;;; -------- create-audit-dialog --------
;;;
;;; amp + freq sliders up to 20KHz

(define audit-dialog #f)

(define (create-audit-dialog)
  "(create-audit-dialog) creates a slightly dangerous hearing test dialog (don't push the amps way up if you can't hear anything)"
  (if (not (Widget? audit-dialog))
      (let ((xdismiss (XmStringCreate "Dismiss" XmFONTLIST_DEFAULT_TAG))
	    (xhelp (XmStringCreate "Help" XmFONTLIST_DEFAULT_TAG))
	    (titlestr (XmStringCreate "Hear Anything Yet?" XmFONTLIST_DEFAULT_TAG))
	    (running #f))
	(set! audit-dialog 
	      (XmCreateTemplateDialog (cadr (main-widgets)) "audit"
                (list XmNcancelLabelString   xdismiss
		      XmNhelpLabelString     xhelp
		      XmNautoUnmanage        #f
		      XmNdialogTitle         titlestr
		      XmNresizePolicy        XmRESIZE_GROW
	              XmNnoResize            #f
		      XmNbackground          (basic-color)
		      XmNwidth               400
		      XmNtransient           #f) ))
	(XtAddCallback audit-dialog 
		       XmNcancelCallback (lambda (w context info)
					   (if running (set! running #f))
					   (XtUnmanageChild audit-dialog)))
	(XtAddCallback audit-dialog XmNhelpCallback (lambda (w context info) (snd-print "set 'play' and move the sliders!")))
	(XmStringFree xhelp)
	(XmStringFree xdismiss)
	(XmStringFree titlestr)
	(set! (mus-srate) 44100)
	
	(let* ((frequency 440.0)
	       (amplitude 0.1)
	       (carrier (make-oscil frequency)))
	  (letrec ((v (lambda ()
			(* amplitude
			   (oscil carrier)))))
	    (let* ((mainform 
		    (XtCreateManagedWidget "formd" xmRowColumnWidgetClass audit-dialog
					   (list XmNleftAttachment      XmATTACH_FORM
						 XmNrightAttachment     XmATTACH_FORM
						 XmNtopAttachment       XmATTACH_FORM
						 XmNbottomAttachment    XmATTACH_WIDGET
						 XmNbottomWidget        (XmMessageBoxGetChild audit-dialog XmDIALOG_SEPARATOR)
						 XmNbackground          (basic-color)
						 XmNorientation         XmVERTICAL)))
		   (button 
		    (XtCreateManagedWidget "play" xmToggleButtonWidgetClass mainform
					   (list XmNbackground  (basic-color))))
		   (ampstr (XmStringCreate "amp" XmFONTLIST_DEFAULT_TAG))
		   (amp-scale
		    (XtCreateManagedWidget "amp" xmScaleWidgetClass mainform
					   (list XmNorientation XmHORIZONTAL
						 XmNshowValue   #t
						 XmNbackground  (basic-color)
						 XmNvalue       (inexact->exact (floor (* amplitude 100)))
						 XmNmaximum     100
						 XmNtitleString ampstr
						 XmNdecimalPoints 2)))
		   (freqstr (XmStringCreate "freq" XmFONTLIST_DEFAULT_TAG))
		   (freq-scale
		    (XtCreateManagedWidget "freq" xmScaleWidgetClass mainform
					   (list XmNorientation XmHORIZONTAL
						 XmNshowValue   #t
						 XmNbackground  (basic-color)
						 XmNvalue       (inexact->exact frequency)
						 XmNmaximum     20000
						 XmNtitleString freqstr
						 XmNdecimalPoints 0))))
	      (XmStringFree ampstr)
	      (XmStringFree freqstr)
	      (XtAddCallback amp-scale XmNvalueChangedCallback (lambda (w context info) (set! amplitude (* .01 (.value info)))))
	      (XtAddCallback amp-scale XmNdragCallback (lambda (w context info) (set! amplitude (* .01 (.value info)))))
	      (XtAddCallback freq-scale XmNvalueChangedCallback (lambda (w context info) (set! (mus-frequency carrier) (.value info))))
	      (XtAddCallback freq-scale XmNdragCallback (lambda (w context info) (set! (mus-frequency carrier) (.value info))))
	      (XtAddCallback button XmNvalueChangedCallback 
			     (lambda (w context info)
			       (if running
				   (set! running #f)
				   (let* ((audio-info (open-play-output 1 44100 #f 256))
					  (audio-fd (car audio-info))
					  (outchans (cadr audio-info))
					  (frames (caddr audio-info))
					  (data (make-sound-data outchans frames)))
				     (if (not (= audio-fd -1))
					 (begin
					   (set! running #t)
					   (do ()
					       ((or (c-g?) (not running))
						(begin
						  (set! running #f)
						  (mus-audio-close audio-fd)))
					     (do ((k 0 (1+ k)))
						 ((= k frames))
					       (sound-data-set! data 0 k (v)))
					     (mus-audio-write audio-fd data frames)))))))))))))
  (XtManageChild audit-dialog))

