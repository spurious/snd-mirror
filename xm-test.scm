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
       (listener (|Widget (list-ref (main-widgets) 4)))
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

#!
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
!#



#!

(define tags (list "one" "two" "three" "four"))
(define colors (list "red" "green" "blue" "orange"))
(define fonts (list "fixed"
		    "-adobe-times-bold-r-*-*-14-*-*-*-*-*-*-*"
		    "-adobe-*-medium-i-*-*-18-*-*-*-*-*-*-*"
		    "-*-helvetica-)-*-*-*-18-*-*-*-*-*-*-*"))

(define shell (|Widget (cadr (main-widgets))))

(define pixels
  (let* ((dpy (|XtDisplay shell))
	 (scr (|DefaultScreen dpy))
	 (cmap (|DefaultColormap dpy scr)))
    (map
     (lambda (color)
       (let ((col (|XColor)))
	 (if (= (|XAllocNamedColor dpy cmap color col col) 0)
	     (snd-error "can't allocate ~S" color)
	     (|pixel col))))
     colors)))

(define tabs
  (let ((ctr 0))
    (map
     (lambda (n)
       (set! ctr (+ ctr 1))
       (|XmTabCreate n |XmINCHES (if (= ctr 1) |XmABSOLUTE |XmRELATIVE) |XmALIGNMENT_BEGINNING "."))
     (list 1.5 1.5 1.5 1.5))))

(define tablist (|XmTabListInsertTabs #f tabs (length tabs) 0))
(if (not (= (|XmTabListTabCount tablist) (length tabs))) 
    (snd-print "tablist len: ~A ~A~%" (|XmTabListTabCount tablist) (length tabs)))

(do ((i 0 (1+ i)))
    ((= i (length tabs)))
  (let ((vals (|XmTabGetValues (|XmTabListGetTab tablist i))))
    (snd-print (format #f "~A~%" vals))))
     
(define rendertable (|XmRenderTableAddRenditions #f 
						 (let ((ctr 0))
						   (map (lambda (r)
							  (set! ctr (+ ctr 1))
							  (|XmRenditionCreate (|Widget (cadr (main-widgets)))
									    r
									    (append
									     (if (= ctr 1)
										 (list |XmNtabList tablist)
										 '())
									     (list |XmNrenditionForeground (list-ref pixels (1- ctr))
										   |XmNfontName (list-ref fonts (1- ctr))
										   |XmNfontType |XmFONT_IS_FONT))))
							tags))
						 (length tags)
						 |XmMERGE_NEW))

(snd-print (format #f "tags: ~A~%" (|XmRenderTableGetTags rendertable)))

(for-each
 (lambda (tag)
   (let ((r (|XmRenderTableGetRendition rendertable tag)))
     (snd-print (|XmRenditionRetrieve r
			      (list |XmNrenditionForeground 0
				    |XmNfontName 0
				    |XmNfontType 0
				    |XmNtag 0)))
     ))
 tags)

(define (show-string n)
  (let ((c (|XmStringInitContext n)))
    (call-with-current-continuation
     (lambda (done)
       (do ((i 0 (1+ i)))
	   (#f)
	 (let ((type (|XmStringGetNextTriple (cadr c))))
	   (if (= (car type) |XmSTRING_COMPONENT_TEXT)
	       (snd-print (format #f " ~A -> ~A~%" i (cdr type)))
	       (if (= (car type) |XmSTRING_COMPONENT_TAB)
		   (snd-print (format #f " tab~%"))
		   (if (= (car type) |XmSTRING_COMPONENT_END)
		       (done #f))))))))
    (|XmStringFreeContext (cadr c))))
					
(define tab (|XmStringComponentCreate |XmSTRING_COMPONENT_TAB 0 #f))
(define row #f)
(define table '())

(let ((our-tags tags))
  (for-each 
   (lambda (word)
     (let ((entry (|XmStringGenerate word
				     #f
				     |XmCHARSET_TEXT
				     (car our-tags))))


       (if row
	   (let ((tmp (|XmStringConcat row tab)))
	     (|XmStringFree row)
	     (set! row (|XmStringConcatAndFree tmp entry)))
	   (set! row entry))
       (set! our-tags (cdr our-tags))
       (if (null? our-tags) 
	   (begin
	     (set! our-tags tags)
	     (set! table (cons row table))
	     (set! row #f)))))
   (list "this" "is" "a" "test" "of" "the" "renditions" "and" "rendertables" "perhaps" "all" "will" "go" "well" "and" "then" "again" "perhaps" "not")))

(for-each
 (lambda (n)
   (show-string n))
 table)

(define (add-main-pane name type args)
  (|XtCreateManagedWidget name type (|Widget (list-ref (main-widgets) 3)) args))

(define rowcol (|XmCreateRowColumn (|Widget (list-ref (main-widgets) 3)) "rowcol" '()))
(define lst (|XmCreateScrolledList rowcol "lst" (list |XmNitems table
						      |XmNitemCount (length table)
						      |XmNwidth 400
						      |XmNvisibleItemCount (length table))))
(|XtSetValues lst (list |XmNrenderTable rendertable)) ; why can't this be done at creation time?
(|XtManageChild lst)
(|XtManageChild rowcol)

(define widget (|XmCreateComboBox rowcol "widget" (list |XmNcomboBoxType |XmDROP_DOWN_COMBO_BOX)))
(for-each
 (lambda (n)
   (|XmComboBoxAddItem widget (|XmStringCreateLocalized n) 0 #f))
 (list "one" "two" "three"))
(|XmComboBoxUpdate widget)
(|XtManageChild widget)


(let* ((shell (|Widget (cadr (main-widgets))))
       (mainpane (|Widget (list-ref (main-widgets) 3)))
       (dpy (|XtDisplay shell))
       (rc (|XtCreateManagedWidget "rc" |xmRowColumnWidgetClass mainpane '()))
       (one (|XtCreateManagedWidget "One" |xmPushButtonWidgetClass rc '()))
       (two (|XtCreateManagedWidget "Two" |xmPushButtonWidgetClass rc '())))
  (|XtAddCallback one |XmNactivateCallback (lambda (w c i)
					     (let ((help_cursor (|XCreateFontCursor dpy |XC_question_arrow)))
					       (snd-print (format #f "got ~S~%"
							  (|XtName (|XmTrackingLocate one help_cursor #f))))))))

(|XGetWindowProperty (|XtDisplay (|Widget (cadr (main-widgets))) )
		     (|XtWindow (|Widget (cadr (main-widgets))) )
		     (|XInternAtom (|XtDisplay (|Widget (cadr (main-widgets))))
				   "SND_VERSION"
				   #f)
		     0 32 #f |XA_STRING)
-> (0 (Atom 31) 8 10 0 "16-Aug-01")

(define (add-main-pane name type args)
  (|XtCreateManagedWidget name type (|Widget (list-ref (main-widgets) 3)) args))

(|XtAppAddActions app (list (list "try1" (lambda (w e strs)
					   (snd-print (format #f "try1: ~A~%" strs))))
			    (list "try2" (lambda (w e strs)
					   (snd-print (format #f "try2: ~A~%" strs))))))

(define tab (|XtParseTranslationTable 
	      (format #f "Ctrl <Key>osfLeft:  try1()~%Ctrl <Key>osfRight: try2()~%Ctrl <Key>osfUp:  try1(hiho)~%Ctrl <Key>osfDown: try2(down, up)~%")))


(define pane (add-main-pane "hiho" |xmTextWidgetClass 
	       '()))

(|XtOverrideTranslations pane tab)


!#
