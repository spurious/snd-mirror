
(load-from-path "snd-motif.scm")
(load-from-path "examp.scm")
(load-from-path "extensions.scm")
(load-from-path "dsp.scm")
(load-from-path "draw.scm")
(load-from-path "env.scm")
(load-from-path "enved.scm")
(load-from-path "hooks.scm")
(load-from-path "marks.scm")
(load-from-path "mix.scm")
(load-from-path "moog.scm")
(load-from-path "play.scm")
(load-from-path "popup.scm")
(load-from-path "rubber.scm")
(load-from-path "zip.scm")
(load-from-path "special-menu.scm") 
(load-from-path "new-backgrounds.scm") 
(load-from-path "marks-menu.scm") 
(load-from-path "mix-menu.scm") 
(load-from-path "new-buttons.scm") 
(load-from-path "panic.scm") 
(load-from-path "ladspa.scm") 
(load-from-path "ladspa-help.scm") 
(load-from-path "fft-menu.scm") 
(load-from-path "edit123.scm") 

(title-with-date)
(keep-file-dialog-open-upon-ok)
(keep-mix-dialog-open-upon-ok)
(make-hidden-controls-dialog)
(check-for-unsaved-edits #t)
(add-hook! after-open-hook show-disk-space)

;(define wd (make-pixmap (cadr (main-widgets)) rough))
;(for-each-child (cadr (main-widgets)) (lambda (w) (XtSetValues w (list XmNbackgroundPixmap wd))))

(define wd (make-pixmap (cadr (main-widgets)) rough))

;(define (paint-all widget)
;  (for-each-child
;    widget
;    (lambda (w)
;      (XtSetValues w (list XmNbackgroundPixmap wd)))))

(define (paint-all widget)
   (for-each-child
     widget
     (lambda (w)
       (if (or (not (XmIsPushButton w))
	       (string=? (XtName w) "revlen-label")
	       (string=? (XtName w) "revscl-label")
	       (string=? (XtName w) "contrast-label")
	       (string=? (XtName w) "expand-label")
	       (string=? (XtName w) "srate-label")
	       (string=? (XtName w) "amp-label"))
           (XtSetValues w (list XmNbackgroundPixmap wd))))))

(paint-all (cadr (main-widgets)))
(for-each
 (lambda (w)
   (if w
       (paint-all w)))
 (dialog-widgets))

(add-hook! new-widget-hook paint-all)

(set! (mix-waveform-height) 32)

;;; (with-level-meters 2)

(add-mark-pane)

(add-sound-file-extension "ogg")
(add-sound-file-extension "OGG")
(add-sound-file-extension "sf")
(add-sound-file-extension "SF2")
(add-sound-file-extension "mp3")
(add-sound-file-extension "MP3")
(add-sound-file-extension "W01")
(add-sound-file-extension "W02")
(add-sound-file-extension "W03")
(add-sound-file-extension "W04")
(add-sound-file-extension "W05")
(add-sound-file-extension "W06")
(add-sound-file-extension "W07")
(add-sound-file-extension "W08")
(add-sound-file-extension "W09")
(add-sound-file-extension "W10")
(add-sound-file-extension "w01")
(add-sound-file-extension "w02")
(add-sound-file-extension "w03")
(add-sound-file-extension "w04")
(add-sound-file-extension "w05")
(add-sound-file-extension "w06")
(add-sound-file-extension "w07")
(add-sound-file-extension "w08")
(add-sound-file-extension "w09")
(add-sound-file-extension "w10")


;;;
;;; OpenGL FFT display
;;;

(define (draw-it)
   (|glXMakeCurrent (XtDisplay (cadr (main-widgets)))
                    (XtWindow (car (channel-widgets)))
                    (snd-glx-context))
   (|glEnable |GL_DEPTH_TEST)
   (|glDepthFunc |GL_LEQUAL)
   (|glClearDepth 1.0)
   (|glClearColor 0.0 0.0 0.0 0.0)
   (|glLoadIdentity)
   (|gluPerspective 40.0 1.0 10.0 200.0)
   (|glTranslatef 0.0 0.0 -50.0)
   (|glRotatef -58.0 0.0 1.0 0.0)
   (let ((vals (XtVaGetValues (car (channel-widgets)) (list XmNwidth 0 XmNheight 0))))
     (|glViewport 0 0 (list-ref vals 1) (list-ref vals 3)))
   (|glClear (logior |GL_COLOR_BUFFER_BIT |GL_DEPTH_BUFFER_BIT))
   (|glBegin |GL_POLYGON)
   (|glColor3f 0.0 0.0 0.0)   (|glVertex3f -10.0 -10.0 0.0)
   (|glColor3f 0.7 0.7 0.7)   (|glVertex3f 10.0 -10.0 0.0)
   (|glColor3f 1.0 1.0 1.0)   (|glVertex3f -10.0 10.0 0.0)
   (|glEnd)
   (|glBegin |GL_POLYGON)
   (|glColor3f 1.0 1.0 0.0)   (|glVertex3f 0.0 -10.0 -10.0)
   (|glColor3f 0.0 1.0 0.7)   (|glVertex3f 0.0 -10.0 10.0)
   (|glColor3f 0.0 0.0 1.0)   (|glVertex3f 0.0 5.0 -10.0)
   (|glEnd)
   (|glBegin |GL_POLYGON)
   (|glColor3f 1.0 1.0 0.0)   (|glVertex3f -10.0 6.0 4.0)
   (|glColor3f 1.0 0.0 1.0)   (|glVertex3f -10.0 3.0 4.0)
   (|glColor3f 0.0 0.0 1.0)   (|glVertex3f 4.0 -9.0 -10.0)
   (|glColor3f 1.0 0.0 1.0)   (|glVertex3f 4.0 -6.0 -10.0)
   (|glEnd)
   (|glXSwapBuffers (XtDisplay (cadr (main-widgets)))
                    (XtWindow (car (channel-widgets))))
   (|glFlush))


;;;
;;; disable original Play radio button
;;;

;(add-hook! after-open-hook
;           (lambda (snd)
;             (XtUnmanageChild (find-child (list-ref (sound-widgets snd) 2) "play"))))


;;;
;;; main menu additions
;;;

;;; -------- add delete and rename options to the file menu

(define (add-delete-option)
  (add-to-menu 0 "Delete" ; add Delete option to File menu
	       (lambda ()
		 ;; close current sound and delete it (after requesting confirmation)
		 (if (>= (selected-sound) 0)
		     (let ((filename (file-name)))
		       (close-sound)
		       (if (yes-or-no? (format #f "delete ~S?" filename))
			   (delete-file filename)))))
	       8)) ; place after File:New

(define (add-rename-option)
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
				(help-dialog "Rename" "Give a new file name to rename the currently selected sound.")))

	      (XtAddCallback new-dialog XmNokCallback 
			      (lambda (w c i)
				(let ((new-name (XmTextFieldGetString rename-text)))
				  (if (and (string? new-name)
					   (> (string-length new-name) 0)
					   (>= (selected-sound) 0))
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
				     (list XmNleftAttachment      XmATTACH_FORM
					   XmNrightAttachment     XmATTACH_FORM
					   XmNtopAttachment       XmATTACH_FORM
					   XmNbottomAttachment    XmATTACH_WIDGET
					   XmNbottomWidget        (XmMessageBoxGetChild rename-dialog XmDIALOG_SEPARATOR)
					   XmNorientation         XmVERTICAL
					   XmNbackground          (basic-color))))
		     (label (XtCreateManagedWidget "new name:" xmLabelWidgetClass mainform
				     (list XmNleftAttachment      XmATTACH_FORM
					   XmNrightAttachment     XmATTACH_NONE
					   XmNtopAttachment       XmATTACH_FORM
					   XmNbottomAttachment    XmATTACH_FORM
					   XmNbackground          (basic-color)))))
		(set! rename-text 
		      (XtCreateManagedWidget "newname" xmTextFieldWidgetClass mainform
				     (list XmNleftAttachment      XmATTACH_WIDGET
					   XmNleftWidget          label
					   XmNrightAttachment     XmATTACH_FORM
					   XmNtopAttachment       XmATTACH_FORM
					   XmNbottomAttachment    XmATTACH_FORM
					   XmNbackground          (basic-color))))
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

(install-searcher-with-colors (lambda (file) #t))
(add-delete-option)
(add-rename-option)

;;;
;;; poup menu stuff
;;;

(change-graph-popup-color "pink")

;;;(add-selection-popup)


(define (change-selection-popup-color new-color)
  ;; new-color can be the color name, an xm Pixel, a snd color, or a list of rgb values (as in Snd's make-color)
  (let ((color-pixel
         (if (string? new-color) ; assuming X11 color names here
             (let* ((shell (cadr (main-widgets)))
                    (dpy (XtDisplay shell))
                    (scr (DefaultScreen dpy))
                    (cmap (DefaultColormap dpy scr))
                    (col (XColor)))
               (if (= (XAllocNamedColor dpy cmap new-color col col) 0)
                   (snd-error "can't allocate ~S" new-color)
                   (.pixel col)))
             (if (color? new-color)
                     new-color
                     ;; assume a list of rgb vals?
                     (apply make-color new-color)))))
    (for-each-child
     selection-popup-menu
     (lambda (n)
       (XmChangeColor n color-pixel)))))
(change-selection-popup-color "coral")

(define (change-fft-popup-color new-color)
  (let ((color-pixel
         (if (string? new-color) ; assuming X11 color names here
             (let* ((shell (cadr (main-widgets)))
                    (dpy (XtDisplay shell))
                    (scr (DefaultScreen dpy))
                    (cmap (DefaultColormap dpy scr))
                    (col (XColor)))
               (if (= (XAllocNamedColor dpy cmap new-color col col) 0)
                   (snd-error "can't allocate ~S" new-color)
                   (.pixel col)))
             (if (color? new-color)
                     new-color
                     ;; assume a list of rgb vals?
                     (apply make-color new-color)))))
    (for-each-child
     fft-popup-menu
     (lambda (n)
       (XmChangeColor n color-pixel)))))
(change-fft-popup-color "orange")

;(change-listener-popup-color "red")

(add-to-menu 1 #f #f) ; separator

;;;
;;; additions to Edit menu
;;;

(define selctr 0)

;;; -------- cut selection -> new file

(define (cut-selection->new)
  (if (selection?)
      (let ((new-file-name (format #f "sel-~D.snd" selctr)))
	(set! selctr (+ selctr 1))
	(save-selection new-file-name)
	(delete-selection)
	(open-sound new-file-name))))

;;; (add-to-menu 1 "Cut Selection -> New" cut-selection->new)

;;; -------- append sound (and append selection for lafs)

(define (append-sound name)
  ;; appends sound file
  (insert-sound name (frames)))

(define (append-selection)
  (if (selection?)
      (insert-selection (frames))))

(add-to-menu 1 "Append Selection" append-selection)

;;; Replace with selection
;;;

(define (replace-with-selection)
  (let ((beg (cursor))
        (len (selection-frames)))
    (delete-samples beg len)
    (insert-selection beg)))

(add-to-menu 1 "Replace with Selection" replace-with-selection)

;;; (add-to-menu 1 #f #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; open and convert stereo MP3 files automatically
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook! open-raw-sound-hook
           (lambda (file choices)
             (list 2 44100 (if (little-endian?) mus-lshort mus-bshort))))

(add-hook! open-hook
           (lambda (filename)
             (if (= (mus-sound-header-type filename) mus-raw)
                 (let ((rawfile (string-append filename ".raw")))
                   (system (format #f "mpg123 -s ~A > ~A" filename rawfile))
                   rawfile)
                 #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; open and convert stereo OGG files automatically
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook! open-raw-sound-hook
           (lambda (file choices)
             (list 2 44100 (if (little-endian?) mus-lshort mus-bshort))))

(add-hook! open-hook
           (lambda (filename)
             (if (= (mus-sound-header-type filename) mus-raw)
                 (let ((rawfile (string-append filename ".raw")))
                   (system (format #f "ogg123 -d raw -f ~A ~A" rawfile filename))
                   rawfile)
                 #f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; set up a region play list
;;; TODO: a GUI for this feature !
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (region-play-list data)
  ;; data is list of lists (list (list time region)...), time in secs
  (for-each
   (lambda (tone)
     (let ((time (* 1000 (car tone)))
           (region (cadr tone)))
       (if (region? region)
           (in time (lambda () (play-region region))))))
   data))

;;; (region-play-list (list (list 0.0 0) (list 0.5 1) (list 1.0 2) (list 1.0 0)))

;;; Deselect function
;;;

(define (deselect-all)
  (if (selection?)
      (set! (selection-member? #t) #f)))

