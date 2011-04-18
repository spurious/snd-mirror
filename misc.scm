(provide 'snd-misc.scm)

(if (not (provided? 'snd-motif)) (snd-error "misc.scm only works in the Motif version of Snd."))

(if (not (provided? 'snd-snd-motif.scm)) (load "snd-motif.scm"))
(if (not (provided? 'snd-examp.scm)) (load "examp.scm"))
(if (not (provided? 'snd-extensions.scm)) (load "extensions.scm"))
(if (not (provided? 'snd-dsp.scm)) (load "dsp.scm"))
(if (not (provided? 'snd-draw.scm)) (load "draw.scm"))
(if (not (provided? 'snd-env.scm)) (load "env.scm"))
(if (not (provided? 'snd-enved.scm)) (load "enved.scm"))
(if (not (provided? 'snd-hooks.scm)) (load "hooks.scm"))
(if (not (provided? 'snd-marks.scm)) (load "marks.scm"))
(if (not (provided? 'snd-mix.scm)) (load "mix.scm"))
(if (not (provided? 'snd-moog.scm)) (load "moog.scm"))
(if (not (provided? 'snd-play.scm)) (load "play.scm"))
(if (not (provided? 'snd-rubber.scm)) (load "rubber.scm"))
(if (not (provided? 'snd-zip.scm)) (load "zip.scm"))
(if (not (provided? 'snd-new-effects.scm)) (load "new-effects.scm"))
(if (not (provided? 'snd-special-menu.scm)) (load "special-menu.scm"))
(if (not (provided? 'snd-new-backgrounds.scm)) (load "new-backgrounds.scm"))
(if (not (provided? 'snd-marks-menu.scm)) (load "marks-menu.scm"))
(if (not (provided? 'snd-fft-menu.scm)) (load "fft-menu.scm"))
(if (not (provided? 'snd-edit123.scm)) (load "edit123.scm"))
(if (not (provided? 'snd-effects-utils.scm)) (load "effects-utils.scm"))

(keep-file-dialog-open-upon-ok)
(set! (ask-about-unsaved-edits) #t)
(if (not (hook-member show-disk-space after-open-hook))
    (hook-push after-open-hook show-disk-space))

;(define wd (make-pixmap (cadr (main-widgets)) rough)) ; this comes from new-backgrounds.scm
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
       (if (and (Widget? w)
		(or (not (XmIsPushButton w))
		    (string=? (XtName w) "revlen-label")
		    (string=? (XtName w) "revscl-label")
		    (string=? (XtName w) "contrast-label")
		    (string=? (XtName w) "expand-label")
		    (string=? (XtName w) "srate-label")
		    (string=? (XtName w) "amp-label")))
	   (XtSetValues w (list XmNbackgroundPixmap wd))))))

(paint-all (cadr (main-widgets)))
(for-each
 (lambda (w)
   (if (and w
	    (Widget? w))
       (paint-all w)))
 (dialog-widgets))

(if (not (hook-member paint-all new-widget-hook))
    (hook-push new-widget-hook paint-all))

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
;;; disable original Play radio button
;;;

;(hook-push after-open-hook
;           (lambda (snd)
;             (XtUnmanageChild (find-child (list-ref (sound-widgets snd) 2) "play"))))


;;;
;;; main menu additions
;;;

;;; -------- add delete and rename options to the file menu

(define (add-delete-option)
  (add-to-menu 0 "Delete" ; add Delete option to File menu
	       (lambda ()
		 ;; close current sound and delete it
		 (if (>= (selected-sound) 0)
		     (let ((filename (file-name)))
		       (close-sound)
		       (delete-file filename))))
	       8)) ; place after File:New

(define (add-rename-option)
  (let ((rename-dialog #f)
	(rename-text #f))
    (add-to-menu 0 "Rename" 
      (lambda ()
	;; open dialog to get new name, save-as that name, open
	(if (not rename-dialog)
	    ;; make a standard dialog
	    (let* ((xdismiss (XmStringCreate "Go Away" XmFONTLIST_DEFAULT_TAG))
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
		   (list XmNarmColor   (selection-color)
			 XmNbackground color)))
	       (list XmDIALOG_HELP_BUTTON XmDIALOG_CANCEL_BUTTON XmDIALOG_OK_BUTTON)
	       (list (highlight-color) (highlight-color) (highlight-color)))
    
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
				      (let ();(current-name (file-name)))
					(save-sound-as new-name)
					(close-sound)
					;(rename-file current-name new-name)
					;; (delete-file current-name) perhaps?
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

;;; -------- append selection

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

(hook-push open-raw-sound-hook
           (lambda (file choices)
             (list 2 44100 (if (little-endian?) mus-lshort mus-bshort))))

(hook-push open-hook
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

(hook-push open-raw-sound-hook
           (lambda (file choices)
             (list 2 44100 (if (little-endian?) mus-lshort mus-bshort))))

(hook-push open-hook
           (lambda (filename)
             (if (= (mus-sound-header-type filename) mus-raw)
                 (let ((rawfile (string-append filename ".raw")))
                   (system (format #f "ogg123 -d raw -f ~A ~A" rawfile filename))
                   rawfile)
                 #f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; set up a region play list
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (region-play-list data)
  ;; data is list of lists (list (list time region)...), time in secs
  (for-each
   (lambda (tone)
     (let ((time (* 1000 (car tone)))
           (region (cadr tone)))
       (if (region? region)
           (in time (lambda () (play region))))))
   data))

;;; (region-play-list (list (list 0.0 0) (list 0.5 1) (list 1.0 2) (list 1.0 0)))

;;; Deselect function
;;;

(define deselect-all unselect-all)

