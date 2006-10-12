(use-modules (ice-9 format))

(provide 'snd-special-menu.scm)

(if (provided? 'xm)
    (if (not (provided? 'snd-effects-utils.scm))
	(load-from-path "effects-utils.scm")))

(if (provided? 'xg)
    (if (not (provided? 'snd-gtk-effects-utils.scm))
	(load-from-path "gtk-effects-utils.scm")))

(if (not (provided? 'snd-edit-menu.scm)) (load-from-path "edit-menu.scm"))
(if (not (defined? 'start-enveloping)) (load-from-path "enved.scm"))
(if (not (defined? 'explode-sf2)) (load-from-path "examp.scm"))

(define special-list '()) ; menu labels are updated to show current default settings

(define special-menu (add-to-main-menu "Special" (lambda ()
						   (update-label special-list))))

;;; -------- Append file
;;;

(add-to-menu edit-menu "Append file"
  (lambda ()
    (select-file
     (lambda (filename)
        (insert-sound filename (frames))))))

(add-to-menu special-menu #f #f)


;;; -------- MIDI to WAV
;;;

(add-to-menu special-menu "MIDI to WAV"
  (lambda ()
    (select-file
      (lambda (filename)
        (shell (format #f "timidity -Ow ~a" filename)))
      "Select MIDI file" "." "*.mid" "Converts MIDI file to WAV using TiMidity. \
Output will be named after the original MIDI file, i.e., foo.mid converts to foo.wav. \
You must have TiMidity and a patch set installed for this function to work. \
See the TiMidity home page at http://www.onicos.com/staff/iz/timidity/ for more details.")))

(add-to-menu special-menu #f #f)


;;; -------- Record input channel (Motif only)
;;;

(define record-input-channel 4)
(define record-input-label "Record input channel")
(define record-input-dialog #f)
(define record-input-default-widget #f)
(define record-input-menu-label #f)

(define radio-buttons-yes #t) ; radio-buttons or combo-box choice

(define (cp-record-input)
  (set! (recorder-in-device) record-input-channel)) 

(if (provided? 'xm)
    (begin

      (define (post-record-input-dialog)
        (if (not (Widget? record-input-dialog))
            (let ((initial-record-input-channel 4))

              (set! record-input-dialog
                    (make-effect-dialog 
		     record-input-label

		     (lambda (w context info) (cp-record-input))

		     (lambda (w context info)
		       (help-dialog "Record input channel Help"
"Select the appropriate radio button to set the record input channel. Note that if you are using the ALSA drivers you may need to set the record channel from your system mixer (e.g. aumix) as well."))
		     (lambda (w c i)
		       (set! record-input-channel initial-record-input-channel)
		       (if radio-buttons-yes
			   (XtSetValues record-input-default-widget (list XmNselectedPosition 1))
			   (XmToggleButtonSetState record-input-default-widget #t #t)))))

	      (if radio-buttons-yes
		  (let* ((s1 (XmStringCreateLocalized "Record input channel"))
			 (frame (XtCreateManagedWidget "frame" xmFrameWidgetClass record-input-dialog
				   (list XmNborderWidth 1
					 XmNshadowType XmSHADOW_ETCHED_IN
					 XmNpositionIndex 2)))
			 (frm (XtCreateManagedWidget "frm" xmFormWidgetClass frame
				(list XmNleftAttachment      XmATTACH_FORM
				      XmNrightAttachment     XmATTACH_FORM
				      XmNtopAttachment       XmATTACH_FORM
				      XmNbottomAttachment    XmATTACH_FORM
				      XmNbackground          (basic-color))))
			 (rc (XtCreateManagedWidget "rc" xmRowColumnWidgetClass frm
				   (list XmNorientation         XmHORIZONTAL
					 XmNradioBehavior #t
					 XmNradioAlwaysOne #t
					 XmNentryClass          xmToggleButtonWidgetClass
					 XmNisHomogeneous #t
					 XmNleftAttachment      XmATTACH_FORM
					 XmNrightAttachment     XmATTACH_FORM
					 XmNtopAttachment       XmATTACH_FORM
					 XmNbottomAttachment    XmATTACH_NONE
					 XmNbackground          (basic-color))))
			 (lab (XtCreateManagedWidget "Record input channel" xmLabelWidgetClass frm
				   (list XmNleftAttachment      XmATTACH_FORM
					 XmNrightAttachment     XmATTACH_FORM
					 XmNtopAttachment       XmATTACH_WIDGET
					 XmNtopWidget           rc
					 XmNbottomAttachment    XmATTACH_FORM
					 XmNlabelString         s1
					 XmNalignment           XmALIGNMENT_BEGINNING
					 XmNbackground          (basic-color)))))
		    (for-each 
		     (lambda (size)
		       (let ((button (XtCreateManagedWidget (format #f "~D" size) xmToggleButtonWidgetClass rc
				        (list XmNbackground           (basic-color)
					      XmNvalueChangedCallback (list (lambda (w c i) (if (.set i) (set! record-input-channel c))) size)
					      XmNset                  (= size record-input-channel)))))
			 (if (= size record-input-channel)
			     (set! record-input-default-widget button))))
		     (list 4 5 6 19))
		    (XmStringFree s1)))))
        (activate-dialog record-input-dialog))

      (set! record-input-menu-label (add-to-menu special-menu "Record input channel" (lambda () (post-record-input-dialog)))))

    (set! record-input-menu-label (add-to-menu special-menu record-input-label cp-record-input)))

(set! special-list (cons (lambda ()
                           (let ((new-label (format #f "Record input channel (~D)"
                                                record-input-channel)))
                             (if record-input-menu-label (change-label record-input-menu-label new-label))
                             (set! record-input-label new-label)))
                         special-list))

(add-to-menu special-menu #f #f)


(define env-file-menu-label #f)
(define env-file #f)
(define yes-env-label "Envelope new file (Off)")
(define no-env-label "Envelope new file (On)")

(define (yesenv!)
  (set! env-file #t)
  (if env-file-menu-label (change-label env-file-menu-label no-env-label))
  (start-enveloping))

(define (noenv!)
  (set! env-file #f)
  (if env-file-menu-label (change-label env-file-menu-label yes-env-label))
  (stop-enveloping))

(set! env-file-menu-label 
      (add-to-menu special-menu yes-env-label
		   (lambda ()
		     (if env-file
			 (noenv!)
			 (yesenv!)))))



;;; -------- Play panned
;;;

(define play-panned-file 1)
(define play-panned-label "Play panned")
(define play-panned-dialog #f)
(define play-panned-menu-label #f)

(define (cp-play-panned)
  (play-panned play-panned-file))

(if (or (provided? 'xm)
	(provided? 'xg))
    (begin

      (define (post-play-panned-dialog)
        (if (not play-panned-dialog)
            (let ((initial-play-panned-file 1)
                  (sliders '()))

              (set! play-panned-dialog
                    (make-effect-dialog 
		     play-panned-label
		     
		     (if (provided? 'snd-gtk)
			 (lambda (w context) (cp-play-panned))
			 (lambda (w context info) (cp-play-panned)))

		     (if (provided? 'snd-gtk)
			 (lambda (w context)
			   (help-dialog "Play panned"
					"Move the slider to select the file to play with panning envelope."))
			 (lambda (w context info)
			   (help-dialog "Play panned"
					"Move the slider to select the file to play with panning envelope.")))

		     (if (provided? 'snd-gtk)
			 (lambda (w data)
			   (set! play-panned-file initial-play-panned-file)
			   (set! (.value (GTK_ADJUSTMENT (car sliders))) play-panned-file)
			   (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car sliders))))
			 (lambda (w c i)
			   (set! play-panned-file initial-play-panned-file)
			   (XtSetValues (car sliders) (list XmNvalue play-panned-file))))))

              (set! sliders
                    (add-sliders 
		     play-panned-dialog
		     (list (list "soundfile number" 0 initial-play-panned-file 25
				 
				 (if (provided? 'snd-gtk)
				     (lambda (w context)
				       (set! play-panned-file (.value (GTK_ADJUSTMENT w))))
				     (lambda (w context info)
				       (set! play-panned-file (.value info))))
				 1))))))

        (activate-dialog play-panned-dialog))

      (set! play-panned-menu-label (add-to-menu special-menu "Play panned" (lambda () (post-play-panned-dialog)))))

    (set! play-panned-menu-label (add-to-menu special-menu play-panned-label cp-play-panned)))

(set! special-list (cons (lambda ()
                           (let ((new-label (format #f "Play panned (~D)"  play-panned-file)))
                             (if play-panned-menu-label (change-label play-panned-menu-label new-label))
                             (set! play-panned-label new-label)))
                         special-list))


(add-to-menu special-menu #f #f)


;;; -------- Save as MP3
;;;

(define save-as-mp3-wav-file-number 0)
(define save-as-mp3-label "Save as MP3")
(define save-as-mp3-dialog #f)
(define save-as-mp3-menu-label #f)

(define (cp-save-as-mp3)
  (save-sound-as "tmp.wav" save-as-mp3-wav-file-number mus-riff)
  (system (format #f "bladeenc tmp.wav tmp-~D.mp3" save-as-mp3-wav-file-number)))

(if (or (provided? 'xm)
	(provided? 'xg))
    (begin

      (define (post-save-as-mp3-dialog)
        (if (not save-as-mp3-dialog)

            (let ((initial-save-as-mp3-wav-file-number 0)
                  (sliders '()))
              (set! save-as-mp3-dialog
                    (make-effect-dialog 
		     save-as-mp3-label

		     (if (provided? 'snd-gtk)
			 (lambda (w context) (cp-save-as-mp3))
			 (lambda (w context info) (cp-save-as-mp3)))

		     (if (provided? 'snd-gtk)
			 (lambda (w context)
			   (help-dialog "Save as MP3"
					"Move the slider to select the file to save as an MP3. \
The new MP3 will be named tmp-N.mp3 by default.  Bladeenc is currently the only supported encoder. \
Please see the Web page at bladeenc.mp3.no for details regarding Bladeenc."))
			 (lambda (w context info)
			   (help-dialog "Save as MP3"
					"Move the slider to select the file to save as an MP3. \
The new MP3 will be named tmp-N.mp3 by default.  Bladeenc is currently the only supported encoder. \
Please see the Web page at bladeenc.mp3.no for details regarding Bladeenc.")))

		     (if (provided? 'snd-gtk)
			 (lambda (w data)
			   (set! save-as-mp3-wav-file-number initial-save-as-mp3-wav-file-number)
			   (set! (.value (GTK_ADJUSTMENT (car sliders))) save-as-mp3-wav-file-number)
			   (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car sliders))))
			 (lambda (w c i)
			   (set! save-as-mp3-wav-file-number initial-save-as-mp3-wav-file-number)
			   (XtSetValues (car sliders) (list XmNvalue save-as-mp3-wav-file-number))))))

              (set! sliders
                    (add-sliders
		     save-as-mp3-dialog
		     (list (list "soundfile number" 0 initial-save-as-mp3-wav-file-number 250
				 (if (provided? 'snd-gtk)
				     (lambda (w data)
				       (set! save-as-mp3-wav-file-number (.value (GTK_ADJUSTMENT w))))
				     (lambda (w context info)
				       (set! save-as-mp3-wav-file-number (.value info))))
				 1))))))
        (activate-dialog save-as-mp3-dialog))

      (set! save-as-mp3-menu-label (add-to-menu special-menu "Save as MP3" (lambda () (post-save-as-mp3-dialog)))))

    (set! save-as-mp3-menu-label (add-to-menu special-menu save-as-mp3-label cp-save-as-mp3)))

(set! special-list (cons (lambda ()
                           (let ((new-label (format #f "Save as MP3 (~D)"  save-as-mp3-wav-file-number)))
                             (if save-as-mp3-menu-label (change-label save-as-mp3-menu-label new-label))
                             (set! save-as-mp3-label new-label)))
                         special-list))



;;; -------- Save as Ogg File
;;;

(define save-as-ogg-wav-file-number 0)
(define save-as-ogg-label "Save as Ogg file")
(define save-as-ogg-dialog #f)
(define save-as-ogg-menu-label #f)

(define (cp-save-as-ogg)
  (save-sound-as "tmp.wav" save-as-ogg-wav-file-number mus-riff)
  (system (format #f "oggenc tmp.wav -o tmp-~D.ogg" save-as-ogg-wav-file-number)))

(if (or (provided? 'xm)
	(provided? 'xg))
    (begin

      (define (post-save-as-ogg-dialog)
        (if (not save-as-ogg-dialog)

            (let ((initial-save-as-ogg-wav-file-number 0)
                  (sliders '()))

              (set! save-as-ogg-dialog
                    (make-effect-dialog 
		     save-as-ogg-label

		     (if (provided? 'snd-gtk)
			 (lambda (w context) (cp-save-as-ogg))
			 (lambda (w context info) (cp-save-as-ogg)))

		     (if (provided? 'snd-gtk)
			 (lambda (w context)
			   (help-dialog "Save as Ogg file"
					"Move the slider to select the file to save as an Ogg file. \
The new file will be named tmp-N.ogg by default. Oggenc is currently the only supported Ogg encoder. \
Please see the Web page at www.xiphophorus.org for details regarding the Ogg/Vorbis project."))
			 (lambda (w context info)
			   (help-dialog "Save as Ogg file"
					"Move the slider to select the file to save as an Ogg file. \
The new file will be named tmp-N.ogg by default. Oggenc is currently the only supported Ogg encoder. \
Please see the Web page at www.xiphophorus.org for details regarding the Ogg/Vorbis project.")))

		     (if (provided? 'snd-gtk)
			 (lambda (w data)
			   (set! save-as-ogg-wav-file-number initial-save-as-ogg-wav-file-number)
			   (set! (.value (GTK_ADJUSTMENT (car sliders))) save-as-ogg-wav-file-number)
			   (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car sliders))))
			 (lambda (w c i)
			   (set! save-as-ogg-wav-file-number initial-save-as-ogg-wav-file-number)
			   (XtSetValues (car sliders) (list XmNvalue save-as-ogg-wav-file-number))))))

              (set! sliders
                    (add-sliders 
		     save-as-ogg-dialog
		     (list (list "soundfile number" 0 initial-save-as-ogg-wav-file-number 250
				 (if (provided? 'snd-gtk)
				     (lambda (w data)
				       (set! save-as-ogg-wav-file-number (.value (GTK_ADJUSTMENT w))))
				     (lambda (w context info)
				       (set! save-as-ogg-wav-file-number (.value info))))
				 1))))))
        (activate-dialog save-as-ogg-dialog))

      (set! save-as-ogg-menu-label (add-to-menu special-menu "Save as Ogg file" (lambda () (post-save-as-ogg-dialog)))))

    (set! save-as-ogg-menu-label (add-to-menu special-menu save-as-ogg-label cp-save-as-ogg)))

(set! special-list (cons (lambda ()
                           (let ((new-label (format #f "Save as Ogg file (~D)"  save-as-ogg-wav-file-number)))
                             (if save-as-ogg-menu-label (change-label save-as-ogg-menu-label new-label))
                             (set! save-as-ogg-label new-label)))
                         special-list))

(add-to-menu special-menu #f #f)

(add-to-menu special-menu "Explode SF2" explode-sf2) 

