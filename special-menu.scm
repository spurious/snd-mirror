(provide 'snd-special-menu.scm)

(if (provided? 'xm)
    (begin
      (if (not (provided? 'snd-effects-utils.scm))
	  (load "effects-utils.scm"))
      (if (not (provided? 'snd-snd-motif.scm))
	  (load "snd-motif.scm")))) ; select-file

(if (provided? 'xg)
    (begin
      (if (not (provided? 'snd-gtk-effects-utils.scm))
	  (load "gtk-effects-utils.scm"))
      (if (not (provided? 'snd-snd-gtk.scm))
	  (load "snd-gtk.scm")))) ; select-file

(if (not (provided? 'snd-edit-menu.scm)) (load "edit-menu.scm"))
(if (not (defined? 'start-enveloping)) (load "enved.scm"))
(if (not (defined? 'explode-sf2)) (load "examp.scm"))

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
        (system (format #f "timidity -Ow ~a" filename)))
      "Select MIDI file" "." "*.mid" "Converts MIDI file to WAV using TiMidity. \
Output will be named after the original MIDI file, i.e., foo.mid converts to foo.wav. \
You must have TiMidity and a patch set installed for this function to work. \
See the TiMidity home page at http://www.onicos.com/staff/iz/timidity/ for more details.")))

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
			   (gtk_adjustment_set_value (GTK_ADJUSTMENT (car sliders)) play-panned-file)
			   ;;; (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car sliders)))
			   )
			 (lambda (w c i)
			   (set! play-panned-file initial-play-panned-file)
			   (XtSetValues (car sliders) (list XmNvalue play-panned-file))))))

              (set! sliders
                    (add-sliders 
		     play-panned-dialog
		     (list (list "soundfile number" 0 initial-play-panned-file 25
				 
				 (if (provided? 'snd-gtk)
				     (lambda (w context)
				       (set! play-panned-file (gtk_adjustment_get_value (GTK_ADJUSTMENT w))))
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
			   (gtk_adjustment_set_value (GTK_ADJUSTMENT (car sliders)) save-as-mp3-wav-file-number)
			   ;;; (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car sliders)))
			   )
			 (lambda (w c i)
			   (set! save-as-mp3-wav-file-number initial-save-as-mp3-wav-file-number)
			   (XtSetValues (car sliders) (list XmNvalue save-as-mp3-wav-file-number))))))

              (set! sliders
                    (add-sliders
		     save-as-mp3-dialog
		     (list (list "soundfile number" 0 initial-save-as-mp3-wav-file-number 250
				 (if (provided? 'snd-gtk)
				     (lambda (w data)
				       (set! save-as-mp3-wav-file-number (gtk_adjustment_get_value (GTK_ADJUSTMENT w))))
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
			   (gtk_adjustment_set_value (GTK_ADJUSTMENT (car sliders)) save-as-ogg-wav-file-number)
			   ;;; (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car sliders)))
			   )
			 (lambda (w c i)
			   (set! save-as-ogg-wav-file-number initial-save-as-ogg-wav-file-number)
			   (XtSetValues (car sliders) (list XmNvalue save-as-ogg-wav-file-number))))))

              (set! sliders
                    (add-sliders 
		     save-as-ogg-dialog
		     (list (list "soundfile number" 0 initial-save-as-ogg-wav-file-number 250
				 (if (provided? 'snd-gtk)
				     (lambda (w data)
				       (set! save-as-ogg-wav-file-number (gtk_adjustment_get_value (GTK_ADJUSTMENT w))))
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

