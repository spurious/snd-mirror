(use-modules (ice-9 format))

(define special-list '()) ; menu labels are updated to show current default settings

(define special-menu (add-to-main-menu "Special" (lambda ()
						   (define (update-label special)
						     (if (not (null? special))
							 (begin
							   ((car special))
							   (update-label (cdr special)))))
						   (update-label special-list))))
(define (all-chans)
  (let ((sndlist '())
	(chnlist '()))
    (for-each (lambda (snd)
		(do ((i (1- (channels snd)) (1- i)))
		    ((< i 0))
		  (set! sndlist (cons snd sndlist))
		  (set! chnlist (cons i chnlist))))
	      (sounds))
    (list sndlist chnlist)))

(define map-chan-with-sync
  (lambda (func origin)
    (let ((snc (sync)))
      (if (> snc 0)
	  (apply map
		 (lambda (snd chn)
		   (if (= (sync snd) snc)
		       (map-chan (func) #f #f origin snd chn)))
		 (all-chans))
	  (map-chan (func) #f #f origin)))))


(add-to-menu special-menu "Insert file"
  (lambda ()
    (select-file
      (lambda (filename)
        (insert-sound filename))
      "Insert file" "." "*" "file will be inserted at cursor")))

(define (append-sound name)
  ;; appends sound file
  (insert-sound name (frames)))

(add-to-menu special-menu "Append file" (lambda () (append-sound name)))

(add-to-menu special-menu #f #f)

;(add-to-menu special-menu #f #f)

;;; -------- Record input channel
;;;

(define record-input-channel 4)
(define record-input-label "Record input channel")
(define record-input-dialog #f)
(define record-input-default-widget #f)

(define radio-buttons-yes #t) ; radio-buttons or combo-box choice

(define (cp-record-input)
  (set! (recorder-in-device) record-input-channel)) 

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-record-input-dialog)
        (if (not (|Widget? record-input-dialog))
            ;; if record-input-dialog doesn't exist, create it
            (let ((initial-record-input-channel 4))
              (set! record-input-dialog
                    (make-effect-dialog record-input-label
                                        (lambda (w context info) (cp-record-input))
                                        (lambda (w context info) (|XtUnmanageChild record-input-dialog))
                                        (lambda (w context info)
                                          (help-dialog "Record input channel Help"
"Select the appropriate radio button to set the record input channel. Note that if you are using the ALSA drivers you may need to set the record channel from your system mixer (e.g. aumix) as well."))
                                        (lambda (w c i)
                                          (set! record-input-channel initial-record-input-channel)
					  (if radio-buttons-yes
					      (|XtSetValues record-input-default-widget (list |XmNselectedPosition 1))
					      (|XmToggleButtonSetState record-input-default-widget #t #t)))))

	      (if radio-buttons-yes
		  (let* ((s1 (|XmStringCreateLocalized "Record input channel"))
			 (frame (|XtCreateManagedWidget "frame" |xmFrameWidgetClass record-input-dialog
				   (list |XmNborderWidth 1
					 |XmNshadowType |XmSHADOW_ETCHED_IN
					 |XmNpositionIndex 2)))
			 (frm (|XtCreateManagedWidget "frm" |xmFormWidgetClass frame
				(list |XmNleftAttachment      |XmATTACH_FORM
				      |XmNrightAttachment     |XmATTACH_FORM
				      |XmNtopAttachment       |XmATTACH_FORM
				      |XmNbottomAttachment    |XmATTACH_FORM
				      |XmNbackground          (|Pixel (snd-pixel (basic-color))))))
			 (rc (|XtCreateManagedWidget "rc" |xmRowColumnWidgetClass frm
				   (list |XmNorientation         |XmHORIZONTAL
					 |XmNradioBehavior #t
					 |XmNradioAlwaysOne #t
					 |XmNentryClass          |xmToggleButtonWidgetClass
					 |XmNisHomogeneous #t
					 |XmNleftAttachment      |XmATTACH_FORM
					 |XmNrightAttachment     |XmATTACH_FORM
					 |XmNtopAttachment       |XmATTACH_FORM
					 |XmNbottomAttachment    |XmATTACH_NONE
					 |XmNbackground          (|Pixel (snd-pixel (basic-color))))))
			 (lab (|XtCreateManagedWidget "Record input channel" |xmLabelWidgetClass frm
				   (list |XmNleftAttachment      |XmATTACH_FORM
					 |XmNrightAttachment     |XmATTACH_FORM
					 |XmNtopAttachment       |XmATTACH_WIDGET
					 |XmNtopWidget           rc
					 |XmNbottomAttachment    |XmATTACH_FORM
					 |XmNlabelString         s1
					 |XmNalignment           |XmALIGNMENT_BEGINNING
					 |XmNbackground          (|Pixel (snd-pixel (basic-color)))))))
		    (for-each 
		     (lambda (size)
		       (let ((button (|XtCreateManagedWidget (format #f "~D" size) |xmToggleButtonWidgetClass rc
				        (list |XmNbackground           (|Pixel (snd-pixel (basic-color)))
					      |XmNvalueChangedCallback (list (lambda (w c i) (if (|set i) (set! record-input-channel c))) size)
					      |XmNset                  (= size record-input-channel)))))
			 (if (= size record-input-channel)
			     (set! record-input-default-widget button))))
		     (list 4 5 6 19))
		    (|XmStringFree s1)))))
        (activate-dialog record-input-dialog))

      (add-to-menu special-menu "Record input channel" (lambda () (post-record-input-dialog))))

    (add-to-menu special-menu record-input-label cp-record-input))

(set! special-list (cons (lambda ()
                           (let ((new-label (format #f "Record input channel (~1,2D)"
                                                record-input-channel)))
                             (change-menu-label special-menu record-input-label new-label)
                             (set! record-input-label new-label)))
                         special-list))

(add-to-menu special-menu #f #f)



;;; ------ FFT edit
;;;

;(define fft-edit-low-frequency 100)
;(define fft-edit-high-frequency 1000)
;(define fft-edit-label "FFT edit")
;(define fft-edit-dialog #f)
;
;(define (cp-fft-edit)
;  (fft-edit fft-edit-low-frequency fft-edit-high-frequency))
;
;(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
;    (begin
;
;      (define (post-fft-edit-dialog)
;        (if (not (|Widget? fft-edit-dialog))
;            ;; if fft-edit-dialog doesn't exist, create it
;            (let ((initial-fft-edit-low-frequency 100)
;                  (initial-fft-edit-high-frequency 1000)
;                  (sliders '()))
;              (set! fft-edit-dialog
;                    (make-effect-dialog fft-edit-label
;                                        (lambda (w context info)
;                                          (cp-fft-edit))
;                                        (lambda (w context info)
;                                          (|XtUnmanageChild fft-edit-dialog))
;                                        (lambda (w context info)
;                                          (help-dialog "FFT edit"
;                                                       "A simple example of FFT-based editing. It takes an FFT of the entire sound, removes all energy below the low frequency\n\ and above the high frequency, then computes the inverse FFT. "))
;                                        (lambda (w c i)
;                                          (set! fft-edit-low-frequency initial-fft-edit-low-frequency)
;                                          (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* fft-edit-low-frequency 1))))
;                                          (set! fft-edit-high-frequency initial-fft-edit-high-frequency)
;                                          (|XtSetValues (cadr sliders) (list |XmNvalue (inexact->exact (* fft-edit-high-frequency 1)))))))
;              (set! sliders
;                   (add-sliders fft-edit-dialog
;                                 (list (list "low frequency" 20 initial-fft-edit-low-frequency 22050
;                                             (lambda (w context info)
;                                               (set! fft-edit-low-frequency (/ (|value info) 1)))
;                                             1)
;                                       (list "high frequency" 20 initial-fft-edit-high-frequency 22050
;                                             (lambda (w context info)
;                                               (set! fft-edit-high-frequency (/ (|value info) 1)))
;                                             1))))))
;        (activate-dialog fft-edit-dialog))
;
;      (add-to-menu special-menu "FFT edit" (lambda () (post-fft-edit-dialog))))
;
;    (add-to-menu special-menu fft-edit-label cp-fft-edit))
;
;(set! special-list (cons (lambda ()
;                           (let ((new-label (format #f "FFT edit (~1,2D ~1,2D)" fft-edit-low-frequency fft-edit-high-frequency)))
;                             (change-menu-label special-menu fft-edit-label new-label)
;                             (set! fft-edit-label new-label)))
;                         special-list))
;
;
;
;;;; ------ FFT squelch
;;;;
;
;(define fft-squelch-amount 0.0)
;(define fft-squelch-label "FFT squelch")
;(define fft-squelch-dialog #f)
;
;(define (cp-fft-squelch)
; (fft-squelch fft-squelch-amount))
;
;(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
;    (begin
;
;      (define (post-fft-squelch-dialog)
;        (if (not (|Widget? fft-squelch-dialog))
;            ;; if fft-squelch-dialog doesn't exist, create it
;            (let ((initial-fft-squelch-amount 0.0)
;                  (sliders '()))
;              (set! fft-squelch-dialog
;                    (make-effect-dialog fft-squelch-label
;                                        (lambda (w context info)
;                                          (cp-fft-squelch))
;                                        (lambda (w context info)
;                                          (|XtUnmanageChild fft-squelch-dialog))
;                                        (lambda (w context info)
;                                          (help-dialog "FFT squelch"
;                                                "Removes all energy below the squelch amount.\n\ This is sometimes useful for noise-reduction."))
;                                        (lambda (w c i)
;                                          (set! fft-squelch-amount initial-fft-squelch-amount)
;                                          (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* fft-squelch-amount 100)))))))
;              (set! sliders
;                    (add-sliders fft-squelch-dialog
;                                 (list (list "squelch amount" 0.0 initial-fft-squelch-amount 1.0
;                                             (lambda (w context info)
;                                               (set! fft-squelch-amount (/ (|value info) 100)))
;                                             100))))))
;        (activate-dialog fft-squelch-dialog))
;
;      (add-to-menu special-menu "FFT squelch" (lambda () (post-fft-squelch-dialog))))
;
;    (add-to-menu special-menu fft-squelch-label cp-fft-squelch))
;
;(set! special-list (cons (lambda ()
;                           (let ((new-label (format #f "FFT squelch (~1,2F)" fft-squelch-amount)))
;                             (change-menu-label special-menu fft-squelch-label new-label)
;                             (set! fft-squelch-label new-label)))
;                         special-list))
;

;(add-to-menu special-menu #f #f)

(define env-file #f)
(define yes-env-label "Envelope new file (Off)")
(define no-env-label "Envelope new file (On)")

(define (yesenv!)
  (set! env-file #t)
  (change-menu-label special-menu yes-env-label no-env-label)
  (start-enveloping))

(define (noenv!)
  (set! env-file #f)
  (change-menu-label special-menu no-env-label yes-env-label)
  (stop-enveloping))

(add-to-menu special-menu yes-env-label
  (lambda ()
    (if env-file
        (noenv!)
        (yesenv!))))

;(add-to-menu special-menu "Start enveloping" (lambda () (start-enveloping)))
;(add-to-menu special-menu "Stop enveloping" (lambda () (stop-enveloping)))


;;; -------- Play panned
;;;

(define play-panned-file 1)
(define play-panned-label "Play panned")
(define play-panned-dialog #f)

(define (cp-play-panned)
  (play-panned play-panned-file))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-play-panned-dialog)
        (if (not (|Widget? play-panned-dialog))
            ;; if play-panned-dialog doesn't exist, create it
            (let ((initial-play-panned-file 1)
                  (sliders '()))
              (set! play-panned-dialog
                    (make-effect-dialog play-panned-label
                                        (lambda (w context info) (cp-play-panned))
                                        (lambda (w context info) (|XtUnmanageChild play-panned-dialog))
                                        (lambda (w context info)
                                          (help-dialog "Play panned"
                                                       "Move the slider to select the file to play with panning envelope."))
                                        (lambda (w c i)
                                          (set! play-panned-file initial-play-panned-file)
                                          (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* play-panned-file 1)))))))
              (set! sliders
                    (add-sliders play-panned-dialog
                                 (list (list "soundfile number" 0 initial-play-panned-file 25
                                             (lambda (w context info)
                                               (set! play-panned-file (/ (|value info) 1)))
                                             1))))))
        (activate-dialog play-panned-dialog))

      (add-to-menu special-menu "Play panned" (lambda () (post-play-panned-dialog))))

    (add-to-menu special-menu play-panned-label cp-play-panned))

(set! special-list (cons (lambda ()
                           (let ((new-label (format #f "Play panned (~1,2D)"  play-panned-file)))
                             (change-menu-label special-menu play-panned-label new-label)
                             (set! play-panned-label new-label)))
                         special-list))


(add-to-menu special-menu #f #f)

;;; -------- Save as MP3
;;;

(define save-as-mp3-wav-file-number 0)
(define save-as-mp3-label "Save as MP3")
(define save-as-mp3-dialog #f)

(define (cp-save-as-mp3)
  (save-sound-as "tmp.wav" save-as-mp3-wav-file-number mus-riff)
  (system (format #f "bladeenc tmp.wav tmp-~D.mp3" save-as-mp3-wav-file-number)))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-save-as-mp3-dialog)
        (if (not (|Widget? save-as-mp3-dialog))
            ;; if save-as-mp3-dialog doesn't exist, create it
            (let ((initial-save-as-mp3-wav-file-number 0)
                  (sliders '()))
              (set! save-as-mp3-dialog
                    (make-effect-dialog save-as-mp3-label
                                        (lambda (w context info) (cp-save-as-mp3))
                                        (lambda (w context info) (|XtUnmanageChild save-as-mp3-dialog))
                                        (lambda (w context info)
                                          (help-dialog "Save as MP3"
                                                       "Move the slider to select the file to save as an MP3.\n\ The new MP3 will be named tmp-N.mp3 by default.\n\ Bladeenc is currently the only supported encoder. Please see the Web page at bladeenc.mp3.no for details regarding Bladeenc."))
                                        (lambda (w c i)
                                          (set! save-as-mp3-wav-file-number initial-save-as-mp3-wav-file-number)
                                          (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* save-as-mp3-wav-file-number 1)))))))
              (set! sliders
                    (add-sliders save-as-mp3-dialog
                                 (list (list "soundfile number" 0 initial-save-as-mp3-wav-file-number 250
                                             (lambda (w context info)
                                               (set! save-as-mp3-wav-file-number (/ (|value info) 1)))
                                             1))))))
        (activate-dialog save-as-mp3-dialog))

      (add-to-menu special-menu "Save as MP3" (lambda () (post-save-as-mp3-dialog))))

    (add-to-menu special-menu save-as-mp3-label cp-save-as-mp3))

(set! special-list (cons (lambda ()
                           (let ((new-label (format #f "Save as MP3 (~1,2D)"  save-as-mp3-wav-file-number)))
                             (change-menu-label special-menu save-as-mp3-label new-label)
                             (set! save-as-mp3-label new-label)))
                         special-list))



;;; -------- Save as Ogg File
;;;

(define save-as-ogg-wav-file-number 0)
(define save-as-ogg-label "Save as Ogg file")
(define save-as-ogg-dialog #f)

(define (cp-save-as-ogg)
  (save-sound-as "tmp.wav" save-as-ogg-wav-file-number mus-riff)
  (system (format #f "oggenc tmp.wav -o tmp-~D.ogg" save-as-ogg-wav-file-number)))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-save-as-ogg-dialog)
        (if (not (|Widget? save-as-ogg-dialog))
            ;; if save-as-ogg-dialog doesn't exist, create it
            (let ((initial-save-as-ogg-wav-file-number 0)
                  (sliders '()))
              (set! save-as-ogg-dialog
                    (make-effect-dialog save-as-ogg-label
                                        (lambda (w context info) (cp-save-as-ogg))
                                        (lambda (w context info) (|XtUnmanageChild save-as-ogg-dialog))
                                        (lambda (w context info)
                                          (help-dialog "Save as Ogg file"
                                                       "Move the slider to select the file to save as an Ogg file. The new file will be named tmp-N.ogg by default.\n\ Oggenc is currently the only supported Ogg encoder. Please see the Web page at www.xiphophorus.org for details regarding the Ogg/Vorbis project."))
                                        (lambda (w c i)
                                          (set! save-as-ogg-wav-file-number initial-save-as-ogg-wav-file-number)
                                          (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* save-as-ogg-wav-file-number 1)))))))
              (set! sliders
                    (add-sliders save-as-ogg-dialog
                                 (list (list "soundfile number" 0 initial-save-as-ogg-wav-file-number 250
                                             (lambda (w context info)
                                               (set! save-as-ogg-wav-file-number (/ (|value info) 1)))
                                             1))))))
        (activate-dialog save-as-ogg-dialog))

      (add-to-menu special-menu "Save as Ogg file" (lambda () (post-save-as-ogg-dialog))))

    (add-to-menu special-menu save-as-ogg-label cp-save-as-ogg))

(set! special-list (cons (lambda ()
                           (let ((new-label (format #f "Save as Ogg file (~1,2D)"  save-as-ogg-wav-file-number)))
                             (change-menu-label special-menu save-as-ogg-label new-label)
                             (set! save-as-ogg-label new-label)))
                         special-list))


