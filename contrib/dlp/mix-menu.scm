(use-modules (ice-9 format))

(define mix-list '()) ; menu labels are updated to show current default settings

(define mix-menu (add-to-main-menu "Mix/Track" (lambda ()
						   (define (update-label mix)
						     (if (not (null? mix))
							 (begin
							   ((car mix))
							   (update-label (cdr mix)))))
						   (update-label mix-list))))
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

;;; ------ Delete mix
;;;

(define delete-mix-number 0)
(define delete-mix-label "Delete mix")
(define delete-mix-dialog #f)

(define (cp-delete-mix)
 (delete-mix delete-mix-number))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-delete-mix-dialog)
        (if (not (Widget? delete-mix-dialog))
            ;; if delete-mix-dialog doesn't exist, create it
            (let ((initial-delete-mix-number 0)
                  (sliders '()))
              (set! delete-mix-dialog
                    (make-effect-dialog delete-mix-label
                                        (lambda (w context info)
                                          (cp-delete-mix))
                                        (lambda (w context info)
                                          (XtUnmanageChild delete-mix-dialog))
                                        (lambda (w context info)
                                          (help-dialog "Delete mix"
                                                "Deletes specified mix."))
                                        (lambda (w c i)
                                          (set! delete-mix-number initial-delete-mix-number)
                                          (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* delete-mix-number 1)))))))
              (set! sliders
                    (add-sliders delete-mix-dialog
                                 (list (list "mix number" 0 initial-delete-mix-number 250
                                             (lambda (w context info)
                                               (set! delete-mix-number (/ (.value info) 1)))
                                             1))))))
        (activate-dialog delete-mix-dialog))

      (add-to-menu mix-menu "Delete mix" (lambda () (post-delete-mix-dialog))))

    (add-to-menu mix-menu delete-mix-label cp-delete-mix))

(set! mix-list (cons (lambda ()
                           (let ((new-label (format #f "Delete mix (~1,2D)" delete-mix-number)))
                             (change-menu-label mix-menu delete-mix-label new-label)
                             (set! delete-mix-label new-label)))
                         mix-list))

;;; -------- Snap mix to beat 
;;;
;;;

(define snapping #f)
(define snap-label "Snap mix to beat (Off)")
(define no-snap-label "Snap mix to beat (On)")

(define (snap!)
  (set! snapping #t)
  (change-menu-label mix-menu snap-label no-snap-label)
  (snap-mix-to-beat))

(define (unsnap!)
  (set! snapping #f)
  (change-menu-label mix-menu no-snap-label snap-label)
  (reset-hook! mix-position-changed-hook))

(add-to-menu mix-menu snap-label
  (lambda ()
    (if snapping
        (unsnap!)
        (snap!))))

(add-to-menu mix-menu #f #f)

;;; -------- Assign all tracks
;;;
;;;

(define renumber-tracks-number 0)
(define renumber-tracks-label "Assign all tracks")
(define renumber-tracks-dialog #f)

(define (set-all-tracks new-num)
  (tree-apply
    (lambda (n)
      (set! (mix-track n) new-num))
    (mixes)))

(define (cp-renumber-tracks)
 (set-all-tracks renumber-tracks-number))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-renumber-tracks-dialog)
        (if (not (Widget? renumber-tracks-dialog))
            ;; if renumber-tracks-dialog doesn't exist, create it
            (let ((initial-renumber-tracks-number 0)
                  (sliders '()))
              (set! renumber-tracks-dialog
                    (make-effect-dialog renumber-tracks-label
                                        (lambda (w context info)
                                          (cp-renumber-tracks))
                                        (lambda (w context info)
                                          (XtUnmanageChild renumber-tracks-dialog))
                                        (lambda (w context info)
                                          (help-dialog "Assign all tracks"
                                                "Assign all tracks to number specified by the slider."))
                                        (lambda (w c i)
                                          (set! renumber-tracks-number initial-renumber-tracks-number)
                                          (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* renumber-tracks-number 1)))))))
              (set! sliders
                    (add-sliders renumber-tracks-dialog
                                 (list (list "new number" 0 initial-renumber-tracks-number 100
                                             (lambda (w context info)
                                               (set! renumber-tracks-number (/ (.value info) 1)))
                                             1))))))
        (activate-dialog renumber-tracks-dialog))

      (add-to-menu mix-menu "Assign all tracks" (lambda () (post-renumber-tracks-dialog))))

    (add-to-menu mix-menu renumber-tracks-label cp-renumber-tracks))

(set! mix-list (cons (lambda ()
                           (let ((new-label (format #f "Assign all tracks (~1,2D)" renumber-tracks-number)))
                             (change-menu-label mix-menu renumber-tracks-label new-label)
                             (set! renumber-tracks-label new-label)))
                         mix-list))



;;; -------- Delete track
;;;
;;;

(define (delete-track track)
  (as-one-edit
    (lambda ()
      (for-each
        (lambda (a)
          (delete-mix a))
        track))))

(define delete-track-number 0)
(define delete-track-label "Delete track")
(define delete-track-dialog #f)

(define (cp-delete-track)
 (delete-track (track delete-track-number)))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-delete-track-dialog)
        (if (not (Widget? delete-track-dialog))
            ;; if delete-track-dialog doesn't exist, create it
            (let ((initial-delete-track-number 0)
                  (sliders '()))
              (set! delete-track-dialog
                    (make-effect-dialog delete-track-label
                                        (lambda (w context info)
                                          (cp-delete-track))
                                        (lambda (w context info)
                                          (XtUnmanageChild delete-track-dialog))
                                        (lambda (w context info)
                                          (help-dialog "Delete track"
                                                "Deletes the track specified by the slider."))
                                        (lambda (w c i)
                                          (set! delete-track-number initial-delete-track-number)
                                          (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* delete-track-number 1)))))))
              (set! sliders
                    (add-sliders delete-track-dialog
                                 (list (list "track number" 0 initial-delete-track-number 100
                                             (lambda (w context info)
                                               (set! delete-track-number (/ (.value info) 1)))
                                             1))))))
        (activate-dialog delete-track-dialog))

      (add-to-menu mix-menu "Delete track" (lambda () (post-delete-track-dialog))))

    (add-to-menu mix-menu delete-track-label cp-delete-track))

(set! mix-list (cons (lambda ()
                           (let ((new-label (format #f "Delete track (~1,2D)" delete-track-number)))
                             (change-menu-label mix-menu delete-track-label new-label)
                             (set! delete-track-label new-label)))
                         mix-list))



;;; -------- Play track
;;;
;;;

(define play-track-number 0)
(define play-track-label "Play track")
(define play-track-dialog #f)

(define (cp-play-track)
 (play-track play-track-number))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-play-track-dialog)
        (if (not (Widget? play-track-dialog))
            ;; if play-track-dialog doesn't exist, create it
            (let ((initial-play-track-number 0)
                  (sliders '()))
              (set! play-track-dialog
                    (make-effect-dialog play-track-label
                                        (lambda (w context info)
                                          (cp-play-track))
                                        (lambda (w context info)
                                          (XtUnmanageChild play-track-dialog))
                                        (lambda (w context info)
                                          (help-dialog "Play track"
                                                "Plays the track specified by the slider."))
                                        (lambda (w c i)
                                          (set! play-track-number initial-play-track-number)
                                          (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* play-track-number 1)))))))
              (set! sliders
                    (add-sliders play-track-dialog
                                 (list (list "track number" 0 initial-play-track-number 100
                                             (lambda (w context info)
                                               (set! play-track-number (/ (.value info) 1)))
                                             1))))))
        (activate-dialog play-track-dialog))

      (add-to-menu mix-menu "Play track" (lambda () (post-play-track-dialog))))

    (add-to-menu mix-menu play-track-label cp-play-track))

(set! mix-list (cons (lambda ()
                           (let ((new-label (format #f "Play track (~1,2D)" play-track-number)))
                             (change-menu-label mix-menu play-track-label new-label)
                             (set! play-track-label new-label)))
                         mix-list))

;;; -------- Reverse track
;;;
;;;

(define reverse-track-number 0)
(define reverse-track-label "Reverse track")
(define reverse-track-dialog #f)

(define (cp-reverse-track)
 (reverse-track (track reverse-track-number)))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-reverse-track-dialog)
        (if (not (Widget? reverse-track-dialog))
            ;; if reverse-track-dialog doesn't exist, create it
            (let ((initial-reverse-track-number 0)
                  (sliders '()))
              (set! reverse-track-dialog
                    (make-effect-dialog reverse-track-label
                                        (lambda (w context info)
                                          (cp-reverse-track))
                                        (lambda (w context info)
                                          (XtUnmanageChild reverse-track-dialog))
                                        (lambda (w context info)
                                          (help-dialog "Reverse track"
                                                "Reverses the order in which a track's members occur."))
                                        (lambda (w c i)
                                          (set! reverse-track-number initial-reverse-track-number)
                                          (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* reverse-track-number 1)))))))
              (set! sliders
                    (add-sliders reverse-track-dialog
                                 (list (list "track number" 0 initial-reverse-track-number 100
                                             (lambda (w context info)
                                               (set! reverse-track-number (/ (.value info) 1)))
                                             1))))))
        (activate-dialog reverse-track-dialog))

      (add-to-menu mix-menu "Reverse track" (lambda () (post-reverse-track-dialog))))

    (add-to-menu mix-menu reverse-track-label cp-reverse-track))

(set! mix-list (cons (lambda ()
                           (let ((new-label (format #f "Reverse track (~1,2D)" reverse-track-number)))
                             (change-menu-label mix-menu reverse-track-label new-label)
                             (set! reverse-track-label new-label)))
                         mix-list))


;;; -------- Set track amplitude
;;;

(define set-track-amp-tracknum 0)
(define set-track-amp-scaler 1.0)
(define set-track-amp-label "Set track amplitude")
(define set-track-amp-dialog #f)

(define (cp-set-track-amp)
  (set-track-amp (track set-track-amp-tracknum) set-track-amp-scaler))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-set-track-amp-dialog)
        (if (not (Widget? set-track-amp-dialog))
            ;; if set-track-amp-dialog doesn't exist, create it
            (let ((initial-set-track-amp-tracknum 0)
                  (initial-set-track-amp-scaler 1.0)
                  (sliders '()))
              (set! set-track-amp-dialog
                    (make-effect-dialog set-track-amp-label
                                        (lambda (w context info)
                                          (cp-set-track-amp))
                                        (lambda (w context info)
                                          (XtUnmanageChild set-track-amp-dialog))
                                        (lambda (w context info)
                                          (help-dialog "Set track amplitude"
                                                       "Move the sliders to set the track number and amp scaling."))
                                        (lambda (w c i)
                                          (set! set-track-amp-tracknum initial-set-track-amp-tracknum)
                                          (XtSetValues (car sliders) (list XmNvalue (inexact->exact (* set-track-amp-tracknum 1))))
                                          (set! set-track-amp-scaler initial-set-track-amp-scaler)
                                          (XtSetValues (cadr sliders) (list XmNvalue (inexact->exact (* set-track-amp-scaler 100)))))))
              (set! sliders
                   (add-sliders set-track-amp-dialog
                                 (list (list "track number" 0 initial-set-track-amp-tracknum 100
                                             (lambda (w context info)
                                               (set! set-track-amp-tracknum (/ (.value info) 1)))
                                             1)
                                       (list "amp scaler" 0.01 initial-set-track-amp-scaler 2.0
                                             (lambda (w context info)
                                               (set! set-track-amp-scaler (/ (.value info) 100)))
                                             100))))))
        (activate-dialog set-track-amp-dialog))

      (add-to-menu mix-menu "Set track amplitude" (lambda () (post-set-track-amp-dialog))))

    (add-to-menu mix-menu set-track-amp-label cp-set-track-amp))

(set! mix-list (cons (lambda ()
                           (let ((new-label (format #f "Set track amplitude (~1,2D ~1,2F)" set-track-amp-tracknum set-track-amp-scaler)))
                             (change-menu-label mix-menu set-track-amp-label new-label)
                             (set! set-track-amp-label new-label)))
                         mix-list))

;;; -------- Set track speed
;;;

(define set-track-speed-tracknum 0)
(define set-track-speed-scaler 1.0)
(define set-track-speed-label "Set track speed")
(define set-track-speed-dialog #f)

(define (cp-set-track-speed)
  (set-track-speed (track set-track-speed-tracknum) set-track-speed-scaler))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-set-track-speed-dialog)
        (if (not (Widget? set-track-speed-dialog))
            ;; if set-track-speed-dialog doesn't exist, create it
            (let ((initial-set-track-speed-tracknum 0)
                  (initial-set-track-speed-scaler 1.0)
                  (sliders '()))
              (set! set-track-speed-dialog
                    (make-effect-dialog set-track-speed-label
                                        (lambda (w context info)
                                          (cp-set-track-speed))
                                        (lambda (w context info)
                                          (XtUnmanageChild set-track-speed-dialog))
                                        (lambda (w context info)
                                          (help-dialog "Set track speed"
                                                       "Move the sliders to set the track number and rate scaling."))
                                        (lambda (w c i)
                                          (set! set-track-speed-tracknum initial-set-track-speed-tracknum)
                                          (XtSetValues (car sliders) (list XmNvalue (inexact->exact (* set-track-speed-tracknum 1))))
                                          (set! set-track-speed-scaler initial-set-track-speed-scaler)
                                          (XtSetValues (cadr sliders) (list XmNvalue (inexact->exact (* set-track-speed-scaler 100)))))))
              (set! sliders
                   (add-sliders set-track-speed-dialog
                                 (list (list "track number" 0 initial-set-track-speed-tracknum 100
                                             (lambda (w context info)
                                               (set! set-track-speed-tracknum (/ (.value info) 1)))
                                             1)
                                       (list "rate scaler" 0.01 initial-set-track-speed-scaler 2.0
                                             (lambda (w context info)
                                               (set! set-track-speed-scaler (/ (.value info) 100)))
                                             100))))))
        (activate-dialog set-track-speed-dialog))

      (add-to-menu mix-menu "Set track speed" (lambda () (post-set-track-speed-dialog))))

    (add-to-menu mix-menu set-track-speed-label cp-set-track-speed))

(set! mix-list (cons (lambda ()
                           (let ((new-label (format #f "Set track speed (~1,2D ~1,2F)" set-track-speed-tracknum set-track-speed-scaler)))
                             (change-menu-label mix-menu set-track-speed-label new-label)
                             (set! set-track-speed-label new-label)))
                         mix-list))

;;; -------- Set track tempo
;;;

(define set-track-tempo-tracknum 0)
(define set-track-tempo-value 1.0)
(define set-track-tempo-label "Set track tempo")
(define set-track-tempo-dialog #f)

(define (cp-set-track-tempo)
  (set-track-tempo (track set-track-tempo-tracknum) set-track-tempo-value))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-set-track-tempo-dialog)
        (if (not (Widget? set-track-tempo-dialog))
            ;; if set-track-tempo-dialog doesn't exist, create it
            (let ((initial-set-track-tempo-tracknum 0)
                  (initial-set-track-tempo-value 1.0)
                  (sliders '()))
              (set! set-track-tempo-dialog
                    (make-effect-dialog set-track-tempo-label
                                        (lambda (w context info)
                                          (cp-set-track-tempo))
                                        (lambda (w context info) (XtUnmanageChild set-track-tempo-dialog))
                                        (lambda (w context info)
                                          (help-dialog "Set track tempo"
                                                       "Affects the time between the successive mix begin points (tempo > 1.0 makes the mixes happen more quickly)."))
                                        (lambda (w c i)
                                          (set! set-track-tempo-tracknum initial-set-track-tempo-tracknum)
                                          (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* set-track-tempo-tracknum 1))))
                                          (set! set-track-tempo-value initial-set-track-tempo-value)
                                          (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* set-track-tempo-value 100)))))))
              (set! sliders
                    (add-sliders set-track-tempo-dialog
                                 (list (list "track number" 0 initial-set-track-tempo-tracknum 100
                                             (lambda (w context info)
                                               (set! set-track-tempo-tracknum (/ (.value info) 1)))
                                             1)
				       (list "tempo" 0 initial-set-track-tempo-value 100
                                             (lambda (w context info)
                                               (set! set-track-tempo-value (/ (.value info) 100)))
                                             100))))))

        (activate-dialog set-track-tempo-dialog))
      (add-to-menu mix-menu "Set track tempo" (lambda () (post-set-track-tempo-dialog)))))

(set! mix-list (cons (lambda ()
                           (let ((new-label (format #f "Set track tempo (~1,2D ~1,2F)" set-track-tempo-tracknum set-track-tempo-value)))
                             (change-menu-label mix-menu set-track-tempo-label new-label)
                             (set! set-track-tempo-label new-label)))
                         mix-list))



;;; -------- Transpose track
;;;

(define transpose-track-number 0)
(define transpose-track-semitones 0)
(define transpose-track-label "Transpose track")
(define transpose-track-dialog #f)

(define (cp-transpose-track)
  (transpose-track (track transpose-track-number) transpose-track-semitones))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-transpose-track-dialog)
        (if (not (Widget? transpose-track-dialog))
            ;; if transpose-track-dialog doesn't exist, create it
            (let ((initial-transpose-track-number 0)
                  (initial-transpose-track-semitones 0)
                  (sliders '()))
              (set! transpose-track-dialog
                    (make-effect-dialog transpose-track-label
                                        (lambda (w context info)
                                          (cp-transpose-track))
                                        (lambda (w context info) (XtUnmanageChild transpose-track-dialog))
                                        (lambda (w context info)
                                          (help-dialog "Transpose track"
                                                       "A simple track transposition utility."))
                                        (lambda (w c i)
                                          (set! transpose-track-number initial-transpose-track-number)
                                          (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* transpose-track-number 1))))
                                          (set! transpose-track-semitones initial-transpose-track-semitones)
                                          (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact transpose-track-semitones 1))))))
              (set! sliders
                    (add-sliders transpose-track-dialog
                                 (list (list "transpose-track number" 0 initial-transpose-track-number 100
                                             (lambda (w context info)
                                               (set! transpose-track-number (/ (.value info) 1)))
                                             1)
                                       (list "transpose-track semitones" -100 initial-transpose-track-semitones 100
                                             (lambda (w context info)
                                               (set! transpose-track-semitones (.value info)))
                                             1))))))

        (activate-dialog transpose-track-dialog))
      (add-to-menu mix-menu "Transpose track" (lambda () (post-transpose-track-dialog)))))

(set! mix-list (cons (lambda ()
                           (let ((new-label (format #f "Transpose track (~1,2D ~1,2D)" transpose-track-number transpose-track-semitones)))
                             (change-menu-label mix-menu transpose-track-label new-label)
                             (set! transpose-track-label new-label)))
                         mix-list))

(add-to-menu mix-menu #f #f)

;;; Save track
;;;

(define save-track-number 0)
(define save-track-label "Save track")
(define save-track-dialog #f)

(define (cp-save-track)
 (save-track (track save-track-number) (format #f "track-~A.snd" save-track-number)))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-save-track-dialog)
        (if (not (Widget? save-track-dialog))
            ;; if save-track-dialog doesn't exist, create it
            (let ((initial-save-track-number 0)
                  (sliders '()))
              (set! save-track-dialog
                    (make-effect-dialog save-track-label
                                        (lambda (w context info)
                                          (cp-save-track))
                                        (lambda (w context info)
                                          (XtUnmanageChild save-track-dialog))
                                        (lambda (w context info)
                                          (help-dialog "Save track"
                                                "Save track to track-N.snd soundfile."))
                                        (lambda (w c i)
                                          (set! save-track-number initial-save-track-number)
                                          (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* save-track-number 1)))))))
              (set! sliders
                    (add-sliders save-track-dialog
                                 (list (list "track number" 0 initial-save-track-number 100
                                             (lambda (w context info)
                                               (set! save-track-number (/ (.value info) 1)))
                                             1))))))
        (activate-dialog save-track-dialog))

      (add-to-menu mix-menu "Save track" (lambda () (post-save-track-dialog))))

    (add-to-menu mix-menu save-track-label cp-save-track))

(set! mix-list (cons (lambda ()
                           (let ((new-label (format #f "Save track (~1,2D)" save-track-number)))
                             (change-menu-label mix-menu save-track-label new-label)
                             (set! save-track-label new-label)))
                         mix-list))

(add-to-menu mix-menu #f #f)

(add-to-menu mix-menu "Colorize tracks" (lambda () (load-from-path "track-colors.scm")))

(define (delete-all-mixes)
  (as-one-edit
    (lambda ()
      (tree-apply
        (lambda (id)
          (delete-mix id))
        (mixes)))))

(define (delete-all-tracks)
  (delete-all-mixes))

(add-to-menu mix-menu "Delete all mixes & tracks" (lambda () (delete-all-tracks)))

