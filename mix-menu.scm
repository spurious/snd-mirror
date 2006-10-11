(use-modules (ice-9 format))
(provide 'snd-mix-menu.scm)

(if (provided? 'xm)
    (if (not (provided? 'snd-effects-utils.scm))
	(load-from-path "effects-utils.scm"))) ; make-effect-dialog

(if (provided? 'xg)
    (if (not (provided? 'snd-gtk-effects-utils.scm))
	(load-from-path "gtk-effects-utils.scm")))

(if (not (provided? 'snd-mix.scm)) (load-from-path "mix.scm"))

(define mix-list '()) ; menu labels are updated to show current default settings

(define mix-menu (add-to-main-menu "Mix/Track" (lambda ()
						 (update-label mix-list))))


;;; ------ Delete mix

(define delete-mix-number 0)
(define delete-mix-label "Delete mix")
(define delete-mix-dialog #f)
(define delete-mix-menu-label #f)

(define (cp-delete-mix)
  (catch 'no-such-mix
	 (lambda ()
	   (delete-mix delete-mix-number))
	 (lambda args (snd-print ";no such mix"))))

(if (or (provided? 'xm) 
	(provided? 'xg))
    (begin
      
      (define (post-delete-mix-dialog)
        (if (not delete-mix-dialog)

            (let ((initial-delete-mix-number 0)
                  (sliders '()))

              (set! delete-mix-dialog
                    (make-effect-dialog 
		     delete-mix-label

		     (if (provided? 'snd-gtk)
			 (lambda (w context) (cp-delete-mix))
			 (lambda (w context info) (cp-delete-mix)))

		     (if (provided? 'snd-gtk)
			 (lambda (w context) (help-dialog "Delete mix" "Deletes the specified mix."))
			 (lambda (w context info) (help-dialog "Delete mix" "Deletes the specified mix.")))

		     (if (provided? 'snd-gtk)
			 (lambda (w data)
			   (set! delete-mix-number initial-delete-mix-number)
			   (set! (.value (GTK_ADJUSTMENT (car sliders))) delete-mix-number)
			   (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car sliders))))
			 (lambda (w c i)
			   (set! delete-mix-number initial-delete-mix-number)
			   (XtSetValues (car sliders) (list XmNvalue delete-mix-number))))))

              (set! sliders
                    (add-sliders 
		     delete-mix-dialog
		     (list (list "mix number" 0 initial-delete-mix-number 250
				 (if (provided? 'snd-gtk)
				     (lambda (w data)
				       (set! delete-mix-number (.value (GTK_ADJUSTMENT w))))
				     (lambda (w context info)
				       (set! delete-mix-number (.value info))))
				 1))))))
        (activate-dialog delete-mix-dialog))
      
      (set! delete-mix-menu-label (add-to-menu mix-menu "Delete mix" (lambda () (post-delete-mix-dialog)))))
    
    (set! delete-mix-menu-label (add-to-menu mix-menu delete-mix-label cp-delete-mix)))

(set! mix-list (cons (lambda ()
		       (let ((new-label (format #f "Delete mix (~D)" delete-mix-number)))
			 (if delete-mix-menu-label (change-label delete-mix-menu-label new-label))
			 (set! delete-mix-label new-label)))
		     mix-list))


;;; -------- Snap mix to beat 

(define snapping #f)
(define snap-label "Snap mix to beat (Off)")
(define no-snap-label "Snap mix to beat (On)")
(define snap-menu-label #f)

(define (snap!)
  (set! snapping #t)
  (if snap-menu-label (change-label snap-menu-label no-snap-label))
  (snap-mix-to-beat))

(define (unsnap!)
  (set! snapping #f)
  (if snap-menu-label (change-label snap-menu-label snap-label))
  (reset-hook! mix-release-hook))

(set! snap-menu-label
      (add-to-menu mix-menu snap-label
		   (lambda ()
		     (if snapping
			 (unsnap!)
			 (snap!)))))

(add-to-menu mix-menu #f #f)


(define (ensure-track new-num)
  ;; make sure the given track exists
  (if (and (not (= new-num 0))
	   (not (track? new-num)))
      (call-with-current-continuation
       (lambda (break)
	 (do ((i 0 (1+ i)))
	     ((= i new-num))
	   (let ((id (make-track)))
	     (if (= id new-num) (break))))))))


;;; -------- Assign all tracks

(define renumber-tracks-number 0)
(define renumber-tracks-label "Assign all tracks")
(define renumber-tracks-dialog #f)
(define renumber-tracks-menu-label #f)

(define (set-all-tracks new-num)
  (ensure-track new-num)
  (lambda ()
    (tree-for-each
     (lambda (n)
       (set! (mix-track n) new-num))
     (mixes))))

(define (cp-renumber-tracks)
  (set-all-tracks renumber-tracks-number))

(if (or (provided? 'xm) 
	(provided? 'xg))
    (begin
      
      (define (post-renumber-tracks-dialog)
        (if (not renumber-tracks-dialog)
            (let ((initial-renumber-tracks-number 0)
                  (sliders '()))

              (set! renumber-tracks-dialog
                    (make-effect-dialog 
		     renumber-tracks-label

		     (if (provided? 'snd-gtk)
			 (lambda (w context) (cp-renumber-tracks))
			 (lambda (w context info) (cp-renumber-tracks)))

		     (if (provided? 'snd-gtk)
			 (lambda (w context)
			   (help-dialog "Assign all tracks"
					"Assign all tracks to number specified by the slider.\
The track number 0 is the 'untrack' so to speak."))
			 (lambda (w context info)
			   (help-dialog "Assign all tracks"
					"Assign all tracks to number specified by the slider.\
The track number 0 is the 'untrack' so to speak.")))

		     (if (provided? 'snd-gtk)
			 (lambda (w data)
			   (set! renumber-tracks-number initial-renumber-tracks-number)
			   (set! (.value (GTK_ADJUSTMENT (car sliders))) renumber-tracks-number)
			   (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car sliders))))
			 (lambda (w c i)
			   (set! renumber-tracks-number initial-renumber-tracks-number)
			   (XtSetValues (car sliders) (list XmNvalue renumber-tracks-number))))))

              (set! sliders
                    (add-sliders 
		     renumber-tracks-dialog
		     (list (list "new number" 0 initial-renumber-tracks-number 100
				 (if (provided? 'snd-gtk)
				     (lambda (w data)
				       (set! renumber-tracks-number (.value (GTK_ADJUSTMENT w))))
				     (lambda (w context info)
				       (set! renumber-tracks-number (.value info))))
				 1))))))
        (activate-dialog renumber-tracks-dialog))
      
      (set! renumber-tracks-menu-label (add-to-menu mix-menu "Assign all tracks" (lambda () (post-renumber-tracks-dialog)))))
    
    (set! renumber-tracks-menu-label (add-to-menu mix-menu renumber-tracks-label cp-renumber-tracks)))

(set! mix-list (cons (lambda ()
		       (let ((new-label (format #f "Assign all tracks (~D)" renumber-tracks-number)))
			 (if renumber-tracks-menu-label (change-label renumber-tracks-menu-label new-label))
			 (set! renumber-tracks-label new-label)))
		     mix-list))



;;; -------- Delete track

(define delete-track-number 1)
(define delete-track-label "Delete track")
(define delete-track-dialog #f)
(define delete-track-menu-label #f)

(define (cp-delete-track)
  (catch 'no-such-track
	 (lambda ()
	   (delete-track delete-track-number))
	 (lambda args 
	   (snd-print ";no such track"))))

(if (or (provided? 'xm) 
	(provided? 'xg))
    (begin
      
      (define (post-delete-track-dialog)
        (if (not delete-track-dialog)
            (let ((initial-delete-track-number 1)
                  (sliders '()))

              (set! delete-track-dialog
                    (make-effect-dialog 
		     delete-track-label

		     (if (provided? 'snd-gtk)
			 (lambda (w context) (cp-delete-track))
			 (lambda (w context info) (cp-delete-track)))

		     (if (provided? 'snd-gtk)
			 (lambda (w context)
			   (help-dialog "Delete track" "Deletes the track specified by the slider."))
			 (lambda (w context info)
			   (help-dialog "Delete track" "Deletes the track specified by the slider.")))

		     (if (provided? 'snd-gtk)
			 (lambda (w data)
			   (set! delete-track-number initial-delete-track-number)
			   (set! (.value (GTK_ADJUSTMENT (car sliders))) delete-track-number)
			   (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car sliders))))
			 (lambda (w c i)
			   (set! delete-track-number initial-delete-track-number)
			   (XtSetValues (car sliders) (list XmNvalue delete-track-number))))))

              (set! sliders
                    (add-sliders 
		     delete-track-dialog
		     (list (list "track number" 0 initial-delete-track-number 100
				 (if (provided? 'snd-gtk)
				     (lambda (w data)
				       (set! delete-track-number (.value (GTK_ADJUSTMENT w))))
				     (lambda (w context info)
				       (set! delete-track-number (.value info))))
				 1))))))
        (activate-dialog delete-track-dialog))
      
      (set! delete-track-menu-label (add-to-menu mix-menu "Delete track" (lambda () (post-delete-track-dialog)))))
    
    (set! delete-track-menu-label (add-to-menu mix-menu delete-track-label cp-delete-track)))

(set! mix-list (cons (lambda ()
		       (let ((new-label (format #f "Delete track (~D)" delete-track-number)))
			 (if delete-track-menu-label (change-label delete-track-menu-label new-label))
			 (set! delete-track-label new-label)))
		     mix-list))



;;; -------- Play track
;;;
;;;

(define play-track-number 1)
(define play-track-label "Play track")
(define play-track-dialog #f)
(define play-track-menu-label #f)

(define (cp-play-track)
  (catch 'no-such-track
	 (lambda ()
	   (play-track play-track-number))
	 (lambda args 
	   (snd-print ";no such track"))))

(if (or (provided? 'xm) 
	(provided? 'xg))
    (begin
      
      (define (post-play-track-dialog)
        (if (not play-track-dialog)
            (let ((initial-play-track-number 1)
                  (sliders '()))

              (set! play-track-dialog
                    (make-effect-dialog 
		     play-track-label

		     (if (provided? 'snd-gtk)
			 (lambda (w context) (cp-play-track))
			 (lambda (w context info) (cp-play-track)))

		     (if (provided? 'snd-gtk)
			 (lambda (w context)
			   (help-dialog "Play track" "Plays the track specified by the slider."))
			 (lambda (w context info)
			   (help-dialog "Play track" "Plays the track specified by the slider.")))

		     (if (provided? 'snd-gtk)
			 (lambda (w data)
			   (set! play-track-number initial-play-track-number)
			   (set! (.value (GTK_ADJUSTMENT (car sliders))) play-track-number)
			   (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car sliders))))
			 (lambda (w c i)
			   (set! play-track-number initial-play-track-number)
			   (XtSetValues (car sliders) (list XmNvalue play-track-number))))))

              (set! sliders
                    (add-sliders 
		     play-track-dialog
		     (list (list "track number" 0 initial-play-track-number 100
				 (if (provided? 'snd-gtk)
                                             (lambda (w data)
                                               (set! play-track-number (.value (GTK_ADJUSTMENT w))))
                                             (lambda (w context info)
                                               (set! play-track-number (.value info))))
				 1))))))
        (activate-dialog play-track-dialog))
      
      (set! play-track-menu-label (add-to-menu mix-menu "Play track" (lambda () (post-play-track-dialog)))))
    
    (set! play-track-menu-label (add-to-menu mix-menu play-track-label cp-play-track)))

(set! mix-list (cons (lambda ()
		       (let ((new-label (format #f "Play track (~D)" play-track-number)))
			 (if play-track-menu-label (change-label play-track-menu-label new-label))
			 (set! play-track-label new-label)))
		     mix-list))

;;; -------- Reverse track

(define reverse-track-number 1)
(define reverse-track-label "Reverse track")
(define reverse-track-dialog #f)
(define reverse-track-menu-label #f)

(define (cp-reverse-track)
  (catch 'no-such-track
	 (lambda ()
	   (reverse-track reverse-track-number))
	 (lambda args 
	   (snd-print ";no such track"))))

(if (or (provided? 'xm) 
	(provided? 'xg))
    (begin
      
      (define (post-reverse-track-dialog)
        (if (not reverse-track-dialog)
            (let ((initial-reverse-track-number 1)
                  (sliders '()))

              (set! reverse-track-dialog
                    (make-effect-dialog 
		     reverse-track-label

		     (if (provided? 'snd-gtk)
			 (lambda (w context) (cp-reverse-track))
			 (lambda (w context info) (cp-reverse-track)))

		     (if (provided? 'snd-gtk)
			 (lambda (w context)
			   (help-dialog "Reverse track" "Reverses the order in which a track's members occur."))
			 (lambda (w context info)
			   (help-dialog "Reverse track" "Reverses the order in which a track's members occur.")))

		     (if (provided? 'snd-gtk)
			 (lambda (w data)
			   (set! reverse-track-number initial-reverse-track-number)
			   (set! (.value (GTK_ADJUSTMENT (car sliders))) reverse-track-number)
			   (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car sliders))))
			 (lambda (w c i)
			   (set! reverse-track-number initial-reverse-track-number)
			   (XtSetValues (car sliders) (list XmNvalue reverse-track-number))))))

              (set! sliders
                    (add-sliders 
		     reverse-track-dialog
		     (list (list "track number" 0 initial-reverse-track-number 100
				 (if (provided? 'snd-gtk)
				     (lambda (w context)
				       (set! reverse-track-number (.value (GTK_ADJUSTMENT w))))
				     (lambda (w context info)
				       (set! reverse-track-number (.value info))))
				 1))))))
        (activate-dialog reverse-track-dialog))
      
      (set! reverse-track-menu-label (add-to-menu mix-menu "Reverse track" (lambda () (post-reverse-track-dialog)))))
    
    (set! reverse-track-menu-label (add-to-menu mix-menu reverse-track-label cp-reverse-track)))

(set! mix-list (cons (lambda ()
		       (let ((new-label (format #f "Reverse track (~D)" reverse-track-number)))
			 (if reverse-track-menu-label (change-label reverse-track-menu-label new-label))
			 (set! reverse-track-label new-label)))
		     mix-list))


;;; -------- Set track amplitude

(define set-track-amp-tracknum 1)
(define set-track-amp-scaler 1.0)
(define set-track-amp-label "Set track amplitude")
(define set-track-amp-dialog #f)
(define set-track-amp-menu-label #f)

(define (cp-set-track-amp)
  (ensure-track set-track-amp-tracknum)
  (set! (track-amp set-track-amp-tracknum) set-track-amp-scaler))

(if (or (provided? 'xm) 
	(provided? 'xg))
    (begin
      
      (define (post-set-track-amp-dialog)
        (if (not set-track-amp-dialog)
            (let ((initial-set-track-amp-tracknum 1)
                  (initial-set-track-amp-scaler 1.0)
                  (sliders '()))

              (set! set-track-amp-dialog
                    (make-effect-dialog 
		     set-track-amp-label

		     (if (provided? 'snd-gtk)
			 (lambda (w context) (cp-set-track-amp))
			 (lambda (w context info) (cp-set-track-amp)))

		     (if (provided? 'snd-gtk)
			 (lambda (w context)
			   (help-dialog "Set track amplitude" "Move the sliders to set the track number and amp scaling."))
			 (lambda (w context info)
			   (help-dialog "Set track amplitude" "Move the sliders to set the track number and amp scaling.")))

		     (if (provided? 'snd-gtk)
			 (lambda (w data)
			   (set! set-track-amp-tracknum initial-set-track-amp-tracknum)
			   (set! (.value (GTK_ADJUSTMENT (car sliders))) set-track-amp-tracknum)
			   (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car sliders)))
			   (set! set-track-amp-scaler initial-set-track-amp-scaler)
			   (set! (.value (GTK_ADJUSTMENT (cadr sliders))) (inexact->exact (round (* set-track-amp-scaler 100))))
			   (gtk_adjustment_value_changed (GTK_ADJUSTMENT (cadr sliders))))
			 (lambda (w c i)
			   (set! set-track-amp-tracknum initial-set-track-amp-tracknum)
			   (XtSetValues (car sliders) (list XmNvalue set-track-amp-tracknum))
			   (set! set-track-amp-scaler initial-set-track-amp-scaler)
			   (XtSetValues (cadr sliders) (list XmNvalue (inexact->exact (round (* set-track-amp-scaler 100)))))))))

              (set! sliders
		    (add-sliders 
		     set-track-amp-dialog
		     (list (list "track number" 0 initial-set-track-amp-tracknum 100
				 (if (provided? 'snd-gtk)
				     (lambda (w context)
				       (set! set-track-amp-tracknum (.value (GTK_ADJUSTMENT w))))
				     (lambda (w context info)
				       (set! set-track-amp-tracknum (.value info))))
				 1)
			   (list "amp scaler" 0.01 initial-set-track-amp-scaler 2.0
				 (if (provided? 'snd-gtk)
				     (lambda (w context)
				       (set! set-track-amp-scaler (/ (.value (GTK_ADJUSTMENT w)) 100)))
				     (lambda (w context info)
				       (set! set-track-amp-scaler (/ (.value info) 100))))
				 100))))))
        (activate-dialog set-track-amp-dialog))
      
      (set! set-track-amp-menu-label (add-to-menu mix-menu "Set track amplitude" (lambda () (post-set-track-amp-dialog)))))
    
    (set! set-track-amp-menu-label (add-to-menu mix-menu set-track-amp-label cp-set-track-amp)))

(set! mix-list (cons (lambda ()
		       (let ((new-label (format #f "Set track amplitude (~D ~1,2F)" set-track-amp-tracknum set-track-amp-scaler)))
			 (if set-track-amp-menu-label (change-label set-track-amp-menu-label new-label))
			 (set! set-track-amp-label new-label)))
		     mix-list))


;;; -------- Set track speed

(define set-track-speed-tracknum 1)
(define set-track-speed-scaler 1.0)
(define set-track-speed-label "Set track speed")
(define set-track-speed-dialog #f)
(define set-track-speed-menu-label #f)

(define (cp-set-track-speed)
  (ensure-track set-track-speed-tracknum)
  (set! (track-speed set-track-speed-tracknum) set-track-speed-scaler))

(if (or (provided? 'xm) 
	(provided? 'xg))
    (begin
      
      (define (post-set-track-speed-dialog)
        (if (not set-track-speed-dialog)
            (let ((initial-set-track-speed-tracknum 1)
                  (initial-set-track-speed-scaler 1.0)
                  (sliders '()))

              (set! set-track-speed-dialog
                    (make-effect-dialog 
		     set-track-speed-label

		     (if (provided? 'snd-gtk)
			 (lambda (w context) (cp-set-track-speed))
			 (lambda (w context info) (cp-set-track-speed)))

		     (if (provided? 'snd-gtk)
			 (lambda (w context)
			   (help-dialog "Set track speed" "Move the sliders to set the track number and rate scaling."))
			 (lambda (w context info)
			   (help-dialog "Set track speed" "Move the sliders to set the track number and rate scaling.")))

		     (if (provided? 'snd-gtk)
			 (lambda (w data)
			   (set! set-track-speed-tracknum initial-set-track-speed-tracknum)
			   (set! (.value (GTK_ADJUSTMENT (car sliders))) set-track-speed-tracknum)
			   (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car sliders)))
			   (set! set-track-speed-scaler initial-set-track-speed-scaler)
			   (set! (.value (GTK_ADJUSTMENT (cadr sliders))) (inexact->exact (round (* set-track-speed-scaler 100))))
			   (gtk_adjustment_value_changed (GTK_ADJUSTMENT (cadr sliders))))
			 (lambda (w c i)
			   (set! set-track-speed-tracknum initial-set-track-speed-tracknum)
			   (XtSetValues (car sliders) (list XmNvalue set-track-speed-tracknum))
			   (set! set-track-speed-scaler initial-set-track-speed-scaler)
			   (XtSetValues (cadr sliders) (list XmNvalue (inexact->exact (round (* set-track-speed-scaler 100)))))))))

              (set! sliders
		    (add-sliders 
		     set-track-speed-dialog
		     (list (list "track number" 0 initial-set-track-speed-tracknum 100
				 (if (provided? 'snd-gtk)
				     (lambda (w context)
				       (set! set-track-speed-tracknum (.value (GTK_ADJUSTMENT w))))
				     (lambda (w context info)
				       (set! set-track-speed-tracknum (.value info))))
				 1)
			   (list "rate scaler" 0.01 initial-set-track-speed-scaler 2.0
				 (if (provided? 'snd-gtk)
				     (lambda (w context)
				       (set! set-track-speed-scaler (/ (.value (GTK_ADJUSTMENT w)) 100)))
				     (lambda (w context info)
				       (set! set-track-speed-scaler (/ (.value info) 100))))
				 100))))))
        (activate-dialog set-track-speed-dialog))
      
      (set! set-track-speed-menu-label (add-to-menu mix-menu "Set track speed" (lambda () (post-set-track-speed-dialog)))))
    
    (set! set-track-speed-menu-label (add-to-menu mix-menu set-track-speed-label cp-set-track-speed)))

(set! mix-list (cons (lambda ()
		       (let ((new-label (format #f "Set track speed (~D ~1,2F)" set-track-speed-tracknum set-track-speed-scaler)))
			 (if set-track-speed-menu-label (change-label set-track-speed-menu-label new-label))
			 (set! set-track-speed-label new-label)))
		     mix-list))


;;; -------- Set track tempo

(define set-track-tempo-tracknum 1)
(define set-track-tempo-value 1.0)
(define set-track-tempo-label "Set track tempo")
(define set-track-tempo-dialog #f)
(define set-track-tempo-menu-label #f)

(define (cp-set-track-tempo)
  (catch 'no-such-track
	 (lambda ()
	   (retempo-track set-track-tempo-tracknum set-track-tempo-value))
	 (lambda args
	   (snd-print ";no such track"))))

(if (or (provided? 'xm) 
	(provided? 'xg))
    (begin
      
      (define (post-set-track-tempo-dialog)
        (if (not set-track-tempo-dialog)
            (let ((initial-set-track-tempo-tracknum 1)
                  (initial-set-track-tempo-value 1.0)
                  (sliders '()))

              (set! set-track-tempo-dialog
                    (make-effect-dialog 
		     set-track-tempo-label

		     (if (provided? 'snd-gtk)
			 (lambda (w context) (cp-set-track-tempo))
			 (lambda (w context info) (cp-set-track-tempo)))

		     (if (provided? 'snd-gtk)
			 (lambda (w context)
			   (help-dialog "Set track tempo"
					"Affects the time between the successive mix begin points (tempo > 1.0 makes the mixes happen more quickly)."))
			 (lambda (w context info)
			   (help-dialog "Set track tempo"
					"Affects the time between the successive mix begin points (tempo > 1.0 makes the mixes happen more quickly).")))

		     (if (provided? 'snd-gtk)
			 (lambda (w data)
			   (set! set-track-tempo-tracknum initial-set-track-tempo-tracknum)
			   (set! (.value (GTK_ADJUSTMENT (car sliders))) set-track-tempo-tracknum)
			   (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car sliders)))
			   (set! set-track-tempo-value initial-set-track-tempo-value)
			   (set! (.value (GTK_ADJUSTMENT (cadr sliders))) (inexact->exact (round (* set-track-tempo-value 100))))
			   (gtk_adjustment_value_changed (GTK_ADJUSTMENT (cadr sliders))))
			 (lambda (w c i)
			   (set! set-track-tempo-tracknum initial-set-track-tempo-tracknum)
			   (XtSetValues (car sliders) (list XmNvalue set-track-tempo-tracknum))
			   (set! set-track-tempo-value initial-set-track-tempo-value)
			   (XtSetValues (cadr sliders) (list XmNvalue (inexact->exact (round (* set-track-tempo-value 100)))))))))

              (set! sliders
                    (add-sliders 
		     set-track-tempo-dialog
		     (list (list "track number" 0 initial-set-track-tempo-tracknum 100
				 (if (provided? 'snd-gtk)
				     (lambda (w context)
				       (set! set-track-tempo-tracknum (.value (GTK_ADJUSTMENT w))))
				     (lambda (w context info)
				       (set! set-track-tempo-tracknum (.value info))))
				 1)
			   (list "tempo" 0 initial-set-track-tempo-value 100
				 (if (provided? 'snd-gtk)
				     (lambda (w context)
				       (set! set-track-tempo-value (/ (.value (GTK_ADJUSTMENT w)) 100)))
				     (lambda (w context info)
				       (set! set-track-tempo-value (/ (.value info) 100))))
				 100))))))
	
        (activate-dialog set-track-tempo-dialog))
      (set! set-track-tempo-menu-label (add-to-menu mix-menu "Set track tempo" (lambda () (post-set-track-tempo-dialog))))))

(set! mix-list (cons (lambda ()
		       (let ((new-label (format #f "Set track tempo (~D ~1,2F)" set-track-tempo-tracknum set-track-tempo-value)))
			 (if set-track-tempo-menu-label (change-label set-track-tempo-menu-label new-label))
			 (set! set-track-tempo-label new-label)))
		     mix-list))


;;; -------- Transpose track

(define transpose-track-number 1)
(define transpose-track-semitones 0)
(define transpose-track-label "Transpose track")
(define transpose-track-dialog #f)
(define transpose-track-menu-label #f)

(define (cp-transpose-track)
  (ensure-track transpose-track-number)
  (transpose-track transpose-track-number transpose-track-semitones))

(if (or (provided? 'xm) 
	(provided? 'xg))
    (begin
      
      (define (post-transpose-track-dialog)
        (if (not transpose-track-dialog)
            (let ((initial-transpose-track-number 1)
                  (initial-transpose-track-semitones 0)
                  (sliders '()))

              (set! transpose-track-dialog
                    (make-effect-dialog 
		     transpose-track-label

		     (if (provided? 'snd-gtk)
			 (lambda (w context) (cp-transpose-track))
			 (lambda (w context info) (cp-transpose-track)))

		     (if (provided? 'snd-gtk)
			 (lambda (w context)
			   (help-dialog "Transpose track" "A simple track transposition utility."))
			 (lambda (w context info)
			   (help-dialog "Transpose track" "A simple track transposition utility.")))

		     (if (provided? 'snd-gtk)
			 (lambda (w data)
			   (set! transpose-track-number initial-transpose-track-number)
			   (set! (.value (GTK_ADJUSTMENT (car sliders))) transpose-track-number)
			   (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car sliders)))
			   (set! transpose-track-semitones initial-transpose-track-semitones)
			   (set! (.value (GTK_ADJUSTMENT (car sliders))) transpose-track-semitones)
			   (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car sliders))))
			 (lambda (w c i)
			   (set! transpose-track-number initial-transpose-track-number)
			   (XtSetValues (car sliders) (list XmNvalue transpose-track-number))
			   (set! transpose-track-semitones initial-transpose-track-semitones)
			   (XtSetValues (cadr sliders) (list XmNvalue transpose-track-semitones))))))

              (set! sliders
                    (add-sliders 
		     transpose-track-dialog
		     (list (list "transpose-track number" 0 initial-transpose-track-number 100
				 (if (provided? 'snd-gtk)
				     (lambda (w context)
				       (set! transpose-track-number (.value (GTK_ADJUSTMENT w))))
				     (lambda (w context info)
				       (set! transpose-track-number (.value info))))
				 1)
			   (list "transpose-track semitones" -100 initial-transpose-track-semitones 100
				 (if (provided? 'snd-gtk)
				     (lambda (w context)
				       (set! transpose-track-semitones (.value (GTK_ADJUSTMENT w))))
				     (lambda (w context info)
				       (set! transpose-track-semitones (.value info))))
				 1))))))
	
        (activate-dialog transpose-track-dialog))
      (set! transpose-track-menu-label (add-to-menu mix-menu "Transpose track" (lambda () (post-transpose-track-dialog))))))

(set! mix-list (cons (lambda ()
		       (let ((new-label (format #f "Transpose track (~D ~D)" transpose-track-number transpose-track-semitones)))
			 (if transpose-track-menu-label (change-label transpose-track-menu-label new-label))
			 (set! transpose-track-label new-label)))
		     mix-list))

(add-to-menu mix-menu #f #f)


;;; -------- Save track

(define save-track-number 1)
(define save-track-label "Save track")
(define save-track-dialog #f)
(define save-track-menu-label #f)

(define (cp-save-track)
  (catch #t
	 (lambda ()
	   (save-track save-track-number (format #f "track-~A.snd" save-track-number)))
	 (lambda args
	   (snd-print ";can't save track"))))

(if (or (provided? 'xm) 
	(provided? 'xg))
    (begin
      
      (define (post-save-track-dialog)
        (if (not save-track-dialog)
            (let ((initial-save-track-number 1)
                  (sliders '()))

              (set! save-track-dialog
                    (make-effect-dialog 
		     save-track-label

		     (if (provided? 'snd-gtk)
			 (lambda (w context) (cp-save-track))
			 (lambda (w context info) (cp-save-track)))

		     (if (provided? 'snd-gtk)
			 (lambda (w context)
			   (help-dialog "Save track" "Save track to track-N.snd soundfile."))
			 (lambda (w context info)
			   (help-dialog "Save track" "Save track to track-N.snd soundfile.")))

		     (if (provided? 'snd-gtk)
			 (lambda (w data)
			   (set! save-track-number initial-save-track-number)
			   (set! (.value (GTK_ADJUSTMENT (car sliders))) save-track-number)
			   (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car sliders))))
			 (lambda (w c i)
			   (set! save-track-number initial-save-track-number)
			   (XtSetValues (car sliders) (list XmNvalue save-track-number))))))

              (set! sliders
                    (add-sliders 
		     save-track-dialog
		     (list (list "track number" 0 initial-save-track-number 100
				 (if (provided? 'snd-gtk)
				     (lambda (w context)
				       (set! save-track-number (.value (GTK_ADJUSTMENT w))))
				     (lambda (w context info)
				       (set! save-track-number (.value info))))
				 1))))))
        (activate-dialog save-track-dialog))
      
      (set! save-track-menu-label (add-to-menu mix-menu "Save track" (lambda () (post-save-track-dialog)))))
    
    (set! save-track-menu-label (add-to-menu mix-menu save-track-label cp-save-track)))

(set! mix-list (cons (lambda ()
		       (let ((new-label (format #f "Save track (~D)" save-track-number)))
			 (if save-track-menu-label (change-label save-track-menu-label new-label))
			 (set! save-track-label new-label)))
		     mix-list))


(add-to-menu mix-menu #f #f)

(add-to-menu mix-menu "Colorize tracks" (lambda () (load-from-path "track-colors.scm")))

(add-to-menu mix-menu "Delete all mixes & tracks" (lambda () (delete-all-tracks)))

