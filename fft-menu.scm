(use-modules (ice-9 format) (ice-9 optargs))

(if (not (provided? 'snd-effects-utils.scm)) (load-from-path "effects-utils.scm")) ; make-effects-dialog
(if (not (provided? 'snd-examp.scm)) (load-from-path "examp.scm")) ; squelch-vowels

(provide 'snd-fft-menu.scm)

(define fft-list '()) ; menu labels are updated to show current default settings

(define fft-menu (add-to-main-menu "FFT Edits" (lambda ()
						   (define (update-label fft)
						     (if (not (null? fft))
							 (begin
							   ((car fft))
							   (update-label (cdr fft)))))
						   (update-label fft-list))))

;;; ------ FFT edit
;;;

(define fft-edit-low-frequency 100)
(define fft-edit-high-frequency 1000)
(define fft-edit-label "FFT notch filter")
(define fft-edit-dialog #f)
(define fft-edit-menu-label #f)

(define (cp-fft-edit)
  (fft-edit fft-edit-low-frequency fft-edit-high-frequency))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-fft-edit-dialog)
        (if (not fft-edit-dialog)
            ;; if fft-edit-dialog doesn't exist, create it
            (let ((initial-fft-edit-low-frequency 100)
                  (initial-fft-edit-high-frequency 1000)
                  (sliders '()))
              (set! fft-edit-dialog
                    (make-effect-dialog fft-edit-label
                                        (lambda (w context info)
                                          (cp-fft-edit))
                                        (lambda (w context info)
                                          (help-dialog "FFT notch filter"
                                                       "A simple example of FFT-based editing. It takes an FFT of the entire sound, removes all energy below the low frequency and above the high frequency, then computes the inverse FFT."))
                                        (lambda (w c i)
						 (set! fft-edit-low-frequency initial-fft-edit-low-frequency)
						 (set! fft-edit-high-frequency initial-fft-edit-high-frequency)
						 (if (provided? 'snd-gtk)
						     (begin
						       (set! (.value (GTK_ADJUSTMENT (car sliders)))  (inexact->exact (* fft-edit-low-frequency 1)))
						       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (car sliders)))
						       (set! (.value (GTK_ADJUSTMENT (cadr sliders)))  (inexact->exact (* fft-edit-high-frequency 1)))
						       (gtk_adjustment_value_changed (GTK_ADJUSTMENT (cadr sliders))))
						     (begin
						       (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* fft-edit-low-frequency 1))))
						       (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* fft-edit-high-frequency 1)))))))))
              (set! sliders
                   (add-sliders fft-edit-dialog
                                 (list (list "low frequency" 20 initial-fft-edit-low-frequency 22050
                                             (lambda (w context info)
                                               (set! fft-edit-low-frequency (/ (.value (if (provided? 'snd-gtk) (GTK_ADJUSTMENT w) info)) 1)))
                                             1)
                                       (list "high frequency" 20 initial-fft-edit-high-frequency 22050
                                             (lambda (w context info)
                                               (set! fft-edit-high-frequency (/ (.value (if (provided? 'snd-gtk) (GTK_ADJUSTMENT w) info)) 1)))
                                             1))))))
        (activate-dialog fft-edit-dialog))

      (set! fft-edit-menu-label (add-to-menu fft-menu "FFT notch filter" (lambda () (post-fft-edit-dialog)))))

    (set! fft-edit-menu-label (add-to-menu fft-menu fft-edit-label cp-fft-edit)))

(set! fft-list (cons (lambda ()
                           (let ((new-label (format #f "FFT notch filter (~1,2D ~1,2D)" fft-edit-low-frequency fft-edit-high-frequency)))
                             (if fft-edit-menu-label (change-label fft-edit-menu-label new-label))
                             (set! fft-edit-label new-label)))
                         fft-list))



;;; ------ FFT squelch
;;;

(define fft-squelch-amount 0.0)
(define fft-squelch-label "FFT squelch")
(define fft-squelch-dialog #f)
(define fft-squelch-menu-label #f)

(define (cp-fft-squelch)
 (fft-squelch fft-squelch-amount))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-fft-squelch-dialog)
        (if (not fft-squelch-dialog)
            ;; if fft-squelch-dialog doesn't exist, create it
            (let ((initial-fft-squelch-amount 0.0)
                  (sliders '()))
              (set! fft-squelch-dialog
                    (make-effect-dialog fft-squelch-label
                                        (lambda (w context info)
                                          (cp-fft-squelch))
                                        (lambda (w context info)
                                          (help-dialog "FFT squelch"
                                                "Removes all energy below the squelch amount. This is sometimes useful for noise-reduction."))
                                        (lambda (w c i)
                                          (set! fft-squelch-amount initial-fft-squelch-amount)
					  (if (provided? 'snd-gtk)
					      (begin
						(set! (.value (GTK_ADJUSTMENT (car sliders)))  (inexact->exact (round (* fft-squelch-amount 100))))
						(gtk_adjustment_value_changed (GTK_ADJUSTMENT (car sliders))))
					      (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (round (* fft-squelch-amount 100)))))))))
              (set! sliders
                    (add-sliders fft-squelch-dialog
                                 (list (list "squelch amount" 0.0 initial-fft-squelch-amount 1.0
					     (lambda (w context info)
						      (set! fft-squelch-amount (/ (.value (if (provided? 'snd-gtk)
											      (GTK_ADJUSTMENT w) 
											      info))
										  100)))
                                             100))))))
        (activate-dialog fft-squelch-dialog))

      (set! fft-squelch-menu-label (add-to-menu fft-menu "FFT squelch" (lambda () (post-fft-squelch-dialog)))))

    (set! fft-squelch-menu-label (add-to-menu fft-menu fft-squelch-label cp-fft-squelch)))

(set! fft-list (cons (lambda ()
                           (let ((new-label (format #f "FFT squelch (~1,2F)" fft-squelch-amount)))
                             (if fft-squelch-menu-label (change-label fft-squelch-menu-label new-label))
                             (set! fft-squelch-label new-label)))
                         fft-list))

(add-to-menu fft-menu #f #f)

(add-to-menu fft-menu "Squelch vowels" (lambda () (squelch-vowels)))
