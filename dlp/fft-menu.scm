(use-modules (ice-9 format))

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

(define (cp-fft-edit)
  (fft-edit fft-edit-low-frequency fft-edit-high-frequency))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-fft-edit-dialog)
        (if (not (Widget? fft-edit-dialog))
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
                                                       "A simple example of FFT-based editing. It takes an FFT of the entire sound, removes all energy below the low frequency\n\ and above the high frequency, then computes the inverse FFT."))
                                        (lambda (w c i)
                                          (set! fft-edit-low-frequency initial-fft-edit-low-frequency)
                                          (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (* fft-edit-low-frequency 1))))
                                          (set! fft-edit-high-frequency initial-fft-edit-high-frequency)
                                          (XtSetValues (list-ref sliders 1) (list XmNvalue (inexact->exact (* fft-edit-high-frequency 1)))))))
              (set! sliders
                   (add-sliders fft-edit-dialog
                                 (list (list "low frequency" 20 initial-fft-edit-low-frequency 22050
                                             (lambda (w context info)
                                               (set! fft-edit-low-frequency (/ (.value info) 1)))
                                             1)
                                       (list "high frequency" 20 initial-fft-edit-high-frequency 22050
                                             (lambda (w context info)
                                               (set! fft-edit-high-frequency (/ (.value info) 1)))
                                             1))))))
        (activate-dialog fft-edit-dialog))

      (add-to-menu fft-menu "FFT notch filter" (lambda () (post-fft-edit-dialog))))

    (add-to-menu fft-menu fft-edit-label cp-fft-edit))

(set! fft-list (cons (lambda ()
                           (let ((new-label (format #f "FFT notch filter (~1,2D ~1,2D)" fft-edit-low-frequency fft-edit-high-frequency)))
                             (change-menu-label fft-menu fft-edit-label new-label)
                             (set! fft-edit-label new-label)))
                         fft-list))



;;; ------ FFT squelch
;;;

(define fft-squelch-amount 0.0)
(define fft-squelch-label "FFT squelch")
(define fft-squelch-dialog #f)

(define (cp-fft-squelch)
 (fft-squelch fft-squelch-amount))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-fft-squelch-dialog)
        (if (not (Widget? fft-squelch-dialog))
            ;; if fft-squelch-dialog doesn't exist, create it
            (let ((initial-fft-squelch-amount 0.0)
                  (sliders '()))
              (set! fft-squelch-dialog
                    (make-effect-dialog fft-squelch-label
                                        (lambda (w context info)
                                          (cp-fft-squelch))
                                        (lambda (w context info)
                                          (help-dialog "FFT squelch"
                                                "Removes all energy below the squelch amount.\n\ This is sometimes useful for noise-reduction."))
                                        (lambda (w c i)
                                          (set! fft-squelch-amount initial-fft-squelch-amount)
                                          (XtSetValues (list-ref sliders 0) (list XmNvalue (inexact->exact (round (* fft-squelch-amount 100))))))))
              (set! sliders
                    (add-sliders fft-squelch-dialog
                                 (list (list "squelch amount" 0.0 initial-fft-squelch-amount 1.0
                                             (lambda (w context info)
                                               (set! fft-squelch-amount (/ (.value info) 100)))
                                             100))))))
        (activate-dialog fft-squelch-dialog))

      (add-to-menu fft-menu "FFT squelch" (lambda () (post-fft-squelch-dialog))))

    (add-to-menu fft-menu fft-squelch-label cp-fft-squelch))

(set! fft-list (cons (lambda ()
                           (let ((new-label (format #f "FFT squelch (~1,2F)" fft-squelch-amount)))
                             (change-menu-label fft-menu fft-squelch-label new-label)
                             (set! fft-squelch-label new-label)))
                         fft-list))

(add-to-menu fft-menu #f #f)

(add-to-menu fft-menu "Squelch vowels" squelch-vowels)

;(define (ramp gen up)
;  "(ramp gen up) is a kind of CLM generator that produces a ramp of a given length, then sticks at 0.0 or 1.0 until the 'up' argument changes"
  ;; gen is list: ctr size
  ;;  the idea here is that we want to ramp in or out a portion of a sound based on some
  ;;  factor of the sound data -- the ramp gen produces a ramp up when 'up' is #t, sticking
  ;;  at 1.0, and a ramp down when 'up' is #f, sticking at 0.0
;  (let* ((ctr (car gen))
;	 (size (cadr gen))
;	 (val (/ ctr size)))
;    (list-set! gen 0 (min size (max 0 (+ ctr (if up 1 -1)))))
;    val))
;
;(define* (make-ramp #:optional (size 128))
;  "(make-ramp &optional size) returns a ramp generator"
;  (list 0 size))

;(define (squelch-consonants)
;  "(squelch-consonants) suppresses portions of a sound that look like unsteady-state"
;  (let* ((fft-size 32)
;	 (fft-mid (inexact->exact (/ fft-size 2)))
;	 (rl (make-vct fft-size))
;	 (im (make-vct fft-size))
;	 (ramper (make-ramp 256)) ; 512 ok too
;	 (peak (/ (maxamp) fft-mid))
;	 (read-ahead (make-sample-reader))
;	 (ctr 0)
;	 (in-vowel #f))
;    (do ((i 0 (1+ i)))
;	((= i (1- fft-size)))
;      (vct-set! rl i (read-ahead)))
;    (set! ctr (1- fft-size))
;    (map-channel (lambda (y)
;		   (vct-set! rl ctr (read-ahead))
;		   (set! ctr (1+ ctr))
;		   (if (= ctr fft-size)
;		       (begin
;			 (fft rl im 1)
;			 (vct-multiply! rl rl)
;			 (vct-multiply! im im)
;			 (vct-add! rl im)
;			 (set! in-vowel (> (+ (vct-ref rl 0) (vct-ref rl 1) (vct-ref rl 2) (vct-ref rl 3)) peak))
;			 ;; fancier version checked here ratio of this sum and
;			 ;;   sum of all rl vals, returned vowel if > 0.5
;			 (set! ctr 0)
;			 (vct-fill! im 0.0)))
;		   (let ((rval (ramp ramper in-vowel)))
;		     ; squelch consonants if just ramp value (not 1.0-val)
;		     (and (> rval 0.0) ; if this is included, the vowel-portions are omitted
;		     ))))))
;
;(add-to-menu fft-menu "Squelch consonants" squelch-consonants)

