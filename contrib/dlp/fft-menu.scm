(use-modules (ice-9 format))

(define fft-list '()) ; menu labels are updated to show current default settings

(define fft-menu (add-to-main-menu "FFT Edits" (lambda ()
						   (define (update-label fft)
						     (if (not (null? fft))
							 (begin
							   ((car fft))
							   (update-label (cdr fft)))))
						   (update-label fft-list))))
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

(define (make-effect-dialog label ok-callback dismiss-callback help-callback reset-callback)
  ;; make a standard dialog
  (let* ((xdismiss (|XmStringCreate "Dismiss" |XmFONTLIST_DEFAULT_TAG))
         (xhelp (|XmStringCreate "Help" |XmFONTLIST_DEFAULT_TAG))
         (xok (|XmStringCreate "DoIt" |XmFONTLIST_DEFAULT_TAG))
         (titlestr (|XmStringCreate label |XmFONTLIST_DEFAULT_TAG))
         (new-dialog (|XmCreateTemplateDialog
                       (|Widget (cadr (main-widgets))) label
                       (list |XmNcancelLabelString   xdismiss
                             |XmNhelpLabelString     xhelp
                             |XmNokLabelString       xok
                             |XmNautoUnmanage        #f
                             |XmNdialogTitle         titlestr
                             |XmNresizePolicy        |XmRESIZE_GROW
                             |XmNnoResize            #f
                             |XmNbackground          (|Pixel (snd-pixel (basic-color)))
                             |XmNtransient           #f))))
    (for-each
     (lambda (button)
       (|XtVaSetValues
         (|XmMessageBoxGetChild new-dialog button)
         (list |XmNarmColor   (|Pixel (snd-pixel (pushed-button-color)))
                |XmNbackground (|Pixel (snd-pixel (basic-color))))))
     (list |XmDIALOG_HELP_BUTTON |XmDIALOG_CANCEL_BUTTON |XmDIALOG_OK_BUTTON))

    (|XtAddCallback new-dialog |XmNcancelCallback dismiss-callback) ; "Dismiss"
    (|XtAddCallback new-dialog |XmNhelpCallback help-callback)  ; "Help"
    (|XtAddCallback new-dialog |XmNokCallback ok-callback)    ; "DoIt"

    (if reset-callback
        ;; add a Reset button
        (let ((reset-button (|XtCreateManagedWidget "Reset" |xmPushButtonWidgetClass new-dialog
                              (list |XmNbackground (|Pixel (snd-pixel (basic-color)))
                                    |XmNarmColor   (|Pixel (snd-pixel (pushed-button-color)))))))
          (|XtAddCallback reset-button |XmNactivateCallback reset-callback)))

    (|XmStringFree xhelp)
    (|XmStringFree xok)
    (|XmStringFree xdismiss)
    (|XmStringFree titlestr)
    new-dialog))

(define (add-sliders dialog sliders)
  ;; sliders is a list of lists, each inner list being (title low initial high callback scale)
  ;; returns list of widgets (for reset callbacks)
  (let ((mainform
         (|XtCreateManagedWidget "formd" |xmRowColumnWidgetClass dialog
           (list |XmNleftAttachment      |XmATTACH_FORM
                 |XmNrightAttachment     |XmATTACH_FORM
                 |XmNtopAttachment       |XmATTACH_FORM
                 |XmNbottomAttachment    |XmATTACH_WIDGET
                 |XmNbottomWidget        (|XmMessageBoxGetChild dialog |XmDIALOG_SEPARATOR)
                 |XmNbackground          (|Pixel (snd-pixel (highlight-color)))
                 |XmNorientation         |XmVERTICAL))))
    (map
     (lambda (slider-data)
       (let* ((title (|XmStringCreate (list-ref slider-data 0) |XmFONTLIST_DEFAULT_TAG))
              (low (list-ref slider-data 1))
              (initial (list-ref slider-data 2))
              (high (list-ref slider-data 3))
              (func (list-ref slider-data 4))
              (scale (list-ref slider-data 5))
              (new-slider
               (|XtCreateManagedWidget (car slider-data) |xmScaleWidgetClass mainform
                  (list |XmNorientation   |XmHORIZONTAL
                        |XmNshowValue     #t
                        |XmNminimum       (inexact->exact (* low scale))
                        |XmNmaximum       (inexact->exact (* high scale))
                        |XmNvalue         (inexact->exact (* initial scale))
                        |XmNdecimalPoints (if (= scale 1000) 3 (if (= scale 100) 2 (if (= scale 10) 1 0)))
                        |XmNtitleString   title
                        ;|XmNborderWidth   1
                        |XmNbackground    (|Pixel (snd-pixel (basic-color)))))))
         (|XmStringFree title)
         (|XtAddCallback new-slider |XmNvalueChangedCallback func)
         new-slider))
     sliders)))


;;; ------ FFT edit
;;;

(define fft-edit-low-frequency 100)
(define fft-edit-high-frequency 1000)
(define fft-edit-label "FFT edit")
(define fft-edit-dialog #f)

(define (cp-fft-edit)
  (fft-edit fft-edit-low-frequency fft-edit-high-frequency))

(if (provided? 'xm) ; if xm module is loaded, popup a dialog here
    (begin

      (define (post-fft-edit-dialog)
        (if (not (|Widget? fft-edit-dialog))
            ;; if fft-edit-dialog doesn't exist, create it
            (let ((initial-fft-edit-low-frequency 100)
                  (initial-fft-edit-high-frequency 1000)
                  (sliders '()))
              (set! fft-edit-dialog
                    (make-effect-dialog fft-edit-label
                                        (lambda (w context info)
                                          (cp-fft-edit))
                                        (lambda (w context info)
                                          (|XtUnmanageChild fft-edit-dialog))
                                        (lambda (w context info)
                                          (help-dialog "FFT edit"
                                                       "A simple example of FFT-based editing. It takes an FFT of the entire sound, removes all energy below the low frequency\n\ and above the high frequency, then computes the inverse FFT."))
                                        (lambda (w c i)
                                          (set! fft-edit-low-frequency initial-fft-edit-low-frequency)
                                          (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* fft-edit-low-frequency 1))))
                                          (set! fft-edit-high-frequency initial-fft-edit-high-frequency)
                                          (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* fft-edit-high-frequency 1)))))))
              (set! sliders
                   (add-sliders fft-edit-dialog
                                 (list (list "low frequency" 20 initial-fft-edit-low-frequency 22050
                                             (lambda (w context info)
                                               (set! fft-edit-low-frequency (/ (|value info) 1)))
                                             1)
                                       (list "high frequency" 20 initial-fft-edit-high-frequency 22050
                                             (lambda (w context info)
                                               (set! fft-edit-high-frequency (/ (|value info) 1)))
                                             1))))))
        (activate-dialog fft-edit-dialog))

      (add-to-menu fft-menu "FFT edit" (lambda () (post-fft-edit-dialog))))

    (add-to-menu fft-menu fft-edit-label cp-fft-edit))

(set! fft-list (cons (lambda ()
                           (let ((new-label (format #f "FFT edit (~1,2D ~1,2D)" fft-edit-low-frequency fft-edit-high-frequency)))
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
        (if (not (|Widget? fft-squelch-dialog))
            ;; if fft-squelch-dialog doesn't exist, create it
            (let ((initial-fft-squelch-amount 0.0)
                  (sliders '()))
              (set! fft-squelch-dialog
                    (make-effect-dialog fft-squelch-label
                                        (lambda (w context info)
                                          (cp-fft-squelch))
                                        (lambda (w context info)
                                          (|XtUnmanageChild fft-squelch-dialog))
                                        (lambda (w context info)
                                          (help-dialog "FFT squelch"
                                                "Removes all energy below the squelch amount.\n\ This is sometimes useful for noise-reduction."))
                                        (lambda (w c i)
                                          (set! fft-squelch-amount initial-fft-squelch-amount)
                                          (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* fft-squelch-amount 100)))))))
              (set! sliders
                    (add-sliders fft-squelch-dialog
                                 (list (list "squelch amount" 0.0 initial-fft-squelch-amount 1.0
                                             (lambda (w context info)
                                               (set! fft-squelch-amount (/ (|value info) 100)))
                                             100))))))
        (activate-dialog fft-squelch-dialog))

      (add-to-menu fft-menu "FFT squelch" (lambda () (post-fft-squelch-dialog))))

    (add-to-menu fft-menu fft-squelch-label cp-fft-squelch))

(set! fft-list (cons (lambda ()
                           (let ((new-label (format #f "FFT squelch (~1,2F)" fft-squelch-amount)))
                             (change-menu-label fft-menu fft-squelch-label new-label)
                             (set! fft-squelch-label new-label)))
                         fft-list))


;(add-to-menu fft-menu #f #f)

