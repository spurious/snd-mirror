
(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
          (snd-error (format #f "nef.scm needs the xm module: ~A" hxm))
          (dlinit hxm "init_xm"))))

(if (not (defined? 'plugins-list))
    (load "plugins-menu.scm"))

(define decimator-bit-depth 5)
(define decimator-sample-rate .5)
(define decimator-dialog #f)

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))

    (add-to-menu
     plugins-menu "Decimator"
     (lambda ()
       (if (not (|Widget? decimator-dialog))
           ;; if decimator-dialog doesn't exist, create it
           (let ((sliders '()))
             (set! decimator-dialog
                   (make-effect-dialog "decimator ladspa plugin"
                                       (lambda (w context info)
						(let* ((snd (selected-sound))
						       (sr (srate snd))
						       (dsr (* decimator-sample-rate sr)))
                                         (apply-ladspa (make-sample-reader (cursor))
                                                       (list "decimator_1202" "decimator" decimator-bit-depth dsr)
                                                       (- (frames) (cursor))
                                                       "decimator")))
                                       (lambda (w context info) (|XtUnmanageChild decimator-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Decimator Help"
                                                      "Move the slider to adjust decimator parameters."))
                                       (lambda (w c i)
                                         (set! decimator-bit-depth 5)
                                         (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* decimator-bit-depth 1))))
				         (set! decimator-sample-rate .5)
				         (|XtSetValues (cadr sliders) (list |XmNvalue (inexact->exact (* decimator-sample-rate 1000)))))))
             (set! sliders
                   (add-sliders decimator-dialog
                                (list (list "bit depth" 1 5 24
                                            (lambda (w context info)
                                              (set! decimator-bit-depth (/ (|value info) 1)))
                                            1)
                                      (list "sample rate (Hz)" 0.001 0.5 1.0
                                            (lambda (w context info)
                                              (set! decimator-sample-rate (/ (|value info) 1000)))
                                            1000))))))
       (activate-dialog decimator-dialog))))
