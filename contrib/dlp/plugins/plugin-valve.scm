
(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
          (snd-error (format #f "nef.scm needs the xm module: ~A" hxm))
          (dlinit hxm "init_xm"))))

(if (not (defined? 'plugins-list))
    (load "plugins-menu.scm"))

(define distortion-level .5)
(define distortion-character .5)
(define valve-saturation-dialog #f)

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))

    (add-to-menu
     plugins-menu "Valve saturation"
     (lambda ()
       (if (not (|Widget? valve-saturation-dialog))
           ;; if valve-saturation-dialog doesn't exist, create it
           (let ((sliders '()))
             (set! valve-saturation-dialog
                   (make-effect-dialog "delay_5s ladspa plugin"
                                       (lambda (w context info)
                                         (apply-ladspa (make-sample-reader (cursor))
                                                       (list "valve_1209" "valve" distortion-level distortion-character)
                                                       (- (frames) (cursor))
                                                       "valve-saturation"))
                                       (lambda (w context info) (|XtUnmanageChild valve-saturation-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Valve saturation Help"
                                                      "Move the sliders to set the distortion parameters."))
                                       (lambda (w c i)
                                         (set! distortion-level .5)
                                         (|XtSetValues (car sliders)
                                            (list |XmNvalue (inexact->exact (* distortion-level 100))))
				         (set! distortion-character .5)
				         (|XtSetValues (cadr sliders) (list |XmNvalue (inexact->exact (* distortion-character 100)))))))
             (set! sliders
                   (add-sliders valve-saturation-dialog
                                (list (list "distortion level" 0.0 0.5 1.0
                                            (lambda (w context info)
                                              (set! distortion-level (/ (|value info) 100.0)))
                                            100)
                                      (list "distortion character" 0.0 0.5 1.0
                                            (lambda (w context info)
                                              (set! distortion-character (/ (|value info) 100.0)))
                                            100))))))
       (activate-dialog valve-saturation-dialog))))
