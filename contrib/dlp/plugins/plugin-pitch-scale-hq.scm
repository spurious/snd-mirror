
(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
          (snd-error (format #f "nef.scm needs the xm module: ~A" hxm))
          (dlinit hxm "init_xm"))))

(if (not (defined? 'plugins-list))
    (load "plugins-menu.scm"))

(define pitch-coefficient 1.0)
(define pitch-scale-dialog #f)

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))

    (add-to-menu
     plugins-menu "Pitch Scaler (High quality)"
     (lambda ()
       (if (not (|Widget? pitch-scale-dialog))
           ;; if pitch-scale-dialog doesn't exist, create it
           (let ((sliders '()))
             (set! pitch-scale-dialog
                   (make-effect-dialog "pitch-scale (high-quality) ladspa plugin"
                                       (lambda (w context info)
                                         (apply-ladspa (make-sample-reader (cursor))
                                                       (list "pitch_scale_1194" "pitchScaleHQ" pitch-coefficient)
                                                       (- (frames) (cursor))
                                                       "pitch-scale"))
                                       (lambda (w context info) (|XtUnmanageChild pitch-scale-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Pitch scaler Help"
                                                      "Move the sliders to set the pitch scaling coefficient."))
                                       (lambda (w c i)
                                         (set! pitch-coefficient .5)
                                         (|XtSetValues (car sliders)
                                            (list |XmNvalue (inexact->exact (* pitch-coefficient 100)))))))
             (set! sliders
                   (add-sliders pitch-scale-dialog
                                (list (list "pitch coefficient" 0.5 1.0 2.0
                                            (lambda (w context info)
                                              (set! pitch-coefficient (/ (|value info) 100.0)))
                                            100))))))
       (activate-dialog pitch-scale-dialog))))
