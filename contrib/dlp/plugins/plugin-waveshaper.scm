
(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
          (snd-error (format #f "nef.scm needs the xm module: ~A" hxm))
          (dlinit hxm "init_xm"))))

(if (not (defined? 'plugins-list))
    (load "plugins-menu.scm"))

(define waveshaper-limiting-amplitude .3)
(define waveshaper-dialog #f)

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))

    (add-to-menu
     plugins-menu "Waveshaper"
     (lambda ()
       (if (not (|Widget? waveshaper-dialog))
           ;; if waveshaper-dialog doesn't exist, create it
           (let ((sliders '()))
             (set! waveshaper-dialog
                   (make-effect-dialog "waveshaper ladspa plugin"
                                       (lambda (w context info)
                                         (apply-ladspa (make-sample-reader (cursor))
                                                       (list "cmt" "wshape_sine" waveshaper-limiting-amplitude)
                                                       (- (frames) (cursor))
                                                       "waveshaper"))
                                       (lambda (w context info) (|XtUnmanageChild waveshaper-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Waveshaper Help"
                                                      "Sine-based waveshaping..."))
                                       (lambda (w c i)
                                         (set! waveshaper-limiting-amplitude 100)
                                         (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* waveshaper-limiting-amplitude 1)))))))
             (set! sliders
                   (add-sliders waveshaper-dialog
                                (list (list "limiting amplitude" 0 1000 32768
                                            (lambda (w context info)
                                              (set! waveshaper-limiting-amplitude (/ (|value info) 1)))
                                            1))))))
       (activate-dialog waveshaper-dialog))))
