
(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
          (snd-error (format #f "nef.scm needs the xm module: ~A" hxm))
          (dlinit hxm "init_xm"))))

(if (not (defined? 'plugins-list))
    (load "plugins-menu.scm"))

(define audio-divider-denominator 1)
(define audio-divider-dialog #f)

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))

    (add-to-menu
     plugins-menu "Audio divider"
     (lambda ()
       (if (not (|Widget? audio-divider-dialog))
           ;; if audio-divider-dialog doesn't exist, create it
           (let ((sliders '()))
             (set! audio-divider-dialog
                   (make-effect-dialog "audio-divider ladspa plugin"
                                       (lambda (w context info)
                                         (apply-ladspa (make-sample-reader (cursor))
                                                       (list "divider_1186" "divider" audio-divider-denominator)
                                                       (- (frames) (cursor))
                                                       "audio-divider"))
                                       (lambda (w context info) (|XtUnmanageChild audio-divider-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Audio divider Help"
                                                      "Generates suboctaves from audio input."))
                                       (lambda (w c i)
                                         (set! audio-divider-denominator 1)
                                         (|XtSetValues (car sliders)
                                            (list |XmNvalue (inexact->exact (* audio-divider-denominator 1)))))))
             (set! sliders
                   (add-sliders audio-divider-dialog
                                (list (list "denominator value" 1 1 8
                                            (lambda (w context info)
                                              (set! audio-divider-denominator (/ (|value info) 1)))
                                            1))))))
       (activate-dialog audio-divider-dialog))))
