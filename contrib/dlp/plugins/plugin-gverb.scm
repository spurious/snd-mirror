;Ports:  "Roomsize (m)" input, control, 1 to 300
;        "Reverb time (s)" input, control, 0.1 to 360
;        "Damping" input, control, 0 to 1
;        "Input bandwidth" input, control, 0 to 1
;        "Dry signal level (dB)" input, control, -70 to 0
;        "Early reflection level (dB)" input, control, -70 to 0
;        "Tail level (dB)" input, control, -70 to 0
;        "Input" input, audio
;        "Left output" output, audio
;        "Right output" output, audio

(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
          (snd-error (format #f "nef.scm needs the xm module: ~A" hxm))
          (dlinit hxm "init_xm"))))

(if (not (defined? 'plugins-list))
    (load "plugins-menu.scm"))

(define gverb-roomsize 100)
(define gverb-reverb-time 10)
(define gverb-damping 1)
(define gverb-bandwidth 1)
(define gverb-dry-level 0)
(define gverb-early-ref-level 0)
(define gverb-tail-level 0)
(define gverb-dialog #f)

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))

    (add-to-menu
     plugins-menu "Gverb"
     (lambda ()
       (if (not (|Widget? gverb-dialog))
           (let ((sliders '()))
             (set! gverb-dialog
                   (make-effect-dialog "gverb ladspa plugin"
                                       (lambda (w context info)
                                         (apply-ladspa (make-sample-reader (cursor))
                                                       (list "gverb_1216.so" "gverb" gverb-roomsize gverb-reverb-time gverb-damping gverb-bandwidth gverb-dry-level gverb-early-ref-level gverb-tail-level)
                                                       (- (frames) (cursor))
                                                       "gverb"))
                                       (lambda (w context info) (|XtUnmanageChild gverb-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Gverb Help"
                                                      "Move the sliders to set the reverb parameters."))
                                       (lambda (w c i)
                                         (set! gverb-roomsize 100)
                                         (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* gverb-roomsize 100))))
                                         (set! gverb-reverb-time 10)
                                         (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* gverb-reverb-time 100))))
                                         (set! gverb-damping 1)
                                         (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* gverb-damping 100))))
                                         (set! gverb-bandwidth 1)
                                         (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* gverb-bandwidth 100))))
                                         (set! gverb-dry-level 0)
                                         (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* gverb-dry-level 0))))
                                         (set! gverb-early-ref-level 0)
                                         (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* gverb-early-ref-level 0))))
                                         (set! gverb-tail-level 0)
                                         (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* gverb-tail-level 0)))))))
             (set! sliders
                   (add-sliders gverb-dialog
                                (list (list "roomsize (m)" 1 100 300
                                            (lambda (w context info)
                                              (set! gverb-roomsize (/ (|value info) 1)))
                                            1)
                                      (list "reverb time (s)" 0.1 1 360.0 
                                            (lambda (w context info)
                                              (set! gverb-reverb-time (/ (|value info) 100)))
                                            100)
 				      (list "damping" 0 1 1
                                            (lambda (w context info)
                                              (set! gverb-damping (/ (|value info) 100)))
                                            100)
                                      (list "input bandwidth" 0 1 1
                                            (lambda (w context info)
                                              (set! gverb-bandwidth (/ (|value info) 100)))
                                            100)
                                      (list "dry level (db)" -70 0 0
                                            (lambda (w context info)
                                              (set! gverb-dry-level (/ (|value info) 1)))
                                            1)
                                      (list "early reflection level (db)" -70 0 0
                                            (lambda (w context info)
                                              (set! gverb-early-ref-level (/ (|value info) 1)))
                                            1)
                                      (list "tail level (db)" -70 0 0
                                            (lambda (w context info)
                                              (set! gverb-tail-level (/ (|value info) 1)))
                                            1))))))
       (activate-dialog gverb-dialog))))
