
(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
          (snd-error (format #f "nef.scm needs the xm module: ~A" hxm))
          (dlinit hxm "init_xm"))))

(if (not (defined? 'plugins-list))
    (load "plugins-menu.scm"))

(define canyon-delay-left-to-right-time .10)
(define canyon-delay-left-to-right-feedback 0)
(define canyon-delay-right-to-left-time .10)
(define canyon-delay-right-to-left-feedback 0)
(define canyon-delay-filter-cutoff 1000)
(define canyon-delay-dialog #f)

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))

    (add-to-menu
     plugins-menu "Canyon Delay (stereo)"
     (lambda ()
       (if (not (|Widget? canyon-delay-dialog))
           (let ((sliders '()))
             (set! canyon-delay-dialog
                   (make-effect-dialog "granular left-to-right-feedback processor ladspa plugin"
                                       (lambda (w context info)
					 (let* ((snd (selected-sound))
                                                       (chns (channels snd))
                                                       (readers (if (= chns 1)
                                                                    (make-sample-reader 0 snd 0)
                                                                    ;; assume stereo -- this could collect an arbitrary list
                                                                    (list (make-sample-reader 0 snd 0)
                                                                          (make-sample-reader 0 snd 1)))))
                                         (apply-ladspa readers
                                                       (list "cmt" "canyon_delay" canyon-delay-left-to-right-time canyon-delay-left-to-right-feedback canyon-delay-right-to-left-time canyon-delay-right-to-left-feedback canyon-delay-filter-cutoff)
                                                       (- (frames) (cursor))
                                                       "canyon delay")))
                                       (lambda (w context info) (|XtUnmanageChild canyon-delay-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Canyon Delay Help"
                                                      "This effect works only with stereo soundfiles.\s\ Move the sliders to set the delay parameters."))
                                       (lambda (w c i)
                                         (set! canyon-delay-left-to-right-time .10)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* canyon-delay-left-to-right-time 100))))
                                         (set! canyon-delay-left-to-right-feedback 0)
                                         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* canyon-delay-left-to-right-feedback 100))))
                                         (set! canyon-delay-right-to-left-time .10)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* canyon-delay-right-to-left-time 100))))
                                         (set! canyon-delay-right-to-left-feedback 0)
                                         (|XtSetValues (list-ref sliders 3) (list |XmNvalue (inexact->exact (* canyon-delay-right-to-left-feedback 100))))
                                         (set! canyon-delay-filter-cutoff 1000)
                                         (|XtSetValues (list-ref sliders 4) (list |XmNvalue (inexact->exact (* canyon-delay-filter-cutoff 1)))))))
             (set! sliders
                   (add-sliders canyon-delay-dialog
                                (list (list "left to right time (s)" 0.01 .10 .99
                                            (lambda (w context info)
                                              (set! canyon-delay-left-to-right-time (/ (|value info) 100)))
                                            100)
                                      (list "left to right feedback (%)" -1 0 1
                                            (lambda (w context info)
                                              (set! canyon-delay-left-to-right-feedback (/ (|value info) 100)))
                                            100)
 				      (list "right to left time (s)" 0.01 .10 .99
                                            (lambda (w context info)
                                              (set! canyon-delay-right-to-left-time (/ (|value info) 100)))
                                            100)
                                      (list "right to left feedback (%)" -1 0 1
                                            (lambda (w context info)
                                              (set! canyon-delay-right-to-left-feedback (/ (|value info) 100)))
                                            100)
                                      (list "low-pass filter cutoff (Hz)" 1 1000 44100
                                            (lambda (w context info)
                                              (set! canyon-delay-filter-cutoff (/ (|value info) 1)))
                                            1))))))
       (activate-dialog canyon-delay-dialog))))
