
(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
          (snd-error (format #f "nef.scm needs the xm module: ~A" hxm))
          (dlinit hxm "init_xm"))))

(if (not (defined? 'plugins-list))
    (load "plugins-menu.scm"))

(define fbdelay5s-time 1.0)
(define fbdelay5s-balance .5)
(define fbdelay5s-feedback 0)
(define fbdelay5s-dialog #f)

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))

    (add-to-menu
     plugins-menu "Feedback Delay (5s max)"
     (lambda ()
       (if (not (|Widget? fbdelay5s-dialog))
           ;; if fbdelay5s-dialog doesn't exist, create it
           (let ((sliders '()))
             (set! fbdelay5s-dialog
                   (make-effect-dialog "fbdelay_5s ladspa plugin"
                                       (lambda (w context info)
                                         (apply-ladspa (make-sample-reader (cursor))
                                                       (list "cmt" "fbdelay_5s" fbdelay5s-time fbdelay5s-balance fbdelay5s-feedback)
                                                       (- (frames) (cursor))
                                                       "fbdelay5s"))
                                       (lambda (w context info) (|XtUnmanageChild fbdelay5s-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Feedback Delay (5s) Help"
                                                      "Feedback delay with maximum delay time of 5 seconds."))
                                       (lambda (w c i)
                                         (set! fbdelay5s-time 1.0)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* fbdelay5s-time 100))))
				         (set! fbdelay5s-balance .5)
				         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* fbdelay5s-balance 100))))
                                         (set! fbdelay5s-feedback 0)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* fbdelay5s-feedback 100)))))))
             (set! sliders
                   (add-sliders fbdelay5s-dialog
                                (list (list "delay time (s)" 0.0 1.0 5.0
                                            (lambda (w context info)
                                              (set! fbdelay5s-time (/ (|value info) 100)))
                                            100)
                                      (list "balance" 0.0 0.5 1.0
                                            (lambda (w context info)
                                              (set! fbdelay5s-balance (/ (|value info) 100)))
                                            100)
                                      (list "feedback" -1 0 1
                                            (lambda (w context info)
                                              (set! fbdelay5s-feedback (/ (|value info) 100)))
                                            100))))))
       (activate-dialog fbdelay5s-dialog))))
