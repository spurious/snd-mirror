
(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
          (snd-error (format #f "nef.scm needs the xm module: ~A" hxm))
          (dlinit hxm "init_xm"))))

(if (not (defined? 'plugins-list))
    (load "plugins-menu.scm"))

(define gsm-simulator-mix .50)
(define gsm-simulator-passes 5)
(define gsm-simulator-error-rate 10)
(define gsm-simulator-dialog #f)

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))

    (add-to-menu
     plugins-menu "GSM Simulator"
     (lambda ()
       (if (not (|Widget? gsm-simulator-dialog))
           ;; if gsm-simulator-dialog doesn't exist, create it
           (let ((sliders '()))
             (set! gsm-simulator-dialog
                   (make-effect-dialog "gsm-simulator ladspa plugin"
                                       (lambda (w context info)
                                         (apply-ladspa (make-sample-reader (cursor))
                                                       (list "gsm_1215" "gsm" gsm-simulator-mix gsm-simulator-passes gsm-simulator-error-rate)
                                                       (- (frames) (cursor))
                                                       "gsm-simulator"))
                                       (lambda (w context info) (|XtUnmanageChild gsm-simulator-dialog))
                                       (lambda (w context info)
                                         (help-dialog "GSM Simulator Help"
                                                      "Move sliders to adjust parameters."))
                                       (lambda (w c i)
				         (set! gsm-simulator-mix .50) 
					 (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* gsm-simulator-mix 100))))
                                         (set! gsm-simulator-passes 5) 
					 (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* gsm-simulator-passes 1))))
                                         (set! gsm-simulator-error-rate 10)
                                         (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* gsm-simulator-error-rate 1)))))))
             (set! sliders
                   (add-sliders gsm-simulator-dialog
                                (list 
                                      (list "wet/dry mix" 0.0 .50 1.0
                                            (lambda (w context info)
                                              (set! gsm-simulator-mix (/ (|value info) 100)))
                                            100)
				      (list "number of passes" 0 5 10
                                            (lambda (w context info)
                                              (set! gsm-simulator-passes (/ (|value info) 1)))
                                            1)
                                      (list "error rate (bits/block)" 0 10 30
                                            (lambda (w context info)
                                              (set! gsm-simulator-error-rate (/ (|value info) 1)))
                                            1))))))
       (activate-dialog gsm-simulator-dialog))))
