
(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
          (snd-error (format #f "nef.scm needs the xm module: ~A" hxm))
          (dlinit hxm "init_xm"))))

(if (not (defined? 'plugins-list))
    (load "plugins-menu.scm"))

(define ringmod-modulation-depth 0)
(define ringmod-frequency 100)
(define ringmod-sine-level 0)
(define ringmod-triangle-level 0)
(define ringmod-sawtooth-level 0)
(define ringmod-square-level 0)
(define ringmod-dialog #f)

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))

    (add-to-menu
     plugins-menu "Ring Modulation"
     (lambda ()
       (if (not (|Widget? ringmod-dialog))
           ;; if ringmod-dialog doesn't exist, create it
           (let ((sliders '()))
             (set! ringmod-dialog
                   (make-effect-dialog "ringmod ladspa plugin"
                                       (lambda (w context info)
                                         (apply-ladspa (make-sample-reader (cursor))
                                                       (list "ringmod_1188" "ringmod_1i1o1l" ringmod-modulation-depth ringmod-frequency ringmod-sine-level ringmod-triangle-level ringmod-sawtooth-level ringmod-square-level)
                                                       (- (frames) (cursor))
                                                       "ringmod"))
                                       (lambda (w context info) (|XtUnmanageChild ringmod-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Ring modulation Help"
                                                      "Ring or amplitude modulation with LFO.\n\ Move the sliders to set the ring modulation parameters.\n\ Modulation depth key: 0=none 1=AM 2=RM"))
                                       (lambda (w c i)
                                         (set! ringmod-modulation-depth 0)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* ringmod-modulation-depth 1))))
				         (set! ringmod-frequency 100)
				         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* ringmod-frequency 1))))
                                         (set! ringmod-sine-level 0)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* ringmod-sine-level 100))))
                                         (set! ringmod-triangle-level 0)
                                         (|XtSetValues (list-ref sliders 3) (list |XmNvalue (inexact->exact (* ringmod-triangle-level 100))))
                                         (set! ringmod-sawtooth-level 0)
                                         (|XtSetValues (list-ref sliders 4) (list |XmNvalue (inexact->exact (* ringmod-sawtooth-level 100))))
                                         (set! ringmod-square-level 0)
                                         (|XtSetValues (list-ref sliders 5) (list |XmNvalue (inexact->exact (* ringmod-square-level 100)))))))
             (set! sliders
                   (add-sliders ringmod-dialog
                                (list (list "modulation depth" 0 0 2
                                            (lambda (w context info)
                                              (set! ringmod-modulation-depth (/ (|value info) 1)))
                                            1)
                                      (list "frequency (Hz)" 1 100 1000
                                            (lambda (w context info)
                                              (set! ringmod-frequency (/ (|value info) 1)))
                                            1)
                                      (list "sine level" -1 0 1
                                            (lambda (w context info)
                                              (set! ringmod-sine-level (/ (|value info) 100)))
                                            100)
                                      (list "triangle level" -1 0 1
                                            (lambda (w context info)
                                              (set! ringmod-triangle-level (/ (|value info) 100)))
                                            100)
                                      (list "sawtooth level" -1 0 1
                                            (lambda (w context info)
                                              (set! ringmod-sawtooth-level (/ (|value info) 100)))
                                            100)
                                      (list "square level" -1 0 1
                                            (lambda (w context info)
                                              (set! ringmod-square-level (/ (|value info) 100)))
                                            100))))))
       (activate-dialog ringmod-dialog))))
