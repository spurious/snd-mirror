
(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
          (snd-error (format #f "nef.scm needs the xm module: ~A" hxm))
          (dlinit hxm "init_xm"))))

(if (not (defined? 'plugins-list))
    (load "plugins-menu.scm"))

(define gs-density 5)
(define gs-scatter 1)
(define gs-grain-length 1)
(define gs-grain-attack 1)
(define gs-dialog #f)

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))

    (add-to-menu
     plugins-menu "Granular Scatter Processor"
     (lambda ()
       (if (not (|Widget? gs-dialog))
           (let ((sliders '()))
             (set! gs-dialog
                   (make-effect-dialog "granular scatter processor ladspa plugin"
                                       (lambda (w context info)
                                         (apply-ladspa (make-sample-reader (cursor))
                                                       (list "cmt" "grain_scatter" gs-density gs-scatter gs-grain-length gs-grain-attack)
                                                       (- (frames) (cursor))
                                                       "grain_scatter"))
                                       (lambda (w context info) (|XtUnmanageChild gs-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Granular Scatter Processor Help"
                                                      "Move the sliders to set the scatter parameters."))
                                       (lambda (w c i)
                                         (set! gs-density 5)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* gs-density 1))))
                                         (set! gs-scatter 1)
                                         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* gs-scatter 100))))
                                         (set! gs-grain-length 1)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* gs-grain-length 100))))
                                         (set! gs-grain-attack 1)
                                         (|XtSetValues (list-ref sliders 3) (list |XmNvalue (inexact->exact (* gs-grain-attack 100)))))))
             (set! sliders
                   (add-sliders gs-dialog
                                (list (list "density (grains/s)" 0 5 100
                                            (lambda (w context info)
                                              (set! gs-density (/ (|value info) 1)))
                                            1)
                                      (list "scatter" 0 1 5
                                            (lambda (w context info)
                                              (set! gs-scatter (/ (|value info) 100)))
                                            100)
 				      (list "grain length (s)" 0 1 5
                                            (lambda (w context info)
                                              (set! gs-grain-length (/ (|value info) 100)))
                                            100)
                                      (list "grain attack (s)" 0 1 5
                                            (lambda (w context info)
                                              (set! gs-grain-attack (/ (|value info) 100)))
                                            100))))))
       (activate-dialog gs-dialog))))
