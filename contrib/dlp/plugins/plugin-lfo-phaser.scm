
(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
          (snd-error (format #f "nef.scm needs the xm module: ~A" hxm))
          (dlinit hxm "init_xm"))))

(if (not (defined? 'plugins-list))
    (load "plugins-menu.scm"))

(define lfo-phaser-rate 10)
(define lfo-phaser-depth .5)
(define lfo-phaser-feedback 0)
(define lfo-phaser-spread 1)
(define lfo-phaser-dialog #f)

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))

    (add-to-menu
     plugins-menu "LFO Phaser"
     (lambda ()
       (if (not (|Widget? lfo-phaser-dialog))
           ;; if lfo-phaser-dialog doesn't exist, create it
           (let ((sliders '()))
             (set! lfo-phaser-dialog
                   (make-effect-dialog "lfo-phaser ladspa plugin"
                                       (lambda (w context info)
                                         (apply-ladspa (make-sample-reader (cursor))
                                                       (list "phasers_1217" "lfoPhaser" lfo-phaser-rate lfo-phaser-depth lfo-phaser-feedback lfo-phaser-spread)
                                                       (- (frames) (cursor))
                                                       "lfo-phaser"))
                                       (lambda (w context info) (|XtUnmanageChild lfo-phaser-dialog))
                                       (lambda (w context info)
                                         (help-dialog "LFO Phaser Help"
                                                      "Move sliders to adjust phaser parameters."))
                                       (lambda (w c i)
				         (set! lfo-phaser-rate 10) 
					 (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* lfo-phaser-rate 1))))
                                         (set! lfo-phaser-depth .5) 
					 (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* lfo-phaser-depth 100))))
                                         (set! lfo-phaser-feedback .5)
                                         (|XtSetValues (list-ref sliders 2) (list |XmNvalue (inexact->exact (* lfo-phaser-feedback 100))))
                                         (set! lfo-phaser-spread 1)
                                         (|XtSetValues (list-ref sliders 3) (list |XmNvalue (inexact->exact (* lfo-phaser-spread 1)))))))
             (set! sliders
                   (add-sliders lfo-phaser-dialog
                                (list 
                                      (list "LFO rate (Hz)" 1 10 100
                                            (lambda (w context info)
                                              (set! lfo-phaser-rate (/ (|value info) 1)))
                                            1)
				      (list "LFO depth" 0.0 .5 1.0
                                            (lambda (w context info)
                                              (set! lfo-phaser-depth (/ (|value info) 100)))
                                            100)
                                      (list "feedback" -1.0 .5 1.0
                                            (lambda (w context info)
                                              (set! lfo-phaser-feedback (/ (|value info) 100)))
                                            100)
                                      (list "spread" 0 1 2
                                            (lambda (w context info)
                                              (set! lfo-phaser-spread (/ (|value info) 1)))
                                            1))))))
       (activate-dialog lfo-phaser-dialog))))
