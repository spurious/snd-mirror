
(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
          (snd-error (format #f "nef.scm needs the xm module: ~A" hxm))
          (dlinit hxm "init_xm"))))

(if (not (defined? 'plugins-list))
    (load "plugins-menu.scm"))

(define mvChorus-voices 3)
(define mvChorus-delay 10)
(define mvChorus-separation 1)
(define mvChorus-detune 0)
(define mvChorus-lfo 10)
(define mvChorus-attenuation 0)
(define mvChorus-dialog #f)

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))

    (add-to-menu
     plugins-menu "Chorus (multivoice)"
     (lambda ()
       (if (not (|Widget? mvChorus-dialog))
           ;; if mvChorus-dialog doesn't exist, create it
           (let ((sliders '()))
             (set! mvChorus-dialog
                   (make-effect-dialog "multivoiceChorus ladspa plugin"
                                       (lambda (w context info)
                                         (apply-ladspa (make-sample-reader (cursor))
                                                       (list "multivoice_chorus_1201" "multivoiceChorus" mvChorus-voices mvChorus-delay mvChorus-separation mvChorus-detune mvChorus-lfo mvChorus-attenuation)
                                                       (- (frames) (cursor))
                                                       "multivoiceChorus"))
                                       (lambda (w context info) (|XtUnmanageChild mvChorus-dialog))
                                       (lambda (w context info)
                                         (help-dialog "multivoiceChorus Help"
                                                      "Move the sliders to set the chorus parameters."))
                                       (lambda (w c i)
                                         (set! mvChorus-voices 3)
                                         (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* mvChorus-voices 1))))
                                         (set! mvChorus-delay 10)
                                         (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* mvChorus-delay 1))))
                                         (set! mvChorus-separation 1)
                                         (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* mvChorus-separation 10))))
                                         (set! mvChorus-detune 0)
                                         (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* mvChorus-detune 1))))
                                         (set! mvChorus-lfo 10)
                                         (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* mvChorus-lfo 1))))
         				 (set! mvChorus-attenuation 0)
				         (|XtSetValues (cadr sliders) (list |XmNvalue (inexact->exact (* mvChorus-attenuation 1)))))))+
             (set! sliders
                   (add-sliders mvChorus-dialog
                                (list (list "voices" 1 3 8
                                            (lambda (w context info)
                                              (set! mvChorus-voices (/ (|value info) 1)))
                                            1)
                                      (list "delay time (ms)" 10 10 40
                                            (lambda (w context info)
                                              (set! mvChorus-delay (/ (|value info) 1)))
                                            1)
 				      (list "voice separation (ms)" 0 1 2
                                            (lambda (w context info)
                                              (set! mvChorus-separation (/ (|value info) 10)))
                                            10)
                                      (list "detune (%)" 0 0 5
                                            (lambda (w context info)
                                              (set! mvChorus-detune (/ (|value info) 1)))
                                            1)
                                      (list "LFO frequency (Hz)" 2 10 30
                                            (lambda (w context info)
                                              (set! mvChorus-lfo (/ (|value info) 1)))
                                            1)
                                      (list "output attenuation (db)" -20 0 0
                                            (lambda (w context info)
                                              (set! mvChorus-attenuation (/ (|value info) 1)))
                                            1))))))
       (activate-dialog mvChorus-dialog))))
