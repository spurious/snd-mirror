
(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
          (snd-error (format #f "nef.scm needs the xm module: ~A" hxm))
          (dlinit hxm "init_xm"))))

(if (not (defined? 'plugins-list))
    (load "plugins-menu.scm"))

(define transient-attack-speed 1)
(define transient-sustain-time 1)
(define transient-dialog #f)

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))

    (add-to-menu
     plugins-menu "Transient Mangler"
     (lambda ()
       (if (not (|Widget? transient-dialog))
           ;; if transient-dialog doesn't exist, create it
           (let ((sliders '()))
             (set! transient-dialog
                   (make-effect-dialog "transient-mangler ladspa plugin"
                                       (lambda (w context info)
                                         (apply-ladspa (make-sample-reader (cursor))
                                                       (list "transient_1206" "transient" transient-attack-speed transient-sustain-time)
                                                       (- (frames) (cursor))
                                                       "transient"))
                                       (lambda (w context info) (|XtUnmanageChild transient-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Transient mangler Help"
                                                      "Move the sliders to set the mangler parameters."))
                                       (lambda (w c i)
                                         (set! transient-attack-speed 1)
                                         (|XtSetValues (list-ref sliders 0) (list |XmNvalue (inexact->exact (* transient-attack-speed 100))))
                                         (set! transient-sustain-time 1)
                                         (|XtSetValues (list-ref sliders 1) (list |XmNvalue (inexact->exact (* transient-sustain-time 100)))))))
             (set! sliders
                   (add-sliders transient-dialog
                                (list (list "attack speed" -1 1 1
                                            (lambda (w context info)
                                              (set! transient-attack-speed (/ (|value info) 100)))
                                            100)
                                      (list "sustain time" -1 1 1
                                            (lambda (w context info)
                                              (set! transient-sustain-time (/ (|value info) 100)))
                                            100))))))
       (activate-dialog transient-dialog))))
