
(if (not (provided? 'xm))
    (let ((hxm (dlopen "xm.so")))
      (if (string? hxm)
          (snd-error (format #f "nef.scm needs the xm module: ~A" hxm))
          (dlinit hxm "init_xm"))))

(if (not (defined? 'plugins-list))
    (load "plugins-menu.scm"))

(define sifter-size 100)
(define sifter-dialog #f)

(if (and (provided? 'snd-ladspa)
         (provided? 'xm))

    (add-to-menu
     plugins-menu "Sifter"
     (lambda ()
       (if (not (|Widget? sifter-dialog))
           (let ((sliders '()))
             (set! sifter-dialog
                   (make-effect-dialog "sifter ladspa plugin"
                                       (lambda (w context info)
                                         (apply-ladspa (make-sample-reader (cursor))
                                                       (list "sifter_1210.so" "sifter" sifter-size)
                                                       (- (frames) (cursor))
                                                       "sifter"))
                                       (lambda (w context info) (|XtUnmanageChild sifter-dialog))
                                       (lambda (w context info)
                                         (help-dialog "Sifter Help"
                                                      "Move the slider to set the sifter size."))
                                       (lambda (w c i)
                                         (set! sifter-size 100)
                                         (|XtSetValues (car sliders) (list |XmNvalue (inexact->exact (* sifter-size 1)))))))
             (set! sliders
                   (add-sliders sifter-dialog
                                (list (list "sifter size" 1 100 2000 
                                            (lambda (w context info)
                                              (set! sifter-size (/ (|value info) 1)))
                                            1))))))
       (activate-dialog sifter-dialog))))
